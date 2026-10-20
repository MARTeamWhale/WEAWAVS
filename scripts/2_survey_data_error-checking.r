################################################################################
#
# Script purpose: Error-checking of vessel survey effort and marine mammal sightings data
# Program: DFO Maritimes - Cetacean Research and Monitoring Program (CRMP)
# Project: Wind Energy Area Wildlife Assessment Vessel Surveys (WEAWAVS)
# Author: Katherine Gavrilchuk, with the assistance of Claude AI
# Affiliation: Fisheries and Oceans Canada
# Contact: katherine.gavrilchuk@dfo-mpo.gc.ca
# Last updated: August 19, 2026
# R version: 4.6.1
#
################################################################################

#----LOAD PACKAGES----

library(data.table)
library(tidyverse)
library(lubridate)
library(sf)
library(purrr)
library(geosphere)
library(rmarkdown)
library(leaflet)
library(stringr)


#----SET WORKING DIRECTORY & PATH TO WORKING DATA----

setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/")

# Specify dataset
dataset = "Fall2026" # Spring2026, Summer2026, Fall2026, Winter2027

# Define paths to each dataset
data_paths <- c(
  Spring2026 = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Spring2026_2026-05-12_FINAL_WORKING",
  Summer2026 = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Summer2026_2026-07-17_FINAL_WORKING",
  Fall2026   = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Fall2026_2026-09-29_FINAL_WORKING"
  # Winter2027 = "..."
)

path2data <- data_paths[[dataset]]


#----LOAD FUNCTIONS----

# Sequential plausibility filter: keep a fix only if it is reachable from the
# last KEPT fix at <= max_speed_kt (jumps < min_jump_km always allowed).
# Handles single-fix spikes AND two interleaved GPS streams (e.g. overlapping
# Garmin units with one wrong position). If nothing has been kept for
# reset_min minutes, the next fix is accepted so the filter can't lock up.
plausible_track <- function(lat, lon, t, max_speed_kt = 25, min_jump_km = 0.5, reset_min = 30) {
  n <- length(lat)
  keep <- logical(n)
  if (n == 0) return(keep)
  rad  <- pi / 180
  tnum <- as.numeric(t)
  keep[1] <- TRUE
  last <- 1
  for (i in seq_len(n)[-1]) {
    dt_h <- (tnum[i] - tnum[last]) / 3600
    a <- sin((lat[i] - lat[last]) * rad / 2)^2 +
      cos(lat[last] * rad) * cos(lat[i] * rad) * sin((lon[i] - lon[last]) * rad / 2)^2
    d_km <- 2 * 6371.0088 * asin(sqrt(a))
    if (d_km <= max(min_jump_km, max_speed_kt * 1.852 * dt_h) || dt_h * 60 > reset_min) {
      keep[i] <- TRUE
      last <- i
    }
  }
  keep
}


# A row gets suspicious_gps == TRUE if either of two things is true: 
# the lat/lon is missing (NA), or the position falls outside the study area's bounding box 
# defined with xmin/xmax (longitude range) and ymin/ymax (latitude range)
flag_and_replace_suspicious_gps <- function(gps_df, xmin, xmax, ymin, ymax, verbose = TRUE) {
  dt <- data.table::copy(as.data.table(gps_df))
  
  dt[, suspicious_gps :=
       vessel_lon < xmin |
       vessel_lon > xmax |
       vessel_lat < ymin |
       vessel_lat > ymax |
       is.na(vessel_lat) |
       is.na(vessel_lon)]
  
  # Save the original (pre-replacement) suspicious rows for review
  suspicious_gps_check <- dt[suspicious_gps == TRUE]
  
  # Reference table of valid GPS fixes only
  good_dt <- dt[suspicious_gps == FALSE, .(datetime_adt, vessel_lat, vessel_lon)]
  setkey(good_dt, datetime_adt)
  setkey(dt, datetime_adt)
  
  # Rolling join (nearest time match): replace suspicious lat/lon with the nearest valid
  # fix, and also record that fix's own timestamp (x.datetime_adt) so we can see how
  # far away in time the replacement actually came from
  dt[suspicious_gps == TRUE,
     c("vessel_lat", "vessel_lon", "replacement_datetime_adt") :=
       good_dt[.SD, on = "datetime_adt", roll = "nearest",
               .(vessel_lat, vessel_lon, replacement_datetime_adt = x.datetime_adt)] ]
  
  cleaned <- as_tibble(dt) |> select(-suspicious_gps, -replacement_datetime_adt)
  
  suspicious_gps_check[, reason := fifelse(
    is.na(vessel_lat) | is.na(vessel_lon), "missing", "out_of_bounds")]
  
  summary_by_source <- suspicious_gps_check[, .N, by = .(gps_source, reason)]
  
  # Row-for-row comparison of original vs. replacement values. Using bind_cols-style
  # alignment (not a join on datetime_adt) since dt[suspicious_gps == TRUE] preserves
  # the same row order before and after the rolling-join update -- this also avoids a
  # potential many-to-many mismatch if two suspicious rows happened to share a timestamp
  replaced_rows <- dt[suspicious_gps == TRUE]
  
  replacement_check <- tibble(
    gps_source        = suspicious_gps_check$gps_source,
    reason            = suspicious_gps_check$reason,
    orig_datetime_adt = suspicious_gps_check$datetime_adt,
    orig_lat          = suspicious_gps_check$vessel_lat,
    orig_lon          = suspicious_gps_check$vessel_lon,
    new_datetime_adt  = replaced_rows$replacement_datetime_adt,
    new_lat           = replaced_rows$vessel_lat,
    new_lon           = replaced_rows$vessel_lon
  ) |>
    mutate(time_diff_min = round(as.numeric(difftime(new_datetime_adt, orig_datetime_adt, units = "mins")), 2))
  
  if (verbose) {
    message(nrow(suspicious_gps_check), " suspicious GPS row(s) flagged and replaced.")
    print(summary_by_source)
  }
  
  list(
    cleaned = cleaned,
    suspicious_check = suspicious_gps_check,
    summary_by_source = summary_by_source,
    replacement_check = replacement_check
  )
}


# Convert lat lon in DD to lat, lon in degree minutes and within same cell
format_ddm <- function(lat, lon) {
  fmt_one <- function(x, pos_hemi, neg_hemi) {
    hemi <- ifelse(x >= 0, pos_hemi, neg_hemi)
    x <- abs(x)
    deg <- floor(x)
    mins <- round((x - deg) * 60, 1)
    carry <- mins >= 60
    deg <- deg + as.integer(carry)
    mins <- ifelse(carry, 0, mins)
    sprintf("%d %.1f  %s", deg, mins, hemi)
  }
  paste(fmt_one(lat, "N", "S"), fmt_one(lon, "E", "W"))
}

# Convert lat/lon DMM strings ("44 39.541 N") to decimal degrees
dmm_to_decimal <- function(x) {
  x    <- trimws(x)
  deg  <- as.numeric(str_extract(x, "^[0-9]+"))
  min  <- as.numeric(str_extract(x, "(?<=\\s)[0-9]+(\\.[0-9]+)?(?=\\s)"))
  hemi <- str_extract(x, "[NSEW]$")
  dd <- deg + min / 60
  dd[hemi %in% c("S", "W")] <- -dd[hemi %in% c("S", "W")]
  dd
}


# Append a QA/QC note to existing notes (or start a new note if none exists)
paste_na <- function(old, new) {
  if_else(is.na(old), new, paste(old, new, sep = "; "))
}


#----DEFINE STUDY AREA----

# V1
#study.bbox <- c(-63.907471, 43.771094, -58.864746, 47.055154)

# V2 (adapted after the Summer2026 survey since they had a distant SAR call)
study.bbox <- c(-65.742188, 41.508577, -52.163086, 47.279229)

xmin <- study.bbox[1]
ymin <- study.bbox[2]
xmax <- study.bbox[3]
ymax <- study.bbox[4]


#----LOAD PLANNED TRANSECTS----

# Planned transects (shapefile)
transects_shp <- st_read(file.path(paste0(getwd(), "/Survey design/WEAWAVS_2026-04-10_DFifield/Planned_transects_Scenario2_Spring2026.shp")), quiet = TRUE)

# Planned transects with metrics (csv)
transects_df <- read.csv(paste0(getwd(),"/Survey design/Scenario2_13-day_transects_with_metrics.csv"))

# Study area polygons (shapefile)
studyarea_shp <- st_read(file.path(paste0(getwd(), "/Survey design/WEAWAVS_2026-04-10_DFifield/Study_area_buf_prelim.shp")), quiet = TRUE)


#----LOAD GPS----

# (1) Mysticetus GPS (first GPS source; daytime only - when the computer was on)
mysti_files <- list.files(file.path(paste0(path2data, "/Mysti_GPS")), pattern = "csv$", full.names = TRUE)

# Combine Mysticetus GPS files
gps_mysti <- map_df(mysti_files, read_csv, show_col_types = FALSE) |>
  # Use floor_date to remove the milliseconds (which aren't necessarily displayed by R but will show up in excel, or if re-imported into R sometimes)
  mutate(datetime_utc = floor_date(as.POSIXct(`Time Created (UTC)`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), unit = "second")) |>
  # Use "_m" to denote it as coming from Mysticetus (just helps error-check after the GPS sources get joined)
  rename(vessel_lat_m = Latitude,
         vessel_lon_m = Longitude,
         sog_kt = `Speed Over Ground (kts)`,
         cog_t = `Course Over Ground (T)`) |>
  select(datetime_utc, vessel_lat_m, vessel_lon_m, sog_kt, cog_t) |>
  arrange(datetime_utc)

# (2) Garmin GPS (second GPS source; daytime and nighttime)
garmin_files <- list.files(file.path(paste0(path2data, "/Garmin_GPS")), pattern = "\\.gpx$", full.names = TRUE)

# Combine Garmin GPS files
gps_garmin <- map_df(garmin_files, function(f){
  df <- st_read(f, layer = "track_points", quiet = TRUE)
  coords <- st_coordinates(df)
  df |>
    mutate(
      datetime_utc = floor_date(as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), unit = "second"),
      vessel_lon_g = coords[,1],
      vessel_lat_g = coords[,2],
      garmin_file  = basename(f)) |>
    st_drop_geometry() |>
    select(datetime_utc, vessel_lat_g, vessel_lon_g, track_fid, track_seg_id, garmin_file)
}) |>
  # Remove track data before 2026-09-09
  filter(datetime_utc >= as.POSIXct("2026-09-09 00:00:00", tz = "UTC")) |>
  # Sort the FULL combined table across all files
  arrange(datetime_utc) |>
  # Remove any duplicate rows (from the overlap period of both Garmin devices)
  distinct() |>
  # Calculate speed over ground from consecutive GPS fixes
  group_by(garmin_file, track_fid, track_seg_id) |>
  mutate(dist_m = geosphere::distHaversine(
    cbind(vessel_lon_g, vessel_lat_g),
    cbind(lag(vessel_lon_g), lag(vessel_lat_g))),
    dt_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs")),
    sog_kt = if_else(dt_sec > 0, (dist_m / dt_sec) * 1.943844, NA_real_)) |>
  ungroup() |>
  select(datetime_utc, vessel_lat_g, vessel_lon_g, sog_kt, garmin_file)

# (3) Ship's NAV GPS (third GPS source; daytime and nighttime - used ONLY to
# fill gaps > 10 min left after combining Mysticetus + Garmin).
# Provided by Teleost crew at end of Spring2026 & Fall2026 surveys; not in Summer2026.
# NOTE: for Spring 2026: The Teleost's NAV GPS was also collected using a TeamWhale
# laptop with the Open GPN software, connected to NAV feed in Bridge; the program or
# laptop often crashed, so it's a patchy source of GPS data and is redundant with the
# previous GPS source, so not needed.

if (dataset == "Spring2026") {
  teleost_gps <- read.csv(file.path(path2data, "Teleost_NAV_GPS", "Teleost_VoyageLog20260512121847.csv"))
} else if (dataset == "Fall2026") {
  # CHECK: Spring file is in /Teleost_NAV_GPS, this one is at the top level -- intended?
  teleost_gps <- read.csv(file.path(path2data, "Teleost_VoyageLog20260930095635.csv"))
} else {
  teleost_gps <- NULL   # no ship NAV GPS for this dataset (e.g. Summer2026)
}


if (!is.null(teleost_gps)) {
  gps_teleost <- teleost_gps |>
    mutate(
      # ASSUMES UTC -- verify against Mysticetus at a time both were logging
      datetime_utc = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      vessel_lat_t = dmm_to_decimal(LAT),
      vessel_lon_t = dmm_to_decimal(LON),
      sog_kt = as.numeric(na_if(trimws(SOG.kn), "N/A")),
      cog_t  = as.numeric(na_if(trimws(COG.deg.T), "N/A"))) |>
    filter(!is.na(datetime_utc), !is.na(vessel_lat_t), !is.na(vessel_lon_t)) |>
    distinct(datetime_utc, .keep_all = TRUE) |>
    arrange(datetime_utc) |>
    select(datetime_utc, vessel_lat_t, vessel_lon_t, sog_kt, cog_t)
} else {
  # Empty placeholder so the code below runs unchanged for datasets without it
  gps_teleost <- tibble(datetime_utc = as.POSIXct(character(), tz = "UTC"),
                        vessel_lat_t = numeric(), vessel_lon_t = numeric(),
                        sog_kt = numeric(), cog_t = numeric())
}


#----COMBINE GPS DATA----

# Source priority: Mysticetus > Garmin > Teleost NAV.
# A lower-priority source is only used where the higher-priority ones have no
# fixes, so the track never interleaves receivers. 

fill_tol_sec    <- 60   # use Garmin only if no Mysticetus fix within 60 s
teleost_gap_min <- 5   # use Teleost only to fill gaps > 5 min

# Seconds from each time in `t` to the nearest time in `ref`
nearest_gap_sec <- function(t, ref) {
  ref <- sort(unique(as.numeric(ref)))
  t   <- as.numeric(t)
  i   <- findInterval(t, ref)
  prev_d <- ifelse(i > 0,           t - ref[pmax(i, 1)],              Inf)
  next_d <- ifelse(i < length(ref), ref[pmin(i + 1, length(ref))] - t, Inf)
  pmin(prev_d, next_d)
}

# Step 1: Mysticetus, plus Garmin only where Mysticetus is absent
gps_mysti1 <- gps_mysti |>
  filter(!is.na(datetime_utc)) |>
  distinct(datetime_utc, .keep_all = TRUE) |>    # duplicate Mysticetus rows
  transmute(datetime_utc,
            vessel_lat = vessel_lat_m, vessel_lon = vessel_lon_m,
            sog_kt, cog_t, gps_source = "mysticetus")

gps_garmin1 <- gps_garmin |>
  filter(!is.na(datetime_utc)) |>
  distinct(datetime_utc, .keep_all = TRUE) |>    # overlap of the two Garmin units
  filter(nearest_gap_sec(datetime_utc, gps_mysti1$datetime_utc) > fill_tol_sec) |>
  transmute(datetime_utc,
            vessel_lat = vessel_lat_g, vessel_lon = vessel_lon_g,
            sog_kt, cog_t = NA_real_, gps_source = "garmin")

gps_mg <- bind_rows(gps_mysti1, gps_garmin1) |>
  arrange(datetime_utc)

# Step 2: gaps > 5 min remaining after Mysticetus + Garmin
mg_gaps <- gps_mg |>
  mutate(gap_start = lag(datetime_utc),
         gap_min   = as.numeric(difftime(datetime_utc, gap_start, units = "mins"))) |>
  filter(gap_min > teleost_gap_min) |>
  transmute(gap_start, gap_end = datetime_utc, gap_min = round(gap_min, 1))

# Teleost fixes falling strictly inside those gaps
teleost_fill <- gps_teleost |>
  inner_join(mg_gaps, by = join_by(datetime_utc > gap_start, datetime_utc < gap_end)) |>
  transmute(datetime_utc,
            vessel_lat = vessel_lat_t, vessel_lon = vessel_lon_t,
            sog_kt, cog_t, gps_source = "teleost")

# Which gaps did Teleost fill?
mg_gaps |>
  left_join(teleost_fill |> select(datetime_utc) |>
              inner_join(mg_gaps, by = join_by(datetime_utc > gap_start, datetime_utc < gap_end)) |>
              count(gap_start, name = "n_teleost_fixes"),
            by = "gap_start") |>
  mutate(n_teleost_fixes = replace_na(n_teleost_fixes, 0L),
         gap_start_adt = with_tz(gap_start, "America/Halifax")) |>
  print(n = Inf)

# Step 3: final combined track
gps <- bind_rows(gps_mg, teleost_fill) |>
  arrange(datetime_utc) |>
  # Remove rows before the first Mysticetus reading (e.g. residual Garmin tracks)
  filter(datetime_utc >= min(datetime_utc[gps_source == "mysticetus"])) |>
  mutate(datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax")) |>
  select(datetime_utc, datetime_adt, vessel_lat, vessel_lon, sog_kt, cog_t, gps_source)

count(gps, gps_source)

# Export combined GPS track
#write.csv(gps, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_gps_combined.csv"),
#          row.names = FALSE)


#----GPS DATA: ERROR-CHECKING----

# (1) Missing or out-of-bounds positions -> DROPPED (not replaced).
# With a fix every few seconds, dropping a bad fix loses nothing; replacing it
# with the nearest good fix in time can copy a position from minutes/hours away.
gps_oob <- gps |>
  filter(is.na(vessel_lat) | is.na(vessel_lon) |
           vessel_lon < xmin | vessel_lon > xmax |
           vessel_lat < ymin | vessel_lat > ymax) |>
  mutate(reason = if_else(is.na(vessel_lat) | is.na(vessel_lon), "missing", "out_of_bounds"))

count(gps_oob, gps_source, reason)

gps <- gps |>
  anti_join(gps_oob, by = c("datetime_utc", "gps_source"))


# (2) Implausible jumps (spikes, interleaved GPS streams)
gps_spd <- gps |>
  arrange(datetime_utc) |>
  mutate(keep = plausible_track(vessel_lat, vessel_lon, datetime_utc,
                                max_speed_kt = 25, min_jump_km = 0.5, reset_min = 30))

# Review: rejected fixes per day and source
gps_rejected <- gps_spd |> filter(!keep)
gps_rejected |>
  count(date_adt = as_date(datetime_adt), gps_source)

gps <- gps_spd |>
  filter(keep) |>
  select(-keep)


# (3) Remaining gaps (after cleaning)
gps_gaps <- gps |>
  arrange(datetime_utc) |>
  mutate(
    row_id = row_number(),
    time_diff_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs")),
    gap_flag = time_diff_sec > 60)

table(gps_gaps$gap_flag)
summary(gps_gaps$time_diff_sec)

# Gaps > 5 min still outstanding
gps_gaps |>
  mutate(gap_start_adt = lag(datetime_adt)) |>
  filter(time_diff_sec > teleost_gap_min * 60) |>
  transmute(gap_start_adt,
            gap_end_adt  = datetime_adt,
            gap_min      = round(time_diff_sec / 60, 1),
            source_after = gps_source)

# Get row_ids of the flagged (post-gap) rows, then include the row immediately before each
gap_rows <- gps_gaps |> filter(gap_flag) |> pull(row_id)
gap_rows_context <- sort(unique(c(gap_rows - 1, gap_rows)))

gps_gaps1 <- gps_gaps |>
  filter(row_id %in% gap_rows_context) |>
  mutate(gap_id = cumsum(c(1, diff(row_id) > 1)))   # groups each pair together for easy viewing

gps_gaps1

# Visualize gaps in the GPS track
# (Esri basemap: CartoDB tiles now need an API key and show a watermark)
leaflet(data = gps_gaps) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addCircleMarkers(
    lng = ~vessel_lon, lat = ~vessel_lat,
    color = ~ifelse(gap_flag %in% TRUE, "red", "grey40"),
    radius = ~ifelse(gap_flag %in% TRUE, 5, 2),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "Time (ADT): ", datetime_adt,
      "<br>Source: ", gps_source,
      "<br>Gap: ", round(time_diff_sec, 1), " sec"))


# Inspect GPS tracks 

# (1) GPS Source = Mysticetus (day-time tracks only) - Tracks split by date

mysticetus_pts <- gps |>
  filter(gps_source == "mysticetus", !is.na(vessel_lat), !is.na(vessel_lon)) |>
  arrange(datetime_utc) |>
  mutate(track_date = as.Date(datetime_adt)) |>   # split by ADT date; switch to datetime_utc if preferred
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE)

mysticetus_lines <- mysticetus_pts |>
  group_by(track_date) |>
  summarise(do_union = FALSE) |>   # keep chronological order within each date
  st_cast("LINESTRING")

# Split into a named list -- one sf object per date, keyed by date label
mysticetus_by_date <- split(mysticetus_lines, mysticetus_lines$track_date)
date_labels <- names(mysticetus_by_date)   # character labels, e.g. "2026-04-23"

# Map: one polyline per date, each its own toggleable layer (all same color)
m_mysticetus <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron)

for (lbl in date_labels) {
  m_mysticetus <- m_mysticetus |>
    addPolylines(
      data = mysticetus_by_date[[lbl]],
      color = "dodgerblue",   # fixed color for all tracks
      weight = 2,
      opacity = 0.8,
      group = lbl)
}

m_mysticetus <- m_mysticetus |>
  addLayersControl(
    overlayGroups = date_labels,
    options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(date_labels[-1]) |>   # explicitly hide all but the first date by default
  addLegend("bottomright", colors = "dodgerblue", labels = "Mysticetus", title = "GPS Source")

m_mysticetus


# (2) GPS Source = Garmin (day-time tracks only) - Tracks split by date

garmin_pts <- gps |>
  filter(gps_source == "garmin", !is.na(vessel_lat), !is.na(vessel_lon)) |>
  arrange(datetime_utc) |>
  mutate(track_date = as.Date(datetime_adt)) |>   # split by ADT date; switch to datetime_utc if preferred
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE)

garmin_lines <- garmin_pts |>
  group_by(track_date) |>
  summarise(do_union = FALSE) |>   # keep chronological order within each date
  st_cast("LINESTRING")

# Split into a named list -- one sf object per date, keyed by date label
garmin_by_date <- split(garmin_lines, garmin_lines$track_date)
date_labels_garmin <- names(garmin_by_date)   # character labels, e.g. "2026-04-23"

# Map: one polyline per date, each its own toggleable layer (all same color)
m_garmin <- leaflet() |>
  addProviderTiles(providers$CartoDB.Positron)

for (lbl in date_labels_garmin) {
  m_garmin <- m_garmin |>
    addPolylines(
      data = garmin_by_date[[lbl]],
      color = "darkred",   # fixed color for all tracks
      weight = 2,
      opacity = 0.8,
      group = lbl)
}

m_garmin <- m_garmin |>
  addLayersControl(
    overlayGroups = date_labels_garmin,
    options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(date_labels_garmin[-1]) |>   # explicitly hide all but the first date by default
  addLegend("bottomright", colors = "darkred", labels = "Garmin", title = "GPS Source")

m_garmin


# (3) All tracks/dates combined (per GPS source)

# Build a line + point set for each source separately
build_source_data <- function(df, source_name) {
  pts <- df |>
    filter(gps_source == source_name, !is.na(vessel_lat), !is.na(vessel_lon)) |>
    arrange(datetime_utc) |>
    st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE)
  
  line <- pts |>
    summarise(do_union = FALSE) |>
    st_cast("LINESTRING")
  
  list(points = pts, line = line)
}

mysticetus_data <- build_source_data(gps, "mysticetus")
garmin_data     <- build_source_data(gps, "garmin")

# Mysticetus map 
leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolylines(data = mysticetus_data$line, color = "dodgerblue", weight = 2, opacity = 0.8) |>
  addLegend("bottomright", colors = "dodgerblue", labels = "Mysticetus", title = "GPS Source")

# Garmin map
leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolylines(data = garmin_data$line, color = "darkred", weight = 2, opacity = 0.8) |>
  addLegend("bottomright", colors = "darkred", labels = "Garmin", title = "GPS Source")


#----LOAD EFFORT----

# List all EffortEnv csv files
effort_files <- list.files(file.path(paste0(path2data, "/Mysti_data")),
                           pattern = "EffortEnv\\.csv$", 
                           recursive = TRUE, full.names = TRUE)


# Import & combine all EffortEnv files
effort <- effort_files |>
  map_df(~read_csv(.,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE)) 


#----EFFORT DATA: ERROR-CHECKING----

# De-duplicate rows, sort by the trusted "locked" timestamp, and
# flag missing / duplicated values in the editable Datetime_UTC column.
effort1 <- effort |>
  # Some rows were entirely duplicated; remove those
  distinct() |> 
  arrange(`Datetime_UTC_locked (UTC)`) |>
  mutate(
    # Flag any missing values (NA or blank) in the Datetime_UTC (UTC) column
    datetime_missing = is.na(`Datetime_UTC (UTC)`) | trimws(`Datetime_UTC (UTC)`) == "",
    # Flag duplicates datetimes in the `Datetime_UTC (UTC)`column: occurs when using the shortcut "Ctl-Shft-+" (which copies the previous row)
    datetime_duplicates = coalesce(`Datetime_UTC (UTC)` == lag(`Datetime_UTC (UTC)`), FALSE)) |>
  relocate(c(datetime_missing, datetime_duplicates))

table(effort1$datetime_missing)
table(effort1$datetime_duplicates)

# -----------------------------------------------------------------------
# MANUAL STEP (do this before proceeding):
# Verify the Notes for every row flagged TRUE above, to see if
# `Datetime_UTC (UTC)` was manually (and correctly) adjusted on purpose.
# Only proceed with the automated corrections below for rows that are
# genuine data-entry errors
# -----------------------------------------------------------------------

# Correct the datetime column
#   - datetime_missing == TRUE  -> fill from Datetime_UTC_locked (UTC)
#   - datetime_duplicates == TRUE -> replace with Datetime_UTC_locked (UTC)
# Keep the flags so corrected rows stay traceable.
effort2 <- effort1 |>
  mutate(`Datetime_UTC (UTC)` = case_when(
    datetime_missing    ~ `Datetime_UTC_locked (UTC)`, # fill missing values
    datetime_duplicates ~ `Datetime_UTC_locked (UTC)`, # correct duplicated-paste errors
    TRUE                ~ `Datetime_UTC (UTC)`))        # otherwise, keep as-is

# Additional data manipulation
effort3 <- effort2 |>
  mutate(# Create a datetime_utc column in the format "YYYY-MM-DD HH:MM:SS" (NOTE: for some reason, had to modify to include the miliseconds specification for this df)
    datetime_utc = floor_date(as.POSIXct(`Datetime_UTC (UTC)`, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), unit = "second"),
    # Convert to Atlantic time (with_tz = same instant, different zone)
    datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
    # Create date variable based on Atlantic time zone
    date_adt = as_date(datetime_adt)) |>
  arrange(datetime_adt) |>
  # Remove rows marked to be deleted (check them first before deleting).
  # NA-safe: a blank/NA Delete_row is treated as "keep", not silently dropped 
  filter(is.na(Delete_row) | Delete_row != "True") |>
  # Relocate to first columns
  relocate(c(datetime_utc, datetime_adt)) |>
  # GPS_pos = the vessel position; need to manipulate it to get lat and lon
  # in separate columns and as decimal degrees
  separate(GPS_pos, into = c("lat_deg", "lat_min", "lat_dir",
                             "lon_deg", "lon_min", "lon_dir"),
           sep = "\\s+", remove = FALSE) |>
  mutate(lat_deg = as.numeric(lat_deg),
         lat_min = as.numeric(lat_min),
         lon_deg = as.numeric(lon_deg),
         lon_min = as.numeric(lon_min),
         # Use "_e" to mark it as coming from Effort/Env (helps error-check
         # after the GPS sources get joined)
         vessel_lat_e = lat_deg + lat_min / 60,
         vessel_lon_e = lon_deg + lon_min / 60,
         vessel_lat_e = if_else(lat_dir == "S", -vessel_lat_e, vessel_lat_e),
         vessel_lon_e = if_else(lon_dir == "W", -vessel_lon_e, vessel_lon_e),
         gps_source = "effortenv") |>
  # Start and end of actual ("realized") daily effort times, where:
  # - start of daily effort = first datetime_adt of the day where
  #   Effort %in% c("Bridge-On", "On", "Transit-On")
  # - end of daily effort = the last datetime_adt of the day
  group_by(date_adt) |>
  mutate(start.effort_adt = {
    idx <- Effort %in% c("Bridge-On", "On", "Transit-On")
    if (any(idx, na.rm = TRUE)) {
      min(datetime_adt[idx], na.rm = TRUE)
    } else {
      as.POSIXct(NA, tz = "America/Halifax")
    }
  },
  end.effort_adt = max(datetime_adt, na.rm = TRUE)) |>
  ungroup() |>
  # Relocate new lat/lon columns to after GPS_pos and verify they are correct
  relocate(c(vessel_lat_e, vessel_lon_e), .after = GPS_pos) |>
  # Remove unnecessary columns (flag_* columns are intentionally KEPT)
  select(-c(datetime_missing, datetime_duplicates, lat_deg, lat_min, 
            lon_deg, lon_min, lat_dir, lon_dir,
            #Lock_row, 
            Delete_row)) |>
  # Relocate columns closer to start of df
  relocate(c(start.effort_adt, end.effort_adt), .after = `Datetime_UTC (UTC)`)


# Checks:

head(effort3)

# Transect_ID check against the reference list in transects_df
effort4 <- effort3 |>
  mutate(
    Transect_ID_orig = Transect_ID,  # keep original for reference
    Transect_ID = Transect_ID |> na_if("Na") |> na_if("na"),
    Transect_ID_flag = case_when(
      is.na(Transect_ID) ~ NA_character_,
      Transect_ID %in% transects_df$Transect_ID ~ "OK",
      TRUE ~ "REVIEW"))  # doesn't match any known transect ID

# Show the flagged row(s) with enough context to identify the correct Transect_ID
effort4 |>
  filter(Transect_ID_flag == "REVIEW") |>
  select(datetime_adt, Transect_ID_orig, Vessel, Effort)

# Suggest the closest valid Transect_ID using string distance, in case it's a typo
review_value <- effort4 |>
  filter(Transect_ID_flag == "REVIEW") |>
  pull(Transect_ID_orig) |>
  unique()

review_value

stringdist::stringdist(review_value, transects_df$Transect_ID) |>
  setNames(transects_df$Transect_ID) |>
  sort() |>
  head(5)

# Known corrections: map incorrect Transect_ID values to their correct counterpart
# add more corrections here as they're identified, e.g.: "WRONG_ID" = "CORRECT_ID"
transect_id_corrections <- c(
  "FBM37" = "FMB37",
  "S10" = "SB10")

# Re-check to verify any typos were resolved
effort4 <- effort4 |>
  mutate(
    Transect_ID = if_else(Transect_ID %in% names(transect_id_corrections),
                          transect_id_corrections[Transect_ID],
                          Transect_ID),
    Transect_ID_flag = case_when(
      is.na(Transect_ID) ~ NA_character_,
      Transect_ID %in% transects_df$Transect_ID ~ "OK",
      TRUE ~ "REVIEW"))

table(effort4$Transect_ID_flag)

# Inspect unique transect_IDs
unique(sort(effort4$Transect_ID))


#----JOIN GPS + EFFORT----

# Join gps1 to effort3
gps_effort <- gps |>
  full_join(effort4, by = "datetime_utc") |>
  arrange(datetime_utc) |>
  mutate(row_id = row_number()) |>   # stable key for re-joining spatial results later
  mutate(
    # Coalesce vessel_lat/vessel_lon as matched pairs, not independently --
    # if EITHER coordinate is missing from the gps columns, fall back to the
    # effort-side pair entirely, so we never mix a lat from one source with a lon from the other
    use_effort_pos = is.na(vessel_lat) | is.na(vessel_lon),
    vessel_lat = if_else(use_effort_pos, vessel_lat_e, vessel_lat),
    vessel_lon = if_else(use_effort_pos, vessel_lon_e, vessel_lon),
    # Coalesce datetime_adt from each df
    datetime_adt = coalesce(datetime_adt.x, datetime_adt.y),
    # Coalesce gps_source from each df
    gps_source = coalesce(gps_source.x, gps_source.y),
    # Create date variable based on Atlantic Daylight Time zone
    date_adt = as_date(datetime_adt)) |>
  select(-c(use_effort_pos, datetime_adt.x, datetime_adt.y,
            gps_source.x, gps_source.y))

# Checks: 

# (1) Erroneous GPS:
# Now that gps and effort dataframes have been joined, re-check whether there are any erroneous GPS positions
result_effort <- flag_and_replace_suspicious_gps(gps_effort, xmin, xmax, ymin, ymax)
gps_effort <- result_effort$cleaned
suspicious_gps_check_effort <- result_effort$suspicious_check
replacement_check_effort <- result_effort$replacement_check

#...Inspect to ensure the replacement of GPS positions makes sense!
# NOTE: For the Spring2026 data, the erroneous GPS positions correspond to the default vehicle location set in Mysticetus
# which was in the middle of Canada (to act as a flag to the user that a GPS fix hadn't been acquired yet via the GPS puck)

# (2) Missing coordinates:
# Confirm there are no rows with missing coordinates before spatial join
n_missing_coords <- gps_effort |>
  filter(is.na(vessel_lat) | is.na(vessel_lon)) |>
  nrow()

n_missing_coords

# Alternatively: inspect any missing-coordinates rows directly
gps_effort |>
  filter(is.na(vessel_lat) | is.na(vessel_lon))

# Assign 'strata' (FMB, SB, or NONE) based on whether a lat/lon falls within studyarea_shp$WEA

# Re-project the studyarea_shp to match that of the gps_effort lat/lon (EPSG: 4326 or the WGS 84 (World Geodetic System 1984))
studyarea_shp <- st_transform(studyarea_shp, crs = 4326)

strata_lookup <- gps_effort |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE) |>
  st_join(studyarea_shp |> select(WEA), join = st_within) |>
  st_drop_geometry() |>
  transmute(row_id, strata = case_when(WEA == "French/Middle Bank" ~ "FMB",
                                       WEA == "Sydney Bight"       ~ "SB",
                                       TRUE ~ "NONE"))

# Join the strata_lookup to the gps_effort
gps_effort <- gps_effort |>
  left_join(strata_lookup, by = "row_id") |>
  select(-row_id)

# Quick visualization check: Did the strata categories get properly defined?

# Map GPS points color-coded by strata
strata_pal <- colorFactor(
  palette = c("firebrick", "darkorange", "gray70"),
  levels = c("FMB", "SB", "NONE"))

leaflet(gps_effort) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = studyarea_shp,
    color = "black",
    weight = 1,
    fillOpacity = 0.05,
    label = ~WEA) |>
  addCircleMarkers(
    lng = ~vessel_lon,
    lat = ~vessel_lat,
    color = ~strata_pal(strata),
    radius = 2,
    stroke = FALSE,
    fillOpacity = 0.6) |>
  addLegend("bottomright", pal = strata_pal, values = ~strata, title = "Strata")

# Select columns of interest, fill gaps and 
# create variable for daily 'realized' vs 'available' time window for effort/operations
gps_effort1 <- gps_effort |>
  # Select columns of interest
  select(any_of(c("datetime_utc", "datetime_adt", "date_adt", "vessel_lat", "vessel_lon", "gps_source",
         "Vessel", "strata", "start.effort_adt", "end.effort_adt", "Effort", 
         "Action", "Transect_ID", "Port_obs", "Stb_obs", "BigEye_obs", "Data_recorder",
         "SBO", "Center_MMO","Stb_MMO", "Sea_state", "Swell", "Port_vis", "Stb_vis", "Precipitation",
         "Glare_int", "Glare_left", "Glare_right", "sog_kt", "cog_t", "Notes", "QAQC_notes"))) |>
  # Strip stray "names" attribute carried over from the join (shows as "Named chr" in str())
  mutate(Transect_ID = unname(Transect_ID)) |>
  arrange(datetime_adt) |>
  group_by(date_adt) |>
  # Fill the blanks downward (state/categorical columns only)
  fill(any_of(c("strata", "Effort", "start.effort_adt", "end.effort_adt",
                "Transect_ID", "Port_obs", "Stb_obs", "BigEye_obs", "Data_recorder",
                "Sea_state", "Swell", "Port_vis", "Stb_vis", "Precipitation", "Glare_int",
                "Glare_left", "Glare_right")), .direction = "down") |>
  # Fill daily bounds upward (captures rows before the first logged effort entry)
  fill(c(start.effort_adt, end.effort_adt), .direction = "up") |>
  # Fill in the Vessel name for all rows
  fill(Vessel, .direction = "downup") |>
  ungroup()
  
# TBD if keeping...
  # Replace remaining missing effort values and in_effort_window values
  # mutate(Effort = replace_na(Effort, "Off"),
  #        # Remove any Transect_IDs if in Transit-On effort
  #        Transect_ID = case_when(Effort == "Transit-On" ~ NA_character_,
  #                                TRUE ~ Transect_ID)) |>
  # Set certain columns to NA if Effort = Off
  # mutate(across(c(Transect_ID, Port_obs, Stb_obs, BigEye_obs, Data_recorder,
  #                 Sea_state, Swell, Port_vis, Stb_vis, Precipitation, Glare_int,
  #                 Glare_left, Glare_right), ~ replace(.x, Effort == "Off", NA)))

# Checks
summary(gps_effort1$vessel_lat)
summary(gps_effort1$vessel_lon)
unique(gps_effort1$date_adt)
unique(gps_effort1$Transect_ID)
table(gps_effort1$date_adt, gps_effort1$strata)

# Export
write.csv(gps_effort1, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_gps-effortenv.csv"),
          row.names = FALSE)


#----SIGHTINGS----

# List all Sighting csv files
sight_files <- list.files(file.path(paste0(path2data, "/Mysti_data")),
                           pattern = "Sighting\\.csv$", 
                           recursive = TRUE, full.names = TRUE)

# Import & combine all Sighting files
sight <- sight_files |>
  map_df(~read_csv(.,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE))

# Inspect any rows that were marked for deletion which did not get deleted
check <- sight |>
  filter(Delete_row == "True")
# Spring2026: 1 row marked for deletion with no explanation given in notes; out of caution, this row will be deleted.

# Data manipulation...
# De-duplicate rows, sort by the trusted "locked" timestamp, and
# flag missing / duplicated values in the editable Datetime_UTC column.
sight1 <- sight |>
  # Some rows were entirely duplicated (Summer2026); remove those
  distinct() |> 
  # Remove rows marked for deletion
  filter(Delete_row != "True") |>
  # Remove confirmed or possible resights (duplicates)
  filter(!Resight %in% c("Yes", "Possible")) |>
  # Flag any missing datetime values
  arrange(`Datetime_UTC_locked (UTC)`) |>
  mutate(# Flag any missing values (NA or blank) in the Datetime_UTC (UTC) column
         datetime_missing = is.na(`Datetime_UTC (UTC)`) | trimws(`Datetime_UTC (UTC)`) == "",
         # Flag any repeated datetimes in the `Datetime_UTC (UTC)`column; often due to using the shortcut "Ctl-Shft-+" which copies the previous row
         datetime_duplicates = coalesce(`Datetime_UTC (UTC)` == lag(`Datetime_UTC (UTC)`), FALSE)) |>
  relocate(c(datetime_missing, datetime_duplicates))

table(sight1$datetime_missing)
table(sight1$datetime_duplicates)
sight1 |> filter(datetime_duplicates == TRUE)

# -----------------------------------------------------------------------
# MANUAL STEP (do this before proceeding):
# Verify the Notes for every row flagged TRUE above, to see if
# `Datetime_UTC (UTC)` was manually (and correctly) adjusted on purpose.
# Only proceed with the automated corrections below for rows that are
# genuine data-entry errors.
# -----------------------------------------------------------------------

# Correct the duplicated datetime first, but keep the flags for now
sight2 <- sight1 |>
  mutate(`Datetime_UTC (UTC)` = if_else(datetime_duplicates, # If datetime_duplicates == TRUE,
                                        `Datetime_UTC_locked (UTC)`, # then replace with value from `Datetime_UTC_locked (UTC)`
                                        `Datetime_UTC (UTC)`)) # otherwise (if FALSE), keep the original `Datetime_UTC (UTC)`

sight3 <- sight2 |>
  # Rename columns that have parentheses in their name
  rename(PSD_m = "PSD (m)",
         Distance_m = "Distance (m)") |>
  mutate(# Create a datetime_utc column 
         datetime_utc = floor_date(as.POSIXct(`Datetime_UTC (UTC)`, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), unit = "second"),
         # Convert to Atlantic Daylight time
         datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"), # with_tz() = converts the same moment in time to another timezone
         # Create date variable based on Atlantic Daylight Time zone
         date_adt = as_date(datetime_adt),
         Min_count = as.numeric(Min_count),
         Best_count = as.numeric(Best_count),
         Max_count = as.numeric(Max_count),
         Angle_num = as.numeric(str_extract(Angle_rel, "\\d+")),
         PSD_m = as.numeric(PSD_m),
         Reticles = as.numeric(Reticles),
         Distance_m = as.numeric(Distance_m),
         Angle_abs = as.numeric(Angle_abs),
         # Reminder: The "Angle_abs" takes into account the vessel's heading (so it's normal that it is different compared to Angle_rel)
         # Set vessel (according to dataset)
         Vessel = case_when(dataset == "Summer2026" ~ "KopitHopson",
                            TRUE ~ "Teleost"),
         # Remove the parentheses, everything inside them & any extra space before the parentheses
         Obs_platform = trimws(gsub("\\s*\\(.*\\)", "", Obs_platform)),
         # Adjust Distance_tool entries
         Distance_tool = case_when(Distance_tool %in% c("Fujinon25x150", "BigEyes") ~ "BigEyes_Fujinon25x150",
                                   TRUE ~ Distance_tool)) |>
  # Format the lat/lon columns
  separate(GPS_pos, into = c("lat_deg", "lat_min", "lat_dir",
                             "lon_deg", "lon_min", "lon_dir"),
           sep = "\\s+", remove = FALSE) |>
  mutate(lat_deg = as.numeric(lat_deg),
         lat_min = as.numeric(lat_min),
         lon_deg = as.numeric(lon_deg),
         lon_min = as.numeric(lon_min),
         vessel_lat = lat_deg + lat_min / 60,
         vessel_lon = lon_deg + lon_min / 60,
         vessel_lat = if_else(lat_dir == "S", -vessel_lat, vessel_lat),
         vessel_lon = if_else(lon_dir == "W", -vessel_lon, vessel_lon)) |>
  # Coalesce lat/lon columns
  mutate(Sgt_lat = as.numeric(Sgt_lat),
         Sgt_lon = as.numeric(Sgt_lon),
         Sgt_lat = coalesce(Sgt_lat, vessel_lat),
         Sgt_lon = coalesce(Sgt_lon, vessel_lon)) |>
  # Re-name Species for those which were dead
  mutate(Species = if_else(!is.na(Notes) & str_detect(Notes, regex("dead", ignore_case = TRUE)),
                           paste0(Species, " (dead)"), 
                           Species),
         Sgt_type = case_when(Species %in% c("Mola mola", "White shark", "Leatherback turtle", 
                                             "Unknown shark", "Other", "Other (dead)") ~ "Incidental", 
                              TRUE ~ Sgt_type)) |>
  # Remove columns no longer needed
  select(-c(lat_deg, lat_min, lon_deg, lon_min, lat_dir, lon_dir, Delete_row,
            datetime_missing, datetime_duplicates)) |>
  # Select to arrange column order
  select(any_of(c("Vessel","Datetime_UTC_locked (UTC)", "Datetime_UTC (UTC)", 
                  "datetime_utc", "datetime_adt", "date_adt", "vessel_lat", "vessel_lon", 
                  "Sgt_lat", "Sgt_lon", "Sgt_type", "Sgt_side", "Observer", "MMO", 
                  "Obs_platform", "Distance_tool", "Reticles", "Distance_m", "Angle_rel", 
                  "Angle_num", "Angle_abs", "Distance_tool", "PSD_m", "Species", 
                  "Min_count", "Best_count", "Max_count", "Resight", "Photos_taken", 
                  "Camera", "Frame_first", "Frame_last", "Notes", "QAQC_notes",
                  "datetime_missing", "datetime_duplicates"))) |>
  # Stable key for re-joining spatial results later
  mutate(row_id = row_number())

# Assign 'strata' (FMB, SB, or NONE) based on whether a lat/lon falls within studyarea_shp$WEA
  
# Re-project the studyarea_shp to match that of the gps_effort lat/lon (EPSG: 4326 or the WGS 84 (World Geodetic System 1984))
studyarea_shp <- st_transform(studyarea_shp, crs = 4326)

strata_lookup <- sight3 |>
   st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE) |>
   st_join(studyarea_shp |> select(WEA), join = st_within) |>
   st_drop_geometry() |>
   transmute(row_id, strata = case_when(WEA == "French/Middle Bank" ~ "FMB",
                                        WEA == "Sydney Bight"       ~ "SB",
                                        TRUE ~ "NONE"))
  
# Join the strata_lookup to sight1
sight3 <- sight3 |>
  left_join(strata_lookup, by = "row_id") |>
  select(-row_id)

# Checks

# Group_size
sight3 <- sight3 |>
  mutate(
    Min_count_blank  = is.na(Min_count),
    Best_count_blank = is.na(Best_count),
    Max_count_blank  = is.na(Max_count),
    flag_group_size_all_missing = Min_count_blank & Best_count_blank & Max_count_blank,
    flag_group_size_review = (Min_count_blank | Best_count_blank | Max_count_blank) & !flag_group_size_all_missing,
    Min_count  = if_else(flag_group_size_all_missing, 1, Min_count),
    Best_count = if_else(flag_group_size_all_missing, 1, Best_count),
    Max_count  = if_else(flag_group_size_all_missing, 1, Max_count),
    QAQC_notes = case_when(
      flag_group_size_review & is.na(QAQC_notes)  ~ "Flag for manual review: Min_count, Best_count, and/or Max_count missing",
      flag_group_size_review & !is.na(QAQC_notes) ~ paste(QAQC_notes, "Flag for manual review: Min_count, Best_count, and/or Max_count missing", sep = "; "),
      flag_group_size_all_missing & is.na(QAQC_notes)  ~ "Min_count, Best_count, and Max_count all missing; assumed group size = 1 for each",
      flag_group_size_all_missing & !is.na(QAQC_notes) ~ paste(QAQC_notes, "Min_count, Best_count, and Max_count all missing; assumed group size = 1 for each", sep = "; "),
      TRUE ~ QAQC_notes)) |>
  select(-Min_count_blank, -Best_count_blank, -Max_count_blank)


# Verify Reticle values

# Fujinon 7x50 max number of reticles = 13; check if there are rows where Reticles > 13 (these would be data entry errors)
sight3 |>
  filter(Distance_tool == "Fujinon7x50" & Reticles > 13)

# Fujinon 25x150 (Big Eyes) max number of reticles = [TO FILL IN] 
#sight3 |>
#  filter(Distance_tool == "BigEyes_Fujinon25x150" & Reticles > [TO FILL IN])

new_note <- "KG 20260907: Reticles exceed max possible value; set Reticles and Distance to NA and change to Sgt_type = Incidental"

sight3 <- sight3 |>
  mutate(
    flag_fujinon_reticle_error = Distance_tool == "Fujinon7x50" & Reticles > 13,
    QAQC_notes = case_when(
      flag_fujinon_reticle_error & is.na(QAQC_notes)  ~ new_note,
      flag_fujinon_reticle_error & !is.na(QAQC_notes) ~ paste(QAQC_notes, new_note, sep = "; "),
      TRUE ~ QAQC_notes),
    Distance_m = case_when(
      flag_fujinon_reticle_error ~ NA_real_,
      TRUE ~ Distance_m),
    Reticles = case_when(
      flag_fujinon_reticle_error ~ NA_real_,
      TRUE ~ Reticles),
    PSD_m = case_when(
      flag_fujinon_reticle_error ~ NA_real_,
      TRUE ~ PSD_m),
    Sgt_type = case_when(
      flag_fujinon_reticle_error ~ "Incidental",
      TRUE ~ Sgt_type)) |>
  select(-flag_fujinon_reticle_error)  


# Verify Distance values make sense
# TO DO: Fill in max expected distance
summary(sight3$Distance_m)

# Verify the Sgt_type is all filled in
table(sight3$Sgt_type)

# Verify that all "On-effort" sightings have necessary information
x <- sight3 |>
  filter(Sgt_type == "On-effort") |>
  select(datetime_adt, Sgt_type, Species, Reticles, Distance_m, Distance_tool,
         Angle_rel, Angle_num, Angle_abs, PSD_m)
# Spring2026: On-effort Sighting of a grey seal at 2026-04-30T17:02:30.9 utc is missing distance;
# TO DO: Check in the Mysti auto-saves to see if Distance was entered at one point but accidentally deleted;
# if can't find distance, this sighting will have to be changed to Sgt_type = "incidental"

# Reproduce Angle_abs calculation that Mysticetus does (to confirm it's accurate)
# [TO DO...]

# Reproduce distance_m calculation that Mysticetus does (to confirm it's accurate)
# [TO DO...]

# Verify the Angle_abs (calculation) for the BigEyes sightings
# [TO DO] Might need to convert the BigEyes angles to +/- 90 degrees

# Verify Species ID
unique(sight3$Species)
table(sight3$Species)

# Manually verify the rows which contain Species == "Other"
y <- sight3 |> filter(grepl("Other", Species))
# Unless it was a legitimate MM species which was specified in the notes, 
# these sightings can be excluded from summaries

# Export

# (1) Marine mammal species only
sight4 <- sight3 |>
  # Remove non-MM species
  filter(!Species %in% c("Mola mola", "White shark", "Leatherback turtle", 
                         "Unknown shark", "Other", "Other (dead)")) |>
  # Adjust the Sgt_lat & Sgt_lon
  mutate(lat = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lat,
                         TRUE ~ vessel_lat),
         lon = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lon,
                         TRUE ~ vessel_lon)) |>
  relocate(c(lat, lon), .after = "Sgt_lon")

unique(sight4$Species)
table(sight4$Species)
# Compare to original data entered:
table(sight$Species)

# Export
write.csv(sight4, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_MM_sightings.csv"),
          row.names = FALSE)

# (2) All sightings (except "Other") to provide to DFO MAR's Whale Sightings Database (WSDB)
# (also, remove unnecessary columns and add a couple of helpful columns to identify data collector & survey)
sight5 <- sight3 |>
  filter(!Species %in% c("Other", "Other (dead)")) |>
  mutate(Data_collector = "DFO_Maritimes_CRMP",
         Survey = paste0("WEAWAVS_", dataset)) |>
  select(any_of(c("Data_collector", "Survey", "Vessel", "datetime_utc", 
                  "datetime_adt", "vessel_lat", "vessel_lon", "Sgt_lat", "Sgt_lon", 
                  "Sgt_type", "MMO", "Observer", "Species", "Min_count", "Best_count", "Max_count",
                  "Photos_taken", "Camera", "Frame_first", "Frame_last", "Notes", "PSD_m"))) |>
  mutate(lat = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lat,
                       TRUE ~ vessel_lat),
         lon = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lon,
                       TRUE ~ vessel_lon)) |>
  relocate(c(lat, lon, PSD_m), .after = "Sgt_lon")

#...............................................................................
# MANUAL VERIFICATION: Make sure the correct lat/lon were assigned to each 
# sighting before removing unnecessary columns 
#...............................................................................

sight6 <- sight5 |>
  select(-c(vessel_lat, vessel_lon, Sgt_lat, Sgt_lon, PSD_m))

# Export
write.csv(sight6, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_Sightings_WSDB.csv"),
          row.names = FALSE)


#----GPS + EFFORT + MM SIGHTINGS----

# Join gps_effort1 to sight3 (to have the relevant effort/env variables associated with the sightings) 
gps_effort1_j <- gps_effort1 |> 
  ungroup() |>                                   
  rename(gps_datetime_adt = datetime_adt,
         gps_datetime_utc = datetime_utc,
         gps_lat           = vessel_lat,
         gps_lon           = vessel_lon) |>
  select(-(strata))

sight_gps_effort <- sight4 |> 
  left_join(gps_effort1_j,
            join_by(Vessel == Vessel,
                    closest(datetime_adt >= gps_datetime_adt))) |> 
  mutate(time_diff_sec  = as.numeric(datetime_adt - gps_datetime_adt, units = "secs"),
         # time_diff_flag: TRUE if the matched GPS fix is more than 10 seconds away from
         # the sighting's recorded datetime_adt, OR if no GPS fix could be matched at all
         # (time_diff_sec is NA — e.g. the sighting occurred before any GPS logging that day).
         # Use this to QA the join: rows flagged TRUE either have a suspiciously stale
         # position fix, or are missing a GPS match entirely and need manual review.
         time_diff_flag = is.na(time_diff_sec) | time_diff_sec > 10,
         Notes = coalesce(Notes.x, Notes.y),
         date_adt = coalesce(date_adt.x, date_adt.y),
         QAQC_notes = coalesce(QAQC_notes.x, QAQC_notes.y)) |>
  select(-c(Notes.x, Notes.y, date_adt.x, date_adt.y,
            QAQC_notes.x, QAQC_notes.y))

summary(sight_gps_effort$time_diff_sec)
sum(sight_gps_effort$time_diff_flag, na.rm = TRUE)
  
# Export
write.csv(sight_gps_effort, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_MM_sightings_gps_effort.csv"),
          row.names = FALSE)


#### Flag for analysis stage:
# Verify that vessel speed is  >= 5kt while "On" or "Bridge-On" or "Transit-On" effort


