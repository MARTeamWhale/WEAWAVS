################################################################################
#
# Script purpose: Error-checking of vessel survey effort and marine mammal sightings data
# Program: DFO Maritimes - Cetacean Research and Monitoring Program (CRMP)
# Project: Wind Energy Area Wildlife Assessment Vessel Surveys (WEAWAVS)
# Author: Katherine Gavrilchuk, with the assistance of Claude AI
# Affiliation: Fisheries and Oceans Canada
# Contact: katherine.gavrilchuk@dfo-mpo.gc.ca
# Last updated: Oct 20, 2026
# R version: 4.6.1
#
################################################################################

#----LOAD PACKAGES----

library(tidyverse)
library(sf)
library(geosphere)
library(leaflet)


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

# Seconds from each time in `t` to the nearest time in `ref`
nearest_gap_sec <- function(t, ref) {
  ref <- sort(unique(as.numeric(ref)))
  t   <- as.numeric(t)
  i   <- findInterval(t, ref)
  prev_d <- ifelse(i > 0,           t - ref[pmax(i, 1)],              Inf)
  next_d <- ifelse(i < length(ref), ref[pmin(i + 1, length(ref))] - t, Inf)
  pmin(prev_d, next_d)
}


# GPS cleaning: drop missing / out-of-bounds positions, then implausible jumps
# (spikes, interleaved Garmin streams). Returns the cleaned track plus what was
# removed, so each stage can be reviewed.
clean_gps <- function(df, xmin, xmax, ymin, ymax) {
  oob <- df |>
    filter(is.na(vessel_lat) | is.na(vessel_lon) |
             vessel_lon < xmin | vessel_lon > xmax |
             vessel_lat < ymin | vessel_lat > ymax) |>
    mutate(reason = if_else(is.na(vessel_lat) | is.na(vessel_lon), "missing", "out_of_bounds"))
  
  checked <- df |>
    anti_join(oob, by = c("datetime_utc", "gps_source")) |>
    arrange(datetime_utc) |>
    mutate(keep = plausible_track(vessel_lat, vessel_lon, datetime_utc,
                                  max_speed_kt = 25, min_jump_km = 0.5, reset_min = 30))
  
  list(
    cleaned  = checked |> filter(keep) |> select(-keep),
    oob      = oob,
    rejected = checked |> filter(!keep) |> select(-keep)
  )
}

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
  # Remove track data more than a day before the first Mysticetus fix
  filter(datetime_utc >= min(gps_mysti$datetime_utc, na.rm = TRUE) - days(1)) |>
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
# NOTE:
# Garmin rows have cog_t = NA. 
# If we ever need a course for those stretches, use geosphere::bearing() between consecutive fixes to calculate it.

# (3) Ship's NAV GPS (third GPS source; daytime and nighttime - used ONLY to
# fill gaps > 5 min left after combining Mysticetus + Garmin).
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

fill_tol_sec    <- 20   # use Garmin only if no Mysticetus fix within 20 s
teleost_gap_min <- 5    # use Teleost only to fill gaps > 5 min


# STEP 1: Mysticetus + Garmin (Garmin only where Mysticetus is absent) -> gps_mg

gps_mysti1 <- gps_mysti |>
  filter(!is.na(datetime_utc)) |>
  distinct(datetime_utc, .keep_all = TRUE) |>    # duplicate Mysticetus rows
  transmute(datetime_utc,
            vessel_lat = vessel_lat_m, vessel_lon = vessel_lon_m,
            sog_kt, cog_t, gps_source = "mysticetus")

gps_garmin1 <- gps_garmin |>
  filter(!is.na(datetime_utc)) |>
  distinct(datetime_utc, .keep_all = TRUE) |>    # exact-second overlap of the two Garmin units
  filter(nearest_gap_sec(datetime_utc, gps_mysti1$datetime_utc) > fill_tol_sec) |>
  transmute(datetime_utc,
            vessel_lat = vessel_lat_g, vessel_lon = vessel_lon_g,
            sog_kt, cog_t = NA_real_, gps_source = "garmin")

gps_mg <- bind_rows(gps_mysti1, gps_garmin1) |>
  arrange(datetime_utc) |>
  # Remove rows before the first Mysticetus reading (e.g. residual Garmin tracks)
  filter(datetime_utc >= min(datetime_utc[gps_source == "mysticetus"])) |>
  mutate(datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax")) |>
  select(datetime_utc, datetime_adt, vessel_lat, vessel_lon, sog_kt, cog_t, gps_source)


# STEP 2: Clean Mysticetus + Garmin -> gps_mg_clean

mg_check     <- clean_gps(gps_mg, xmin, xmax, ymin, ymax)
gps_mg_clean <- mg_check$cleaned

# Review: what was removed
count(mg_check$oob, gps_source, reason)
count(mg_check$rejected, date_adt = as_date(datetime_adt), gps_source)


# STEP 3: Teleost time-zone check (needs gps_mysti1, so it sits here)
# The plausibility filter won't catch a 3-hour error inside a long night gap
# (over 5 hours it allows jumps of ~230 km). This compares Teleost positions
# against Mysticetus at matching times for three candidate offsets; the offset
# with the smallest median distance (tens of metres) is the right one.
# If shift_h = 0 wins, UTC is correct. If +3 wins, read the Teleost file with
# tz = "America/Halifax" instead.

if (nrow(gps_teleost) > 0) {
  tz_check <- map_df(c(-3, 0, 3), function(h) {
    gps_teleost |>
      mutate(t_time = datetime_utc + hours(h)) |>
      select(t_time, vessel_lat_t, vessel_lon_t) |>
      inner_join(gps_mysti1 |> select(datetime_utc, vessel_lat, vessel_lon),
                 by = join_by(closest(t_time >= datetime_utc))) |>
      filter(as.numeric(difftime(t_time, datetime_utc, units = "secs")) <= 15) |>
      summarize(shift_h = h, n_matched = n(),
                median_offset_m = median(distHaversine(cbind(vessel_lon_t, vessel_lat_t),
                                                       cbind(vessel_lon, vessel_lat))))
  })
  print(tz_check)
}


# STEP 4: Gaps > 5 min in the CLEANED Mysticetus + Garmin track -> mg_gaps

mg_gaps <- gps_mg_clean |>
  mutate(gap_start = lag(datetime_utc),
         gap_min   = as.numeric(difftime(datetime_utc, gap_start, units = "mins"))) |>
  filter(gap_min > teleost_gap_min) |>
  transmute(gap_start, gap_end = datetime_utc, gap_min = round(gap_min, 1))


# STEP 5: Teleost fixes falling strictly inside those gaps -> teleost_fill

teleost_fill <- gps_teleost |>
  inner_join(mg_gaps, by = join_by(datetime_utc > gap_start, datetime_utc < gap_end)) |>
  transmute(datetime_utc,
            datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
            vessel_lat = vessel_lat_t, vessel_lon = vessel_lon_t,
            sog_kt, cog_t, gps_source = "teleost")

# Which gaps did Teleost fill?
mg_gaps |>
  left_join(teleost_fill |>
              select(datetime_utc) |>
              inner_join(mg_gaps, by = join_by(datetime_utc > gap_start, datetime_utc < gap_end)) |>
              count(gap_start, name = "n_teleost_fixes"),
            by = "gap_start") |>
  mutate(n_teleost_fixes = replace_na(n_teleost_fixes, 0L),
         gap_start_adt   = with_tz(gap_start, "America/Halifax")) |>
  print(n = Inf)


# STEP 6: FINAL track = cleaned Mysticetus/Garmin + Teleost fill, re-checked -> gps
# (second cleaning pass catches any bad Teleost fixes or jumps at the joins)

final_check <- clean_gps(bind_rows(gps_mg_clean, teleost_fill), xmin, xmax, ymin, ymax)
gps <- final_check$cleaned

# Review: should be (nearly) empty
count(final_check$oob, gps_source, reason)
count(final_check$rejected, date_adt = as_date(datetime_adt), gps_source)

count(gps, gps_source)


#----GPS DATA: REMAINING GAPS (final track)----

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
            source_after = gps_source) |>
  print(n = Inf)

# Gaps > 1 min with the fix before each, for easy viewing
gap_rows <- gps_gaps |> filter(gap_flag) |> pull(row_id)
gap_rows_context <- sort(unique(c(gap_rows - 1, gap_rows)))

gps_gaps1 <- gps_gaps |>
  filter(row_id %in% gap_rows_context) |>
  mutate(gap_id = cumsum(c(1, diff(row_id) > 1)))   # groups each pair together

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

# Export combined GPS track
write.csv(gps, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_gps_combined.csv"),
          row.names = FALSE)


# View/inspect GPS tracks 

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
  addProviderTiles(providers$Esri.OceanBasemap)

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


# (2) GPS Source = Garmin (day- and night-time tracks) - Tracks split by date

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
  addProviderTiles(providers$Esri.OceanBasemap)

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





#----EFFORT DATA: ERROR-CHECKING----

#----EFFORT: LOAD----

# List all EffortEnv csv files (one per day / export)
effort_files <- list.files(file.path(path2data, "Mysti_data"),
                           pattern = "EffortEnv\\.csv$",
                           recursive = TRUE, full.names = TRUE)

# Read every column as character so files from different days/surveys
# always combine cleanly; types are converted explicitly further down.
# source_file records which file each row came from (useful when reviewing).
effort_raw <- effort_files |>
  map_df(~ read_csv(.x,
                    col_types = cols(.default = col_character()),
                    show_col_types = FALSE) |>
           mutate(source_file = basename(.x)))

nrow(effort_raw)


#----EFFORT STEP 1: STANDARDIZE COLUMN NAMES ACROSS SURVEYS----

# Column names changed between surveys. Old names are renamed to the current
# (Fall2026) names, so the rest of the workflow only ever sees one set.
#
# Observer columns by survey:
#   Spring2026: Port_obs, Stb_obs, BigEye_obs, Indep_obs   
#   Summer2026: SBO, Center_MMO, Stb_MMO,            
#   Fall2026:   Port_MMO, Stb_MMO, BigEyes_MMO
# Spring's Port_obs/Stb_obs/BigEye_obs and Summer's Center_MMO are renamed to
# their Fall equivalents. Summer's SBO and Spring's Indep_obs have no Fall
# equivalent, so they are kept under their own names (not used anywhere in
# this workflow).
# Two separate renames, because two different old names (Port_obs and
# Center_MMO) both map to Port_MMO.
effort_name_map <- c(
  Port_MMO    = "Port_obs",     # Spring2026
  Stb_MMO     = "Stb_obs",      # Spring2026
  BigEyes_MMO = "BigEye_obs"    # Spring2026
)
effort_name_map_summer <- c(
  Port_MMO    = "Center_MMO"    # Summer2026
)

effort_std <- effort_raw |>
  rename(any_of(effort_name_map)) |>
  rename(any_of(effort_name_map_summer))


# Columns the workflow cannot run without -> stop with a clear message
effort_required <- c("Datetime_UTC_locked (UTC)", "Datetime_UTC (UTC)", "GPS_pos", "Effort")
missing_required <- setdiff(effort_required, names(effort_std))
if (length(missing_required) > 0) {
  stop("Effort data is missing required column(s): ", paste(missing_required, collapse = ", "))
}

# Columns used later but not essential -> added as empty (NA) if absent,
# so the same code runs for every survey
effort_optional <- c("Vessel", "Action", "Transect_ID", "Port_MMO", "Stb_MMO",
                     "BigEyes_MMO", "Data_recorder", "Sea_state", "Swell",
                     "Port_vis", "Stb_vis", "Precipitation", "Glare_int",
                     "Glare_left", "Glare_right", "Notes", "QAQC_notes", "Delete_row")
added_cols <- setdiff(effort_optional, names(effort_std))
for (col in added_cols) effort_std[[col]] <- NA_character_
if (length(added_cols) > 0) message("Added empty column(s): ", paste(added_cols, collapse = ", "))

# Survey-specific columns known to exist in some surveys (kept as they are)
effort_known_other <- c("Indep_obs", "SBO", "Strata", "Lock_row")

# Any OTHER columns this workflow doesn't know about (kept, just reported;
# should be empty unless the data sheet changed again)
setdiff(names(effort_std),
        c(effort_required, effort_optional, effort_known_other, "source_file"))


#----EFFORT STEP 2: REMOVE EXACT DUPLICATES AND DELETED ROWS----

# (a) Exact duplicate rows (every column identical). source_file is ignored
#     here, so the same row exported in two files is also caught.
n_before <- nrow(effort_std)
effort_dedup <- effort_std |>
  distinct(across(-source_file), .keep_all = TRUE)
message(n_before - nrow(effort_dedup), " exact duplicate row(s) removed")

# (b) Rows flagged Delete_row = "True" by the team.
#     NA-safe and case-insensitive: blank/NA counts as "keep".
effort_deleted <- effort_dedup |>
  filter(tolower(trimws(coalesce(Delete_row, "false"))) == "true")

effort_deleted |> select(source_file, `Datetime_UTC (UTC)`, Effort, Action, Notes)   # review

effort_del <- effort_dedup |>
  anti_join(effort_deleted, by = names(effort_dedup))


#----EFFORT STEP 3: CHECK, CORRECT AND PARSE DATETIMES----

# Two datetime columns:
#  - Datetime_UTC_locked (UTC): set automatically when the row is created (trusted)
#  - Datetime_UTC (UTC):        editable; observers may adjust it on purpose
#
# Known errors in the editable column:
#  - missing/blank
#  - duplicated from the previous row (the "Ctrl-Shift-+" shortcut copies the
#    previous row, including its time)

dt_format <- "%Y-%m-%dT%H:%M:%OS"   # e.g. "2026-09-10T20:16:00.6"

effort_dt_flags <- effort_del |>
  mutate(locked_utc_full = as.POSIXct(`Datetime_UTC_locked (UTC)`, format = dt_format, tz = "UTC")) |>
  # Order by the trusted locked time (fractional seconds kept for ordering)
  arrange(locked_utc_full) |>
  mutate(
    locked_missing     = is.na(locked_utc_full),
    datetime_missing   = is.na(`Datetime_UTC (UTC)`) | trimws(`Datetime_UTC (UTC)`) == "",
    datetime_duplicate = coalesce(`Datetime_UTC (UTC)` == lag(`Datetime_UTC (UTC)`), FALSE),
    # How far the editable time was moved from the locked time (seconds)
    edit_minus_locked_sec = round(as.numeric(difftime(
      as.POSIXct(`Datetime_UTC (UTC)`, format = dt_format, tz = "UTC"),
      locked_utc_full, units = "secs")), 1),
    # Editable time differs from the locked time (by 1 s or more; smaller
    # differences are just the fractional seconds)
    datetime_edited = coalesce(abs(edit_minus_locked_sec) >= 1, FALSE)
  )

# Counts
table(locked_missing = effort_dt_flags$locked_missing)
table(datetime_missing = effort_dt_flags$datetime_missing)
table(datetime_duplicate = effort_dt_flags$datetime_duplicate)
table(datetime_edited = effort_dt_flags$datetime_edited)

# -----------------------------------------------------------------------
# MANUAL STEP (before continuing):
# Review EVERY row listed below: missing or duplicated editable times, and
# any row where the editable time differs from the locked time. Read the
# Notes to check whether the time was changed on purpose.
# In the next step, only missing and duplicated (copy-shortcut) times are
# corrected automatically; intentional edits are kept as entered.
# -----------------------------------------------------------------------
effort_dt_flags |>
  filter(locked_missing | datetime_missing | datetime_duplicate | datetime_edited) |>
  select(source_file, `Datetime_UTC_locked (UTC)`, `Datetime_UTC (UTC)`,
         edit_minus_locked_sec, datetime_missing, datetime_duplicate,
         datetime_edited, Effort, Action, Notes) |>
  print(n = Inf)

# Correct the editable datetime, parse it, and record what was changed.
# The flag columns are kept so corrected rows stay traceable.
effort_dt <- effort_dt_flags |>
  mutate(
    Datetime_UTC_orig = `Datetime_UTC (UTC)`,
    # A corrected duplicate now equals its locked time, so it no longer
    # counts as an intentional edit
    datetime_edited = datetime_edited & !datetime_missing & !datetime_duplicate,
    QAQC_notes = case_when(
      datetime_missing   ~ paste_na(QAQC_notes, "Datetime_UTC missing: filled from Datetime_UTC_locked"),
      datetime_duplicate ~ paste_na(QAQC_notes, "Datetime_UTC duplicated previous row (copy shortcut): replaced with Datetime_UTC_locked"),
      TRUE ~ QAQC_notes),
    `Datetime_UTC (UTC)` = case_when(
      datetime_missing   ~ `Datetime_UTC_locked (UTC)`,
      datetime_duplicate ~ `Datetime_UTC_locked (UTC)`,
      TRUE               ~ `Datetime_UTC (UTC)`),
    # Event time, to the second
    datetime_utc = floor_date(as.POSIXct(`Datetime_UTC (UTC)`, format = dt_format, tz = "UTC"), unit = "second"),
    # Same instant in Atlantic time (with_tz = same moment, different zone)
    datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
    # Survey day, based on Atlantic time
    date_adt = as_date(datetime_adt)) |>
  arrange(datetime_utc) |>
  relocate(datetime_utc, datetime_adt, date_adt)

# Any datetimes that still couldn't be parsed? (should be 0)
sum(is.na(effort_dt$datetime_utc))
effort_dt |> filter(is.na(datetime_utc)) |>
  select(source_file, `Datetime_UTC_locked (UTC)`, `Datetime_UTC (UTC)`, Notes)


#----EFFORT STEP 4: PARSE GPS_pos TO DECIMAL DEGREES----

# GPS_pos is entered as degrees + decimal minutes, e.g. "44 40.9  N 63 36.8  W".
# A regular expression is used instead of separate(), so malformed or blank
# entries become NA (and are reported) instead of shifting pieces into the
# wrong columns.
parse_gps_pos <- function(x) {
  m <- str_match(trimws(x),
                 "^(\\d+)\\s+(\\d+(?:\\.\\d+)?)\\s*([NS])\\s+(\\d+)\\s+(\\d+(?:\\.\\d+)?)\\s*([EW])$")
  lat_min <- as.numeric(m[, 3])
  lon_min <- as.numeric(m[, 6])
  lat <- as.numeric(m[, 2]) + lat_min / 60
  lon <- as.numeric(m[, 5]) + lon_min / 60
  # Minutes must be < 60; anything else is a typo
  lat[!is.na(lat_min) & lat_min >= 60] <- NA
  lon[!is.na(lon_min) & lon_min >= 60] <- NA
  lat[m[, 4] %in% "S"] <- -lat[m[, 4] %in% "S"]
  lon[m[, 7] %in% "W"] <- -lon[m[, 7] %in% "W"]
  tibble(vessel_lat_e = lat, vessel_lon_e = lon)
}

# "_e" marks the position as coming from Effort/Env
effort_gps <- effort_dt |>
  bind_cols(parse_gps_pos(effort_dt$GPS_pos)) |>
  mutate(gps_source = "effortenv") |>
  relocate(vessel_lat_e, vessel_lon_e, .after = GPS_pos)

# GPS_pos entries that were filled in but could not be parsed (review these)
effort_gps |>
  filter(!is.na(GPS_pos), trimws(GPS_pos) != "", is.na(vessel_lat_e) | is.na(vessel_lon_e)) |>
  select(datetime_adt, GPS_pos, Effort, Notes)


#----EFFORT STEP 5: CLEAN TRANSECT_ID----

# Known typos: "WRONG_ID" = "CORRECT_ID". Add new ones as they're found.
transect_id_corrections <- c(
  "FBM37" = "FMB37",
  "S10"   = "SB10"
)

effort_tr <- effort_gps |>
  mutate(
    Transect_ID_orig = Transect_ID,   # keep the original for reference
    # Standardize: trim spaces, upper case, and treat "Na"/"na"/"NA"/blank as missing
    Transect_ID = toupper(trimws(Transect_ID)),
    Transect_ID = na_if(na_if(Transect_ID, "NA"), ""),
    # Apply known corrections
    Transect_ID_corrected = Transect_ID %in% names(transect_id_corrections),
    Transect_ID = if_else(Transect_ID_corrected,
                          unname(transect_id_corrections[Transect_ID]),
                          Transect_ID),
    QAQC_notes = if_else(Transect_ID_corrected,
                         paste_na(QAQC_notes, paste0("Transect_ID corrected: ", Transect_ID_orig, " -> ", Transect_ID)),
                         QAQC_notes),
    # Check against the planned transect list
    Transect_ID_flag = case_when(
      is.na(Transect_ID)                        ~ NA_character_,
      Transect_ID %in% transects_df$Transect_ID ~ "OK",
      TRUE                                      ~ "REVIEW")) |>
  select(-Transect_ID_corrected)

table(effort_tr$Transect_ID_flag, useNA = "ifany")

# Any remaining unknown IDs, with context and the 3 closest valid IDs
# (likely typos). Add confirmed fixes to transect_id_corrections above.
review_ids <- effort_tr |>
  filter(Transect_ID_flag == "REVIEW") |>
  distinct(Transect_ID) |>
  pull()

effort_tr |>
  filter(Transect_ID_flag == "REVIEW") |>
  select(datetime_adt, Transect_ID_orig, Effort, Action, Notes)

if (length(review_ids) > 0) {
  map(set_names(review_ids), function(id) {
    stringdist::stringdist(id, transects_df$Transect_ID) |>
      setNames(transects_df$Transect_ID) |>
      sort() |>
      head(3)
  })
}

sort(unique(effort_tr$Transect_ID))


#----EFFORT STEP 6: CHECK CATEGORICAL VALUES----

# Lists any values outside the expected categories (typos, spacing, case).
# This is a REPORT only; nothing is changed.
expected_values <- list(
  Effort    = c("On", "Bridge-On", "Transit-On", "Off", "Closing"),
  Sea_state = c(as.character(0:6), ">6"),
  Port_vis  = c("Excellent", "Moderate", "Poor", "Restricted"),
  Stb_vis   = c("Excellent", "Moderate", "Poor", "Restricted")
)

imap_dfr(expected_values, function(expected, col) {
  effort_tr |>
    filter(!is.na(.data[[col]]), !(.data[[col]] %in% expected)) |>
    count(value = .data[[col]], name = "n") |>
    mutate(column = col, .before = 1)
})

# Full value tables for the other categorical columns (to eyeball)
effort_tr |> count(Swell)
effort_tr |> count(Precipitation)
effort_tr |> count(Glare_int)


#----EFFORT STEP 7: WEATHER RULE (Effort -> Off)----

# Effort is set to "Off" whenever sea state >= 6 (BFT 6 or ">6") or either
# side's visibility is "Restricted". The change is recorded in QAQC_notes.
# Only On, Bridge-On and Transit-On rows are changed: "Closing" and missing
# (NA) effort are left as they are.
# Weather is only evaluated on the conditions written on each row; blank
# conditions are left alone (to re-assess later if needed).
effort_wx <- effort_tr |>
  mutate(
    high_sea  = Sea_state %in% c("6", ">6"),
    restr_vis = Port_vis %in% "Restricted" | Stb_vis %in% "Restricted",
    to_off    = (high_sea | restr_vis) & Effort %in% c("On", "Bridge-On", "Transit-On"),
    reason    = case_when(
      high_sea & restr_vis ~ "sea state >= 6 and restricted visibility",
      high_sea             ~ "sea state >= 6",
      restr_vis            ~ "restricted visibility"),
    QAQC_notes = if_else(
      to_off,
      paste_na(QAQC_notes, paste0("Effort changed ", coalesce(Effort, "NA"), " -> Off: ", reason)),
      QAQC_notes),
    Effort = if_else(to_off, "Off", Effort)) |>
  select(-high_sea, -restr_vis, -to_off, -reason)

table(effort_wx$Sea_state, effort_wx$Effort, useNA = "ifany")

# Flag rows with no Effort entered: Effort should always be filled in, so
# these need manual verification (they are flagged, not changed).
effort_wx <- effort_wx |>
  mutate(effort_missing = is.na(Effort) | trimws(Effort) == "")

table(effort_missing = effort_wx$effort_missing)

effort_wx |>
  filter(effort_missing) |>
  select(source_file, datetime_adt, Effort, Action, Transect_ID, Notes) |>
  print(n = Inf)


#----EFFORT STEP 8: CHECK POSITIONS AGAINST THE GPS TRACK----

# Every effort row needs a valid position. Each row's logged position is
# compared with the cleaned GPS track at the row's time:
#  - the track position is linearly interpolated between the GPS fixes just
#    before and after the row's time (fixes are ~10 s apart, so this is
#    accurate to tens of metres at survey speed)
#  - it is only trusted if those two fixes are close together in time
#    (max_bracket_s); inside a longer GPS gap the row is left for review
#
# The position must reflect the EVENT time (Datetime_UTC, possibly edited by
# hand), not the time the row was created (the locked time, which is when
# GPS_pos was captured). The logged position is replaced by the track
# position at the event time when:
#  - the event time was edited (differs from the locked time)
#  - the position is missing (or couldn't be parsed)
#  - the position is outside the study-area bounding box
#  - the position is more than max_offset_km from the track (e.g. a typo)
# The original position is kept, and a QAQC note is added.
# (Rows whose time was a copy-shortcut duplicate are NOT re-positioned: the
# shortcut copies the previous row's time but not its GPS_pos, and the
# corrected time equals the locked time at which GPS_pos was captured.)

# THRESHOLDS AND THEIR RATIONALE
#
# max_offset_km = 1 km
#   How far a logged position may be from the GPS track (at the same time)
#   before it is treated as an error. Expected, legitimate differences are
#   all well below 1 km:
#    - GPS_pos precision: recorded to 0.1 minute of arc, i.e. up to ~185 m
#      in latitude and ~130 m in longitude at 44 N (rounding alone).
#    - timing: GPS_pos is captured when the row is created; a few seconds of
#      difference at a survey speed of ~10 kt (~5 m/s) adds tens of metres.
#    - different GPS receivers/antenna positions on the ship: tens of metres.
#   Combined, these stay under ~0.4 km, so 1 km leaves a safety margin of
#   more than 2x and avoids flagging valid positions.
#   Meanwhile, the typical data-entry error (a wrong digit in the whole
#   minutes, e.g. 44 40.9 entered as 44 49.0) is at least 1 minute of arc,
#   i.e. >= ~1.3-1.85 km, so it is caught.
#   (A typo in the tenths digit, <= ~185 m, is not detectable this way, but
#   is also too small to matter for these analyses.)
#
# max_bracket_s = 300 s (5 min)
#   The replacement position is linearly interpolated between the GPS fixes
#   just before and just after the event time, i.e. it assumes the ship
#   travelled in a straight line at constant speed between them. Normally
#   the fixes are ~10 s apart and the error is negligible; this threshold
#   only matters inside GPS gaps.
#   In 5 min at ~10 kt, the ship travels ~1.5 km. Even in the worst case (a
#   90-degree turn halfway through the gap), the interpolated position is
#   within ~0.5 km of the true one, i.e. still well inside the 1 km
#   tolerance above. Beyond ~5 min the possible error can exceed 1 km, so an
#   interpolated position would be no more trustworthy than the logged one;
#   such rows are listed as unresolved instead of being given a guess.
max_offset_km <- 1
max_bracket_s <- 300

# Interpolate the track position at times t (returns NA outside the track)
interp_track <- function(t, track_t, track_lat, track_lon) {
  o  <- order(track_t)
  tt <- as.numeric(track_t[o]); la <- track_lat[o]; lo <- track_lon[o]
  t  <- as.numeric(t)
  n  <- length(tt)
  i  <- findInterval(t, tt)                  # index of the fix at/before t
  inside <- !is.na(t) & i >= 1 & i < n
  i0 <- pmin(pmax(i, 1), n - 1)
  i1 <- i0 + 1
  span <- tt[i1] - tt[i0]
  w <- ifelse(span > 0, (t - tt[i0]) / span, 0)
  tibble(
    trk_lat       = ifelse(inside, la[i0] + w * (la[i1] - la[i0]), NA_real_),
    trk_lon       = ifelse(inside, lo[i0] + w * (lo[i1] - lo[i0]), NA_real_),
    trk_bracket_s = ifelse(inside, span, NA_real_)
  )
}

effort_pos <- effort_wx |>
  bind_cols(interp_track(effort_wx$datetime_utc, gps$datetime_utc,
                         gps$vessel_lat, gps$vessel_lon)) |>
  mutate(
    trk_ok = !is.na(trk_lat) & trk_bracket_s <= max_bracket_s,
    pos_offset_km = round(distHaversine(cbind(vessel_lon_e, vessel_lat_e),
                                        cbind(trk_lon, trk_lat)) / 1000, 2),
    pos_issue = case_when(
      is.na(vessel_lat_e) | is.na(vessel_lon_e)               ~ "missing",
      vessel_lon_e < xmin | vessel_lon_e > xmax |
        vessel_lat_e < ymin | vessel_lat_e > ymax             ~ "out_of_bounds",
      datetime_edited                                         ~ "time_edited",
      trk_ok & pos_offset_km > max_offset_km                  ~ "far_from_track",
      TRUE                                                    ~ NA_character_),
    pos_replaced = !is.na(pos_issue) & trk_ok,
    # keep originals before replacing
    vessel_lat_e_orig = vessel_lat_e,
    vessel_lon_e_orig = vessel_lon_e,
    vessel_lat_e = if_else(pos_replaced, trk_lat, vessel_lat_e),
    vessel_lon_e = if_else(pos_replaced, trk_lon, vessel_lon_e),
    QAQC_notes = if_else(
      pos_replaced,
      paste_na(QAQC_notes, paste0("Position replaced (", pos_issue,
                                  "): interpolated from GPS track at event time")),
      QAQC_notes))

# Summary: issues found, and whether they could be fixed
effort_pos |> count(pos_issue, pos_replaced)

# Replaced positions (review a few on a map if in doubt)
effort_pos |>
  filter(pos_replaced) |>
  select(datetime_adt, GPS_pos, vessel_lat_e_orig, vessel_lon_e_orig,
         vessel_lat_e, vessel_lon_e, pos_issue, pos_offset_km, trk_bracket_s)

# UNRESOLVED: a position problem with no usable GPS track at that time.
# These keep their original (possibly wrong) position for now; to re-assess
# once we see how many there are (options: leave, set to NA, fix by hand).
effort_pos |>
  filter(!is.na(pos_issue), !pos_replaced) |>
  select(datetime_adt, GPS_pos, vessel_lat_e, vessel_lon_e, pos_issue,
         trk_bracket_s, Effort, Notes)


#----EFFORT STEP 9: DAILY EFFORT START / END, AND FINAL TIDY----

# Realized daily effort window:
#  - start = first time that day with Effort On, Bridge-On or Transit-On
#  - end   = last effort entry of the day
effort_clean <- effort_pos |>
  group_by(date_adt) |>
  mutate(
    start.effort_adt = {
      on_rows <- Effort %in% c("Bridge-On", "On", "Transit-On")
      if (any(on_rows)) min(datetime_adt[on_rows]) else as.POSIXct(NA, tz = "America/Halifax")
    },
    end.effort_adt = max(datetime_adt, na.rm = TRUE)) |>
  ungroup() |>
  arrange(datetime_utc) |>
  # Remove working columns; keep the audit columns
  # (Datetime_UTC_orig, datetime_missing, datetime_duplicate, datetime_edited,
  #  edit_minus_locked_sec, Transect_ID_orig, Transect_ID_flag, effort_missing,
  #  vessel_*_e_orig, pos_issue, pos_replaced)
  select(-any_of(c("locked_utc_full", "locked_missing", "Delete_row",
                   "trk_lat", "trk_lon", "trk_bracket_s", "trk_ok", "pos_offset_km"))) |>
  relocate(start.effort_adt, end.effort_adt, .after = date_adt)



#----JOIN GPS + EFFORT----

#----JOIN STEP 1: EFFORT ROWS (position at the event time)----

# Why the GPS track position rather than the logged GPS_pos:
# GPS_pos comes from the same Mysticetus GPS, but it is stored as text
# rounded to 0.1 minute of arc (e.g. "44 40.9 N"), i.e. up to ~90 m off in
# latitude and ~65 m in longitude. The track itself is stored at full
# precision every ~10 s. If an effort row were inserted into the track with
# its rounded position, the track would step sideways to that point and back
# (fix -> rounded effort position -> next fix), adding distance that the ship
# never travelled. With ~800 effort rows, this adds up across the survey.
# Interpolating the track at the event time puts the effort row exactly on
# the line. Where the track has a gap (fixes > max_bracket_s apart), the
# cleaned logged position is used instead. pos_source records which was used.
effort_rows <- effort_clean |>
  bind_cols(interp_track(effort_clean$datetime_utc, gps$datetime_utc,
                         gps$vessel_lat, gps$vessel_lon)) |>
  mutate(
    use_track  = !is.na(trk_lat) & trk_bracket_s <= max_bracket_s,
    vessel_lat = if_else(use_track, trk_lat, vessel_lat_e),
    vessel_lon = if_else(use_track, trk_lon, vessel_lon_e),
    pos_source = if_else(use_track, "gps_track_interpolated", "effortenv_logged"),
    row_type   = "effort") |>
  select(-c(trk_lat, trk_lon, trk_bracket_s, use_track))

# How many effort rows used each position source
count(effort_rows, pos_source)

# Size of the difference between logged and track positions (metres).
# Expect mostly < ~150 m (rounding); larger values were already handled in
# effort step 8.
summary(distHaversine(cbind(effort_rows$vessel_lon_e, effort_rows$vessel_lat_e),
                      cbind(effort_rows$vessel_lon,   effort_rows$vessel_lat)))


#----JOIN STEP 2: STACK GPS FIXES AND EFFORT ROWS----

gps_rows <- gps |>
  mutate(date_adt   = as_date(datetime_adt),
         pos_source = gps_source,
         row_type   = "gps")

gps_effort <- bind_rows(effort_rows, gps_rows) |>
  # Time order. If an effort entry and a GPS fix share the same second, the
  # effort row goes first, so its new state applies to that fix.
  arrange(datetime_utc, row_type != "effort") |>
  mutate(row_id = row_number())   # stable key for the spatial join below

# Checks
nrow(gps_effort) == nrow(gps) + nrow(effort_clean)   # should be TRUE
count(gps_effort, row_type, pos_source)

# Missing or out-of-bounds positions (should be 0: both inputs were cleaned)
gps_effort |>
  filter(is.na(vessel_lat) | is.na(vessel_lon) |
           vessel_lon < xmin | vessel_lon > xmax |
           vessel_lat < ymin | vessel_lat > ymax) |>
  select(datetime_adt, row_type, pos_source, vessel_lat, vessel_lon, Notes)


#----JOIN STEP 3: ASSIGN STRATA (FMB, SB, NONE) FROM POSITION----

# Each row's position is tested against the WEA polygons (WGS84, EPSG:4326).
# "NONE" = outside both WEAs. Rows without a position (should be none) get NA.
studyarea_shp <- st_transform(studyarea_shp, crs = 4326)

strata_lookup <- gps_effort |>
  filter(!is.na(vessel_lat), !is.na(vessel_lon)) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326) |>
  st_join(studyarea_shp |> select(WEA), join = st_within) |>
  st_drop_geometry() |>
  transmute(row_id,
            strata = case_when(WEA == "French/Middle Bank" ~ "FMB",
                               WEA == "Sydney Bight"       ~ "SB",
                               TRUE                        ~ "NONE"))

# A point inside two overlapping polygons would be duplicated (should be 0)
sum(duplicated(strata_lookup$row_id))

gps_effort <- gps_effort |>
  left_join(strata_lookup, by = "row_id") |>
  select(-row_id)

table(gps_effort$date_adt, gps_effort$strata, useNA = "ifany")

# Visual check: points colour-coded by strata.
# (Every 5th GPS fix + all effort rows, to keep the map responsive;
#  Esri basemap, since CartoDB tiles now show an API-key watermark.)
strata_pal <- colorFactor(palette = c("firebrick", "darkorange", "gray60"),
                          levels  = c("FMB", "SB", "NONE"))

gps_effort |>
  filter(row_type == "effort" | row_number() %% 5 == 0) |>
  leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  addPolygons(data = studyarea_shp, color = "black", weight = 1,
              fillOpacity = 0.05, label = ~WEA) |>
  addCircleMarkers(lng = ~vessel_lon, lat = ~vessel_lat,
                   color = ~strata_pal(strata), radius = 2,
                   stroke = FALSE, fillOpacity = 0.6) |>
  addLegend("bottomright", pal = strata_pal, values = ~strata, title = "Strata")


#----JOIN STEP 4: FILL EFFORT STATE AND CONDITIONS ONTO EVERY GPS FIX----

# Effort state, observers and conditions are only written on effort rows.
# Each value applies until the next effort entry, so it is filled DOWN onto
# the GPS fixes that follow, within each survey day (ADT).
#
# - GPS fixes before the first effort entry of a day have no state entered
#   yet; they are set to Effort = "Off" (observers were not on effort). This
#   happens AFTER the Transect_ID fill, so that pre-effort period stays its
#   own block. No QAQC note is added: these are GPS fixes, not data entries.
#   (Effort rows with a blank Effort, flagged earlier as effort_missing, are
#   filled from the previous entry instead.)
# - Row-specific columns (Action, Notes, QAQC_notes) are NOT filled.
# - strata is NOT filled: every row has its own position-based value.
# - Strata (capital S) is the value RECORDED in the Summer2026 data sheet;
#   it is filled like the other recorded columns, and is separate from the
#   position-based strata.

# Columns carried into the final data frame
keep_cols <- c(
  # time and position
  "datetime_utc", "datetime_adt", "date_adt", "row_type",
  "vessel_lat", "vessel_lon", "gps_source", "pos_source", "sog_kt", "cog_t",
  # survey context
  "Vessel", "strata", "Strata", "start.effort_adt", "end.effort_adt",
  # effort
  "Effort", "Action", "Transect_ID",
  # observers (standardized names, plus survey-specific ones if present)
  "Port_MMO", "Stb_MMO", "BigEyes_MMO", "SBO", "Indep_obs", "Data_recorder",
  # conditions
  "Sea_state", "Swell", "Port_vis", "Stb_vis", "Precipitation",
  "Glare_int", "Glare_left", "Glare_right",
  # notes (row-specific)
  "Notes", "QAQC_notes")

# Recorded values that stay in force until the next effort entry
fill_down_cols <- c(
  "Strata", "Effort", "start.effort_adt", "end.effort_adt",
  "Port_MMO", "Stb_MMO", "BigEyes_MMO", "SBO", "Indep_obs", "Data_recorder",
  "Sea_state", "Swell", "Port_vis", "Stb_vis", "Precipitation",
  "Glare_int", "Glare_left", "Glare_right")

gps_effort1 <- gps_effort |>
  select(any_of(keep_cols)) |>
  arrange(datetime_utc, row_type != "effort") |>
  group_by(date_adt) |>
  # Fill recorded values down, within the day
  fill(any_of(fill_down_cols), .direction = "down") |>
  # Daily effort window: also fill UP, so fixes before the first entry of
  # the day still know that day's window
  fill(any_of(c("start.effort_adt", "end.effort_adt")), .direction = "up") |>
  # Vessel name on every row of the day
  fill(any_of("Vessel"), .direction = "downup") |>
  # Transect_ID: fill only within a continuous block of the same effort
  # state, so an ID can't carry into a later, separate period once the
  # effort state has changed in between. consecutive_id() starts a new block
  # at each change (NA, before the first entry, counts as its own block).
  mutate(effort_block = consecutive_id(Effort)) |>
  group_by(date_adt, effort_block) |>
  fill(Transect_ID, .direction = "down") |>
  ungroup() |>
  select(-effort_block) |>
  # Fixes before the first effort entry of the day (still NA after the
  # downward fill) -> "Off"
  mutate(Effort = coalesce(Effort, "Off"))

# Flag for future analysis: vessel speed < 5 kt while on effort.
# Note: Garmin speeds are calculated fix-to-fix, so they are noisier than
# Mysticetus speeds; effort rows have no speed (NA -> not flagged).
gps_effort1 <- gps_effort1 |>
  mutate(slow_on_effort = Effort %in% c("On", "Bridge-On", "Transit-On") &
           coalesce(sog_kt < 5, FALSE))

# Approximate time flagged per day (GPS fixes are ~10 s apart)
gps_effort1 |>
  filter(slow_on_effort, row_type == "gps") |>
  count(date_adt, name = "n_fixes") |>
  mutate(approx_min = round(n_fixes * 10 / 60))



#----JOIN: FINAL CHECKS----

nrow(gps_effort1)
count(gps_effort1, row_type)

# Positions (no NAs expected)
summary(gps_effort1$vessel_lat)
summary(gps_effort1$vessel_lon)

# Days, strata and transects
unique(gps_effort1$date_adt)
table(gps_effort1$date_adt, gps_effort1$strata)
sort(unique(gps_effort1$Transect_ID))

# Effort states per day (no NA expected: pre-effort fixes are now "Off")
table(gps_effort1$date_adt, gps_effort1$Effort, useNA = "ifany")

# Transect_ID should only appear during effort blocks where one was entered;
# this shows which effort states carry a Transect_ID (eyeball for surprises)
table(gps_effort1$Effort, !is.na(gps_effort1$Transect_ID), useNA = "ifany")

# Export
write.csv(gps_effort1, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_gps-effort.csv"),
          row.names = FALSE)



#----SIGHTINGS----


#----SIGHTINGS: LOAD----

# List all Sighting csv files
sight_files <- list.files(file.path(path2data, "Mysti_data"),
                          pattern = "Sighting\\.csv$",
                          recursive = TRUE, full.names = TRUE)

# Read every column as character so files from different days/surveys
# always combine cleanly; types are converted explicitly further down.
# source_file records which file each row came from (useful when reviewing).
sight_raw <- sight_files |>
  map_df(~ read_csv(.x,
                    col_types = cols(.default = col_character()),
                    show_col_types = FALSE) |>
           mutate(source_file = basename(.x)))

nrow(sight_raw)


#----SIGHTINGS STEP 1: STANDARDIZE COLUMN NAMES ACROSS SURVEYS----

# Column differences between surveys:
#   Spring2026: Observer, Sgt_side, Trial_outcome, Sgt_complete, Lock_row
#               (no Initial_detector, no Calf_count)
#   Summer2026: MMO (no Observer), Initial_detector, Calf_count, Lock_row
#   Fall2026:   Observer, Initial_detector, Calf_count
# Summer's MMO is kept under its own name (separate from Initial_detector
# and from Observer). Columns with parentheses are renamed to plain names.
# Format: new_name = "old_name"; any_of() only renames a column if it exists.
sight_name_map <- c(
  Distance_m = "Distance (m)",
  PSD_m      = "PSD (m)"
)

sight_std <- sight_raw |>
  rename(any_of(sight_name_map))

# Columns the workflow cannot run without -> stop with a clear message
sight_required <- c("Datetime_UTC_locked (UTC)", "Datetime_UTC (UTC)", "GPS_pos",
                    "Sgt_type", "Species")
missing_required <- setdiff(sight_required, names(sight_std))
if (length(missing_required) > 0) {
  stop("Sightings data is missing required column(s): ", paste(missing_required, collapse = ", "))
}

# Columns used later but not essential -> added as empty (NA) if absent,
# so the same code runs for every survey
sight_optional <- c("Sgt_ID", "Sgt_side", "Initial_detector", "Observer", "Obs_platform",
                    "Reticles", "Distance_m", "Distance_tool", "Angle_rel", "Angle_abs",
                    "Sgt_pos", "Sgt_lat", "Sgt_lon", "PSD_m",
                    "Min_count", "Best_count", "Max_count", "Calf_count",
                    "Photos_taken", "Camera", "Frame_first", "Frame_last",
                    "Resight", "Notes", "QAQC_notes", "Delete_row")
added_cols <- setdiff(sight_optional, names(sight_std))
for (col in added_cols) sight_std[[col]] <- NA_character_
if (length(added_cols) > 0) message("Added empty column(s): ", paste(added_cols, collapse = ", "))

# Survey-specific columns known to exist in some surveys (kept as they are)
sight_known_other <- c("MMO", "Trial_outcome", "Sgt_complete", "Lock_row")

# Any OTHER columns this workflow doesn't know about (kept, just reported;
# should be empty unless the data sheet changed again)
setdiff(names(sight_std),
        c(sight_required, sight_optional, sight_known_other, "source_file"))


#----SIGHTINGS STEP 2: REMOVE EXACT DUPLICATES AND DELETED ROWS----

# (a) Exact duplicate rows (every column identical; e.g. Summer2026).
#     source_file is ignored, so the same row exported twice is also caught.
n_before <- nrow(sight_std)
sight_dedup <- sight_std |>
  distinct(across(-source_file), .keep_all = TRUE)
message(n_before - nrow(sight_dedup), " exact duplicate row(s) removed")

# (b) Rows flagged Delete_row = "True" by the team (NA-safe, case-insensitive).
#     Removed before the datetime checks so they can't trigger duplicate flags.
sight_deleted <- sight_dedup |>
  filter(tolower(trimws(coalesce(Delete_row, "false"))) == "true")

# Review (e.g. Spring2026: 1 row marked for deletion with no explanation;
# deleted out of caution)
sight_deleted |> select(source_file, `Datetime_UTC (UTC)`, Species, Sgt_type, Notes)

sight_del <- sight_dedup |>
  anti_join(sight_deleted, by = names(sight_dedup))


#----SIGHTINGS STEP 3: CHECK, CORRECT AND PARSE DATETIMES----

# Same rules as the effort data:
#  - Datetime_UTC_locked (UTC): set automatically when the row is created (trusted)
#  - Datetime_UTC (UTC):        editable; observers may adjust it on purpose
#    (e.g. back-dated to when the animal was first seen)
# Known errors in the editable column: missing/blank, or duplicated from the
# previous row (the "Ctrl-Shift-+" shortcut copies the previous row).

dt_format <- "%Y-%m-%dT%H:%M:%OS"   # e.g. "2026-09-11T16:57:31.2"

sight_dt_flags <- sight_del |>
  mutate(locked_utc_full = as.POSIXct(`Datetime_UTC_locked (UTC)`, format = dt_format, tz = "UTC")) |>
  # Order by the trusted locked time (fractional seconds kept for ordering)
  arrange(locked_utc_full) |>
  mutate(
    locked_missing     = is.na(locked_utc_full),
    datetime_missing   = is.na(`Datetime_UTC (UTC)`) | trimws(`Datetime_UTC (UTC)`) == "",
    datetime_duplicate = coalesce(`Datetime_UTC (UTC)` == lag(`Datetime_UTC (UTC)`), FALSE),
    # How far the editable time was moved from the locked time (seconds)
    edit_minus_locked_sec = round(as.numeric(difftime(
      as.POSIXct(`Datetime_UTC (UTC)`, format = dt_format, tz = "UTC"),
      locked_utc_full, units = "secs")), 1),
    # Editable time differs from the locked time (by 1 s or more; smaller
    # differences are just the fractional seconds)
    datetime_edited = coalesce(abs(edit_minus_locked_sec) >= 1, FALSE)
  )

# Counts
table(locked_missing = sight_dt_flags$locked_missing)
table(datetime_missing = sight_dt_flags$datetime_missing)
table(datetime_duplicate = sight_dt_flags$datetime_duplicate)
table(datetime_edited = sight_dt_flags$datetime_edited)

# -----------------------------------------------------------------------
# MANUAL STEP (before continuing):
# Review EVERY row listed below: missing or duplicated editable times, and
# any row where the editable time differs from the locked time. Read the
# Notes to check whether the time was changed on purpose.
# In the next step, only missing and duplicated (copy-shortcut) times are
# corrected automatically; intentional edits are kept as entered.
# -----------------------------------------------------------------------
sight_dt_flags |>
  filter(locked_missing | datetime_missing | datetime_duplicate | datetime_edited) |>
  select(source_file, `Datetime_UTC_locked (UTC)`, `Datetime_UTC (UTC)`,
         edit_minus_locked_sec, datetime_missing, datetime_duplicate,
         datetime_edited, Species, Sgt_type, Notes) |>
  print(n = Inf)

# Correct the editable datetime, parse it, and record what was changed
sight_dt <- sight_dt_flags |>
  mutate(
    Datetime_UTC_orig = `Datetime_UTC (UTC)`,
    # A corrected duplicate now equals its locked time, so it no longer
    # counts as an intentional edit
    datetime_edited = datetime_edited & !datetime_missing & !datetime_duplicate,
    QAQC_notes = case_when(
      datetime_missing   ~ paste_na(QAQC_notes, "Datetime_UTC missing: filled from Datetime_UTC_locked"),
      datetime_duplicate ~ paste_na(QAQC_notes, "Datetime_UTC duplicated previous row (copy shortcut): replaced with Datetime_UTC_locked"),
      TRUE ~ QAQC_notes),
    `Datetime_UTC (UTC)` = case_when(
      datetime_missing   ~ `Datetime_UTC_locked (UTC)`,
      datetime_duplicate ~ `Datetime_UTC_locked (UTC)`,
      TRUE               ~ `Datetime_UTC (UTC)`),
    # Event time, to the second (milliseconds dropped)
    datetime_utc = floor_date(as.POSIXct(`Datetime_UTC (UTC)`, format = dt_format, tz = "UTC"), unit = "second"),
    # Same instant in Atlantic time
    datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
    # Survey day, based on Atlantic time
    date_adt = as_date(datetime_adt)) |>
  arrange(datetime_utc) |>
  relocate(datetime_utc, datetime_adt, date_adt)

# Any datetimes that still couldn't be parsed? (should be 0)
sum(is.na(sight_dt$datetime_utc))


#----SIGHTINGS STEP 4: RESIGHTS----

# Resights ("Yes" or "Possible") are repeat sightings of a group already
# recorded. They are KEPT in the cleaned data (they are still observations,
# and go through all the checks below) and are filtered out later
sight_res <- sight_dt |> mutate(Resight = trimws(Resight))

# Check for unexpected values (expect "No", "Yes", "Possible" or blank)
table(sight_res$Resight, useNA = "ifany")


#----SIGHTINGS STEP 5: PARSE NUMBERS, STANDARDIZE TEXT FIELDS----

num_cols <- c("Reticles", "Distance_m", "Angle_abs", "PSD_m", "Sgt_lat", "Sgt_lon",
              "Min_count", "Best_count", "Max_count")

# Values entered in a numeric field that are not numbers (they would
# otherwise silently become NA). Review these.
map_dfr(num_cols, function(col) {
  sight_res |>
    filter(!is.na(.data[[col]]), trimws(.data[[col]]) != "",
           is.na(suppressWarnings(as.numeric(.data[[col]])))) |>
    transmute(column = col, value = .data[[col]], datetime_adt, Species, Notes)
})

sight_num <- sight_res |>
  mutate(
    across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))),
    # Relative angle: number and side, e.g. "23R" -> 23 and "R"
    # (decimals allowed, e.g. "22.5L")
    Angle_num  = as.numeric(str_extract(Angle_rel, "\\d+(\\.\\d+)?")),
    Angle_side = str_extract(toupper(trimws(Angle_rel)), "[LR]$"),
    # Reminder: Angle_abs takes the vessel's heading into account, so it is
    # normal that it differs from Angle_rel
    Sgt_type = trimws(Sgt_type),
    Sgt_type_orig = Sgt_type,   # kept to see every change made below
    # Vessel (according to dataset)
    Vessel = case_when(dataset == "Summer2026" ~ "KopitHopson",
                       TRUE                    ~ "Teleost"),
    # Remove parentheses (and their content) from the platform name
    Obs_platform = trimws(gsub("\\s*\\(.*\\)", "", Obs_platform)),
    # One name for the Big Eyes
    Distance_tool = case_when(Distance_tool %in% c("Fujinon25x150", "BigEyes") ~ "BigEyes_Fujinon25x150",
                              TRUE ~ Distance_tool))

# Expected text values (report only)
table(sight_num$Sgt_type, useNA = "ifany")        # expect On-effort / Incidental
table(sight_num$Distance_tool, useNA = "ifany")
table(sight_num$Obs_platform, useNA = "ifany")

# Relative angles outside 0-180 (should be none)
sight_num |>
  filter(Angle_num > 180) |>
  select(datetime_adt, Angle_rel, Distance_tool, Species, Notes)


#----SIGHTINGS STEP 6: VESSEL POSITION (checked against the GPS track)----

# Same rules and thresholds as effort step 8 (see the rationale there):
# the vessel position must reflect the EVENT time. It is replaced by the
# GPS track position, interpolated at the event time, when:
#  - the event time was edited (differs from the locked time)
#  - the position is missing (or couldn't be parsed)
#  - the position is outside the study-area bounding box
#  - the position is more than max_offset_km from the track
# The track is only used where the GPS fixes on either side are no more
# than max_bracket_s apart. The original position is kept, with a QAQC note.

sight_pos <- sight_num |>
  bind_cols(parse_gps_pos(sight_num$GPS_pos) |>
              rename(vessel_lat = vessel_lat_e, vessel_lon = vessel_lon_e)) |>
  bind_cols(interp_track(sight_num$datetime_utc, gps$datetime_utc,
                         gps$vessel_lat, gps$vessel_lon)) |>
  mutate(
    trk_ok = !is.na(trk_lat) & trk_bracket_s <= max_bracket_s,
    pos_offset_km = round(distHaversine(cbind(vessel_lon, vessel_lat),
                                        cbind(trk_lon, trk_lat)) / 1000, 2),
    pos_issue = case_when(
      is.na(vessel_lat) | is.na(vessel_lon)                   ~ "missing",
      vessel_lon < xmin | vessel_lon > xmax |
        vessel_lat < ymin | vessel_lat > ymax                 ~ "out_of_bounds",
      datetime_edited                                         ~ "time_edited",
      trk_ok & pos_offset_km > max_offset_km                  ~ "far_from_track",
      TRUE                                                    ~ NA_character_),
    pos_replaced = !is.na(pos_issue) & trk_ok,
    vessel_lat_orig = vessel_lat,
    vessel_lon_orig = vessel_lon,
    vessel_lat = if_else(pos_replaced, trk_lat, vessel_lat),
    vessel_lon = if_else(pos_replaced, trk_lon, vessel_lon),
    QAQC_notes = if_else(
      pos_replaced,
      paste_na(QAQC_notes, paste0("Vessel position replaced (", pos_issue,
                                  "): interpolated from GPS track at event time")),
      QAQC_notes)) |>
  select(-c(trk_lat, trk_lon, trk_bracket_s, trk_ok))

# GPS_pos entries that were filled in but could not be parsed
sight_pos |>
  filter(!is.na(GPS_pos), trimws(GPS_pos) != "", is.na(vessel_lat_orig)) |>
  select(datetime_adt, GPS_pos, Species, Notes)

# Summary of position issues and fixes
sight_pos |> count(pos_issue, pos_replaced)

# Replaced positions (review, with context)
sight_pos |>
  filter(pos_replaced) |>
  select(datetime_adt, GPS_pos, vessel_lat_orig, vessel_lon_orig, vessel_lat, vessel_lon,
         pos_issue, pos_offset_km, edit_minus_locked_sec, Species, Notes)

# UNRESOLVED: a position problem with no usable GPS track at that time
sight_pos |>
  filter(!is.na(pos_issue), !pos_replaced) |>
  select(datetime_adt, GPS_pos, vessel_lat, vessel_lon, pos_issue, Species, Notes)


#----SIGHTINGS STEP 7: RETICLES AND DISTANCES----

# (a) Reticle readings beyond the maximum of the binoculars are data-entry
#     errors: the distance can't be trusted, so Reticles, Distance_m and
#     PSD_m are set to NA, and an On-effort sighting becomes Incidental
#     (an On-effort sighting needs a distance). The sighting position is
#     then set to the vessel position in step 8.

# TO DO: maximum reticles for the Big Eyes (Fujinon 25x150) is not yet
# known; fill it in below once confirmed (NA = not checked).
max_reticles <- c(
  "Fujinon7x50"           = 13,
  "BigEyes_Fujinon25x150" = NA
)

sight_dist <- sight_pos |>
  mutate(
    max_ret = unname(max_reticles[Distance_tool]),
    reticle_error = coalesce(!is.na(max_ret) & Reticles > max_ret, FALSE),
    # Note written first, while the original Reticles value is still there
    QAQC_notes = if_else(
      reticle_error,
      paste_na(QAQC_notes, paste0("Reticles (", Reticles, ") exceed max for ", Distance_tool,
                                  " (", max_ret, "): Reticles, Distance_m and PSD_m set to NA; ",
                                  "sighting position set to vessel position",
                                  if_else(Sgt_type %in% "On-effort", "; Sgt_type On-effort -> Incidental", ""))),
      QAQC_notes),
    Sgt_type   = if_else(reticle_error & Sgt_type %in% "On-effort", "Incidental", Sgt_type),
    Reticles   = if_else(reticle_error, NA_real_, Reticles),
    Distance_m = if_else(reticle_error, NA_real_, Distance_m),
    PSD_m      = if_else(reticle_error, NA_real_, PSD_m)) |>
  select(-max_ret)

sight_dist |> filter(reticle_error) |>
  select(datetime_adt, Distance_tool, Species, Sgt_type, QAQC_notes)

# (b) Distance range by tool (report only; eyeball for implausible values)
sight_dist |>
  group_by(Distance_tool) |>
  summarize(n = n(), n_with_distance = sum(!is.na(Distance_m)),
            min_m = min(Distance_m, na.rm = TRUE), median_m = median(Distance_m, na.rm = TRUE),
            max_m = max(Distance_m, na.rm = TRUE), .groups = "drop")

# (c) On-effort sightings missing the information needed for analysis
#     (report only; fix by hand, e.g. check Mysticetus auto-saves; if a
#     distance can't be recovered, change to Incidental with a QAQC note).
sight_dist |>
  filter(Sgt_type == "On-effort",
         is.na(Distance_m) | is.na(Angle_num) | is.na(Angle_abs)) |>
  select(datetime_adt, Species, Distance_tool, Reticles, Distance_m, Angle_rel, Angle_abs, PSD_m, Notes)

# TO DO: verify Angle_abs for the Big Eyes sightings
# (might need to convert Big Eyes angles to +/- 90 degrees).


#----SIGHTINGS STEP 8: SIGHTING POSITION----

# Mysticetus calculates the animal's position (Sgt_lat/Sgt_lon) from the
# vessel position, the absolute bearing (Angle_abs) and the radial distance
# (Distance_m).
#
# (a) Verification: the same calculation is reproduced from the LOGGED vessel
#     position and compared with Mysticetus' Sgt_lat/Sgt_lon.
#     Tolerance = 250 m: GPS_pos is rounded to 0.1 minute (up to ~113 m), so
#     a correct Mysticetus position can differ from the recalculation by up
#     to ~113 m; 250 m gives a >2x margin. Larger differences are listed.
#     This is a REPORT only; no positions are recalculated.
# (b) Final sighting position (never recalculated):
#     - Mysticetus position, when there is a distance and bearing
#     - vessel position, when there is no distance/bearing (e.g. most
#       incidental sightings, or a reticle error in step 7), or when
#       Mysticetus did not provide a position (listed for review below)
#     Note: if the vessel position was corrected in step 6, the Mysticetus
#     position is still kept as is.
sgt_tol_m <- 250

# Destination point from a start position, bearing (deg) and distance (m);
# NA where any input is missing
dest_point <- function(lon, lat, bearing, dist_m) {
  out <- matrix(NA_real_, nrow = length(lon), ncol = 2)
  ok  <- !is.na(lon) & !is.na(lat) & !is.na(bearing) & !is.na(dist_m)
  if (any(ok)) out[ok, ] <- geosphere::destPoint(cbind(lon[ok], lat[ok]), bearing[ok], dist_m[ok])
  tibble(lon = out[, 1], lat = out[, 2])
}

sgt_check <- dest_point(sight_dist$vessel_lon_orig, sight_dist$vessel_lat_orig,
                        sight_dist$Angle_abs, sight_dist$Distance_m)

sight_sgt <- sight_dist |>
  mutate(
    sgt_check_diff_m = round(distHaversine(cbind(Sgt_lon, Sgt_lat),
                                           cbind(sgt_check$lon, sgt_check$lat))),
    has_dist = !is.na(Distance_m) & !is.na(Angle_abs),
    sgt_pos_source = if_else(has_dist & !is.na(Sgt_lat) & !is.na(Sgt_lon),
                             "mysticetus", "vessel"),
    Sgt_lat_orig = Sgt_lat,
    Sgt_lon_orig = Sgt_lon,
    Sgt_lat = if_else(sgt_pos_source == "mysticetus", Sgt_lat, vessel_lat),
    Sgt_lon = if_else(sgt_pos_source == "mysticetus", Sgt_lon, vessel_lon))

# (a) Mysticetus positions that don't match the reproduced calculation (review)
sight_sgt |>
  filter(sgt_check_diff_m > sgt_tol_m) |>
  select(datetime_adt, GPS_pos, Angle_abs, Distance_m, Sgt_lat_orig, Sgt_lon_orig,
         sgt_check_diff_m, Species, Notes)

# Sightings with a distance and bearing but no Mysticetus position
# (given the vessel position; review)
sight_sgt |>
  filter(has_dist, is.na(Sgt_lat_orig) | is.na(Sgt_lon_orig)) |>
  select(datetime_adt, Species, Sgt_type, Distance_m, Angle_abs, Notes)

sight_sgt <- sight_sgt |> select(-has_dist)

count(sight_sgt, sgt_pos_source)


#----SIGHTINGS STEP 9: GROUP SIZE----

# - All three counts missing  -> group size assumed to be 1 (with a note)
# - Some counts missing       -> flagged for manual review
# - Min > Best or Best > Max  -> flagged for manual review
# - Calf_count > Best_count   -> flagged for manual review
sight_grp <- sight_sgt |>
  mutate(
    counts_all_missing = is.na(Min_count) & is.na(Best_count) & is.na(Max_count),
    counts_partial     = (is.na(Min_count) | is.na(Best_count) | is.na(Max_count)) & !counts_all_missing,
    counts_order_issue = coalesce(Min_count > Best_count | Best_count > Max_count, FALSE),
    # Calf_count is text ("Unknown" is valid); only numeric entries are compared
    calf_count_issue   = coalesce(suppressWarnings(as.numeric(Calf_count)) > Best_count, FALSE),
    QAQC_notes = if_else(counts_all_missing,
                         paste_na(QAQC_notes, "Min_count, Best_count and Max_count all missing: assumed group size = 1 for each"),
                         QAQC_notes),
    QAQC_notes = if_else(counts_partial,
                         paste_na(QAQC_notes, "Flag for manual review: Min_count, Best_count and/or Max_count missing"),
                         QAQC_notes),
    QAQC_notes = if_else(counts_order_issue,
                         paste_na(QAQC_notes, "Flag for manual review: counts not in order Min <= Best <= Max"),
                         QAQC_notes),
    QAQC_notes = if_else(calf_count_issue,
                         paste_na(QAQC_notes, "Flag for manual review: Calf_count > Best_count"),
                         QAQC_notes),
    Min_count  = if_else(counts_all_missing, 1, Min_count),
    Best_count = if_else(counts_all_missing, 1, Best_count),
    Max_count  = if_else(counts_all_missing, 1, Max_count))

sight_grp |>
  filter(counts_all_missing | counts_partial | counts_order_issue | calf_count_issue) |>
  select(datetime_adt, Species, Min_count, Best_count, Max_count, Calf_count, Notes)


#----SIGHTINGS STEP 10: SPECIES----

# Marine mammal species 
mm_species <- c("Atlantic white-sided dolphin", "Blue whale", "Bottlenose dolphin",
                "Common dolphin", "Fin whale", "Fin/sei whale", "Grey seal",
                "Harbour porpoise", "Harbour seal", "Humpback whale", "Killer whale",
                "Minke whale", "Pilot whale", "White-beaked dolphin",
                "Unknown baleen whale", "Unknown cetacean", "Unknown small cetacean",
                "Unknown dolphin", "Unknown seal")

# Non-marine-mammal species: always Incidental, and excluded from sight_mm
non_mm_species <- c("Mola mola", "White shark", "Basking shark", "Unknown shark",
                    "Leatherback turtle", "Tuna", "Other")

# (a) Dead animals: Species gets " (dead)" when the Notes mention a dead
#     animal. Text matching can give false positives (e.g. "dead ahead"),
#     so the matches are listed for review; every relabelled row also gets
#     a QAQC note.
dead_pattern <- regex("\\bdead\\b|carcass", ignore_case = TRUE)

sight_sp0 <- sight_grp |>
  mutate(Species = trimws(Species),
         dead_in_notes = coalesce(str_detect(Notes, dead_pattern), FALSE))

sight_sp0 |>
  filter(dead_in_notes) |>
  select(`Datetime_UTC_locked (UTC)`, datetime_adt, Species, Notes)

sight_sp <- sight_sp0 |>
  mutate(
    is_dead = dead_in_notes & !is.na(Species) &
      !str_detect(Species, "\\(dead\\)$"),            # don't label twice
    QAQC_notes = if_else(is_dead,
                         paste_na(QAQC_notes, "Species labelled '(dead)': Notes mention a dead animal"),
                         QAQC_notes),
    Species = if_else(is_dead, paste0(Species, " (dead)"), Species),
    # (b) Non-marine-mammal species recorded as On-effort -> Incidental
    species_base = str_remove(Species, " \\(dead\\)$"),
    non_mm_on_effort = species_base %in% non_mm_species & Sgt_type %in% "On-effort",
    QAQC_notes = if_else(non_mm_on_effort,
                         paste_na(QAQC_notes, "Sgt_type On-effort -> Incidental: not a marine mammal species"),
                         QAQC_notes),
    Sgt_type = if_else(non_mm_on_effort, "Incidental", Sgt_type)) |>
  select(-c(dead_in_notes, non_mm_on_effort))

# (c) Species review
table(sight_sp$Species, useNA = "ifany")

# Species names not in either list (typos or new species; review)
sight_sp |>
  filter(!species_base %in% c(mm_species, non_mm_species)) |>
  count(Species)

# "Other": keep only if the Notes identify a marine mammal species;
# otherwise these are excluded from the marine mammal summaries
sight_sp |>
  filter(species_base == "Other") |>
  select(datetime_adt, Species, Best_count, Notes)


#----SIGHTINGS STEP 11: EFFORT STATE AT THE SIGHTING TIME----

# A sighting labelled On-effort must have happened during On, Bridge-On or
# Transit-On effort. Each sighting is matched to the effort entry in force
# at its time (from that entry until the next one).
# - Effort "Off"  -> Sgt_type changed to Incidental (QAQC note says whether
#                    effort was Off because of the weather rule)
# - Effort "Closing", missing, or no matching entry -> listed for review only
effort_intervals <- effort_clean |>
  arrange(datetime_utc) |>
  transmute(
    eff_start = datetime_utc,
    eff_end   = coalesce(lead(datetime_utc), datetime_utc + days(1)),  # last entry of the survey
    Effort_at_sgt = Effort,
    effort_off_weather = coalesce(str_detect(QAQC_notes, fixed("-> Off:")), FALSE))

sight_eff <- sight_sp |>
  left_join(effort_intervals,
            by = join_by(datetime_utc >= eff_start, datetime_utc < eff_end)) |>
  mutate(
    on_effort_but_off = Sgt_type %in% "On-effort" & Effort_at_sgt %in% "Off",
    QAQC_notes = if_else(
      on_effort_but_off,
      paste_na(QAQC_notes, if_else(effort_off_weather,
                                   "Sgt_type On-effort -> Incidental: effort Off due to weather thresholds",
                                   "Sgt_type On-effort -> Incidental: effort was Off at the sighting time")),
      QAQC_notes),
    Sgt_type = if_else(on_effort_but_off, "Incidental", Sgt_type)) |>
  select(-c(eff_start, eff_end, effort_off_weather))

# The join shouldn't duplicate any sightings (should be TRUE)
nrow(sight_eff) == nrow(sight_sp)

# Reclassified sightings
sight_eff |>
  filter(on_effort_but_off) |>
  select(datetime_adt, Species, Best_count, Effort_at_sgt, QAQC_notes)

# On-effort sightings during Closing, missing or unmatched effort (review)
sight_eff |>
  filter(Sgt_type %in% "On-effort",
         !Effort_at_sgt %in% c("On", "Bridge-On", "Transit-On")) |>
  select(datetime_adt, Species, Effort_at_sgt, Notes)


#----SIGHTINGS STEP 12: STRATA----

# Strata (FMB, SB, NONE) from the VESSEL position (the area being surveyed),
# same method as the GPS + effort join.
studyarea_shp <- st_transform(studyarea_shp, crs = 4326)

sight_eff <- sight_eff |> mutate(row_id = row_number())

strata_lookup <- sight_eff |>
  filter(!is.na(vessel_lat), !is.na(vessel_lon)) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326) |>
  st_join(studyarea_shp |> select(WEA), join = st_within) |>
  st_drop_geometry() |>
  transmute(row_id,
            strata = case_when(WEA == "French/Middle Bank" ~ "FMB",
                               WEA == "Sydney Bight"       ~ "SB",
                               TRUE                        ~ "NONE"))

sum(duplicated(strata_lookup$row_id))   # should be 0

sight_strata <- sight_eff |>
  left_join(strata_lookup, by = "row_id") |>
  select(-row_id)

table(sight_strata$date_adt, sight_strata$strata, useNA = "ifany")


#----SIGHTINGS STEP 13: FINAL TIDY AND EXPORT----

# Map position: the animal's position for On-effort sightings with a
# distance; otherwise the vessel position.
sight_clean <- sight_strata |>
  mutate(map_lat = if_else(Sgt_type %in% "On-effort" & !is.na(PSD_m), Sgt_lat, vessel_lat),
         map_lon = if_else(Sgt_type %in% "On-effort" & !is.na(PSD_m), Sgt_lon, vessel_lon)) |>
  select(any_of(c(
    # identification and time
    "Vessel", "source_file", "Sgt_ID", "Datetime_UTC_locked (UTC)", "Datetime_UTC (UTC)",
    "datetime_utc", "datetime_adt", "date_adt",
    # positions
    "strata", "GPS_pos", "vessel_lat", "vessel_lon", "Sgt_lat", "Sgt_lon",
    "sgt_pos_source", "map_lat", "map_lon",
    # sighting
    "Sgt_type", "Effort_at_sgt", "Initial_detector", "Observer", "MMO", "Sgt_side", "Obs_platform",
    "Distance_tool", "Reticles", "Distance_m", "Angle_rel", "Angle_num", "Angle_side",
    "Angle_abs", "PSD_m",
    "Species", "Min_count", "Best_count", "Max_count", "Calf_count",
    "Resight", "Photos_taken", "Camera", "Frame_first", "Frame_last",
    "Trial_outcome", "Sgt_complete", "Lock_row",
    "Notes", "QAQC_notes",
    # audit columns (what was checked or changed)
    "Sgt_type_orig", "Datetime_UTC_orig", "datetime_missing", "datetime_duplicate",
    "datetime_edited", "edit_minus_locked_sec",
    "vessel_lat_orig", "vessel_lon_orig", "pos_issue", "pos_replaced",
    "Sgt_lat_orig", "Sgt_lon_orig", "sgt_check_diff_m", "reticle_error",
    "counts_all_missing", "counts_partial", "counts_order_issue", "calf_count_issue",
    "is_dead"))) |>
  arrange(datetime_utc)

# Marine mammals only (resights still included; filter them out in the
# summary tables and maps)
sight_mm <- sight_clean |>
  filter(!str_remove(Species, " \\(dead\\)$") %in% non_mm_species)

# Final checks
nrow(sight_clean); nrow(sight_mm)
table(sight_clean$Sgt_type_orig, sight_clean$Sgt_type, useNA = "ifany")   # every Sgt_type change
table(sight_mm$Species)
table(sight_clean$Resight, useNA = "ifany")
sum(is.na(sight_clean$vessel_lat))   # should be 0

# Export
write.csv(sight_clean, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_all_sightings.csv"),
          row.names = FALSE)
write.csv(sight_mm, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_mm_sightings.csv"),
          row.names = FALSE)



#----DFO MAR's Whale Sightings Database----

# All sightings except "Other", resights removed, for DFO MAR's Whale
# Sightings Database (WSDB). lat/lon = animal position for On-effort
# sightings with a distance, otherwise vessel position (map_lat/map_lon).
sight_wsdb <- sight_clean |>
  filter(!Resight %in% c("Yes", "Possible"),
         !str_remove(Species, " \\(dead\\)$") %in% "Other") |>
  mutate(Data_collector = "DFO_Maritimes_CRMP",
         Survey = paste0("WEAWAVS_", dataset),
         lat = map_lat,
         lon = map_lon) |>
  select(any_of(c("Data_collector", "Survey", "Vessel", "datetime_utc", "datetime_adt",
                  "lat", "lon", "Sgt_type", "MMO", "Observer", "Species",
                  "Min_count", "Best_count", "Max_count", "Photos_taken", "Camera",
                  "Frame_first", "Frame_last", "Notes")))

# Check: which position each row uses (vessel vs Mysticetus animal position)
count(sight_clean |> filter(!Resight %in% c("Yes", "Possible")), Sgt_type, sgt_pos_source)

# Export
write.csv(sight_wsdb, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_Sightings_WSDB.csv"),
          row.names = FALSE)

