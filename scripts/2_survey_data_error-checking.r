################################################################################
#
# Script purpose: Error-checking of vessel survey effort and marine mammal sightings data
# Program: DFO Maritimes - Cetacean Research and Monitoring Program (CRMP)
# Project: Wind Energy Area Wildlife Assessment Vessel Surveys (WEAWAVS)
# Author: Katherine Gavrilchuk
# Affiliation: Fisheries and Oceans Canada
# Contact: katherine.gavrilchuk@dfo-mpo.gc.ca
# Last updated: August 19, 2026
# R version: 4.6.1
#
################################################################################


#----SET WORKING DIRECTORY & PATH TO WORKING DATA----

setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/")

# Specify dataset
dataset = "Spring2026" # Spring2026, Summer2026, Fall2026, Winter2027

# Define paths to each dataset
data_paths <- c(
  Spring2026 = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Spring2026_2026-05-12_FINAL_WORKING",
  Summer2026 = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Summer2026_2026-07-17_FINAL_WORKING"
  # Fall2026   = "...",
  # Winter2027 = "..."
)

path2data <- data_paths[[dataset]]


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

# > # Check for other function-name collisions between the two packages
#intersect(ls("package:data.table"), ls("package:dplyr"))
# "between" "first"   "last" 


#----LOAD FUNCTIONS----

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


#----DEFINE STUDY AREA----

study.bbox <- c(-63.907471, 43.771094, -58.864746, 47.055154)

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
  mutate(datetime_utc = as.POSIXct(`Time Created (UTC)`, tz = "UTC")) |>
  # Use "_m" to denote it as coming from Mysticetus (just helps error-check after the GPS sources get joined)
  rename(vessel_lat_m = Latitude,
         vessel_lon_m = Longitude,
         sog_kt = `Speed Over Ground (kts)`,
         cog_t = `Course Over Ground (T)`) |>
  select(datetime_utc, vessel_lat_m, vessel_lon_m, sog_kt, cog_t) |>
  arrange(datetime_utc)

# (2) Garmin GPS (second GPS source; daytime and nighttime)
garmin_files <- list.files(file.path(paste0(path2data, "/Garmin_GPS")), pattern = "\\.gpx$", full.names = TRUE)

# Check structure of gpx file
# test_file <- garmin_files[1]
# raw <- st_read(test_file, layer = "track_points", quiet = TRUE)
# 
# # Inspect the raw 'time' column exactly as returned by st_read, before any transformation
# str(raw$time)
# attr(raw$time, "tzone")
# head(raw$time, 5)
# names(raw)

# (2) Garmin GPS (second GPS source; daytime and nighttime)
garmin_files <- list.files(file.path(paste0(path2data, "/Garmin_GPS")), pattern = "\\.gpx$", full.names = TRUE)

# Combine Garmin GPS files
gps_garmin <- map_df(garmin_files, function(f){
  
  df <- st_read(f, layer = "track_points", quiet = TRUE)
  
  coords <- st_coordinates(df)
  
  df |>
    mutate(
      datetime_utc = with_tz(force_tz(time, "America/Halifax"), "UTC"),
      vessel_lon_g = coords[,1],
      vessel_lat_g = coords[,2]) |>
    st_drop_geometry() |>
    select(datetime_utc, vessel_lat_g, vessel_lon_g, track_fid, track_seg_id) |>
    arrange(datetime_utc) |>
    # Calculate speed over ground from consecutive fixes, within each track segment
    # so a gap between segments/files doesn't get treated as a single (bogus) speed
    group_by(track_fid, track_seg_id) |>
    mutate(
      dist_m = geosphere::distHaversine(
        cbind(vessel_lon_g, vessel_lat_g),
        cbind(lag(vessel_lon_g), lag(vessel_lat_g))),
      dt_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs")),
      sog_kt = if_else(dt_sec > 0, (dist_m / dt_sec) * 1.943844, NA_real_)  # m/s -> knots
    ) |>
    ungroup() |>
    select(datetime_utc, vessel_lat_g, vessel_lon_g, sog_kt)
  
})


# (3) Ship's NAV GPS (third GPS source; daytime and nighttime - to fill in outstanding GPS gaps, if needed)
# (provided by Teleost crew at end of survey)
teleost_gps <- read.csv(paste0(path2data,"/Teleost_NAV_GPS/Teleost_VoyageLog20260512121847.csv"))

# Helper function: convert lat/lon DMM strings to decimal degrees
dmm_to_decimal <- function(x) {
  deg  <- as.numeric(str_extract(x, "^[0-9]+"))
  min  <- as.numeric(str_extract(x, "(?<=\\s)[0-9]+\\.[0-9]+(?=\\s)"))
  hemi <- str_extract(x, "[NSEW]$")
  dd <- deg + min / 60
  dd[hemi %in% c("S", "W")] <- -dd[hemi %in% c("S", "W")]
  dd
}

teleost_gps2 <- teleost_gps |>
  mutate(
    datetime_utc = ymd_hms(paste(Date, Time), tz = "UTC"),
    datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
    vessel_lat   = dmm_to_decimal(LAT),
    vessel_lon   = dmm_to_decimal(LON),
    sog_kt       = SOG.kn,
    cog_degT    = COG.deg.T,
    hdg_degT = HDG.deg.T,
    depth.m = Depth.m) |>
  select(datetime_utc, datetime_adt, vessel_lat, vessel_lon, 
         sog_kt, cog_degT, hdg_degT, depth.m) |>
  arrange(datetime_utc)

write.csv(teleost_gps2, paste0(path2data, "/Data_summaries/data_combined_cleaned/Teleost_NAV_GPS_", dataset, ".csv"), row.names = FALSE)

# NOTE for Spring 2026: The Teleost's NAV GPS was also collected using a TeamWhale laptop with the Open GPN software, connected to NAV feed in Bridge; the program or laptop often crashed, so its a patchy source of GPS data and is redundant with the previous GPS source, so not needed.


#----COMBINE GPS DATA----

# Start with Mysticetus and Garmin sources; if outstanding large (to be defined) GPS gaps remain, integrate the ship's NAV GPS source.
gps <- bind_rows(gps_mysti, gps_garmin) |>
  arrange(datetime_utc) |>
  # Convert to Atlantic Daylight time
  mutate(datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax")) |> # with_tz() = converts the same moment in time to another timezone)
  # Relocate to first columns
  relocate(c(datetime_utc, datetime_adt)) |>
  # There are some duplicates in the Mysticetus GPS rows
  distinct()  |> # n = 6795 duplicate rows removed; sometimes duplicates occurred if ship was stationary for longer periods
  # Combine gps sources into just two columns (lat/lon)
  mutate(vessel_lat = if_else(!is.na(vessel_lat_m), vessel_lat_m, vessel_lat_g),
         vessel_lon = if_else(!is.na(vessel_lat_m), vessel_lon_m, vessel_lon_g),
         gps_source = case_when(!is.na(vessel_lat_m) & !is.na(vessel_lon_m) ~ "mysticetus",
                                !is.na(vessel_lat_g) & !is.na(vessel_lon_g) ~ "garmin",
                                TRUE ~ NA_character_)) |>
  arrange(datetime_utc) |>
  select(datetime_utc, datetime_adt, vessel_lat, vessel_lon, sog_kt, cog_t, gps_source)


#----GPS DATA: ERROR-CHECKING----

# (1) Data gaps
# Check time interval between successive GPS positions & flag gaps > 1 minute
gps_gaps <- gps |>
  arrange(datetime_utc) |>
  mutate(
    time_diff_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs")),
    gap_flag = time_diff_sec > 60)

table(gps_gaps$gap_flag)
summary(gps_gaps$time_diff_sec)

# Visualize gaps in the GPS track
leaflet(data = gps_gaps) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    lng = ~vessel_lon, lat = ~vessel_lat,
    color = ~ifelse(gap_flag, "red", "grey80"),
    radius = ~ifelse(gap_flag, 5, 2),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "Time: ", datetime_utc,
      "<br>Gap: ", round(time_diff_sec, 1), " sec"))


# (2) Erroneous GPS locations (uses function at the top of script)
result_main <- flag_and_replace_suspicious_gps(gps, xmin, xmax, ymin, ymax)

gps1 <- result_main$cleaned
suspicious_gps_check <- result_main$suspicious_check
replacement_check <- result_main$replacement_check

# Export combined Mysticetus & Garmin GPS track
write.csv(gps1, paste0(path2data, "/Data_summaries/data_combined_cleaned/Mysti-Garmin_GPS_", dataset, ".csv"), row.names = FALSE)


# Inspect GPS tracks 

# (1) GPS Source = Mysticetus (day-time tracks only) - Tracks split by date
mysticetus_pts <- gps1 |>
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

garmin_pts <- gps1 |>
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

mysticetus_data <- build_source_data(gps1, "mysticetus")
garmin_data     <- build_source_data(gps1, "garmin")

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

# (1) Datetime checks
effort1 <- effort |>
  arrange(`Datetime_UTC_locked (UTC)`) |>
  # Check for any missing values (NA or blank) in the Datetime_UTC (UTC) column, flag those
  mutate(datetime_missing = is.na(`Datetime_UTC (UTC)`) | trimws(`Datetime_UTC (UTC)`) == "",
         # Check for repeated datetimes in the `Datetime_UTC (UTC)`column -- due to using the shortcut "Ctl-Shft-+" which copies the previous row
         datetime_duplicates = coalesce(`Datetime_UTC (UTC)` == lag(`Datetime_UTC (UTC)`), FALSE)) |>
  relocate(c(datetime_missing, datetime_duplicates))

table(effort1$datetime_missing)
table(effort1$datetime_duplicates)

# Manually verify all "datetime_missing" & "datetime_duplicates" columns marked as TRUE
# Verify the Notes to see if the `Datetime_UTC (UTC)` was manually adjusted
# If they seem to be genuine errors, 1) replace the NAs with the `Datetime_UTC_locked (UTC)` and
# 2) Replace the duplicate `Datetime_UTC (UTC)` with the `Datetime_UTC_locked (UTC)`

effort2 <- effort1 |>
  # Replace datetime_duplicates values with the corresponding value from Datetime_UTC_locked (UTC) only when datetime_duplicates == TRUE
  mutate(`Datetime_UTC (UTC)` = if_else(datetime_duplicates, # If datetime_duplicates == TRUE,
                                        `Datetime_UTC_locked (UTC)`, # then replace with value from `Datetime_UTC_locked (UTC)`
                                        `Datetime_UTC (UTC)`)) # otherwise (if FALSE), keep the original `Datetime_UTC (UTC)`
  
effort3 <- effort2 |>
  # Create a datetime_utc column in the format "YYYY-MM-DD HH:MM:SS"
  mutate(datetime_utc = ymd_hms(`Datetime_UTC (UTC)`, tz = "UTC"),
         # Convert to Atlantic Daylight time
         datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"), # with_tz() = converts the same moment in time to another timezone
         # Create date variable based on Atlantic Daylight Time zone
         date_adt = as_date(datetime_adt)) |>
  arrange(datetime_adt) |>
  # Relocate to first columns
  relocate(c(datetime_utc, datetime_adt)) |>
  # GPS_pos = the vessel position; need to manipulate it to get lat and lon in separate columns and as decimal degrees
  separate(GPS_pos, into = c("lat_deg", "lat_min", "lat_dir",
                             "lon_deg", "lon_min", "lon_dir"),
           sep = "\\s+", remove = FALSE) |>
  mutate(lat_deg = as.numeric(lat_deg),
         lat_min = as.numeric(lat_min),
         lon_deg = as.numeric(lon_deg),
         lon_min = as.numeric(lon_min),
         # Use "_e" to mark it as coming from Effort/Env (just helps error-check after the GPS sources get joined)
         vessel_lat_e = lat_deg + lat_min / 60,
         vessel_lon_e = lon_deg + lon_min / 60,
         vessel_lat_e = if_else(lat_dir == "S", -vessel_lat_e, vessel_lat_e),
         vessel_lon_e = if_else(lon_dir == "W", -vessel_lon_e, vessel_lon_e),
         gps_source = "effortenv") |>
  # Start and end of actual (or 'realized) daily effort times, where:
  # - start of daily effort = first datetime_adt of the day where Effort %in% c("Bridge-On", "On", "Transit-On")
  # - end of daily effort =  the last datetime_adt of the day
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
  # Remove rows marked to be deleted (check them first before deleting)
  filter(Delete_row != "True") |>
  # Remove unnecessary columns
  select(-c(datetime_missing, datetime_duplicates, lat_deg, lat_min, lon_deg, lon_min, 
            lat_dir, lon_dir, Lock_row, Delete_row)) |>
  # Relocate columns closer to start of df
  relocate(c(start.effort_adt, end.effort_adt), .after = `Datetime_UTC (UTC)`)


# Checks:

# Transect_ID check against the reference list in transects_df
effort3 <- effort3 |>
  mutate(
    Transect_ID_orig = Transect_ID,  # keep original for reference
    Transect_ID = na_if(Transect_ID, "Na"),
    Transect_ID_flag = case_when(
      is.na(Transect_ID) ~ NA_character_,
      Transect_ID %in% transects_df$Transect_ID ~ "OK",
      TRUE ~ "REVIEW"))  # doesn't match any known transect ID

# Show the flagged row(s) with enough context to identify the correct Transect_ID
effort3 |>
  filter(Transect_ID_flag == "REVIEW") |>
  select(datetime_adt, Transect_ID_orig, Vessel, Effort)

# Suggest the closest valid Transect_ID using string distance, in case it's a typo
review_value <- effort3 |>
  filter(Transect_ID_flag == "REVIEW") |>
  pull(Transect_ID_orig) |>
  unique()

review_value

stringdist::stringdist(review_value, transects_df$Transect_ID) |>
  setNames(transects_df$Transect_ID) |>
  sort() |>
  head(5)

# Known corrections: map incorrect Transect_ID values to their correct counterpart
transect_id_corrections <- c(
  "FBM37" = "FMB37"
  # add more corrections here as they're identified, e.g.:
  # "WRONG_ID" = "CORRECT_ID"
)

# Re-check to verify any typos were resolved
effort3 <- effort3 |>
  mutate(
    Transect_ID = if_else(Transect_ID %in% names(transect_id_corrections),
                          transect_id_corrections[Transect_ID],
                          Transect_ID),
    Transect_ID_flag = case_when(
      is.na(Transect_ID) ~ NA_character_,
      Transect_ID %in% transects_df$Transect_ID ~ "OK",
      TRUE ~ "REVIEW"))

table(effort3$Transect_ID_flag)

# Inspect unique transect_IDs
unique(sort(effort3$Transect_ID))










# To include somewhere:

# Verify that vessel speed is  >= 5kt while "On" or "Bridge-On" or "Transit-On" effort






#----JOIN GPS + EFFORT----

# Join gps1 to effort3
gps_effort <- gps1 |>
  full_join(effort3, by = "datetime_utc") |>
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

# (2) Missing coordinates:
# Confirm there are no rows with missing coordinates before spatial join
n_missing_coords <- gps_effort |>
  filter(is.na(vessel_lat) | is.na(vessel_lon)) |>
  nrow()

if (n_missing_coords > 0) {
  warning(n_missing_coords, " row(s) have missing vessel_lat/vessel_lon after coalescing -- these will be excluded from the spatial join and assigned strata = 'NONE'.")
} else {
  message("No rows with missing coordinates.")
}

# Alternatively: inspect any missing-coordinates rows directly
gps_effort |>
  filter(is.na(vessel_lat) | is.na(vessel_lon))


# Assign 'strata' (FMB, SB, or NONE) based on whether a lat/lon falls within studyarea_shp$WEA

# Re-project the studyarea_shp to match that of the gps_effort lat/lon (EPSG: 4326 or the WGS 84 (World Geodetic System 1984))
studyarea_shp <- st_transform(studyarea_shp, crs = 4326)

# Check that coordinates make sense
st_bbox(studyarea_shp)
#      xmin      ymin      xmax      ymax 
# -62.06301  44.27166 -59.38481  46.82832 

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


# Checking specific times when strata wasn't assigned the same depending on GPS source
# # 1. Zoom in on the exact window from your screenshot and compare the two sources directly
# compare_window <- gps_effort |>
#   filter(datetime_adt >= as_datetime("2026-05-02 07:39:00", tz = "America/Halifax"),
#          datetime_adt <= as_datetime("2026-05-02 07:42:00", tz = "America/Halifax")) |>
#   select(datetime_adt, gps_source, vessel_lat, vessel_lon, strata) |>
#   arrange(datetime_adt)
# 
# compare_window
# 
# # 2. Were any of these rows touched by the erroneous-GPS replacement step?
# # (only meaningful if you still have result_effort/replacement_check_effort in your environment)
# replacement_check_effort |>
#   filter(orig_datetime_adt >= as_datetime("2026-05-02 07:39:00", tz = "America/Halifax"),
#          orig_datetime_adt <= as_datetime("2026-05-02 07:42:00", tz = "America/Halifax"))
# 
# # 3. Quantify the offset: pair each Garmin fix with the nearest-in-time Mysticetus fix
# #    and compute the distance between them (in meters)
# garmin_dt <- as.data.table(compare_window)[gps_source == "garmin"]
# mysticetus_dt <- as.data.table(compare_window)[gps_source == "mysticetus"]
# setkey(garmin_dt, datetime_adt)
# setkey(mysticetus_dt, datetime_adt)
# 
# paired <- garmin_dt[mysticetus_dt, on = "datetime_adt", roll = "nearest",
#                     .(datetime_adt,
#                       garmin_lat = vessel_lat, garmin_lon = vessel_lon, garmin_strata = strata,
#                       mysticetus_lat = i.vessel_lat, mysticetus_lon = i.vessel_lon, mysticetus_strata = i.strata)]
# 
# paired[, dist_m := geosphere::distHaversine(
#   cbind(garmin_lon, garmin_lat), cbind(mysticetus_lon, mysticetus_lat))]
# 
# paired[order(-dist_m)]
# 
# # Plot to inspect
# leaflet(compare_window) |>
#   addProviderTiles(providers$CartoDB.Positron) |>
#   addPolygons(data = studyarea_shp |> filter(WEA == "French/Middle Bank"),
#               color = "black", weight = 1, fillOpacity = 0.05) |>
#   addCircleMarkers(lng = ~vessel_lon, lat = ~vessel_lat,
#                    color = ~ifelse(gps_source == "garmin", "blue", "red"),
#                    radius = 4, label = ~paste(gps_source, datetime_adt)) |>
#   addLegend("bottomright", colors = c("blue", "red"), labels = c("garmin", "mysticetus"))


# Select columns of interest, fill gaps and 
# create variable for daily 'realized' vs 'available' time window for effort/operations
gps_effort1 <- gps_effort |>
  # Select columns of interest
  select(datetime_utc, datetime_adt, date_adt, vessel_lat, vessel_lon, gps_source,
         Vessel, strata, start.effort_adt, end.effort_adt, Effort, 
         Action, Transect_ID, Port_obs, Stb_obs, BigEye_obs, Data_recorder,
         Sea_state, Swell, Port_vis, Stb_vis, Precipitation,
         Glare_int, Glare_left, Glare_right, sog_kt, cog_t, Notes, QAQC_notes) |>
  # Strip stray "names" attribute carried over from the join (shows as "Named chr" in str())
  mutate(Transect_ID = unname(Transect_ID)) |>
  arrange(datetime_adt) |>
  group_by(date_adt) |>
  # Fill the blanks downward (state/categorical columns only)
  fill(c(strata, Effort, start.effort_adt, end.effort_adt,
         Transect_ID, Port_obs, Stb_obs, BigEye_obs, Data_recorder,
         Sea_state, Swell, Port_vis, Stb_vis, Precipitation, Glare_int,
         Glare_left, Glare_right), .direction = "down") |>
  # Fill daily bounds upward (captures rows before the first logged effort entry)
  fill(c(start.effort_adt, end.effort_adt), .direction = "up") |>
  # Fill in the Vessel name for all rows
  fill(Vessel, .direction = "downup") #|>
# TBD if keeping...
  # Replace remaining missing effort values and in_effort_window values
  # mutate(Effort = replace_na(Effort, "Off"),
  #        # Remove any Transect_IDs if in Transit-On effort
  #        Transect_ID = case_when(Effort == "Transit-On" ~ NA_character_,
  #                                TRUE ~ Transect_ID)) |>
  # ungroup() |>
  # # Set certain columns to NA if Effort = Off
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
write_csv(gps_effort1, paste0(path2data, "/Data_summaries/data_combined_cleaned/spring2026_gps_track_effortenv_FINAL.csv"))



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

# Inspect any rows that were marked for deletion which did not get deleted....
# TO DO



# Data wrangling
sight1 <- sight |>
  rename(PSD_m = "PSD (m)",
         Distance_m = "Distance (m)") |>
  mutate(datetime_utc = ymd_hms(`Datetime_UTC (UTC)`, tz = "UTC"),
         # Convert to Atlantic Daylight time
         datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"), # with_tz() = converts the same moment in time to another timezone
         # Create date variable based on Atlantic Daylight Time zone
         date_adt = as_date(datetime_adt),
         Best_count = as.numeric(Best_count),
         Angle_num = as.numeric(str_extract(Angle_rel, "\\d+")),
         PSD = as.numeric(PSD),
         Reticles = as.numeric(Reticles),
         Distance_m = as.numeric(Distance_m),
         Angle_abs = as.numeric(Angle_abs),
         Observer = as.factor(Observer),
         # Set vessel (according to dataset)
         Vessel = case_when(dataset == "Summer2026" ~ "KopitHopson",
                            TRUE ~ "Teleost"),
         # Remove the parentheses, everything inside them & any extra space before the parentheses
         Obs_platform = trimws(gsub("\\s*\\(.*\\)", "", Obs_platform)),
         # Adjust Distance_tool entries
         Distance_tool = case_when(Distance_tool %in% c("Fujinon25x150", "BigEyes") ~ "BigEyes_Fujinon25x150",
                                   TRUE ~ Distance_tool),
         
         
         # TO DO: Update this with the same way that strata was assigned for gps_effort df
         #Strata = ....
  
  
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
  # Remove confirmed or possible resights (duplicates)
  filter(!Resight %in% c("Yes", "Possible")) |>
  # Re-name Species for those which were dead
  mutate(Species = if_else(!is.na(Notes) & str_detect(Notes, regex("dead", ignore_case = TRUE)),
                           paste0(Species, " (dead)"), 
                           Species)) |>
  # Remove columns no longer needed
  select(-c(lat_deg, lat_min, lon_deg, lon_min, lat_dir, lon_dir, Delete_row)) |>
  # Datetime checks
  arrange(`Datetime_UTC_locked (UTC)`) |>
  # Check for any missing values (NA or blank) in the Datetime_UTC (UTC) column, flag those
  mutate(datetime_missing = is.na(`Datetime_UTC (UTC)`) | trimws(`Datetime_UTC (UTC)`) == "",
         # Check for repeated datetimes in the `Datetime_UTC (UTC)`coumn -- due to using the shortcut "Ctl-Shft-+" which copies the previous row
         datetime_duplicates = coalesce(`Datetime_UTC (UTC)` == lag(`Datetime_UTC (UTC)`), FALSE),
         # Not needed for now...
         #Check whether the rows without Yes/No in Photos actually had photos; if not, assign NA to "No"
         #Photos_taken = replace_na(Photos_taken, "No")
         ) |>
  relocate(c(datetime_missing, datetime_duplicates)) |>
  # Select to arrange column order
  select(Vessel, Strata, "Datetime_UTC_locked (UTC)", "Datetime_UTC (UTC)", datetime_utc, datetime_adt,
         date_adt, vessel_lat, vessel_lon, Sgt_lat, Sgt_lon,
         Sgt_type, Sgt_side, Observer, Obs_platform, Distance_tool, 
         Reticles, Distance_m, Angle_rel, Angle_num, Angle_abs, Distance_tool, PSD_m,
         Species, Min_count, Best_count, Max_count, Resight, Photos_taken, 
         Camera, Frame_first, Frame_last, Notes, QAQC_notes,
         datetime_missing, datetime_duplicates)


# Checks

# Manually verify all "datetime_missing" & "datetime_duplicates" columns marked as TRUE
# Verify the Notes to see if the `Datetime_UTC (UTC)` was manually adjusted
# If they seem to be genuine errors, 1) replace the NAs with the `Datetime_UTC_locked (UTC)` and
# 2) Replace the duplicate `Datetime_UTC (UTC)` with the `Datetime_UTC_locked (UTC)`

table(sight1$datetime_missing)
table(sight1$datetime_duplicates)

sight2 <- sight1 |>
  # Replace datetime_duplicates values with the corresponding value from Datetime_UTC_locked (UTC) only when datetime_duplicates == TRUE
  mutate(`Datetime_UTC (UTC)` = if_else(datetime_duplicates, # If datetime_duplicates == TRUE,
                                        `Datetime_UTC_locked (UTC)`, # then replace with value from `Datetime_UTC_locked (UTC)`
                                        `Datetime_UTC (UTC)`)) |> # otherwise (if FALSE), keep the original `Datetime_UTC (UTC)`
  select(-c(datetime_missing, datetime_duplicates))
  
# Verify the Sgt_type is all filled in
table(sight2$Sgt_type)

# Verify Reticle values make sense
summary(sight2$Reticles)
# Fujinon 7x50 max reticles = [TO FILL IN]
# Fujinon 25x150 (Big Eyes) max reticles = [TO FILL IN] 



# Verify the Angle_abs (calculation) for the BigEyes sightings
# [TO DO] Might need to convert the BigEyes angles to +/- 90 degrees



# Verify Distance values make sense
summary(sight2$Distance_m)

# Verify that all "On-effort" sightings have relevant info
x <- sight2 |>
  filter(Sgt_type == "On-effort") |>
  select(datetime_adt, Sgt_type, Reticles, Distance_m, Distance_tool,
         Angle_rel, Angle_num, Angle_abs, PSD)
# On-effort Sighting of a grey seal at 2026-04-30T17:02:30.9 utc is missing distance;
# Can check in the auto-saves to see if Distance was entered at one point but accidentally deleted;
# If can't find distance, this sighting will have to be changed to Sgt_type = "incidental"

# Reproduce Angle_abs calculation that Mysticetus does (to confirm it's accurate)
# [TO DO...]

# Reproduce distance_m calculation that Mysticetus does (to confirm it's accurate)
# [TO DO...]

# Verify Species ID
unique(sight2$Species)
table(sight2$Species)

# Reticles vs Distance_m
sight2 |>
  filter(Distance_tool == "Fujinon7x50") |>
  ggplot(aes(x = Distance_m, y = Reticles)) +
  geom_point()

#...............................................................................


# Export

# (1) Marine mammal species only
sight3 <- sight2 |>
  filter(!Species %in% c("Mola mola", "White shark", "Other", "Other (dead)"))

# Export
write_csv(sight3, file = paste0(here(),"/Data_summaries/tables/spring2026_MM_sightings_FINAL.csv"))

# (2) All sightings (except "Other") to provide to DFO MAR's Whale Sightings Database (WSDB)
# (also, remove unnecessary columns and add a couple of helpful columns to identify data collector & survey)
sight4 <- sight2 |>
  filter(!Species %in% c("Other", "Other (dead)")) |>
  mutate(Data_collector = "DFO_Maritimes_CRMP",
         Survey = "WEAWAVS_Spring2026") |>
  select(Data_collector, Survey, Vessel, Strata, datetime_utc, datetime_adt,
         vessel_lat, vessel_lon, Sgt_lat, Sgt_lon, Sgt_type, Sgt_side, Observer,
         Species, Min_count, Best_count, Max_count,
         Photos_taken, Camera, Frame_first, Frame_last, Notes)

unique(sight4$Species)

# Export
write_csv(sight4, file = paste0(here(),"/Data_summaries/tables/WEAWAVS_Spring2026_Sightings_WSDB_updated20260604.csv"))



#----GPS + EFFORT + SIGHTINGS----

names(gps_effort1)
names(sight2)

# Keep only necessary columns from gps_effort1 (to make join cleaner) 
gps_effort2 <- gps_effort1 |> 
  select(datetime_adt, Effort, avail_effort_window, real_effort_window, 
         Transect_ID, Port_obs, Stb_obs, BigEye_obs, Data_recorder, 
         Sea_state, Swell, Port_vis, Stb_vis, Precipitation, 
         Glare_int, Glare_left, Glare_right, sog_kt, cog_t) |> 
  rename(gps_datetime_adt = datetime_adt)

# Join gps_effort2 to sight2 (to have the relevant effort/env variables associated with the sightings) 
sight_gps_effort <- sight3 |> 
  left_join(gps_effort2, join_by(closest(datetime_adt >= gps_datetime_adt))) |> 
  mutate(time_diff_sec = abs(as.numeric(datetime_adt - gps_datetime_adt, units = "secs")), 
         time_diff_flag = time_diff_sec > 5)
  
# Export
write_csv(sight_gps_effort, file = paste0(here(), "/Data_summaries/tables/spring2026_MM_sightings_gps_effort_FINAL.csv"))


#----QUICK MAPS + SUMMARY TABLES----







# TO DO: MOVE TO SCRIPT # 3.....


#-------------------------------------------
# SURVEY SUMMARY TABLES, FIGURES, HISTOGRAMS
#-------------------------------------------

# List of tables:
#...


# Table 1a: Full study area - for each "date_adt", both within the "avail_effort_window" and the "real_effort_window", 
# calculate the time (hours) and distance (km) spent in different effort categories.

# Note: distance needs to be calculated incrementally, from one datetime_adt to the next (not from the start and end time of each effort block)

# =========================================================
# STEP 1: Calculate incremental time + distance
# =========================================================

gps_effort_step <- gps_effort1 %>%
  
  arrange(datetime_adt) %>%
  
  group_by(date_adt) %>%
  
  mutate(
    
    # Next position/time
    next_time = lead(datetime_adt),
    next_lat  = lead(vessel_lat),
    next_lon  = lead(vessel_lon),
    
    # Time difference to next record (seconds)
    dt_sec = as.numeric(
      difftime(next_time, datetime_adt, units = "secs")
    ),
    
    # Incremental distance between consecutive points (m)
    dist_m = geosphere::distHaversine(
      cbind(vessel_lon, vessel_lat),
      cbind(next_lon, next_lat)
    ),
    
    # Convert to km
    dist_km = dist_m / 1000
    
  ) %>%
  
  # Remove final row of each day
  filter(!is.na(dt_sec)) %>%
  
  # OPTIONAL QA/QC:
  # Remove unrealistic time gaps (>10 sec)
  filter(dt_sec <= 10) %>%
  
  ungroup()

# =========================================================
# STEP 2: Calculate total daily effort windows
# =========================================================

daily_totals <- gps_effort_step %>%
  
  group_by(date_adt) %>%
  
  summarise(
    
    # Actual realized effort time (hours)
    total_h_real_effort_window =
      round(
        sum(dt_sec[real_effort_window], na.rm = TRUE) / 3600,
        2
      ),
    
    # Fixed available effort window (06:30–19:00)
    total_h_avail_effort_window =
      round(
        as.numeric(
          difftime(
            first(end.avail.effort_adt),
            first(start.avail.effort_adt),
            units = "hours"
          )
        ),
        2
      ),
    
    .groups = "drop"
  )

# =========================================================
# STEP 3: Summarize effort categories by day
# =========================================================

effort_summary <- gps_effort_step %>%
  
  group_by(date_adt, Effort) %>%
  
  summarise(
    
    # ---------------------------------
    # REALIZED EFFORT WINDOW
    # ---------------------------------
    
    hours_real =
      round(
        sum(dt_sec[real_effort_window], na.rm = TRUE) / 3600,
        2
      ),
    
    km_real =
      round(
        sum(dist_km[real_effort_window], na.rm = TRUE),
        2
      ),
    
    # ---------------------------------
    # AVAILABLE EFFORT WINDOW
    # ---------------------------------
    
    hours_avail =
      round(
        sum(dt_sec[avail_effort_window], na.rm = TRUE) / 3600,
        2
      ),
    
    km_avail =
      round(
        sum(dist_km[avail_effort_window], na.rm = TRUE),
        2
      ),
    
    # ---------------------------------
    # Spatial coverage
    # ---------------------------------
    
    strata_covered =
      paste(
        unique(
          na.omit(
            Strata[Strata != "Not.in.strata"]
          )
        ),
        collapse = ", "
      ),
    
    transects_visited =
      paste(
        unique(na.omit(Transect_ID)),
        collapse = ", "
      ),
    
    .groups = "drop"
  ) %>%
  
  # Add daily totals
  left_join(daily_totals, by = "date_adt") %>%
  
  # Calculate proportions WITHIN each date
  group_by(date_adt) %>%
  
  mutate(
    
    # ---------------------------------
    # Proportion of realized effort window
    # ---------------------------------
    
    prop_time_real =
      round(
        hours_real / total_h_real_effort_window,
        2
      ),
    
    prop_dist_real =
      round(
        km_real / sum(km_real, na.rm = TRUE),
        2
      ),
    
    # ---------------------------------
    # Proportion of available effort window
    # ---------------------------------
    
    prop_time_avail =
      round(
        hours_avail / total_h_avail_effort_window,
        2
      ) #,
    
   # prop_dist_avail =
  #    round(
  #      km_avail / sum(km_avail, na.rm = TRUE),
  #      2
  #    )
    
  ) %>%
  
  ungroup() %>%
  
  arrange(date_adt, desc(hours_real))

# =========================================================
# View result
# =========================================================

effort_summary




# OLD:

# Table 1a:
effort_summary <- gps_effort1 |>

  arrange(datetime_adt) |>
  group_by(date_adt)|>
  # Time to next record (seconds)
  mutate(dt_sec = as.numeric(difftime(lead(datetime_adt), datetime_adt,
                                      units = "secs"))) |>
  filter(!is.na(dt_sec)) |>
  
  # Total hours within "realized effort window"
  mutate(total_h_real_effort_window = round(as.numeric(difftime(
            max(end.effort_adt, na.rm = TRUE),
            min(start.effort_adt, na.rm = TRUE),
            units = "hours")), 2)) |>
  
  # Total hours within "available effort window"
  mutate(total_h_avail_effort_window = round(as.numeric(difftime(
    max(end.avail.effort_adt, na.rm = TRUE),
    min(start.avail.effort_adt, na.rm = TRUE),
    units = "hours")), 2)) |>
  
  # TO DO: modify code below so that it computes the proportion of time spent in each effort category for 
  # both the real_effort_window and the avail_effort_window
  group_by(date_adt, Effort, total_h_real_effort_window) |>
  
  summarise(hours = round(sum(dt_sec) / 3600, 2),
            prop_time_real = round(hours / total_h_real_effort_window, 2),
            strata_covered = paste(unique(na.omit(strata[strata != "Not.in.strata"])),
                                   collapse = ", "),
            transects_visited = paste(unique(na.omit(Transect_ID)), collapse = ", "),
            .groups = "drop") |>
  arrange(date_adt, desc(hours)) |>
  distinct()

# Reduced effort summary
effort_summary_total <- effort_summary %>%
  group_by(Effort) %>%
  summarise(total_hours = round(sum(hours, na.rm = TRUE), 2), .groups = "drop") %>%
  mutate(total_effort_window_hours = round(sum(total_hours), 2),
         prop_total_time = round(total_hours / total_effort_window_hours, 2)) %>%
  arrange(desc(total_hours))

effort_summary_total

# Export
write_csv(effort_summary, file = "reports/effort_summary_FINAL.csv")
write_csv(effort_summary_total, file = "reports/effort_summary_total_FINAL.csv")


# Table 1b: Per strata (FMB, SB, Not.in.strata) - for each "date_adt", both within the "avail_effort_window" and the "real_effort_window", 
# calculate the time (hours) and distance (km) spent in different effort categories.

# NOTE: Before computing the required summaries for Table 1b, "overlay" the study area shapefile onto the 
# vessel_lon and vessel_lat of the gps_effort2 df. There should be 2 polygons in the study area called 
# French/Middle Bank (FMB) and Sydney Bight (SB); if not, stop here and we'll determine which is which.
# Next, for each row containing a vessel_lon and vessel_lat within gps_effort2, create a new column called
# "Strata_shp" and assign each row to either "FMB", "SB" or "Not.in.strata" based on whether the GPS position 
# is within, or outside of, each strata.





# TO DO...................................................

# Table 2: Non-survey days

# Number of hours lost to 'making turns' between transects within each strata
# - within each day, produce a summary table showing the total distance
# and total time spent between the last occurrence of a transect_ID and the occurrence
# of the next transect_ID (or a different transect_ID; not necessarily sequential)

# Number of hours lost to bad weather
# - calculate proportion of time spent in different Sea state and visibility categories
# (needs to be the sum of sequential time blocks in each category)




# ---------------------------
# Distances covered per
# continuous effort segment
# ---------------------------

gps_effort_steps <- gps_effort1 |>
  
  # Keep only rows with coordinates
  # AND within effort window
  filter(
    !is.na(vessel_lat),
    !is.na(vessel_lon),
    in_effort_window) |>
  
  # Optional:
  # keep only observation window
  # filter(obs_window)
  
  arrange(datetime_adt) |>
  
  ungroup() |>
  
  # IMPORTANT:
  # group by date + strata BEFORE
  # defining effort segments
  group_by(strata, date_adt) |>
  
  mutate(
    
    # Start new segment whenever
    # effort changes
    effort_change =
      Effort != lag(
        Effort,
        default = first(Effort)
      ),
    
    segment_id = cumsum(effort_change)
  ) |>
  
  # IMPORTANT:
  # include date + strata in grouping
  group_by(strata, date_adt, segment_id) |>
  
  mutate(
    
    lon_lag = lag(vessel_lon),
    lat_lag = lag(vessel_lat),
    
    # Great-circle distance (m)
    step_dist_m =
      geosphere::distHaversine(
        cbind(lon_lag, lat_lag),
        cbind(vessel_lon, vessel_lat)
      ),
    
    step_dist_km = step_dist_m / 1000
  ) |>
  
  ungroup()


# ---------------------------
# Distance per effort segment
# ---------------------------

effort_segments <- gps_effort_steps |>
  
  group_by(
    strata,
    date_adt,
    segment_id,
    Effort
  ) |>
  
  summarise(
    segment_start = min(datetime_adt),
    segment_end = max(datetime_adt),
    segment_km = sum(step_dist_km, na.rm = TRUE),
    .groups = "drop")


# ---------------------------
# Total distance per
# effort category
# ---------------------------

effort_totals <- effort_segments |>
  
  group_by(
    strata,
    date_adt,
    Effort
  ) |>
  
  summarise(
    
    total_km =
      round(
        sum(segment_km, na.rm = TRUE),
        2
      ),
    
    .groups = "drop"
  ) |>
  
  arrange(
    date_adt,
    strata,
    Effort
  )

effort_totals

# Export
write_csv(effort_totals, file = "reports/Distance_by_strata_date_effort.csv")



# ---------------------------
# Distance covered per planned transect
# (while in On or Bridge-On effort)
# ---------------------------

# Step 1: Calculate GPS step distances

gps_transect_steps <- gps_effort1 %>%
  
  # Keep only survey effort
  filter(Effort  == "On") %>%
  # filter(Effort %in% c("On", "Bridge-On")) %>%
  
  # Remove missing coordinates
  filter(!is.na(vessel_lat),
         !is.na(vessel_lon),
         !is.na(Transect_ID)) %>%
  
  # Ensure chronological order
  arrange(datetime_utc) %>%
  
  ungroup() %>%
  
  # Create segment IDs whenever:
  # 1) effort changes
  # 2) transect changes
  mutate(
    new_segment =
      Effort != lag(Effort, default = first(Effort)) |
      Transect_ID != lag(Transect_ID, default = first(Transect_ID)),
    
    segment_id = cumsum(new_segment)) %>%
  
  # Calculate distances within segments
  group_by(segment_id) %>%
  
  mutate(lon_lag = lag(vessel_lon),
         lat_lag = lag(vessel_lat),
         
         # Great-circle distance (m)
         step_dist_m = geosphere::distHaversine(
           cbind(lon_lag, lat_lag),
           cbind(vessel_lon, vessel_lat)),
         
         # Convert to km
         step_dist_km = step_dist_m / 1000,
         
         # Remove unrealistic jumps
         step_dist_km = ifelse(step_dist_km > 2, NA, step_dist_km)) %>%
  
  ungroup()


# Step 2: Summarize observed distance
# per transect

transect_effort_summary <- gps_transect_steps %>%
  
  group_by(Transect_ID) %>%
  
  summarise(
    surveyed_km = sum(step_dist_km, na.rm = TRUE),
    n_segments = n_distinct(segment_id),
    survey_start = min(datetime_utc),
    survey_end   = max(datetime_utc),
    .groups = "drop") %>%
  
  # Join planned transect distances
  left_join(
    transects_df1 %>%
      select(Transect_ID,
             Strata,
             Direction,
             Transect_km),
    by = "Transect_ID") %>%
  
  # Calculate coverage %
  mutate(
    pct_covered = (surveyed_km / Transect_km) * 100) %>%
  
  # Round numeric values
  mutate(
    across(c(surveyed_km,
             Transect_km,
             pct_covered),
           ~ round(.x, 1))) %>%
  
  arrange(Transect_ID)

transect_effort_summary




# ---------------------------
# % of planned transects completed
# by strata
# ---------------------------

# Transect completion summary using ALL planned transects

# Define completion threshold
completion_threshold <- 95

# Start from ALL planned transects
transect_completion_summary <- transects_df1 %>%
  
  distinct(Transect_ID,
           Strata,
           Transect_km) %>%
  
  # Join observed survey coverage
  left_join(
    transect_effort_summary %>%
      select(Transect_ID,
             surveyed_km,
             pct_covered),
    by = "Transect_ID"
  ) %>%
  
  # Replace missing survey values with 0
  mutate(
    surveyed_km = replace_na(surveyed_km, 0),
    pct_covered = replace_na(pct_covered, 0),
    
    completed = pct_covered >= completion_threshold
  ) %>%
  
  # Summarize by strata
  group_by(Strata) %>%
  
  summarise(
    n_transects = n(),
    n_completed = sum(completed, na.rm = TRUE),
    pct_completed =
      round((n_completed / n_transects) * 100, 1),
    .groups = "drop"
  )

transect_completion_summary



# ---------------------------
# % of planned transect distance covered
# by strata
# ---------------------------


# Step 1: Total surveyed distance

# Use ALL planned transects as the base table
# so unsurveyed transects contribute 0 km

surveyed_by_strata <- transects_df1 %>%
  
  distinct(Transect_ID,Strata,Transect_km) %>%
  
  # Join observed survey distances
  left_join(transect_effort_summary %>% 
              select(Transect_ID, surveyed_km),
            by = "Transect_ID") %>%
  
  # Replace unsurveyed transects with 0 km
  mutate(surveyed_km = replace_na(surveyed_km, 0)) %>%
  
  # Sum surveyed distance by strata
  group_by(Strata) %>%
  
  summarise(surveyed_km = sum(surveyed_km, na.rm = TRUE), .groups = "drop")

# ---------------------------
# Step 2: Total planned distance
# ---------------------------

planned_by_strata <- transects_df1 %>%
  
  distinct(Transect_ID, Strata, Transect_km) %>%
  
  group_by(Strata) %>%
  
  summarise(planned_km = sum(Transect_km, na.rm = TRUE), .groups = "drop")

# ---------------------------
# Step 3: Combine and calculate %
# ---------------------------

strata_distance_summary <- planned_by_strata %>%
  
  left_join(surveyed_by_strata, by = "Strata") %>%
  
  mutate(surveyed_km = replace_na(surveyed_km, 0),
         
         pct_distance_covered =
           round((surveyed_km / planned_km) * 100, 1)) %>%
  
  arrange(Strata)

strata_distance_summary

write_csv(strata_distance_summary, file = "reports/strata_distance_summary_May11.csv")


# Summary by Strata
species_summary_by_strata <- sight2 %>%
  
  # Remove species containing "unknown"
  filter(!str_detect(str_to_lower(Species), "unknown")) %>%
  
  group_by(Strata, Species) %>%
  
  summarise(
    n_individuals = sum(best_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  arrange(Strata, desc(n_individuals))

# Number of unique species per strata
unique_species_by_strata <- sight2 %>%
  
  filter(!str_detect(str_to_lower(Species), "unknown")) %>%
  
  group_by(Strata) %>%
  
  summarise(
    n_unique_species = n_distinct(Species),
    .groups = "drop"
  )

# Join together
summary_table <- species_summary_by_strata %>%
  left_join(unique_species_by_strata, by = "Strata")

summary_table

write_csv(sight.tbl, file = "reports/sightings.cumulative.count_May11.csv")




#----PSD Histograms----

psd_df <- sight |>
  # Remove NAs in PSD
  filter(!is.na(PSD)) |>
  # Change class to numeric
  mutate(PSD = as.numeric(PSD),
  #        # Create distance bins
  #        PSD_bin = case_when(PSD <= 100 ~ "0-100",
  #                            PSD > 100 & PSD <= 200 ~ "101-200",
  #                            PSD > 200 & PSD <= 300 ~ "201-300",
  #                            PSD > 300 & PSD <= 400 ~ "301-400",
  #                            PSD > 400 & PSD <= 500 ~ "401-500",
  #                            PSD > 500 & PSD <= 600 ~ "501-600",
  #                            PSD > 600 & PSD <= 700 ~ "601-700",
  #                            PSD > 700 & PSD <= 800 ~ "701-800",
  #                            PSD > 800 & PSD <= 900 ~ "801-900",
  #                            PSD > 900 & PSD <= 1000 ~ "901-1000",
  #                            PSD > 1000 ~ ">1000")) |>
  # # Set order of the distance bins
  # mutate(PSD_bin = factor(PSD_bin, levels = c(
  #       "0-100", "101-200", "201-300", "301-400",
  #       "401-500", "501-600", "601-700", "701-800",
  #       "801-900", "901-1000", ">1000")),
        # Create species groupings
        Species_group = case_when(
          Species %in% c("Fin/sei whale", "Humpback whale", 
                          "Minke whale", "Unknown baleen whale", "Blue whale") ~ "Baleen whales",
          Species %in% c("Unknown dolphin", "White-beaked dolphin") ~ "Dolphins (excl. KW)",
          Species %in% c("Grey seal", "Unknown seal", "Harbour seal") ~ "Seals")) |>
  mutate(Species_group = factor(Species_group, levels = c(
    "Seals", "Dolphins (excl. KW)", "Baleen whales")))
          
table(psd_df$Sgt_type)
table(psd_df$Species)
table(psd_df$Species_group)


# Histograms of PSD per species or species group

# (1) Species groups

# First, remove the NA from the Species_group column
psd_df2 <- psd_df |>
  filter(!is.na(Species_group)) |>
# Keep sightings within 3km only (there is one outlier at 4.7km)
filter(PSD <= 3000)

table(psd_df$Species_group, psd_df$Sgt_type)

# Create easier labels
labels_df <- psd_df2 %>%
  count(Species_group, name = "n_group") %>%
  arrange(Species_group) %>%   # <-- respects factor order
  mutate(Species_group_label = paste0(Species_group, " (n = ", n_group, ")"))

# Join labels to df
psd_df2 <- psd_df2 %>%
  left_join(labels_df, by = "Species_group") %>%
  mutate(Species_group_label = factor(Species_group_label,
                                      levels = labels_df$Species_group_label))

p1 <- ggplot(psd_df2, aes(x = PSD)) +
  geom_histogram(binwidth = 100,  boundary = 0, # Consistent bin alignment
                 fill = "steelblue", color = "white") +
  facet_wrap(~ Species_group_label, scales = "free_y", ncol=1) +
  labs(title = "Perpendicular Sighting Distance - Species groups",
       x = "Distance (m)",
       y = "Number of sightings") +
  scale_x_continuous(breaks = seq(0, 3000, by = 100)) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 14, face = "bold"))  # facet label size

p1

# Export 
ggsave("reports/psd_histogram_species_groups_May3.png", plot = p1, width = 7, height = 7, units = "in", dpi = 300)


# (2) Species with > 10 sightings

table(psd_df$Species)

psd_df2 <- psd_df %>%
  filter(Species %in% c("Grey seal", "Harbour porpoise", "Humpback whale")) %>%
  mutate(Species = factor(Species,
                          levels = c("Grey seal", "Harbour porpoise", "Humpback whale"))) %>%
  group_by(Species) %>%
  mutate(Species_label = paste0(Species, " (n = ", n(), ")")) %>%
  ungroup() %>%
  mutate(Species_label = factor(Species_label, levels = unique(Species_label)))


p2 <- ggplot(psd_df2, aes(x = PSD)) +
  geom_histogram(binwidth = 100, boundary = 0,fill = "steelblue", color = "white") +
  facet_wrap(~ Species_label, scales = "free_y", ncol = 1) +
  labs(title = "Perpendicular Sighting Distance - Selected Species",
       x = "Distance (m)", 
       y = "Number of sightings") +
  scale_x_continuous(breaks = seq(0, 3000, by = 100)) +
  theme(plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14, face = "bold"))

p2

# Export 
ggsave("reports/psd_histogram_select_species_May3.png", plot = p2, width = 7, height = 7, units = "in", dpi = 300)



# What could be causing the spike within the 400-500m bin?
x <- sight |>
  #filter(Species == "Humpback whale") |>
  filter(Observer != "Other")
  arrange(PSD)

# Histogram with different depth bins
ggplot(x, aes(x = PSD)) +
  geom_histogram(breaks = seq(0, 3000, by = 200), fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(0, 3000, by = 200)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(title = "Perpendicular Sighting Distance - Humpback whale")

# Export to insect in excel
write.csv(x, "psd_HW.csv")

# Scatterplots of Angle_rel, Angle_abs, and Reticles with PSD, and by observer

# Angle_rel ~ PSD
ggplot(x, aes(x = PSD, y = Angle_num)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 3000, by = 200)) +
  labs(x = "PSD", y = "Angle_rel") + 
  # Add a rectangle to delineate distances of interest
  annotate("rect",
           xmin = 400, xmax = 600,
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "grey") +
  theme_minimal()

# Angle_abs ~ PSD
ggplot(x, aes(x = PSD, y = Angle_abs)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 3000, by = 200)) +
  labs(x = "PSD", y = "Angle_abs") + 
  # Add a rectangle to delineate distances of interest
  annotate("rect",
           xmin = 400, xmax = 600,
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "grey") +
  theme_minimal()

# Reticles ~ PSD
ggplot(x, aes(x = PSD, y = Reticles)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 3000, by = 200)) +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  labs(x = "PSD", y = "Reticles") + 
  # Add a rectangle to delineate distances of interest
  annotate("rect",
           xmin = 400, xmax = 600,
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "grey") +
  theme_minimal()


# Sea state ~ PSD

# Reduce effort & sight dataframes to relevant columns
effort1 <- effort |>
  select(datetime_utc, Effort, Sea_state, Swell, Port_vis, Stb_vis)

sight1 <- sight |>
  select(datetime_utc, Sgt_type, Observer, Obs_platform, 
         Reticles, Distance_m, Angle_num, Species, Best_count, PSD)

effort.sight <- effort1 |>
  full_join(sight1) |>
  arrange(datetime_utc) |>
  mutate(date = as.Date(datetime_utc)) |>
  group_by(date) |>
  fill(c(Effort, Sea_state, Swell, Port_vis, Stb_vis), 
       .direction = "down") |>
  filter(!is.na(Sea_state))

# PSC ~ Sea_state (BFT)
ggplot(effort.sight, aes(x = Sea_state, y = PSD)) + 
  geom_point(alpha = 0.6) 


# PSD histogram faceted by observer
ggplot(x, aes(x = PSD)) +
  geom_histogram(breaks = seq(0, 3000, by = 200), fill = "steelblue", color = "white") +
  facet_wrap(~Observer)
  scale_x_continuous(breaks = seq(0, 3000, by = 200)) +
  labs(title = "Perpendicular Sighting Distance by Observer")
  
# Angle_rel histogram faceted by observer
x1 <- x |>
  filter(Angle_num < 120) |>
  filter(Sgt_type == "On-effort")

ggplot(x1, aes(x = Angle_num)) +
  geom_histogram(fill = "steelblue", color = "white") +
  facet_wrap(~Observer) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) +
  labs(title = "Angle of detection by Observer")

table(x1$Sgt_type)


# Depth(m) ~ PSD

library(terra)

# Load GEBCO bathymetry raster
bathy <- rast("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/Mapping/ETOPO1_Bed_g_geotiff.tif")

# Prepare sight df
sight1 <- sight |>
  arrange(datetime_utc) |>
  mutate(row_id = row_number())

# Convert to spatial points
pts <- terra::vect(
  sight1 |> dplyr::select(row_id, Sgt_lon, Sgt_lat),
  geom = c("Sgt_lon", "Sgt_lat"),
  crs = "EPSG:4326")

# Extract bathymetry values
depth_vals <- extract(bathy, pts)

# Combine with your original data
sight1$depth_m <- depth_vals[,2]*-1

x <- sight1 |>
  filter(Species == "Humpback whale") |>
  mutate(PSD = as.numeric(PSD),
         Reticles = as.numeric(Reticles),
         Angle_abs = as.numeric(Angle_abs)) |>
  arrange(PSD)

# Depth(m) ~ PSD
ggplot(x, aes(x = PSD, y = depth_m)) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 3000, by = 200)) +
  #scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  labs(x = "PSD", y = "Depth (m)") + 
  # Add a rectangle to delineate distances of interest
  annotate("rect",
           xmin = 400, xmax = 600,
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "grey") +
  theme_minimal()







# ---------------------------
# Interactive mapping
# ---------------------------

# Create a leaflet map that allows:
# Toggle layers (date_adt, Effort, Sea state, Port_vis, Stb_vis)
# Effort color-coded tracklines
# Cumulative sightings layer
# Planned transects overlay
# Uses with gps_effort1 df + sight df + transects (shp) + study area (shp)

# =========================================================
# Interactive leaflet survey map
# =========================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# =========================================================
# 1. STUDY AREA
# =========================================================

studyarea_shp <- st_read(
  file.path(project_path,
            "spatial/Study_area_buf_prelim.shp"),
  quiet = TRUE) |>
  st_transform(4326)

# =========================================================
# 2. GPS TRACK PREPARATION
# =========================================================

gps_sf <- gps_effort1 |>
  filter(
    !is.na(vessel_lon),
    !is.na(vessel_lat)  ) |>
  arrange(datetime_adt) |>
  st_as_sf(
    coords = c("vessel_lon", "vessel_lat"),
    crs = 4326,
    remove = FALSE  )

# Create segmented tracklines
track_lines <- gps_sf |>
  
  group_by(date_adt) |>
  
  mutate(
    time_diff = as.numeric(
      difftime(
        datetime_adt,
        lag(datetime_adt),
        units = "mins"
      )
    ),
    
    effort_change = Effort != lag(Effort),
    
    new_segment =
      replace_na(effort_change, TRUE) |
      replace_na(time_diff > 10, TRUE),
    
    segment_id = cumsum(new_segment)
  ) |>
  
  ungroup()

# Build LINESTRING geometries
track_lines_sf <- track_lines |>
  
  group_by(
    date_adt,
    Effort,
    segment_id
  ) |>
  
  summarise(
    do_union = FALSE,
    .groups = "drop") |>
  
  st_cast("LINESTRING")

# =========================================================
# 3. SIGHTINGS LAYER
# =========================================================

sight_sf <- sight |>
  
  filter(
    !is.na(Sgt_lon),
    !is.na(Sgt_lat)) |>
  
  st_as_sf(
    coords = c("Sgt_lon", "Sgt_lat"),
    crs = 4326)

# =========================================================
# 4. COLOR PALETTES
# =========================================================

# =========================================================
# EFFORT COLORS
# =========================================================

effort_cols <- c(
  "On" = "#00FF00",
  "Off" = "#FF0000",
  "Transit-On" = "#FFD700",
  "Bridge-On" = "#0000FF"
)

# Create explicit color column
track_lines_sf <- track_lines_sf |>
  
  mutate(
    effort_color = effort_cols[Effort]
  )

# Transect palette
compl_pal <- colorFactor(
  palette = c(
    "Complement 1" = "grey40",
    "Complement 2" = "black"
  ),
  domain = transects_shp$complement
)

# =========================================================
# 5. INITIALIZE MAP
# =========================================================

m <- leaflet(options = leafletOptions(
  preferCanvas = TRUE)) |>
  
  addProviderTiles(
    providers$Esri.OceanBasemap)

# =========================================================
# 6. ADD TRANSECTS
# =========================================================

m <- m |>
  addPolylines(
    data = transects_shp,
    color = ~compl_pal(complement),
    weight = 2,
    opacity = 0.6,
    group = "Planned Transects",
    popup = ~paste0(
      "<b>Transect:</b> ", label, "<br>",
      "<b>Strata:</b> ", strata, "<br>",
      "<b>Complement:</b> ", complement))

# =========================================================
# 7. ADD STUDY AREA
# =========================================================

m <- m |>
  addPolygons(
    data = studyarea_shp,
    color = "black",
    weight = 1,
    opacity = 0.5,
    fill = FALSE,
    group = "Study Area")

# =========================================================
# 8. ADD DATE-SPECIFIC TRACKS + SIGHTINGS
# =========================================================

dates <- sort(unique(track_lines_sf$date_adt))

for (d in dates) {
  
  d_chr <- as.character(d)
  
  track_group <- paste0("Track: ", d_chr)
  
  sight_group <- paste0("Sightings: ", d_chr)
  
  # FILTER TRACKS
  tracks_d <- track_lines_sf |>
    filter(date_adt == d)
  
  # FILTER SIGHTINGS
  sights_d <- sight_sf |>
    filter(date_adt == d)
  
  # -----------------------------
  # TRACKLINES
  # -----------------------------
  
  m <- m |>
    
  addPolylines(
        data = tracks_d,
        
        color = ~effort_color,
      
      weight = 3,
      
      opacity = 0.9,
      
      group = track_group,
      
      popup = ~paste0(
        "<b>Date:</b> ", date_adt,
        "<br><b>Effort:</b> ", Effort
      )
    )
  
  # -----------------------------
  # SIGHTINGS
  # -----------------------------
  
  m <- m |>
    
    addCircleMarkers(
      data = sights_d,
      
      radius = 4,
      
      color = "purple",
      
      stroke = FALSE,
      
      fillOpacity = 0.8,
      
      group = sight_group,
      
      popup = ~paste0(
        "<b>Species:</b> ", Species,
        "<br><b>Count:</b> ", Best_count
      )
    )
}

# =========================================================
# 9. LAYER CONTROLS
# =========================================================

m <- m |>
  
  addLayersControl(
    
    overlayGroups = c(
      "Study Area",
      "Planned Transects",
      
      paste0("Track: ", dates),
      
      paste0("Sightings: ", dates)
    ),
    
    options = layersControlOptions(
      collapsed = FALSE
    )
  )


# =========================================================
# 10. LEGENDS
# =========================================================

m <- m |>
  
  addLegend(
    position = "topleft",
    
    colors = unname(effort_cols),
    
    labels = names(effort_cols),
    
    title = "Effort",
    
    opacity = 1
  ) |>
  
  addLegend(
    position = "bottomleft",
    
    pal = compl_pal,
    
    values = transects_shp$complement,
    
    title = "Transect Complement"
  )

# =========================================================
# 11. VIEW MAP
# =========================================================

m

# =========================================================
# 12. EXPORT HTML
# =========================================================

saveWidget(
  m,
  "survey_leaflet_map.html",
  selfcontained = TRUE
)







## OLD...................
# Import study area (shapefile; for mapping)
# studyarea_shp <- st_read(file.path(project_path, "spatial/Study_area_buf_prelim.shp"),
#   quiet = TRUE)
# 
# # Reproject to WGS84 (required for leaflet)
# studyarea_shp <- st_transform(studyarea_shp, 4326)
# 
# # Prepare GPS track (convert to sf + lines)
# 
# # Convert points to sf
# gps_sf <- gps_effort1 |>
#   filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
#   st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326) |>
#   arrange(datetime_adt) 
# 
# # Create an "effort segment ID" (detect breaks in effort)
# track_lines <- gps_sf |>
#   group_by(date_adt) |>
#   mutate(time_diff = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")),
#          effort_change = Effort != lag(Effort),
#          new_segment = replace_na(effort_change, TRUE) | replace_na(time_diff > 10, TRUE),
#          segment_id = cumsum(new_segment)) |>
#   ungroup()
# 
# #unique(track_lines$Effort)
# 
# # Build lines using segments
# track_lines_sf <- track_lines |>
#   group_by(date_adt, Effort, segment_id) |>
#   summarise(do_union = FALSE) |>
#   st_cast("LINESTRING") |>
#   ungroup() |>
#   mutate(Effort = factor(Effort, levels = c(
#       "On", "Off", "Transit-On", "Bridge-On"))) # , "Closing"
# 
# #st_geometry_type(track_lines_sf)
# 
# # Prepare sightings layer
# sight_sf <- sight |>
#   filter(!is.na(Sgt_lon), !is.na(Sgt_lat)) |>
#   st_as_sf(coords = c("Sgt_lon", "Sgt_lat"), crs = 4326)
# 
# # Effort color palette
# effort_pal <- colorFactor(
#   palette = c(
#     "On" = "#00FF00",
#     "Off" = "#FF0000",
#     "Transit-On" = "#FFD700",
#     "Bridge-On" = "#0000FF",
#     #"Closing" = "#FFA500"
#     ),
#   domain = unique(track_lines_sf$Effort))
# 
# # Color-code transect 'complement'
# compl_pal <- colorFactor(palette = c("Complement 1" = "grey40", 
#                                      "Complement 2" = "black"),
#   domain = transects_shp$complement)
# 
# # Check before building map:
# data.frame(
#   Effort = levels(track_lines_sf$Effort),
#   Color = effort_pal(levels(track_lines_sf$Effort)))
# 
# 
# # Build the interactive map
# m <- leaflet() |>
#   # Base map
#   addProviderTiles(providers$Esri.OceanBasemap) |>
#  # Planned transects
#   addPolylines(
#     data = transects_shp,
#     color = ~compl_pal(complement),
#     weight = 2,
#     opacity = 0.6,
#     group = "Planned Transects",
#     popup = ~paste0(
#       "<b>Transect:</b> ", label, "<br>",
#       "<b>Strata:</b> ", strata, "<br>",
#       "<b>Complement:</b> ", complement))
# 
# # Add study area shapefile
# m <- m |>
#   addPolygons(
#     data = studyarea_shp,
#     color = "black",
#     weight = 1,
#     opacity = 0.3,
#     fill = FALSE,
#     group = "Study Area")
# 
# # Add legend for transect complement
# m <- m %>%
#   addLegend(
#     "bottomleft",
#     pal = compl_pal,
#     values = transects_shp$complement) #,
#     #title = "Transect Complement")
# 
# # Add effort-colored tracklines
# m <- m %>%
#   addPolylines(
#     data = track_lines_sf,  
#     color = ~effort_pal(Effort),
#     weight = 3,
#     opacity = 0.8,
#     group = ~paste(date, Effort, segment_id))
# 
# 
# # Add per-date toggle layers
# dates <- unique(track_lines$date)
# 
# for (d in dates) {
#   
#   m <- m %>%
#     addPolylines(
#       data = track_lines %>% filter(date == d),
#       color = ~effort_pal(Effort),
#       weight = 3,
#       opacity = 0.9,
#       group = paste("Date:", d))
# }
# 
# # Add sightings layer
# m <- m %>%
#   addCircleMarkers(
#     data = sight_sf,
#     radius = 4,
#     color = "purple",
#     stroke = FALSE,
#     fillOpacity = 0.8,
#     popup = ~paste0("<b>Species:</b> ", Species, "<br>",
#                     "<b>Count:</b> ", Best_count),
#     group = "Sightings")
# 
# 
# # TO DO: the code chunk below needs to be updated..
# # It should be a GPS trackline, color-coded by Sea_State (not circle markers)
# 
# # Optional: Sea state layer
# # sea_pal <- colorNumeric("Blues", gps_sf$Sea_state)
# # 
# # m <- m %>%
# #   addCircleMarkers(
# #     data = gps_sf,
# #     radius = 2,
# #     color = ~sea_pal(as.numeric(Sea_state)),
# #     stroke = FALSE,
# #     fillOpacity = 0.6,
# #     group = "Sea State")
# 
# # Add layer controls
# m <- m %>%
#   addLayersControl(
#     overlayGroups = c(
#       "Planned Transects",
#       unique(paste("Track:", track_lines$date)),
#       "Sightings",
#       #"Sea State",
#       paste("Date:", dates)),
#     options = layersControlOptions(collapsed = FALSE))
# 
# 
# # Add legend
# m <- m %>%
#   addLegend(
#     "bottomright",
#     pal = effort_pal,
#     values = track_lines$Effort,
#     title = "Effort")
# 
# m
# 
# 
# # ---------------------------
# # Progress bar
# # ---------------------------
# 
# # Update below: use transects_df1$total_transect_km to build progress bar
# total_surveyed_km <- gps_effort2 |>
#   summarise(total_km = sum(transect_km)) |>
#   pull(total_km)
# 
# # % Transects completed
# percent_complete <- total_surveyed_km / total_transect_km * 100


# ---------------------------
# DAILY EFFORT SUMMARY
# ---------------------------
effort_summary <- gps_effort %>%
 group_by(date, Effort) %>%
  summarise(dist_km = sum(dist_km, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Effort, values_from = dist_km, values_fill = 0)

# ---------------------------
# DAILY SIGHTINGS
# ---------------------------
sight_daily <- sight %>%
  filter(as.Date(datetime_utc) == survey_date)

# ---------------------------
# CUMULATIVE SIGHTINGS
# ---------------------------
sight_cum <- sight %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    total_count = sum(as.numeric(Best_count), na.rm = TRUE),
    .groups = "drop")

# ---------------------------
# RENDER RMD
# ---------------------------
output_file <- paste0("WEAWAVeS_Report_", survey_date, ".html")

rmarkdown::render(
  input = file.path(project_root, "R scripts/survey_report.Rmd"),
  output_file = output_file,
  output_dir = file.path(project_root, "reports"),
  params = list(
    survey_date = survey_date,
    gps_effort = gps_effort,
    effort_summary = effort_summary,
    sight_daily = sight_daily,
    sight_cum = sight_cum,
    transects = transects),
  envir = new.env(parent = globalenv()))










#-----Animated Map-------


# =========================================================
# Animated vessel survey tracks with ocean basemap
# =========================================================

# -------------------------
# Load packages
# -------------------------

library(tidyverse)
library(sf)
library(ggplot2)
library(gganimate)
library(transformr)
library(viridis)
library(av)
library(gifski)
library(basemaps)

# =========================================================
# 1. PREPARE GPS DATA
# =========================================================

gps_anim <- gps_effort1 |>
  
  select(
    date_adt,
    datetime_adt,
    vessel_lat,
    vessel_lon,
    Effort
  ) |>
  
  # Remove missing coordinates
  filter(
    !is.na(vessel_lat),
    !is.na(vessel_lon)
  ) |>
  
  # Ensure numeric coordinates
  mutate(
    vessel_lat = as.numeric(vessel_lat),
    vessel_lon = as.numeric(vessel_lon)
  ) |>
  
  # Arrange chronologically
  arrange(datetime_adt) |>
  
  # IMPORTANT:
  # Downsample to speed rendering
  # Adjust "by =" as needed
  slice(seq(1, n(), by = 30))

# =========================================================
# 2. CHECK STATIC PLOT FIRST
# =========================================================

# This verifies the coordinates and plotting work correctly
# BEFORE animation or basemap steps

p_static <- ggplot(
  gps_anim,
  aes(
    x = vessel_lon,
    y = vessel_lat,
    color = Effort
  )
) +
  
  geom_path(
    aes(group = 1),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  
  geom_point(size = 1.2) +
  
  scale_color_viridis_d(option = "turbo") +
  
  coord_fixed() +
  
  labs(
    title = "Survey Vessel Track",
    color = "Effort"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "bottom"
  )

# IMPORTANT:
# Run this first and confirm it displays properly
p_static

# =========================================================
# 3. CREATE ANIMATED PLOT (NO BASEMAP YET)
# =========================================================

p_anim <- ggplot(
  gps_anim,
  aes(
    x = vessel_lon,
    y = vessel_lat,
    color = Effort
  )
) +
  
  geom_path(
    aes(group = 1),
    linewidth = 0.8,
    alpha = 0.8
  ) +
  
  geom_point(size = 1.5) +
  
  scale_color_viridis_d(option = "turbo") +
  
  coord_fixed() +
  
  labs(
    title = "Survey Track Progression",
    
    subtitle = "{format(frame_along, '%Y-%m-%d %H:%M')}",
    
    color = "Effort"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "bottom"
  ) +
  
  transition_reveal(datetime_adt)

# =========================================================
# 4. RENDER TEST ANIMATION
# =========================================================

# IMPORTANT:
# Confirm this works BEFORE adding a basemap

animate(
  p_anim,
  
  width = 1200,
  height = 800,
  
  fps = 12,
  
  nframes = 150,
  
  renderer = gifski_renderer("survey_tracks_test.gif")
)

# =========================================================
# 5. ADD OCEAN BASEMAP (ONLY AFTER TEST WORKS)
# =========================================================

# Convert to sf object for basemap compatibility

gps_sf <- st_as_sf(
  gps_anim,
  coords = c("vessel_lon", "vessel_lat"),
  crs = 4326,
  remove = FALSE
)

# Build animated plot with ocean basemap

p_ocean <- ggplot() +
  
  # Ocean basemap
  basemap_gglayer(
    gps_sf,
    map_service = "esri",
    map_type = "Ocean"
  ) +
  
  # Vessel track
  geom_path(
    data = gps_anim,
    
    aes(
      x = vessel_lon,
      y = vessel_lat,
      color = Effort,
      group = 1
    ),
    
    linewidth = 0.8,
    alpha = 0.9
  ) +
  
  # Current vessel position
  geom_point(
    data = gps_anim,
    
    aes(
      x = vessel_lon,
      y = vessel_lat,
      color = Effort
    ),
    
    size = 1.8
  ) +
  
  scale_color_viridis_d(option = "turbo") +
  
  coord_sf(expand = FALSE) +
  
  labs(
    title = "Survey Track Progression",
    
    subtitle = "{format(frame_along, '%Y-%m-%d %H:%M')}",
    
    color = "Effort"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  
  transition_reveal(datetime_adt)

# =========================================================
# 6. RENDER FINAL MP4
# =========================================================

animate(
  p_ocean,
  
  width = 1400,
  height = 900,
  
  fps = 15,
  
  nframes = 250,
  
  renderer = av_renderer("survey_tracks_ocean.mp4")
)