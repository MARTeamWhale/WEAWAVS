################################################################################
#
# Script purpose: Produce within-survey summary tables and maps
# Program: DFO Maritimes - Cetacean Research and Monitoring Program (CRMP)
# Project: Wind Energy Area Wildlife Assessment Vessel Surveys (WEAWAVS)
# Author: Katherine Gavrilchuk, with the assistance of Claude AI
# Affiliation: Fisheries and Oceans Canada
# Contact: katherine.gavrilchuk@dfo-mpo.gc.ca
# Last updated: Oct 19, 2026
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
library(kableExtra)
library(scales)


#----SET DATE----

REPORT.DATE = "2026-09-30"

# REPORT TITLE:
# WEAWAVS - Fall Survey - CCGS Teleost:
# Daily Progress Report for [REPORT DATE]


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

# Helper function

# Append a QA/QC note to existing notes (or start a new note if none exists)
paste_na <- function(old, new) {
  if_else(is.na(old), new, paste(old, new, sep = "; "))
}


#----DEFINE STUDY AREA----

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
      # Rename lat/lon columns
      vessel_lon_g = coords[,1],
      vessel_lat_g = coords[,2]) |>
    st_drop_geometry() |>
    # Keep columns of interest
    select(datetime_utc, vessel_lat_g, vessel_lon_g, track_fid, track_seg_id)
}) |>
  # Remove track data before 2026-09-09
  filter(datetime_utc >= as.POSIXct("2026-09-09 00:00:00", tz = "UTC")) |>
  # Sort the FULL combined table across all files
  arrange(datetime_utc) |>
  # Remove any duplicate rows (from the overlap period of both Garmin devices)
  distinct() |>
  # Calculate speed over ground from consecutive GPS fixes
  group_by(track_fid, track_seg_id) |>
  mutate(dist_m = geosphere::distHaversine(
    cbind(vessel_lon_g, vessel_lat_g),
    cbind(lag(vessel_lon_g), lag(vessel_lat_g))),
    dt_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs")),
    sog_kt = if_else(dt_sec > 0, (dist_m / dt_sec) * 1.943844, NA_real_)) |>
  ungroup() |>
  select(datetime_utc, vessel_lat_g, vessel_lon_g, sog_kt)


# Checking gaps in Garmin tracks...

# # Raw import, one row per fix, tagged with its source file
# raw <- map_df(garmin_files, function(f){
#   df <- st_read(f, layer = "track_points", quiet = TRUE)
#   coords <- st_coordinates(df)
#   df |>
#     st_drop_geometry() |>
#     mutate(file = basename(f),
#            lon = coords[,1], lat = coords[,2]) |>
#     select(file, time, lon, lat, track_fid, track_seg_id)
# })
# 
# # Start/end times of each file
# file_times <- raw |>
#   filter(!is.na(time)) |>
#   mutate(time = with_tz(time, "UTC")) |>          # display in UTC
#   group_by(file) |>
#   summarise(
#     n_fixes = n(),
#     start   = min(time),
#     end     = max(time),
#     n_before_2026_09_09 = sum(time < as.POSIXct("2026-09-09", tz = "UTC"))
#   ) |>
#   arrange(start) |>
#   mutate(gap_to_next_min = as.numeric(difftime(lead(start), end, units = "mins")))
# 
# file_times
# 
# # Time gaps
# raw_gaps <- raw |>
#   filter(!is.na(time)) |>
#   arrange(time) |>
#   mutate(
#     gap_start  = lag(time),
#     gap_min    = as.numeric(difftime(time, gap_start, units = "mins")),
#     file_before = lag(file),
#     lon_before  = lag(lon),
#     lat_before  = lag(lat)
#   ) |>
#   filter(gap_min > 10) |>
#   select(gap_start, gap_end = time, gap_min,
#          file_before, file_after = file,
#          lon_before, lat_before, lon_after = lon, lat_after = lat)
# 
# raw_gaps
# 
# # Investigate gap within Leatherback_20260915:
# raw |>
#   filter(file == "GARMIN_Leatherback_20260915.gpx", !is.na(time)) |>
#   mutate(time = with_tz(time, "UTC")) |>
#   arrange(time) |>
#   mutate(gap_min = as.numeric(difftime(time, lag(time), units = "mins"))) |>
#   filter(gap_min > 10) |>
#   select(gap_end = time, gap_min, lon, lat)


#----COMBINE GPS DATA----

# Start with Mysticetus and Garmin sources; if outstanding large (to be defined) GPS gaps remain, integrate the ship's NAV GPS source.
gps <- bind_rows(gps_mysti, gps_garmin) |>
  arrange(datetime_utc) |>
  # Convert to Atlantic Daylight time
  mutate(datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax")) |> # with_tz() = converts the same moment in time to another timezone)
  # Relocate to first columns
  relocate(c(datetime_utc, datetime_adt)) |>
  # There are some duplicates in the Mysticetus GPS rows
  distinct()  |>
  # Combine gps sources into just two columns (lat/lon)
  mutate(vessel_lat = if_else(!is.na(vessel_lat_m), vessel_lat_m, vessel_lat_g),
         vessel_lon = if_else(!is.na(vessel_lat_m), vessel_lon_m, vessel_lon_g),
         gps_source = case_when(!is.na(vessel_lat_m) & !is.na(vessel_lon_m) ~ "mysticetus",
                                !is.na(vessel_lat_g) & !is.na(vessel_lon_g) ~ "garmin",
                                TRUE ~ NA_character_)) |>
  arrange(datetime_utc) |>
  # Remove rows before the first mysticetus GPS reading (in the Summer2026 there were some residual garmin tracks from May 11)
  filter(datetime_utc >= min(datetime_utc[gps_source == "mysticetus"])) |>
  select(datetime_utc, datetime_adt, vessel_lat, vessel_lon, sog_kt, cog_t, gps_source)

# Export combined Mysticetus & Garmin GPS track
#write.csv(gps, paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_gps_mysti-garmin.csv"),
#          row.names = FALSE)

# Quick map check:
# 
# library(mapview)
# 
# # Note: The "API KEY REQUIRED" text comes from the basemap, not the data. 
# # mapview's default background is CartoDB Positron, and CARTO now requires an 
# # API key for those tiles, so it stamps that watermark over them. 
# # Switching to basemaps that don't need a key fixes it.
# mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldImagery", "Esri.OceanBasemap"))
# 
# gps_sf <- gps |>
#   filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
#   st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326)
# 
# mapview(gps_sf, cex = 1, lwd = 0,
#         legend = FALSE,
#         map.types = c("Esri.OceanBasemap"))
# 
# # To add a line through the GPS points:
# gps_line <- gps_sf |>
#   arrange(datetime_utc) |>
#   summarise(do_union = FALSE) |>
#   st_cast("LINESTRING")
# 
# # Gaps > 10 min: split into multiple lines/tracks
# gps_segments <- gps_sf |>
#   arrange(datetime_utc) |>
#   mutate(
#     gap_min = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")),
#     new_seg = is.na(gap_min) | gap_min > 10,
#     seg_id  = cumsum(new_seg)
#   ) |>
#   group_by(seg_id) |>
#   filter(n() >= 2) |>                       # a line needs at least 2 points
#   summarise(
#     start   = min(datetime_utc),
#     end     = max(datetime_utc),
#     n_fixes = n(),
#     do_union = FALSE
#   ) |>
#   st_cast("LINESTRING")
# 
# gps_segments   # one row per continuous track section
# 
# mapview(gps_segments,
#         color = "grey20", lwd = 1.5,
#         legend = FALSE,
#         map.types = "Esri.OceanBasemap")


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


# Data manipulation
effort1 <- effort |>
  mutate(# Create a datetime_utc column in the format "YYYY-MM-DD HH:MM:SS" (NOTE: for some reason, had to modify to include the miliseconds specification for this df)
    datetime_utc = floor_date(as.POSIXct(`Datetime_UTC (UTC)`, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"), unit = "second"),
    # Convert to Atlantic time (with_tz = same instant, different zone)
    datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
    # Create date variable based on Atlantic time zone
    date_adt = as_date(datetime_adt)) |>
  arrange(datetime_adt) |>
  # Remove rows marked to be deleted (check them first before deleting).
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
  ungroup() |>
  # Relocate new lat/lon columns to after GPS_pos and verify they are correct
  relocate(c(vessel_lat_e, vessel_lon_e), .after = GPS_pos)

# NEW!

# Update effort to "Off" whenever Sea state >= 6 or visibility is Restricted,
# and record the change in QAQC_notes
effort2 <- effort1 |>
  mutate(
    high_sea  = Sea_state %in% c("6", ">6"),
    restr_vis = Port_vis %in% "Restricted" | Stb_vis %in% "Restricted",
    to_off    = (high_sea | restr_vis) & !(Effort %in% "Off"),
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

table(effort2$Sea_state, effort2$Effort)
table(effort2$Port_vis, effort2$Effort)
table(effort2$Stb_vis, effort2$Effort)


#----JOIN GPS + EFFORT----

# Join gps1 to effort2
gps_effort <- gps |>
  full_join(effort2, by = "datetime_utc") |>
  arrange(datetime_utc) |>
  mutate(row_id = row_number()) |>
  mutate(
    # Coalesce vessel_lat/vessel_lon as matched pairs, not independently:
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

# Select columns of interest, fill gaps
gps_effort1 <- gps_effort |>
  select(any_of(c("datetime_utc", "datetime_adt", "date_adt", "vessel_lat", "vessel_lon", "gps_source",
                  "Vessel", "strata", "start.effort_adt", "end.effort_adt", "Effort",
                  "Action", "Transect_ID", "Port_obs", "Stb_obs", "BigEye_obs", "Data_recorder",
                  "SBO", "Center_MMO","Stb_MMO", "Sea_state", "Swell", "Port_vis", "Stb_vis", "Precipitation",
                  "Glare_int", "Glare_left", "Glare_right", "sog_kt", "cog_t", "Notes", "QAQC_notes"))) |>
  mutate(Transect_ID = unname(Transect_ID)) |>
  arrange(datetime_adt) |>
  group_by(date_adt) |>
  # Fill selected columns 'down' first
  fill(any_of(c("strata", "Effort", "start.effort_adt", "end.effort_adt",
                "Port_obs", "Stb_obs", "BigEye_obs", "Data_recorder",
                "Sea_state", "Swell", "Port_vis", "Stb_vis", "Precipitation", "Glare_int",
                "Glare_left", "Glare_right")), .direction = "down") |>
  # Mark contiguous effort blocks: any change in Effort starts a new block
  mutate(effort_change = Effort != lag(Effort, default = first(Effort)),
         effort_block = cumsum(effort_change)) |>
  group_by(date_adt, effort_block) |>
  # Fill Transect_ID ONLY within a single contiguous effort block, so a stale
  # ID from an earlier On/Bridge-On period can never leak into a later,
  # different On/Bridge-On period once the effort state has changed in between
  fill(Transect_ID, .direction = "down") |>
  ungroup() |>
  select(-effort_change, -effort_block) |>
  group_by(date_adt) |>
  # Fill in vessel for every row
  fill(Vessel, .direction = "downup") |>
  ungroup()

# NEW
# Distance to planned transect

# Name of the transect ID column in transects_shp 
transect_id_col <- "label"

# One (multi)line per planned transect, in WGS84
transect_lines <- transects_shp |>
  st_transform(4326) |>
  select(Transect_ID = all_of(transect_id_col)) |>
  mutate(Transect_ID = as.character(Transect_ID)) |>
  group_by(Transect_ID) |>
  summarise(.groups = "drop") |>
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>                 # join multi-part pieces into one line
  st_cast("LINESTRING")

# Check: Transect_IDs in the GPS data with no matching planned line (should be empty)
setdiff(unique(na.omit(gps_effort1$Transect_ID)), transect_lines$Transect_ID)

# GPS fixes on a transect, paired with their planned transect line
gps_effort1 <- gps_effort1 |> mutate(row_tmp = row_number())

on_tr <- gps_effort1 |>
  filter(!is.na(Transect_ID),
         Effort %in% c("On", "Bridge-On"),
         !is.na(vessel_lat), !is.na(vessel_lon)) |>
  select(row_tmp, Transect_ID, vessel_lon, vessel_lat) |>
  inner_join(as_tibble(transect_lines) |> rename(tr_geom = geometry),
             by = "Transect_ID")

on_tr_pts <- on_tr |>
  select(-tr_geom) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326)

# Geodesic distance (m) from each fix to ITS OWN transect line
on_tr$dist_to_transect_m <- as.numeric(
  st_distance(on_tr_pts, on_tr$tr_geom, by_element = TRUE))

# Position of each fix along its planned transect (0 = start, 1 = end).
# Done in a projected CRS (Statistics Canada Lambert); since it's a fraction
# of the line, it is scaled by the planned km from transects_df later.
on_tr$frac_along <- st_line_project(
  st_transform(on_tr$tr_geom, 3347),
  st_transform(st_geometry(on_tr_pts), 3347),
  normalized = TRUE)

# Join back; rows without a Transect_ID stay NA
gps_effort1 <- gps_effort1 |>
  left_join(on_tr |> select(row_tmp, dist_to_transect_m, frac_along), by = "row_tmp") |>
  mutate(dist_to_transect = round(dist_to_transect_m, 1)) |>
  select(-row_tmp)

# Quick look: distance summary per transect
gps_effort1 |>
  filter(!is.na(dist_to_transect_m)) |>
  group_by(Transect_ID) |>
  summarise(n_fixes = n(),
            median_m = median(dist_to_transect_m),
            max_m = max(dist_to_transect_m),
            .groups = "drop") |>
  arrange(desc(max_m))

# Checks
unique(gps_effort1$date_adt)
unique(gps_effort1$Transect_ID)
table(gps_effort1$date_adt, gps_effort1$strata)



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

# Data manipulation
sight1 <- sight |>
  # Remove rows marked for deletion
  filter(Delete_row != "True") |>
  # Remove confirmed or possible resights (duplicates)
  filter(!Resight %in% c("Yes", "Possible")) |>
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
                                             "Unknown shark", "Other", "Tuna", "Other (dead)") ~ "Incidental",
                              TRUE ~ Sgt_type)) |>
  # Remove columns no longer needed
  select(-c(lat_deg, lat_min, lon_deg, lon_min, lat_dir, lon_dir, Delete_row)) |>
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

# NEW!
# A sighting currently labelled On-effort should become Incidental if, at the moment of the sighting, the (updated) effort state was Off.
effort_intervals <- effort2 |>
  arrange(datetime_adt) |>
  mutate(
    start = datetime_adt,
    end   = lead(datetime_adt),
    end   = coalesce(end, start + lubridate::days(1))) |>   # last record of the survey
  select(start, end, Effort_int = Effort,
         Sea_state_int = Sea_state, Port_vis_int = Port_vis, Stb_vis_int = Stb_vis)

sight2 <- sight1 |>
  left_join(effort_intervals,
            by = join_by(datetime_adt >= start, datetime_adt < end)) |>
  mutate(
    Sgt_type_orig = Sgt_type,
    reclassified  = coalesce(Sgt_type == "On-effort" & Effort_int == "Off", FALSE),
    Sgt_type      = if_else(reclassified, "Incidental", Sgt_type),
    QAQC_notes    = if_else(
      reclassified,
      paste_na(QAQC_notes, "Sgt_type changed On-effort -> Incidental: effort Off due to weather thresholds"),
      QAQC_notes))

# Review what changed, per day:

# The affected sightings, with the conditions that triggered them
sight2 |>
  filter(reclassified) |>
  select(row_id, datetime_adt, Species, Best_count,
         start, end, Effort_int, Sea_state_int, Port_vis_int, Stb_vis_int)

# The join shouldn't duplicate any sightings (should be TRUE)
nrow(sight2) == nrow(sight1)

# Every sighting should have matched an effort interval (should be 0)
sum(is.na(sight2$Effort_int))


# Assign 'strata' (FMB, SB, or NONE) based on whether a lat/lon falls within studyarea_shp$WEA

# Re-project the studyarea_shp to match that of the gps_effort lat/lon (EPSG: 4326 or the WGS 84 (World Geodetic System 1984))
studyarea_shp <- st_transform(studyarea_shp, crs = 4326)

strata_lookup <- sight2 |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE) |>
  st_join(studyarea_shp |> select(WEA), join = st_within) |>
  st_drop_geometry() |>
  transmute(row_id, strata = case_when(WEA == "French/Middle Bank" ~ "FMB",
                                       WEA == "Sydney Bight"       ~ "SB",
                                       TRUE ~ "NONE"))

# Join the strata_lookup to sight1
sight3 <- sight2 |>
  left_join(strata_lookup, by = "row_id") |>
  select(-c(row_id, start, end, Effort_int, Sea_state_int, Port_vis_int, 
            Stb_vis_int, Sgt_type_orig, reclassified))



#---PREPARE DATA FOR SUMMARIES---

# Step 1: Create segment-level effort data
# Each row should represent the movement between GPS point i and i+1.
gps_seg <- gps_effort1 |>
  mutate(datetime_adt = as.POSIXct(datetime_adt, format = "%Y-%m-%d %H:%M:%S", tz = "America/Halifax"),
         # The rows where Effort = NA is when Mysticetus was running and receiving GPS positions, but the row in Effort/Env was not yet entered.
         # These rows should be considered Off effort
         Effort = coalesce(Effort, "Off")) |>
  arrange(datetime_adt) |>
  group_by(date_adt) |>
  mutate(next_lon = lead(vessel_lon),
         next_lat = lead(vessel_lat),
         next_time = lead(datetime_adt),
         dist_km = distHaversine(cbind(vessel_lon, vessel_lat), cbind(next_lon, next_lat)) / 1000,
         duration_h = as.numeric(difftime(next_time, datetime_adt, units = "hours"))) |>
  filter(!is.na(dist_km))

# Quick fix: ignore implausible GPS jumps during On/Bridge-On effort
# (no rows dropped; the jump just contributes 0 km)
gps_seg <- gps_seg |>
  mutate(hop_spd_kt = dist_km / pmax(duration_h, 1/3600) / 1.852,
         bad_hop    = Effort %in% c("On", "Bridge-On") & dist_km > 0.5 & hop_spd_kt > 25,
         dist_km    = if_else(bad_hop, 0, dist_km))

# Should be a handful of rows, not thousands
gps_seg |> filter(bad_hop) |>
  select(date_adt, datetime_adt, Transect_ID, gps_source, dist_km = hop_spd_kt)

# Step 2: Create effort blocks (important because effort status changes throughout the day)
gps_seg <- gps_seg |>
  mutate(effort_change = Effort != lag(Effort, default = first(Effort)),
         effort_block = cumsum(effort_change))

# Step 3: Summarize blocks
effort_blocks <- gps_seg |>
  group_by(date_adt, effort_block, Effort) |>
  summarize(start_time_adt = min(datetime_adt),
            end_time_adt = max(next_time),
            duration_h = sum(duration_h),
            distance_km = sum(dist_km),
            .groups = "drop")


#----SUMMARY TABLES----

# Table 1: Distance & hours spent across all effort states
# Table 2a: Proportion of transects completed during on-effort (measures planned-line coverage during On/Bridge-On effort)
# Table 2b: Planned-line coverage per transect and strata, across all dates
# Table 3a: All sightings by Species & Date
# Table 3b: All sightings by Species
# Table 4: Environmental summary: proportion of time spent in different sea state (BFT) and visibility conditions

# Total length of the union of intervals [s, e] (overlaps counted once)
union_length <- function(s, e) {
  if (length(s) == 0) return(0)
  o <- order(s); s <- s[o]; e <- e[o]
  total <- 0; cur_s <- s[1]; cur_e <- e[1]
  for (i in seq_along(s)[-1]) {
    if (s[i] <= cur_e) {
      cur_e <- max(cur_e, e[i])
    } else {
      total <- total + (cur_e - cur_s)
      cur_s <- s[i]; cur_e <- e[i]
    }
  }
  total + (cur_e - cur_s)
}

# Stretch of planned line covered between consecutive on-effort fixes.
# frac_along only exists for On/Bridge-On fixes with a Transect_ID, so pairs
# that cross into another effort state or transect are dropped automatically.

planned <- transects_df |> select(Transect_ID, Planned_km = Transect_km)

cover_int <- gps_effort1 |>
  arrange(datetime_adt) |>
  group_by(date_adt) |>
  mutate(next_frac = lead(frac_along),
         next_tr   = lead(Transect_ID),
         next_time = lead(datetime_adt)) |>
  ungroup() |>
  filter(!is.na(frac_along), !is.na(next_frac), Transect_ID == next_tr) |>
  left_join(planned, by = "Transect_ID") |>
  mutate(hop_h  = pmax(as.numeric(difftime(next_time, datetime_adt, units = "hours")), 1/3600),
         hop_km = abs(next_frac - frac_along) * Planned_km) |>
  # Same quick fix: skip implausible jumps along the line
  filter(!(hop_km > 0.5 & hop_km / hop_h / 1.852 > 25)) |>
  transmute(date_adt, Transect_ID,
            s = pmin(frac_along, next_frac),
            e = pmax(frac_along, next_frac))


# Table 1: Distance & hours spent across all effort states
table1 <- gps_seg |>
  group_by(date_adt, Effort) |>
  summarize(Distance_km = round(sum(dist_km), 1),
            Duration_h  = round(sum(duration_h), 1)) |>
  rename(Date_adt = date_adt)

# Pivot wide:
#table1_wide <- table1 |>
#  pivot_wider(names_from = Effort,
#              values_from = c(Distance_km, Duration_h))


# Table 2a: Planned-line coverage by date and transect
table2a <- cover_int |>
  group_by(date_adt, Transect_ID) |>
  summarize(Prop_completed = union_length(s, e), .groups = "drop") |>
  left_join(planned, by = "Transect_ID") |>
  mutate(Realized_km = Prop_completed * Planned_km) |>
  rename(Date_adt = date_adt) |>
  select(Date_adt, Transect_ID, Realized_km, Planned_km, Prop_completed)

# Table 2b: Coverage per transect across ALL dates (union, so a stretch
# re-surveyed on a second day isn't counted twice)
realized_by_transect <- cover_int |>
  group_by(Transect_ID) |>
  summarize(frac = union_length(s, e), .groups = "drop") |>
  left_join(planned, by = "Transect_ID") |>
  transmute(Transect_ID, Realized_km = frac * Planned_km)

# Strata-level summary: join onto the FULL transect list so a transect with
# zero on-effort coverage still counts its planned_km, and planned_km isn't
# double-counted for transects surveyed on multiple dates
table2b_strata <- transects_df |>
  select(Strata, Transect_ID, Planned_km = Transect_km) |>
  left_join(realized_by_transect, by = "Transect_ID") |>
  mutate(Realized_km = replace_na(Realized_km, 0),
         Realized_km = pmin(Realized_km, Planned_km)) |>   # cap per transect first
  group_by(Strata) |>
  summarize(Realized_km = sum(Realized_km),
            Planned_km  = sum(Planned_km),
            .groups = "drop") |>
  mutate(prop_completed = Realized_km / Planned_km) |>
  rename(Prop_completed = prop_completed)

table2b_total <- table2b_strata |>
  summarize(Realized_km = sum(Realized_km),
            Planned_km  = sum(Planned_km)) |>
  mutate(Prop_completed = Realized_km / Planned_km)

# Overall survey progress: Transect completion
n_transects_planned <- n_distinct(transects_df$Transect_ID)

transect_completion_any <- planned |>
  left_join(realized_by_transect, by = "Transect_ID") |>
  mutate(Realized_km = replace_na(Realized_km, 0),
         prop_completed = Realized_km / Planned_km)

# Transects considered complete (>= threshold of planned line covered)
completion_threshold <- 0.99
n_transects_completed <- transect_completion_any |>
  filter(prop_completed >= completion_threshold) |>
  nrow()

# Table 3a: All sightings by species and date
table3a <- sight3 |>
  group_by(date_adt, Species) |>
  # Turn this filter on if only summarizing marine mammal sightings
  #filter(!Species %in% c("Mola mola", "Other", "Basking shark", 
  #                       "Leatherback turtle", "Leatherback turtle (dead)", "Tuna")) |>
  filter(!Species == "Other") |>
  summarize(n_sightings = n(),
            n_individuals = sum(Best_count, na.rm = TRUE),
            .groups = "drop") |>
  mutate(survey = dataset,
         vessel = case_when(dataset == "Summer2026" ~ "KopitHopson",
                            TRUE ~ "Teleost")) |>
  rename(Survey = survey,
         Vessel = vessel,
         Date_adt = date_adt) |>
  relocate(c(Survey, Vessel))


# Table 3b: All sightings by species
table3b <- sight3 |>
  # Turn this filter on if only summarizing marine mammal sightings
  #filter(!Species %in% c("Mola mola", "Other", "Basking shark", 
  #                       "Leatherback turtle", "Leatherback turtle (dead)", "Tuna")) |>
  filter(!Species == "Other") |>
 group_by(Species) |>
 summarize(n_sightings = n(),
           n_individuals = sum(Best_count, na.rm = TRUE),
           .groups = "drop") |>
 mutate(Survey = dataset,
        Vessel = case_when(dataset == "Summer2026" ~ "KopitHopson",
                           TRUE ~ "Teleost")) |>
 relocate(c(Survey, Vessel))

table3b_totals <- table3b |>
  summarize(Survey        = "",
            Vessel        = "",
            Species       = "Totals",
            n_sightings   = sum(n_sightings),
            n_individuals = sum(n_individuals))

table3b_display <- bind_rows(table3b, table3b_totals)



# Table 4: Environmental summary -- hours (and proportion of time) spent in each
# sea state (BFT) and visibility category, within each day's survey window

bft_levels <- c(as.character(0:6), ">6")
vis_levels <- c("Excellent", "Moderate", "Poor", "Restricted")   # best -> worst

# Daily survey window: first to last EffortEnv entry of each day
survey_window <- effort2 |>
  group_by(date_adt) |>
  summarize(win_start = min(datetime_adt),
            win_end   = max(datetime_adt),
            .groups = "drop")

env_seg <- gps_seg |>
  ungroup() |>
  left_join(survey_window, by = "date_adt") |>
  filter(datetime_adt >= win_start, datetime_adt < win_end) |>
  mutate(Sea_state = trimws(Sea_state),
         # Worst of port/starboard visibility (if one side is missing, use the other)
         Vis_worst = vis_levels[pmax(match(trimws(Port_vis), vis_levels),
                                     match(trimws(Stb_vis),  vis_levels),
                                     na.rm = TRUE)])

# Hours per category: summed per day first, then across all days.
# Time with no recorded value is excluded, so proportions sum to 1
# across the recorded categories only.
summarize_env <- function(df, var, levels) {
  df |>
    filter(!is.na({{ var }})) |>
    group_by(date_adt, Category = {{ var }}) |>
    summarize(Duration_h = sum(duration_h), .groups = "drop") |>    # per day
    group_by(Category) |>
    summarize(Duration_h = sum(Duration_h), .groups = "drop") |>    # all days
    mutate(Category = factor(Category, levels = c(levels, setdiff(Category, levels)))) |>
    arrange(Category) |>
    mutate(Prop_time  = round(Duration_h / sum(Duration_h), 2),
           Duration_h = round(Duration_h, 1))
}

# Table 4a: Sea state
table4a <- summarize_env(env_seg, Sea_state, bft_levels) |>
  rename(Sea_state_BFT = Category)

# Table 4b: Visibility (worst of port/starboard)
table4b <- summarize_env(env_seg, Vis_worst, vis_levels) |>
  rename(Visibility = Category)

table4a
table4b

# Survey time lost to bad weather: Off effort while sea state >= 6 OR
# visibility Restricted (each segment counted once, even if both applied)
weather_lost_h <- env_seg |>
  filter(Effort == "Off",
         Sea_state %in% c("6", ">6") | Vis_worst %in% "Restricted") |>
  summarize(h = sum(duration_h)) |>
  pull(h)

weather_lost_days <- weather_lost_h / 12

# Check: breakdown by cause (Both should NOT be double-counted above)
env_seg |>
  filter(Effort == "Off") |>
  mutate(cause = case_when(
    Sea_state %in% c("6", ">6") & Vis_worst %in% "Restricted" ~ "Both",
    Sea_state %in% c("6", ">6")                               ~ "Sea state",
    Vis_worst %in% "Restricted"                               ~ "Visibility")) |>
  filter(!is.na(cause)) |>
  group_by(cause) |>
  summarize(hours = sum(duration_h))



#----TRANSECT COMPLETION IN ANY EFFORT STATE----

max_dist_any_m <- 500

fix_any <- gps_effort1 |>
  filter(!is.na(vessel_lat), !is.na(vessel_lon)) |>
  left_join(survey_window, by = "date_adt") |>
  filter(datetime_adt >= win_start, datetime_adt <= win_end) |>
  arrange(datetime_adt) |>
  mutate(fix_id = row_number()) |>                     # time order = row order
  select(fix_id, date_adt, datetime_adt, vessel_lon, vessel_lat) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326)

# All (fix, transect) pairs within the distance limit
near_list <- st_is_within_distance(fix_any, transect_lines, dist = max_dist_any_m)
pairs <- tibble(fix_id = rep(fix_any$fix_id, lengths(near_list)),
                tr_idx = unlist(near_list))

# Position of each fix along each nearby transect (0 = start, 1 = end)
pairs$frac <- st_line_project(
  st_transform(st_geometry(transect_lines)[pairs$tr_idx], 3347),
  st_transform(st_geometry(fix_any)[pairs$fix_id], 3347),
  normalized = TRUE)

fix_info <- fix_any |> st_drop_geometry() |> select(fix_id, date_adt, datetime_adt)

# Covered intervals: consecutive fixes (in time) both near the same transect
cover_any <- pairs |>
  mutate(Transect_ID = transect_lines$Transect_ID[tr_idx]) |>
  left_join(fix_info, by = "fix_id") |>
  arrange(Transect_ID, fix_id) |>
  group_by(Transect_ID) |>
  mutate(next_id   = lead(fix_id),
         next_frac = lead(frac),
         next_date = lead(date_adt),
         next_time = lead(datetime_adt)) |>
  ungroup() |>
  filter(next_id == fix_id + 1, next_date == date_adt) |>
  left_join(planned, by = "Transect_ID") |>
  mutate(hop_h  = pmax(as.numeric(difftime(next_time, datetime_adt, units = "hours")), 1/3600),
         hop_km = abs(next_frac - frac) * Planned_km) |>
  filter(!(hop_km > 0.5 & hop_km / hop_h / 1.852 > 25)) |>      # skip GPS spikes
  transmute(Transect_ID,
            s = pmin(frac, next_frac),
            e = pmax(frac, next_frac)) |>
  # Always include the On/Bridge-On coverage, so any-state >= on-effort
  bind_rows(cover_int |> select(Transect_ID, s, e))

# Completion per transect (union across all dates)
completion_any_state <- planned |>
  left_join(cover_any |>
              group_by(Transect_ID) |>
              summarize(Prop_completed = union_length(s, e), .groups = "drop"),
            by = "Transect_ID") |>
  mutate(Prop_completed = replace_na(Prop_completed, 0))

n_transects_completed_any <- sum(completion_any_state$Prop_completed >= completion_threshold)

# Compare with the On/Bridge-On version
n_transects_completed; n_transects_completed_any

# Transects not yet completed (any effort state)
incomplete_any <- completion_any_state |>
  filter(Prop_completed < completion_threshold) |>
  arrange(Transect_ID)
incomplete_any


# Quick checks

# frac_along was added in the distance-to-transect block (should be TRUE)
"frac_along" %in% names(gps_effort1)

# Every covered transect has a planned length (should be empty)
setdiff(unique(cover_int$Transect_ID), planned$Transect_ID)

# track km vs. planned-line km per transect
gps_seg |>
  filter(Effort %in% c("On", "Bridge-On"), !is.na(Transect_ID)) |>
  group_by(Transect_ID) |>
  summarize(Track_km = sum(dist_km), .groups = "drop") |>
  left_join(realized_by_transect, by = "Transect_ID") |>
  mutate(ratio = Track_km / Realized_km) |>
  arrange(desc(ratio)) |>
  print(n = Inf)








#----SUMMARY MAPS (LEAFLET)----

# Interactive (Leaflet) Maps (requires internet to view basemap when html report is opened)

# Map 1: GPS tracks color-coded by effort status
# Map 2: Sightings color-coded by species ID

# PREPARE GPS TRACK DATA

# Per-date cutoff: the timestamp of the last logged "Off" effort entry on
# each date_adt. The trackline for that date should not extend past it --
# Mysticetus can keep logging GPS positions after effort/observation has
# ended for the day, and those trailing points shouldn't appear as part of
# that day's realized track.
last_off_time <- effort1 |>
  filter(Effort == "Off") |>
  group_by(date_adt) |>
  summarize(last_off_datetime_utc = max(datetime_utc, na.rm = TRUE), .groups = "drop")

gps_sf <- gps_effort1 |>
  # Keep daytime tracks only
  filter(gps_source == "mysticetus") |>
  filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
  # Truncate each date's track at that date's last logged "Off" entry.
  # Dates with no "Off" entry yet (e.g. survey still underway) are left untruncated.
  left_join(last_off_time, by = "date_adt") |>
  filter(is.na(last_off_datetime_utc) | datetime_utc <= last_off_datetime_utc) |>
  select(-last_off_datetime_utc) |>
  arrange(datetime_utc) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"),
           crs = 4326,
           remove = FALSE)

# To filter out GPS points that fall outside both WEA polygons
studyarea_shp <- st_transform(studyarea_shp, st_crs(gps_sf))

gps_sf <- gps_sf |>
  st_filter(studyarea_shp, .predicate = st_intersects)

# Build continuous track line segments based on effort changes or time gaps
track_lines_sf <- gps_sf |>
  mutate(
    time_diff = as.numeric(difftime(datetime_adt, lag(datetime_adt), units = "mins")),
    effort_change = Effort != lag(Effort),
    # Flags the start of a new track segment: TRUE if the effort status
    # just changed, OR if more than 10 minutes passed since the last GPS fix
    # (a gap big enough that the points shouldn't be connected as one continuous line).
    # replace_na(..., TRUE) forces any NA (from the first row, or any row where
    # the comparison itself is NA) to count as "start a new segment" rather than
    # silently merging into whatever segment came before.
    new_segment = replace_na(effort_change, TRUE) | replace_na(time_diff > 10, TRUE),
    segment_id = cumsum(new_segment)) |>
  mutate(Effort = trimws(Effort)) |>
  group_by(Effort, segment_id) |>
  summarize(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING")



# PREPARE FULL GARMIN TRACK (uninterrupted, whole survey)

# The complete Garmin track, day and night, with no polygon cropping and no
# effort-based truncation -- shows the vessel's full path for the survey so far.
garmin_track_sf <- gps_garmin |>
  filter(!is.na(vessel_lon_g), !is.na(vessel_lat_g)) |>
  arrange(datetime_utc) |>

  # CHECK IF NEEDED...
    #mutate(
  #  time_diff_min = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")),
    # Start a new segment only after a large gap (e.g. Garmin powered off in
    # port between survey legs) -- avoids drawing a straight line across
    # those gaps while keeping the rest of the track visually continuous
  #  new_segment = replace_na(time_diff_min > 60, TRUE),
  #  segment_id = cumsum(new_segment)) |>
  
st_as_sf(coords = c("vessel_lon_g", "vessel_lat_g"), crs = 4326, remove = FALSE) |>
  #group_by(segment_id) |>
  summarize(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING")



# COLOR PALETTES SETUP

# Effort Palettes
effort_colors <- c(
  "Bridge-On"  = "#00FF00",  # Bright Green
  "On"         = "#006400",  # Dark Green
  "Transit-On" = "#FFD700",  # Gold/Yellow
  "Off"        = "#FF0000",  # Red
  "Closing"    = "#800080"   # Purple
)

# Map hex codes directly to the spatial lines data frame
track_lines_sf$LineColor <- unname(effort_colors[track_lines_sf$Effort])
track_lines_sf$LineColor[is.na(track_lines_sf$LineColor)] <- "#808080"

# Extract ONLY the effort states that exist in this active dataset
# This keeps the order exactly matching the effort_colors palette
present_states <- intersect(names(effort_colors), unique(track_lines_sf$Effort))

# Extract the exact matching hex codes for the legend
legend_colors <- effort_colors[present_states]
legend_labels <- present_states

# Transect Palettes
# Different colored complements:
transect_cols <- c("1" = "grey40", "2" = "black")

transect_pal <- colorFactor(
  palette = transect_cols,
  domain  = unique(transects_shp$complement))

# Add a stable row id so line angles join back correctly
transects_shp2 <- transects_shp |> mutate(row_id = row_number())

# Project to planar coords to compute each line's true bearing
transects_proj <- st_transform(transects_shp2, 3857)

line_angles <- st_coordinates(transects_proj) |>
  as.data.frame() |>
  group_by(L1) |>
  summarize(x_start = first(X), y_start = first(Y),
            x_end   = last(X),  y_end   = last(Y),
            .groups = "drop") |>
  mutate(row_id = L1,
         angle_deg = atan2(y_end - y_start, x_end - x_start) * 180 / pi,
         # normalize to (-90, 90] so text is never upside-down
         angle_deg = ifelse(angle_deg > 90, angle_deg - 180,
                            ifelse(angle_deg <= -90, angle_deg + 180, angle_deg))) |>
  select(row_id, angle_deg)

transects_shp2 <- transects_shp2 |>
  left_join(line_angles, by = "row_id") |>
  mutate(label_angle = -angle_deg)

# Stagger the along-line position so labels on closely-spaced parallel
# transects don't all collide at the same cross-section of the map
sample_fracs <- rep(c(0.3, 0.5, 0.7), length.out = nrow(transects_proj))

geom_list <- st_geometry(transects_proj)

midpoint_list <- vector("list", length(geom_list))
for (i in seq_along(geom_list)) {
  midpoint_list[[i]] <- st_line_sample(
    st_sfc(geom_list[[i]], crs = st_crs(transects_proj)),
    sample = sample_fracs[i])
}

midpoints_geom <- do.call(c, midpoint_list) |>
  st_cast("POINT") |>
  st_transform(4326)

# Labels on transects:
transect_midpoints <- st_sf(
  label       = transects_shp2$label,
  label_angle = transects_shp2$label_angle,
  geometry    = midpoints_geom) |>
  mutate(label_html = paste0(
    "<div style='transform: rotate(", round(label_angle, 1), "deg); ",
    "transform-origin: center; white-space: nowrap; font-size:12px; ",
    "font-weight:bold; color:black;'>", label, "</div>"))

# Last recorded GPS position -- marks how current the realized track is
last_position <- gps_sf |>
  filter(!is.na(datetime_adt)) |>
  arrange(datetime_adt) |>
  slice_tail(n = 1)

last_position_label <- format(last_position$datetime_adt, "%B %d, %Y %H:%M")

# Build map
map1 <- leaflet(height = 600, options = leafletOptions(preferCanvas = TRUE)) |>

  # ESRI
  #addProviderTiles(providers$Esri.OceanBasemap)

  # To avoid the 'map not available' message when zooming too far in:
  addProviderTiles(providers$Esri.OceanBasemap,
                   options = providerTileOptions(maxNativeZoom = 8)) |>
  
  # Full Garmin track (uninterrupted, whole survey) -- off by default
   # addPolylines(
   #   data = garmin_track_sf,
   #   color = "gray30",
   #   weight = 1,
   #   opacity = 0.5,
   #   group = "GPS Track (Full)") |>

  # Planned transects
  addPolylines(
    data = transects_shp,
    color = ~transect_pal(complement),
    weight = 2,
    opacity = 0.5,
    group = "Planned Transects") |>

  # Transect ID labels (hidden by default -- toggle on via layers control)
  addLabelOnlyMarkers(
    data = transect_midpoints,
    label = ~lapply(label_html, htmltools::HTML),
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE),
    group = "Transect ID") |>

  # GPS Tracks / Realized transects
  addPolylines(
    data = track_lines_sf,
    color = track_lines_sf$LineColor,
    weight = 3,
    opacity = 0.9,
    popup = ~paste("<strong>Effort Status:</strong>", Effort),
    group = "Realized Transects") |>

  # Last GPS position (report generation position/time)
  addCircleMarkers(
    data = last_position,
    radius = 6,        # size of the circle itself
    color = "black",   # color of the outline/border
    weight = 2,         # thickness (in px) of that outline
    fillColor = "white",  # color of the circle's interior
    fillOpacity = 1,
    popup = ~paste0("<strong>Last Recorded Position</strong><br/>", last_position_label),
    group = "Last Position") |>

  # WEA polygons
  addPolygons(
    data = studyarea_shp,
    color = "black",           # border color
    weight = 2,
    fill = FALSE,              # no fill at all -- outline only
    popup = ~paste("<strong>WEA:</strong>", WEA, "<br><strong>Area:</strong>", round(Area_km2, 1), "km²"),
    group = "WEA Boundaries") |>

  # Effort Legend
  addLegend(
    position = "topright",
    colors   = legend_colors,
    labels   = legend_labels,
    opacity = 1,          # show true swatch colors, not the 0.5 default
    title    = "Effort Status") |>

  # Transect Legend
  addLegend(
    position = "topright",
    pal = transect_pal,
    values = unique(transects_shp$complement),
    title = "Planned Transects") |>
  
   # With full Garmin track..
   # addLayersControl(
   #   overlayGroups = c("Planned Transects", "Realized Transects", "Transect ID", "GPS Track (Full)"),
   #   options = layersControlOptions(collapsed = FALSE)) |>
   # hideGroup(c("Transect ID", "GPS Track (Full)"))

# Without full Garmin track..
addLayersControl(
 overlayGroups = c("Planned Transects", "Realized Transects", "Transect ID"),
 options = layersControlOptions(collapsed = FALSE)) |>
 hideGroup(c("Transect ID"))

map1

# "WEAWAVS Fall Survey: cumulative realized trackline, color-coded by effort status."



# MAP 2: Sightings color-coded by species ID

# PREPARE SIGHTINGS DATA

species_exclude <- c("Mola mola", "Other", "Basking shark", "Tuna")

sight_sf <- sight2 |>
  mutate(Species = trimws(Species)) |>
  filter(!is.na(Species), !Species %in% species_exclude) |>
  mutate(lat = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lat,
                         TRUE ~ vessel_lat),
         lon = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lon,
                         TRUE ~ vessel_lon)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

species_levels <- sort(unique(sight_sf$Species))

# Fixed species
species_colors <- c(
  "Atlantic white-sided dolphin" = "#e6194b",
  "Blue whale"                   = "#0072B2",
  "Bottlenose dolphin"           = "#7F8C8D",
  "Common dolphin"               = "#911eb4",
  "Fin whale"                    = "#8bc34a",
  "Fin/sei whale"                = "#4b3b6b",
  "Grey seal"                    = "#8c5464",
  "Harbour porpoise"             = "#17becf",
  "Harbour seal"                 = "#808000",
  "Humpback whale"               = "#000075",
  "Killer whale"                 = "#0e7c7b",
  "Minke whale"                  = "#4d7c0f",
  "Pilot whale"                  = "#800000",
  "White-beaked dolphin"         = "#c2185b",
  "Unknown baleen whale"         = "#f032e6",
  "Unknown cetacean"             = "#f58231",
  "Unknown small cetacean"       = "#FFDF00",
  "Unknown dolphin"              = "#a8841c",
  "Unknown seal"                 = "#4a4a68",
  "Leatherback turtle"           = "#000000",
  "Leatherback turtle (dead)"    = "#ffffff"
)

sight_sf <- sight_sf |>
  mutate(Species = factor(Species, levels = species_levels),
         color   = unname(species_colors[as.character(Species)]))

# Sanity check -- must be empty
sight_sf |> sf::st_drop_geometry() |> dplyr::filter(is.na(color)) |> dplyr::distinct(Species)

# Build map
map2 <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>

  # To avoid the 'map not available' message when zooming too far in:
  addProviderTiles(providers$Esri.OceanBasemap,
                   options = providerTileOptions(maxNativeZoom = 8)) |>
  
  # Full Garmin track (uninterrupted, whole survey) -- off by default
   # addPolylines(
   #   data = garmin_track_sf,
   #   color = "gray30",
   #   weight = 1,
   #   opacity = 0.5,
   #   group = "GPS Track (Full)") |>

  addPolylines(
    data = track_lines_sf,
    color = "grey70",
    weight = 1.5,
    opacity = 0.8,
    group = "Realized trackline") |>

  addPolygons(
    data = studyarea_shp,
    color = "black",
    weight = 2,
    fill = FALSE,
    popup = ~paste("<strong>WEA:</strong>", WEA, "<br><strong>Area:</strong>", round(Area_km2, 1), "km²"),
    group = "WEA Boundaries") |>

  addCircleMarkers(
    data = sight_sf,
    color = ~color,
    fillColor = ~color,
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    radius = 2.5,
    popup = ~paste0(
      "<strong>Species:</strong> ", Species, "<br/>",
      "<strong>Best Count:</strong> ", Best_count, "<br/>",
      "<strong>Sgt Type:</strong> ", Sgt_type),
    group = "Sightings") |>

  addLegend(
    position = "bottomright",
    colors = unname(species_colors[species_levels]),
    labels = species_levels,
    opacity = 1,          # show true swatch colors, not the 0.5 default
    title = "Species") |>

   addLayersControl(
     #overlayGroups = c("Realized trackline", "Sightings", "GPS Track (Full)"),
     overlayGroups = c("Realized trackline", "Sightings"),
     options = layersControlOptions(collapsed = FALSE))  # |>
   #hideGroup("GPS Track (Full)")


map2

# "WEAWAVS Fall Survey: cumulative marine mammal sightings and realized trackline."







#----SUMMARY MAPS (OFFLINE BASEMAP VERSION)----

#...NOTE: Maps below are currently silenced, but can be unsilenced if these versions
# are preferred over the ones above...

# # Explanation:
# # Leaflet basemaps are always downloaded as tiles from the internet when the map is viewed, so they can’t work offline. 
# # The workaround is to replace the tile layer with basemap layers embedded in the HTML itself:
# # - Land polygons (Natural Earth, 1:10m), drawn with addPolygons()
# # - Bathymetry (NOAA ETOPO, 1 arc-minute), drawn with addRasterImage(), which embeds the raster as an image in the file
# # Both datasets are downloaded once, saved to the project folder, and loaded from disk on every later run. 
# # After that first run, the report needs no internet to build or to view.
# 
# # Interactive (Leaflet) Maps:
# # Map 1: GPS tracks color-coded by effort status
# # Map 2: Sightings color-coded by species ID
# 
# # Required packages (install once; called with :: below, so no library() needed
# # and no masking of dplyr::select() by raster):
# # install.packages(c("rnaturalearth", "rnaturalearthhires", "marmap", "raster"))
# # (rnaturalearthhires may need: install.packages("rnaturalearthhires",
# #   repos = "https://ropensci.r-universe.dev"))
# 
# 
# # OFFLINE BASEMAP (embedded in the HTML -- no internet needed to view the report)
# # Downloaded once (needs internet), then cached locally for all later runs.
# 
# basemap_dir <- file.path(getwd(), "Survey design/basemap_offline")
# dir.create(basemap_dir, showWarnings = FALSE)
# pad <- 1   # degrees of padding around the study area
# 
# # Land polygons (Natural Earth 1:10m)
# land_path <- file.path(basemap_dir, "land_ne10.rds")
# if (!file.exists(land_path)) {
#   sf_use_s2(FALSE)
#   land <- rnaturalearth::ne_download(scale = 10, type = "land",
#                                      category = "physical", returnclass = "sf") |>
#     st_make_valid() |>
#     st_crop(c(xmin = xmin - pad, ymin = ymin - pad, xmax = xmax + pad, ymax = ymax + pad))
#   sf_use_s2(TRUE)
#   saveRDS(land, land_path)
# }
# land_sf <- readRDS(land_path)
# 
# # Bathymetry (NOAA ETOPO, 1 arc-minute)
# bathy_path <- file.path(basemap_dir, "bathy_noaa_1min.rds")
# if (!file.exists(bathy_path)) {
#   bathy <- marmap::getNOAA.bathy(lon1 = xmin - pad, lon2 = xmax + pad,
#                                  lat1 = ymin - pad, lat2 = ymax + pad,
#                                  resolution = 1)
#   saveRDS(bathy, bathy_path)
# }
# bathy_r <- marmap::as.raster(readRDS(bathy_path))
# # Land cells -> 0 (shallowest colour) instead of NA, so the raster covers the
# # whole map; the Natural Earth land polygons are drawn on top. (NA cells were
# # transparent and exposed leaflet's grey background as a blocky halo, because
# # the bathymetry and land layers don't share exactly the same coastline.)
# bathy_r[bathy_r > 0] <- 0
# raster::crs(bathy_r) <- "EPSG:4326"
# 
# # Smooth shading: continuous colour on log10(depth), so shelf depths
# # (0-200 m) use most of the colour range instead of one pale shade
# bathy_depth <- log10(1 - bathy_r)                 # 0 m -> 0; 5000 m -> ~3.7
# 
# # Interpolate to a finer grid so cell edges don't look blocky when zoomed in
# # (fact = 2 -> 4x the cells; raise to 3 for smoother, at the cost of file size)
# bathy_depth <- raster::disaggregate(bathy_depth, fact = 2, method = "bilinear")
# 
# bathy_pal <- colorNumeric(c("#fbfdff", "#eaf3fa", "#d4e6f3", "#b5d3ea", "#8fbbdc"),
#                           domain = raster::values(bathy_depth),
#                           na.color = "transparent")
# 
# # Initial map view = study area extent
# study_bb <- st_bbox(st_transform(studyarea_shp, 4326))
# 
# # Adds the embedded basemap (and study-area zoom) to any leaflet map
# add_offline_basemap <- function(map) {
#   map |>
#     addRasterImage(bathy_depth, colors = bathy_pal, opacity = 1, project = TRUE,
#                    maxBytes = 12 * 1024^2) |>
#     addPolygons(data = land_sf, fillColor = "#ece8dc", fillOpacity = 1,
#                 color = "#8c8c8c", weight = 0.5,
#                 options = pathOptions(interactive = FALSE)) |>
#     fitBounds(lng1 = study_bb[["xmin"]], lat1 = study_bb[["ymin"]],
#               lng2 = study_bb[["xmax"]], lat2 = study_bb[["ymax"]])
# }
# 
# 
# # PREPARE GPS TRACK DATA
# 
# # Per-date cutoff: the timestamp of the last logged "Off" effort entry on
# # each date_adt. The trackline for that date should not extend past it --
# # Mysticetus can keep logging GPS positions after effort/observation has
# # ended for the day, and those trailing points shouldn't appear as part of
# # that day's realized track.
# last_off_time <- effort1 |>
#   filter(Effort == "Off") |>
#   group_by(date_adt) |>
#   summarize(last_off_datetime_utc = max(datetime_utc, na.rm = TRUE), .groups = "drop")
# 
# gps_sf <- gps_effort1 |>
#   # Keep daytime tracks only
#   filter(gps_source == "mysticetus") |>
#   filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
#   # Truncate each date's track at that date's last logged "Off" entry.
#   # Dates with no "Off" entry yet (e.g. survey still underway) are left untruncated.
#   left_join(last_off_time, by = "date_adt") |>
#   filter(is.na(last_off_datetime_utc) | datetime_utc <= last_off_datetime_utc) |>
#   select(-last_off_datetime_utc) |>
#   arrange(datetime_utc) |>
#   st_as_sf(coords = c("vessel_lon", "vessel_lat"),
#            crs = 4326,
#            remove = FALSE)
# 
# # To filter out GPS points that fall outside both WEA polygons
# studyarea_shp <- st_transform(studyarea_shp, st_crs(gps_sf))
# 
# gps_sf <- gps_sf |>
#   st_filter(studyarea_shp, .predicate = st_intersects)
# 
# # Build continuous track line segments based on effort changes or time gaps
# track_lines_sf <- gps_sf |>
#   mutate(
#     time_diff = as.numeric(difftime(datetime_adt, lag(datetime_adt), units = "mins")),
#     effort_change = Effort != lag(Effort),
#     # Flags the start of a new track segment: TRUE if the effort status
#     # just changed, OR if more than 10 minutes passed since the last GPS fix
#     # (a gap big enough that the points shouldn't be connected as one continuous line).
#     # replace_na(..., TRUE) forces any NA (from the first row, or any row where
#     # the comparison itself is NA) to count as "start a new segment" rather than
#     # silently merging into whatever segment came before.
#     new_segment = replace_na(effort_change, TRUE) | replace_na(time_diff > 10, TRUE),
#     segment_id = cumsum(new_segment)) |>
#   mutate(Effort = trimws(Effort)) |>
#   group_by(Effort, segment_id) |>
#   summarize(do_union = FALSE, .groups = "drop") |>
#   st_cast("LINESTRING")
# 
# 
# 
# # PREPARE FULL GARMIN TRACK (uninterrupted, whole survey)
# 
# # The complete Garmin track, day and night, with no polygon cropping and no
# # effort-based truncation -- shows the vessel's full path for the survey so far.
# garmin_track_sf <- gps_garmin |>
#   filter(!is.na(vessel_lon_g), !is.na(vessel_lat_g)) |>
#   arrange(datetime_utc) |>
#   
#   # CHECK IF NEEDED...
#   #mutate(
#   #  time_diff_min = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")),
#   # Start a new segment only after a large gap (e.g. Garmin powered off in
#   # port between survey legs) -- avoids drawing a straight line across
#   # those gaps while keeping the rest of the track visually continuous
#   #  new_segment = replace_na(time_diff_min > 60, TRUE),
#   #  segment_id = cumsum(new_segment)) |>
#   
#   st_as_sf(coords = c("vessel_lon_g", "vessel_lat_g"), crs = 4326, remove = FALSE) |>
#   #group_by(segment_id) |>
#   summarize(do_union = FALSE, .groups = "drop") |>
#   st_cast("LINESTRING")
# 
# 
# 
# # COLOR PALETTES SETUP
# 
# # Effort Palettes
# effort_colors <- c(
#   "Bridge-On"  = "#00FF00",  # Bright Green
#   "On"         = "#006400",  # Dark Green
#   "Transit-On" = "#FFD700",  # Gold/Yellow
#   "Off"        = "#FF0000",  # Red
#   "Closing"    = "#800080"   # Purple
# )
# 
# # Map hex codes directly to the spatial lines data frame
# track_lines_sf$LineColor <- unname(effort_colors[track_lines_sf$Effort])
# track_lines_sf$LineColor[is.na(track_lines_sf$LineColor)] <- "#808080"
# 
# # Extract ONLY the effort states that exist in this active dataset
# # This keeps the order exactly matching the effort_colors palette
# present_states <- intersect(names(effort_colors), unique(track_lines_sf$Effort))
# 
# # Extract the exact matching hex codes for the legend
# legend_colors <- effort_colors[present_states]
# legend_labels <- present_states
# 
# # Transect Palettes
# # Different colored complements:
# transect_cols <- c("1" = "grey40", "2" = "black")
# 
# transect_pal <- colorFactor(
#   palette = transect_cols,
#   domain  = unique(transects_shp$complement))
# 
# # Add a stable row id so line angles join back correctly
# transects_shp2 <- transects_shp |> mutate(row_id = row_number())
# 
# # Project to planar coords to compute each line's true bearing
# transects_proj <- st_transform(transects_shp2, 3857)
# 
# line_angles <- st_coordinates(transects_proj) |>
#   as.data.frame() |>
#   group_by(L1) |>
#   summarize(x_start = first(X), y_start = first(Y),
#             x_end   = last(X),  y_end   = last(Y),
#             .groups = "drop") |>
#   mutate(row_id = L1,
#          angle_deg = atan2(y_end - y_start, x_end - x_start) * 180 / pi,
#          # normalize to (-90, 90] so text is never upside-down
#          angle_deg = ifelse(angle_deg > 90, angle_deg - 180,
#                             ifelse(angle_deg <= -90, angle_deg + 180, angle_deg))) |>
#   select(row_id, angle_deg)
# 
# transects_shp2 <- transects_shp2 |>
#   left_join(line_angles, by = "row_id") |>
#   mutate(label_angle = -angle_deg)
# 
# # Stagger the along-line position so labels on closely-spaced parallel
# # transects don't all collide at the same cross-section of the map
# sample_fracs <- rep(c(0.3, 0.5, 0.7), length.out = nrow(transects_proj))
# 
# geom_list <- st_geometry(transects_proj)
# 
# midpoint_list <- vector("list", length(geom_list))
# for (i in seq_along(geom_list)) {
#   midpoint_list[[i]] <- st_line_sample(
#     st_sfc(geom_list[[i]], crs = st_crs(transects_proj)),
#     sample = sample_fracs[i])
# }
# 
# midpoints_geom <- do.call(c, midpoint_list) |>
#   st_cast("POINT") |>
#   st_transform(4326)
# 
# # Labels on transects:
# transect_midpoints <- st_sf(
#   label       = transects_shp2$label,
#   label_angle = transects_shp2$label_angle,
#   geometry    = midpoints_geom) |>
#   mutate(label_html = paste0(
#     "<div style='transform: rotate(", round(label_angle, 1), "deg); ",
#     "transform-origin: center; white-space: nowrap; font-size:12px; ",
#     "font-weight:bold; color:black;'>", label, "</div>"))
# 
# # Last recorded GPS position -- marks how current the realized track is
# last_position <- gps_sf |>
#   filter(!is.na(datetime_adt)) |>
#   arrange(datetime_adt) |>
#   slice_tail(n = 1)
# 
# last_position_label <- format(last_position$datetime_adt, "%B %d, %Y %H:%M")
# 
# # Build map
# map1 <- leaflet(height = 600, options = leafletOptions(preferCanvas = TRUE)) |>
#   
#   # Embedded offline basemap (bathymetry + land) -- replaces Esri tiles
#   add_offline_basemap() |>
#   
#   # Full Garmin track (uninterrupted, whole survey) -- off by default
#   # addPolylines(
#   #   data = garmin_track_sf,
#   #   color = "gray30",
#   #   weight = 1,
#   #   opacity = 0.5,
#   #   group = "GPS Track (Full)") |>
#   
#   # Planned transects
#   addPolylines(
#     data = transects_shp,
#     color = ~transect_pal(complement),
#     weight = 2,
#     opacity = 0.5,
#     group = "Planned Transects") |>
#   
#   # Transect ID labels (hidden by default -- toggle on via layers control)
#   addLabelOnlyMarkers(
#     data = transect_midpoints,
#     label = ~lapply(label_html, htmltools::HTML),
#     labelOptions = labelOptions(
#       noHide = TRUE,
#       direction = "center",
#       textOnly = TRUE),
#     group = "Transect ID") |>
#   
#   # GPS Tracks / Realized transects
#   addPolylines(
#     data = track_lines_sf,
#     color = track_lines_sf$LineColor,
#     weight = 3,
#     opacity = 0.9,
#     popup = ~paste("<strong>Effort Status:</strong>", Effort),
#     group = "Realized Transects") |>
#   
#   # Last GPS position (report generation position/time)
#   addCircleMarkers(
#     data = last_position,
#     radius = 6,           # size of the circle itself
#     color = "black",      # color of the outline/border
#     weight = 2,           # thickness (in px) of that outline
#     fillColor = "white",  # color of the circle's interior
#     fillOpacity = 1,
#     popup = ~paste0("<strong>Last Recorded Position</strong><br/>", last_position_label),
#     group = "Last Position") |>
#   
#   # WEA polygons
#   addPolygons(
#     data = studyarea_shp,
#     color = "black",           # border color
#     weight = 2,
#     fill = FALSE,              # no fill at all -- outline only
#     popup = ~paste("<strong>WEA:</strong>", WEA, "<br><strong>Area:</strong>", round(Area_km2, 1), "km²"),
#     group = "WEA Boundaries") |>
#   
#   # Effort Legend
#   addLegend(
#     position = "topright",
#     colors   = legend_colors,
#     labels   = legend_labels,
#     opacity = 1,          # show true swatch colors, not the 0.5 default
#     title    = "Effort Status") |>
#   
#   # Transect Legend
#   addLegend(
#     position = "topright",
#     pal = transect_pal,
#     values = unique(transects_shp$complement),
#     title = "Planned Transects") |>
#   
#   # With full Garmin track..
#   # addLayersControl(
#   #   overlayGroups = c("Planned Transects", "Realized Transects", "Transect ID", "GPS Track (Full)"),
#   #   options = layersControlOptions(collapsed = FALSE)) |>
#   # hideGroup(c("Transect ID", "GPS Track (Full)"))
#   
#   # Without full Garmin track..
#   addLayersControl(
#     overlayGroups = c("Planned Transects", "Realized Transects", "Transect ID"),
#     options = layersControlOptions(collapsed = FALSE)) |>
#   hideGroup(c("Transect ID"))
# 
# map1
# 
# # "WEAWAVS Fall Survey: cumulative realized trackline, color-coded by effort status."
# 
# 
# 
# # MAP 2: Sightings color-coded by species ID
# 
# # PREPARE SIGHTINGS DATA
# 
# species_exclude <- c("Mola mola", "Other", "Basking shark", "Tuna")
# 
# sight_sf <- sight3 |>
#   mutate(Species = trimws(Species)) |>
#   filter(!is.na(Species), !Species %in% species_exclude) |>
#   mutate(lat = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lat,
#                          TRUE ~ vessel_lat),
#          lon = case_when(Sgt_type == "On-effort" & !is.na(PSD_m) ~ Sgt_lon,
#                          TRUE ~ vessel_lon)) |>
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# species_levels <- sort(unique(sight_sf$Species))
# 
# # Fixed species
# species_colors <- c(
#   "Atlantic white-sided dolphin" = "#e6194b",
#   "Blue whale"                   = "#0072B2",
#   "Bottlenose dolphin"           = "#7F8C8D",
#   "Common dolphin"               = "#911eb4",
#   "Fin whale"                    = "#8bc34a",
#   "Fin/sei whale"                = "#4b3b6b",
#   "Grey seal"                    = "#8c5464",
#   "Harbour porpoise"             = "#17becf",
#   "Harbour seal"                 = "#808000",
#   "Humpback whale"               = "#000075",
#   "Killer whale"                 = "#0e7c7b",
#   "Minke whale"                  = "#4d7c0f",
#   "Pilot whale"                  = "#800000",
#   "White-beaked dolphin"         = "#c2185b",
#   "Unknown baleen whale"         = "#f032e6",
#   "Unknown cetacean"             = "#f58231",
#   "Unknown small cetacean"       = "#FFDF00",
#   "Unknown dolphin"              = "#a8841c",
#   "Unknown seal"                 = "#4a4a68",
#   "Leatherback turtle"           = "#000000",
#   "Leatherback turtle (dead)"    = "#ffffff"
# )
# 
# sight_sf <- sight_sf |>
#   mutate(Species = factor(Species, levels = species_levels),
#          color   = unname(species_colors[as.character(Species)]))
# 
# # Sanity check -- must be empty
# sight_sf |> sf::st_drop_geometry() |> dplyr::filter(is.na(color)) |> dplyr::distinct(Species)
# 
# # Build map
# map2 <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
#   
#   # Embedded offline basemap (bathymetry + land) -- replaces Esri tiles
#   add_offline_basemap() |>
#   
#   # Full Garmin track (uninterrupted, whole survey) -- off by default
#   # addPolylines(
#   #   data = garmin_track_sf,
#   #   color = "gray30",
#   #   weight = 1,
#   #   opacity = 0.5,
#   #   group = "GPS Track (Full)") |>
#   
#   addPolylines(
#     data = track_lines_sf,
#     color = "grey70",
#     weight = 1.5,
#     opacity = 0.8,
#     group = "Realized trackline") |>
#   
#   addPolygons(
#     data = studyarea_shp,
#     color = "black",
#     weight = 2,
#     fill = FALSE,
#     popup = ~paste("<strong>WEA:</strong>", WEA, "<br><strong>Area:</strong>", round(Area_km2, 1), "km²"),
#     group = "WEA Boundaries") |>
#   
#   addCircleMarkers(
#     data = sight_sf,
#     color = ~color,
#     fillColor = ~color,
#     fillOpacity = 0.8,
#     stroke = TRUE,
#     weight = 1,
#     radius = 2.5,
#     popup = ~paste0(
#       "<strong>Species:</strong> ", Species, "<br/>",
#       "<strong>Best Count:</strong> ", Best_count, "<br/>",
#       "<strong>Sgt Type:</strong> ", Sgt_type),
#     group = "Sightings") |>
#   
#   addLegend(
#     position = "bottomright",
#     colors = unname(species_colors[species_levels]),
#     labels = species_levels,
#     opacity = 1,          # show true swatch colors, not the 0.5 default
#     title = "Species") |>
#   
#   addLayersControl(
#     #overlayGroups = c("Realized trackline", "Sightings", "GPS Track (Full)"),
#     overlayGroups = c("Realized trackline", "Sightings"),
#     options = layersControlOptions(collapsed = FALSE))  # |>
# #hideGroup("GPS Track (Full)")
# 
# map2
# 
# # "WEAWAVS Fall Survey: cumulative marine mammal sightings and realized trackline."





#----RENDER DAILY REPORT----

rmarkdown::render(
  input       = "C:/Users/GavrilchukK/Desktop/GITHUB_REPOS/WEAWAVS/scripts/daily_summary_report.Rmd",
  output_file = paste0("WEAWAVS_", dataset, "_SurveyProgressReport_", REPORT.DATE, ".html"),
  output_dir  = file.path(path2data, "Data_summaries", "reports"),
  params      = list(report_date = REPORT.DATE),
  envir       = environment())
