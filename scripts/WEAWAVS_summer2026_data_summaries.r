##############################################################
# WEAWAVS Summer 2026 - Data Summaries
# 
# Information:
# This script provides cumulative summaries for sightings,
# tracks, and effort for the WEAWAVS Summer survey
##############################################################

setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Spring2026_2026-05-12_FINAL_WORKING")

#----LOAD PACKAGES----

library(here)
library(purrr)
library(tidyverse)
library(sf)
library(geosphere)
library(leaflet)
library(htmlwidgets)


#----LOAD DATA----

#----Planned Transects----

# Import planned transects (shapefile)
transects_shp <- st_read(file.path("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Survey design/WEAWAVS_2026-04-10_DFifield/Planned_transects_Scenario2_Spring2026.shp"),
                         quiet = TRUE)

# Import planned transects (csv file; refer to "survey_design_metrics.R" for generation of survey metrics):
transects_df <- read.csv("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Survey design/Summer2026/Scenario2_13-day_Summer2026_transects_with_metrics.csv")
 
# Calculate some additional metrics
transects_df1 <- transects_df |>
   mutate(total_transect_km = sum(unique(Transect_km))) |>
   # Round values for reporting
   mutate(across(ends_with("km"), ~ round(.x, 1)))
 
# Study area bounding box
study.bbox <- c(-63.907471, 43.771094, -58.864746, 47.055154)
 
xmin <- study.bbox[1]
ymin <- study.bbox[2]
xmax <- study.bbox[3]
ymax <- study.bbox[4]


#----GPS----

# List all Mysticetus GPS csv files (daytime tracks only)
mysti_files <- list.files(file.path(paste0(here(), "/Mysti_GPS")),
                          pattern = "csv$", full.names = TRUE)

# Insect: do the files make sense (any duplicates? missing any?)
mysti_files

# Combine Mysticetus GPS files
gps_mysti <- map_df(mysti_files, read_csv, show_col_types = FALSE) |>
  # Rename datetime column & convert to a datetime object
  mutate(datetime_utc = as.POSIXct(`Time Created (UTC)`, tz = "UTC"),
         # Create a date-only variable
         date_utc = as_date(datetime_utc)) |>
  # Rename lat/lon columns; use "_m" to denote it as coming from Mysticetus 
  rename(vessel_lat_m = Latitude,
         vessel_lon_m = Longitude) |>
  # Select columns of interest
  select(datetime_utc, date_utc, vessel_lat_m, vessel_lon_m) |>
  arrange(datetime_utc)

# Check time interval between successive GPS positions (to look for gaps)
time_int_check <- gps_mysti |>
  # Group by date (don't bother looking at gaps between days)
  group_by(date_utc) |>
  mutate(time_diff_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs"))) |>
  arrange(time_diff_sec)

summary(time_int_check$time_diff_sec)
# max time diffs within a day should be around 10-12 seconds 
# (otherwise the GPS puck lost signal or Mysticetus lost connection to the GPS puck)


#----EFFORT----

# List all EffortEnv csv files
effort_files <- list.files(file.path(paste0(here(), "/Mysti_data")),
                           pattern = "EffortEnv\\.csv$", 
                           recursive = TRUE, full.names = TRUE)

# Insect: do the files make sense (any duplicates? missing any?)
effort_files

# Import & combine all EffortEnv files
effort <- effort_files |>
  map_df(~read_csv(.,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE)) 

# Datetime checks
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
# Return to Mysticetus Editor and open the Final-Edited file and make the necessary corrections
# Then re-export the corrected Observations zip folder and return to this script.

# Data wrangling
effort2 <- effort1 |>
  # Rename datetime column & convert to a datetime object
  mutate(datetime_utc = ymd_hms(`Datetime_UTC (UTC)`, tz = "UTC")) |>
  arrange(datetime_utc) |>
  # GPS_pos = the vessel position; need to manipulate it to get lat and lon in separate columns and as decimal degrees
  separate(GPS_pos, into = c("lat_deg", "lat_min", "lat_dir",
                             "lon_deg", "lon_min", "lon_dir"),
           sep = "\\s+", remove = FALSE) |>
  mutate(lat_deg = as.numeric(lat_deg),
         lat_min = as.numeric(lat_min),
         lon_deg = as.numeric(lon_deg),
         lon_min = as.numeric(lon_min),
         # Use "_e" to mark it as coming from Effort/Env (just helps error-check after the Effort + GPS data get joined)
         vessel_lat_e = lat_deg + lat_min / 60,
         vessel_lon_e = lon_deg + lon_min / 60,
         vessel_lat_e = if_else(lat_dir == "S", -vessel_lat_e, vessel_lat_e),
         vessel_lon_e = if_else(lon_dir == "W", -vessel_lon_e, vessel_lon_e)) |>
  # Remove unnecessary columns
  select(-c(datetime_missing, datetime_duplicates, lat_deg, lat_min, lon_deg, lon_min, 
            lat_dir, lon_dir, Lock_row)) 

# Check Transect_ID for errors
table(effort2$Transect_ID)


#----JOIN GPS + EFFORT----

# Combine GPS + Effort
gps_effort <- gps_mysti |>
  full_join(effort2, by = "datetime_utc") |>
  arrange(datetime_utc) |>
  # Coalesce the lat/lon from Effort/Env with the lat/lon from Mysti_GPS
  mutate(vessel_lat = coalesce(vessel_lat_m, vessel_lat_e),
         vessel_lon = coalesce(vessel_lon_m, vessel_lon_e),
         # Re-create a date-only variable
         date_utc = as_date(datetime_utc)) |>
  # Select columns of interest
  select(datetime_utc, date_utc, vessel_lat, vessel_lon, Effort, Transect_ID) |>
  group_by(date_utc) |>
  # Fill the blanks downward for Effort and Transect_ID
  fill(c(Effort, Transect_ID), .direction = "down") |>
 # Remove any Transect_IDs if in Transit-On effort
   mutate(Transect_ID = case_when(Effort == "Transit-On" ~ NA,
                                 TRUE ~ Transect_ID)) |>
  ungroup() |>
  # Remove rows without Effort (this occurs when Mysticetus starts collecting GPS data before an Effort row is entered - often at the beginning of the day)
  drop_na(Effort) |>
  # Remove GPS positions which occur outside of study area bounding box
  filter(vessel_lon >= xmin,
         vessel_lon <= xmax,
         vessel_lat >= ymin,
         vessel_lat <= ymax)

# Export for easier QGIS mapping
#write_csv(gps_effort, file = paste0(here(),"/Data_summaries/WEAWAVS_summer2026_gps_effort.csv"))


#----SIGHTINGS----

# List all Sighting csv files
sight_files <- list.files(file.path(paste0(here(), "/Mysti_data")),
                          pattern = "Sighting\\.csv$", 
                          recursive = TRUE, full.names = TRUE)

# Import & combine all Sighting files
sight <- sight_files |>
  map_df(~read_csv(.,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE))

# Data wrangling
sight1 <- sight |>
  rename(PSD_m = "PSD (m)",
         Distance_m = "Distance (m)") |>
  mutate(datetime_utc = ymd_hms(`Datetime_UTC (UTC)`, tz = "UTC"),
         # Create a date-only variable
         date_utc = as_date(datetime_utc),
         Best_count = as.numeric(Best_count),
         Angle_num = as.numeric(str_extract(Angle_rel, "\\d+")),
         PSD_m = as.numeric(PSD_m),
         Reticles = as.numeric(Reticles),
         Distance_m = as.numeric(Distance_m),
         Angle_abs = as.numeric(Angle_abs)) |>
  # GPS_pos = the vessel position; need to manipulate it to get lat and lon in separate columns and as decimal degrees
  separate(GPS_pos, into = c("lat_deg", "lat_min", "lat_dir",
                             "lon_deg", "lon_min", "lon_dir"),
           sep = "\\s+", remove = FALSE) %>%
  mutate(lat_deg = as.numeric(lat_deg),
         lat_min = as.numeric(lat_min),
         lon_deg = as.numeric(lon_deg),
         lon_min = as.numeric(lon_min),
         vessel_lat = lat_deg + lat_min / 60,
         vessel_lon = lon_deg + lon_min / 60,
         vessel_lat = if_else(lat_dir == "S", -vessel_lat, vessel_lat),
         vessel_lon = if_else(lon_dir == "W", -vessel_lon, vessel_lon)) |>
  # Coalesce lat/lon columns (for sightings without distance AND angle: Sgt_lat/Sgt_lon won't get calculated -- so need to use the vessel's position ("GPS_pos") as an approximate position for sighting)
  mutate(Sgt_lat = as.numeric(Sgt_lat),
         Sgt_lon = as.numeric(Sgt_lon),
         Sgt_lat = coalesce(Sgt_lat, vessel_lat),
         Sgt_lon = coalesce(Sgt_lon, vessel_lon)) |>
  # Remove confirmed or possible resights (duplicates)
  filter(!Resight %in% c("Yes", "Possible")) |>
  # Re-name Species for those which were dead by looking for the word "dead" in the Notes
  mutate(Species = if_else(!is.na(Notes) & str_detect(Notes, regex("dead", ignore_case = TRUE)),
                           paste0(Species, " (dead)"), 
                           Species)) |>
  # Remove columns no longer needed
  select(-c(lat_deg, lat_min, lon_deg, lon_min, lat_dir, lon_dir)) |>
  # Datetime checks
  arrange(`Datetime_UTC_locked (UTC)`) |>
  # Check for any missing values (NA or blank) in the Datetime_UTC (UTC) column, flag those
  mutate(datetime_missing = is.na(`Datetime_UTC (UTC)`) | trimws(`Datetime_UTC (UTC)`) == "",
         # Check for repeated datetimes in the `Datetime_UTC (UTC)`coumn -- due to using the shortcut "Ctl-Shft-+" which copies the previous row
         datetime_duplicates = coalesce(`Datetime_UTC (UTC)` == lag(`Datetime_UTC (UTC)`), FALSE)) |>
  relocate(c(datetime_missing, datetime_duplicates))

# QA/QC

# Datetime checks 
table(sight1$datetime_missing)
table(sight1$datetime_duplicates)

# Manually verify all "datetime_missing" & "datetime_duplicates" columns marked as TRUE
# Verify the Notes to see if the `Datetime_UTC (UTC)` was manually adjusted
# Return to Mysticetus Editor and open the Final-Edited file and make the necessary corrections
# Then re-export the corrected Observations zip folder and return to this script.

# Verify the Sgt_type is all filled in (that they're aren't any NAs)
table(sight1$Sgt_type)

# Verify Reticle values make sense
summary(sight1$Reticles)

# Verify Distance values make sense
summary(sight1$Distance_m)

# Verify that all "On-effort" sightings have relevant info
x <- sight1 |>
  filter(Sgt_type == "On-effort") |>
  select(datetime_utc, Sgt_type, Reticles, Distance_m, Distance_tool,
         Angle_rel, Angle_num, Angle_abs, PSD_m) |>
  # Remove decimal places from numeric columns
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Reminder: The "Angle_abs" takes into account the vessel's heading (so it's normal that it is different compared to Angle_rel)

# Verify Species ID
unique(sight1$Species)

# Counts per Species ID
table(sight1$Species)

# Export
#write_csv(sight1, file = paste0(here(),"/Data_summaries/tables/WEAWAVS_summer2026_sightings.csv"))


#----Summary tables & maps----

# Table 1: Distance & hours spent across all effort states
# Table 2: Daily 'Bridge-On' effort (distance & duration) and transects visited 
# Table 3: Proportion of transects completed during "Bridge-On" effort
# Table 4: Sightings by Species and group_size (best_count), by date
# Map 1: GPS tracks (daytime only), color-coded by Effort
# Map 2: Sightings (and GPS tracks)), color-coded by Species


# Prepare data

# Step 1: Create segment-level effort data
# Each row should represent the movement between GPS point i and i+1.
gps_seg <- gps_effort |>
  arrange(datetime_utc) |>
  group_by(date_utc) |>
  mutate(next_lon = lead(vessel_lon),
         next_lat = lead(vessel_lat),
         next_time = lead(datetime_utc),
         dist_km = distHaversine(cbind(vessel_lon, vessel_lat), cbind(next_lon, next_lat)) / 1000,
         duration_h = as.numeric(difftime(next_time, datetime_utc, units = "hours"))) |>
  filter(!is.na(dist_km))

# Step 2: Create effort blocks (important because effort status changes throughout the day)
gps_seg <- gps_seg |>
  mutate(effort_change = Effort != lag(Effort, default = first(Effort)),
         effort_block = cumsum(effort_change))

# Step 3: Summarize blocks
effort_blocks <- gps_seg |>
  group_by(date_utc, effort_block, Effort) |>
  summarize(start_time_utc = min(datetime_utc),
            end_time_utc = max(next_time),
            duration_h = sum(duration_h),
            distance_km = sum(dist_km),
            .groups = "drop")

# Table 1: Distance & hours spent across all effort states
table1 <- gps_seg |>
  group_by(date_utc, Effort) |>
  summarize(Distance_km = round(sum(dist_km), 1),
            Duration_h  = round(sum(duration_h), 1))

# Pivot wide (if desired):
table1_wide <- table1 |>
  pivot_wider(names_from = Effort,
              values_from = c(Distance_km, Duration_h))

# Table 2: Daily 'Bridge-On' effort (distance & duration) and transects visited 
table2 <- gps_seg |>
  filter(Effort == "Bridge-On") |> # make sure all Transect_IDs are entered properly
  group_by(date_utc) |>
  summarize(# Distance & hours spent during 'Bridge-On' effort
            BridgeOn_km = sum(dist_km),
            BridgeOn_h = sum(duration_h),
            # Transects visited during "Bridge-On" effort
            n_transects = n_distinct(Transect_ID),
            transects = paste(sort(unique(Transect_ID)), collapse = ", "))

# Table 3: Proportion of transects completed during "Bridge-On" effort
table3 <- gps_seg |>
  filter(Effort == "Bridge-On") |>
  group_by(date_utc, Transect_ID) |>
  summarize(realized_km = sum(dist_km),
    .groups = "drop") |>
  left_join(transects_df1 |>
      select(Transect_ID, planned_km = Transect_km), by = "Transect_ID") |>
  # calculate proportion of each transect completed
  mutate(prop_completed = pmin(realized_km / planned_km, 1)) # pmin caps the proportion to 1.0

# Table 4: Sightings by species and date
table4 <- sight1 |>
  group_by(date_utc, Species) |>
  summarize(sightings = n(),
            individuals = sum(Best_count, na.rm = TRUE),
    .groups = "drop")


# CREATE TWO INTERACTIVE LEAFLET MAPS:
# Map 1: GPS tracks color-coded by effort status 
# Map 2: Sightings color-coded by species ID

# PREPARE GPS TRACK DATA 
gps_sf <- gps_effort |>
  filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
  arrange(datetime_utc) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"),
           crs = 4326,
           remove = FALSE)

# Build continuous track line segments based on effort changes or time gaps
track_lines_sf <- gps_sf |>
  mutate(
    time_diff = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")),
    effort_change = Effort != lag(Effort),
    new_segment = replace_na(effort_change, TRUE) | replace_na(time_diff > 10, TRUE),
    segment_id = cumsum(new_segment)
  ) |>
  # Keep Effort as a clean character string for now
  mutate(Effort = trimws(Effort)) |> 
  group_by(Effort, segment_id) |>
  summarize(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING")

# PREPARE SIGHTINGS DATA
sight_sf <- sight1 |>
  filter(!is.na(Sgt_lon), !is.na(Sgt_lat)) |>
  st_as_sf(coords = c("Sgt_lon", "Sgt_lat"), crs = 4326)

species_levels <- sort(unique(sight_sf$Species))

sight_sf <- sight_sf |>
  mutate(Species = factor(Species, levels = species_levels))


# COLOR PALETTES SETUP 

# Effort Palettes  
master_effort_colors <- c(
  "Bridge-On"  = "#00FF00",  # Bright Green
  "On"         = "#006400",  # Dark Green
  "Transit-On" = "#FFD700",  # Gold/Yellow
  "Off"        = "#FF0000",  # Red
  "Closing"    = "#800080"   # Purple 
)

# Map hex codes directly to the spatial lines data frame
track_lines_sf$LineColor <- unname(master_effort_colors[track_lines_sf$Effort])
track_lines_sf$LineColor[is.na(track_lines_sf$LineColor)] <- "#808080" # Fallback to Grey

# Extract ONLY the effort states that exist in this active dataset
# This keeps the order exactly matching the master dictionary
present_states <- intersect(names(master_effort_colors), unique(track_lines_sf$Effort))

# Extract the exact matching hex codes for the legend
legend_colors <- master_effort_colors[present_states]
legend_labels <- present_states

# Transect Palettes
transect_cols <- c("1" = "grey40", "2" = "black") 
transect_pal <- colorFactor(
  palette = transect_cols,
  domain  = unique(transects_shp$complement)
)

# Species Palettes
species_pal <- colorFactor(
  palette = grDevices::rainbow(length(species_levels)),
  domain  = species_levels
)

# MAP 1: GPS tracks color-coded by effort status 

map_effort <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  
  # Planned transects 
  addPolylines(
    data = transects_shp, 
    color = ~transect_pal(complement),
    weight = 2,
    opacity = 0.5,
    group = "Planned Transects"
  ) |>
  
  # GPS Tracks
  addPolylines(
    data = track_lines_sf,
    color = track_lines_sf$LineColor, 
    weight = 3,
    opacity = 0.9,
    popup = ~paste("<strong>Effort Status:</strong>", Effort),
    group = "GPS Track (day)"
  ) |>
  
  # Effort Legend 
  addLegend(
    position = "topleft",
    colors   = legend_colors,  # Pass explicit hex colors directly
    labels   = legend_labels,  # Pass explicit text labels directly
    opacity  = 0.9,
    title    = "Effort Status"
  ) |>
  
  # Transect Legend
  addLegend(
    position = "bottomleft",
    pal = transect_pal,
    values = unique(transects_shp$complement),
    title = "Planned Transects"
  ) |>
  
  addLayersControl(
    overlayGroups = c("Planned Transects", "GPS Track (day)"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Output Map 1
map_effort
# Adjust output path as needed
saveWidget(map_effort, "gps_effort_map.html", selfcontained = TRUE)



# MAP 2: Sightings color-coded by species ID

map_sightings <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  
  # GPS tracks (not color-coded by effort)
  addPolylines(
    data = track_lines_sf,
    color = "grey70",
    weight = 1.5,
    opacity = 0.4,
    group = "GPS Track (day)"
  ) |>
  
  # Sightings as points
  addCircleMarkers(
    data = sight_sf,
    color = ~species_pal(Species),
    fillColor = ~species_pal(Species),
    fillOpacity = 0.8,
    stroke = TRUE,
    weight = 1,
    radius = 2.5,
    popup = ~paste0(
      "<strong>Species:</strong> ", Species, "<br/>",
      "<strong>Best Count:</strong> ", Best_count, "<br/>",
      "<strong>Sgt Type:</strong> ", Sgt_type
    ),
    group = "Sightings"
  ) |>
  
  # Species Legend
  addLegend(
    position = "bottomright",
    pal = species_pal,
    values = species_levels,
    title = "Species"
  ) |>
  
  addLayersControl(
    overlayGroups = c("GPS Track (day)", "Sightings"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Output Map 2
map_sightings
# Adjust output path as needed
saveWidget(map_sightings, "sightings_map.html", selfcontained = TRUE)
