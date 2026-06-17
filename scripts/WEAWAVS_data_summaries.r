##############################################################
# WEAWAVS Data Summaries
# 
# Information:
# This script provides cumulative summaries for sightings,
# tracks, and effort for the WEAWAVS project
##############################################################

setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Spring2026_2026-05-12_FINAL_WORKING")


#----LOAD PACKAGES----

library(here)
library(purrr)
library(tidyverse)
library(sf)

# library(geosphere)
# library(leaflet)
# library(stringr)




#----LOAD DATA----

#----Planned Transects----

# Import planned transects (shapefile)
transects_shp <- st_read(file.path("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Survey design/WEAWAVS_2026-04-10_DFifield/Planned_transects_Scenario2_Spring2026.shp"),
                         quiet = TRUE)

# Import planned transects (csv file; refer to "survey_design_metrics.R" for generation of survey metrics):
transects_df <- read.csv("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Survey design/Scenario2_13-day_Summer2026_transects_with_metrics.csv")
 
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
  drop_na(Effort)

# Export for easier QGIS mapping
write_csv(gps_effort, file = paste0(here(),"/Data_summaries/WEAWAVS_summer2026_gps_effort.csv"))


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
  relocate(c(datetime_missing, datetime_duplicates)) |>
  # Select to arrange column order
  select("Datetime_UTC_locked (UTC)", "Datetime_UTC (UTC)", datetime_utc, date_utc, 
         vessel_lat, vessel_lon, Sgt_lat, Sgt_lon,
         Sgt_type, Sgt_side, Observer, Obs_platform, Distance_tool, 
         Reticles, Distance_m, Angle_rel, Angle_num, Angle_abs, Distance_tool, PSD_m,
         Species, Min_count, Best_count, Max_count, Resight, Photos_taken, 
         Camera, Frame_first, Frame_last, Notes, QAQC_notes,
         datetime_missing, datetime_duplicates)

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
write_csv(sight1, file = paste0(here(),"/Data_summaries/tables/WEAWAVS_summer2026_sightings.csv"))


#----Summary tables & maps----

# 1) Table 1: Distances, transects visited, and proportion of planned transects completed on "Bridge-On" effort, by date

# 2) Table 2: Distances, hours per effort status, by date 

# 3) Table 3: Sightings by Species_ID and group_size (best_count), by date

# 4) Map 1: GPS tracks (daytime only), color-coded by Effort status
# map layers: basemap (land + ocean bathy), study area polygons, planned transects (color-coded by complement)

# 5) Map 2: Sightings, color-coded by Species_ID

# 6) Map 3: GPS tracks + sightings combined



library(geosphere)

# Step 1: Create segment-level effort data
# Each row should represent the movement between GPS point i and i+1.
gps_seg <- gps_effort |>
  arrange(datetime_utc) |>
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

# Then summarize blocks:
effort_blocks <- gps_seg |>
  group_by(date_utc, effort_block, Effort) |>
  summarize(start_time = min(datetime_utc),
            end_time = max(next_time),
            duration_h = sum(duration_h),
            distance_km = sum(dist_km),
            .groups = "drop")

# Table 1: Planned transects completed by date

# Distance on Bridge-On effort
bridge_daily <- gps_seg |>
  filter(Effort == "Bridge-On") |>
  group_by(date_utc) |>
  summarize(
    bridge_km = sum(dist_km),
    bridge_h = sum(duration_h))

# Transects visited
transects_daily <- gps_seg |>
  filter(Effort == "Bridge-On",
         !is.na(Transect_ID)) |>
  group_by(date_utc) |>
  summarize(
    transects_visited =
      n_distinct(Transect_ID),
    
    transect_list =
      paste(sort(unique(Transect_ID)),
            collapse = ", ")
  )

# Planned completion
# Assuming 57 planned transects:
  
  planned_n <- nrow(transects_df1)

table1 <- gps_seg |>
  filter(Effort == "Bridge-On",
         !is.na(Transect_ID)) |>
  group_by(date_utc) |>
  summarize(
    transects_visited =
      n_distinct(Transect_ID)
  ) |>
  mutate(
    prop_completed =
      transects_visited / planned_n
  )

# calculate proportion of each transect completed.
transect_completion <- gps_seg |>
  filter(Effort == "Bridge-On",
         !is.na(Transect_ID)) |>
  group_by(date_utc, Transect_ID) |>
  summarize(
    realized_km = sum(dist_km),
    .groups = "drop"
  ) |>
  left_join(
    transects_df1 |>
      select(
        Transect_ID,
        planned_km = Transect_km
      ),
    by = "Transect_ID"
  ) |>
  mutate(
    completion =
      pmin(realized_km / planned_km, 1)
  )

# Table 2
# Distance and hours by effort status and date
table2 <- gps_seg |>
  group_by(date_utc, Effort) |>
  summarize(
    distance_km = sum(dist_km),
    hours = sum(duration_h),
    .groups = "drop"
  ) |>
  arrange(date_utc)

# Then pivot wide:
table2_wide <- table2 |>
  pivot_wider(
    names_from = Effort,
    values_from = c(distance_km, hours)
  )

# Table 3: Sightings by species and date
table3 <- sight1 |>
  group_by(date_utc, Species_ID) |>
  summarize(
    sightings = n(),
    individuals = sum(best_count,
                      na.rm = TRUE),
    .groups = "drop"
  )

table3_wide <- table3 |>
  pivot_wider(
    names_from = Species_ID,
    values_from = individuals,
    values_fill = 0
  )

# Map 1: GPS tracks by effort

# Convert GPS points to sf:
gps_sf <- st_as_sf(
  gps_effort1,
  coords = c("vessel_lon", "vessel_lat"),
  crs = 4326
)

# Plot:
  ggplot() +
  
  geom_sf(
    data = study_area_sf,
    fill = NA
  ) +
  
  geom_sf(
    data = transects_shp,
    aes(color = complement),
    linewidth = 0.5
  ) +
  
  geom_path(
    data = gps_effort1,
    aes(
      vessel_lon,
      vessel_lat,
      color = Effort
    ),
    linewidth = 0.4
  ) +
  
  coord_sf()

# Map 2: Sightings colored by species
  ggplot() +
    
    geom_sf(
      data = transects_shp,
      color = "grey70"
    ) +
    
    geom_point(
      data = sight1,
      aes(
        Longitude,
        Latitude,
        color = Species_ID
      ),
      size = 2
    )


# Map 3: Combined effort and sightings
  ggplot() +
    
    geom_path(
      data = gps_effort1,
      aes(
        vessel_lon,
        vessel_lat,
        color = Effort
      ),
      alpha = 0.5
    ) +
    
    geom_point(
      data = sight1,
      aes(
        Longitude,
        Latitude,
        shape = Species_ID,
        fill = Species_ID
      ),
      size = 2.5
    ) +
    
    geom_sf(
      data = transects_shp,
      color = "black",
      linewidth = 0.3
    )


# TO DO:
# 
# 1) transect coverage analysis. Since you have planned transects as LINESTRINGs, you can spatially intersect the realized GPS track with each planned transect and estimate:
# 
# km planned
# km actually surveyed
# % completed
# 
# for every transect. That would let Table 1 report something much stronger than "transects visited" and produce a map where transects are colored by completion percentage (0–100%), which is often one of the most useful survey-performance figures in a final report.
#   
# 2) Render Report as .html fileusing Rmarkdown and make the maps interative (via leaflet?) to enable zooming in, etc. 
  
  
  
  
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
