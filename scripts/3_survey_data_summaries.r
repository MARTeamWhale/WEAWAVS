################################################################################
#
# Script purpose: Summarize vessel survey effort and marine mammal sightings data
# Program: DFO Maritimes - Cetacean Research and Monitoring Program (CRMP)
# Project: Wind Energy Area Wildlife Assessment Vessel Surveys (WEAWAVS)
# Author: Katherine Gavrilchuk, with the assistance of Claude AI
# Affiliation: Fisheries and Oceans Canada
# Contact: katherine.gavrilchuk@dfo-mpo.gc.ca
# Last updated: August 5, 2026
# R version: 4.6.1
#
################################################################################

#----LOAD PACKAGES----

library(purrr)
library(dplyr)
library(tidyr)
library(sf)
library(geosphere)
library(leaflet)
library(htmlwidgets)
library(lubridate)
library(htmltools)
library(ggplot2)
library(marmap)
library(ggspatial)
library(ggnewscale)
library(scales)


#----SET WORKING DIRECTORY & PATH TO WORKING DATA----

setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/")

# Specify dataset
dataset = "Fall2026" # Spring2026, Summer2026, Fall2026, Winter2027

# Define paths to each dataset
data_paths <- c(
  Spring2026 = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Spring2026_2026-05-12_FINAL_WORKING",
  Summer2026 = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Summer2026_2026-07-17_FINAL_WORKING",
  Fall2026   = "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Fall2026_Interim_WORKING"
  # Winter2027 = "..."
)

path2data <- data_paths[[dataset]]


#----LOAD COMBINED/CUMULATIVE SIGHTINGS & GPS-EFFORT----

# MM sightings
sight <- read.csv(paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_MM_sightings.csv"))

# GPS & effort
gpseff <- read.csv(paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_gps-effortenv.csv"))


#----LOAD PLANNED TRANSECTS----

# Planned transects (shapefile)
transects_shp <- st_read(file.path(paste0(getwd(), "/Survey design/WEAWAVS_2026-04-10_DFifield/Planned_transects_Scenario2_Spring2026.shp")), quiet = TRUE)

# Planned transects with metrics (csv)
transects_df <- read.csv(paste0(getwd(),"/Survey design/Scenario2_13-day_transects_with_metrics.csv"))

# Study area polygons (shapefile)
studyarea_shp <- st_read(file.path(paste0(getwd(), "/Survey design/WEAWAVS_2026-04-10_DFifield/Study_area_buf_prelim.shp")), quiet = TRUE)

# Coastal shapefile
coastal_shp <- st_read(file.path("C:/Users/GavrilchukK/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/Mapping/Shapefiles/coastline/canada/gpr_000b11a_e_hires/gpr_000b11a_e.shp"), quiet = TRUE)


#----DEFINE STUDY AREA----

# V2 (adapted after the Summer2026 survey since they had a distant SAR call)
study.bbox <- c(-65.742188, 41.508577, -52.163086, 47.279229)

xmin <- study.bbox[1]
ymin <- study.bbox[2]
xmax <- study.bbox[3]
ymax <- study.bbox[4]



#----SUMMARY TABLES----

# Table 1: Distance & hours spent across all effort states
# Table 2: Daily on-effort (distance & duration) and transects visited 
# Table 3: Proportion of transects completed during on-effort
# Table 4a: Sightings by Species & Date
# Table 4b: Sightings by Species

# Prepare data

table(gpseff$gps_source)
table(gpseff$Effort)

# Step 1: Create segment-level effort data
# Each row should represent the movement between GPS point i and i+1.
gps_seg <- gpseff |>
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




# Table 1: Distance & hours spent across all effort states
table1 <- gps_seg |>
  group_by(date_adt, Effort) |>
  summarize(Distance_km = round(sum(dist_km), 1),
            Duration_h  = round(sum(duration_h), 1))

# Pivot wide (if desired):
table1_wide <- table1 |>
  pivot_wider(names_from = Effort,
              values_from = c(Distance_km, Duration_h))

write.csv(table1_wide, paste0(path2data, "/Data_summaries/tables/WEAWAVS_", dataset, "_Distance-Hours-by-Effort.csv"),
          row.names = FALSE)

# Table 2: Daily 'Bridge-On' effort (distance & duration) and transects visited 
# table2 <- gps_seg |>
#   filter(Effort == "Bridge-On") |> # make sure all Transect_IDs are entered properly
#   group_by(date_utc) |>
#   summarize(# Distance & hours spent during 'Bridge-On' effort
#     BridgeOn_km = sum(dist_km),
#     BridgeOn_h = sum(duration_h),
#     # Transects visited during "Bridge-On" effort
#     n_transects = n_distinct(Transect_ID),
#     transects = paste(sort(unique(Transect_ID)), collapse = ", "))

# Table 3a: Proportion of transects completed during "Bridge-On" or "On" effort, by date and Transect_ID
table3a <- gps_seg |>
   filter(Effort %in% c("On", "Bridge-On")) |>
   #filter(Effort == "On") |>
   group_by(date_adt, Transect_ID) |>
   summarize(realized_km = sum(dist_km),
             .groups = "drop") |>
   left_join(transects_df |>
               select(Transect_ID, planned_km = Transect_km), by = "Transect_ID") |>
   # calculate proportion of each transect completed
   mutate(prop_completed = pmin(realized_km / planned_km, 1)) # pmin caps the proportion to 1.0

#write.csv(table3a, paste0(path2data, "/Data_summaries/tables/WEAWAVS_", dataset, "_prop.transects.on-effort.csv"),
#          row.names = FALSE)

# Table 3b:
# Realized km per transect, summed across all dates (additive, so safe from table3a)
realized_by_transect <- table3a |>
  group_by(Transect_ID) |>
  summarize(realized_km = sum(realized_km), .groups = "drop")

# Strata-level summary: join onto the FULL transect list so a transect with
# zero on-effort coverage still counts its planned_km, and planned_km isn't
# double-counted for transects surveyed on multiple dates
table3b_strata <- transects_df |>
  select(Strata, Transect_ID, planned_km = Transect_km) |>
  left_join(realized_by_transect, by = "Transect_ID") |>
  mutate(realized_km = replace_na(realized_km, 0),
         realized_km   = pmin(realized_km, planned_km)) |>   # cap per transect first
  group_by(Strata) |>
  summarize(realized_km = sum(realized_km),
            planned_km  = sum(planned_km),
            .groups = "drop") |>
  mutate(prop_completed = realized_km / planned_km)

table3b_strata

table3b_total <- table3b_strata |>
  summarize(realized_km = sum(realized_km),
            planned_km  = sum(planned_km)) |>
  mutate(prop_completed = realized_km / planned_km)

table3b_total






# Table 4a: Sightings by species and date
table4a <- sight |>
  group_by(date_adt, Species) |>
  summarize(n_sightings = n(),
            n_individuals = sum(Best_count, na.rm = TRUE),
            .groups = "drop") |>
  mutate(survey = dataset,
         vessel = case_when(dataset == "Spring2026" ~ "Teleost",
                            dataset == "Summer2026" ~ "KopitHopson")) |>
  rename(species = Species) |>
  relocate(c(survey, vessel))

write.csv(table4a, paste0(path2data, "/Data_summaries/tables/WEAWAVS_", dataset, "_MM_sightings_summary_by_date-species.csv"),
          row.names = FALSE)


# Table 4b: Sightings by species
table4b <- sight |>
  group_by(Species) |>
  summarize(n_sightings = n(),
            n_individuals = sum(Best_count, na.rm = TRUE),
            .groups = "drop") |>
  mutate(survey = dataset,
         vessel = case_when(dataset == "Spring2026" ~ "Teleost",
                            dataset == "Summer2026" ~ "KopitHopson")) |>
  rename(species = Species) |>
  relocate(c(survey, vessel))

write.csv(table4b, paste0(path2data, "/Data_summaries/tables/WEAWAVS_", dataset, "_MM_sightings_summary_by_species.csv"),
          row.names = FALSE)


#----SUMMARY MAPS----

# Interactive (Leaflet) Maps:
# Map 1: GPS tracks color-coded by effort status
# Map 2: GPS tracks showing On or Bridge-On effort within strata only  
# Map 3: Sightings color-coded by species ID

# Map 1: GPS tracks color-coded by effort status

# PREPARE GPS TRACK DATA 
gps_sf <- gpseff |>
  filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
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
# Same-colored complements:
#transect_cols <- c("1" = "grey40", "2" = "grey40") 
transect_pal <- colorFactor(
  palette = transect_cols,
  domain  = unique(transects_shp$complement))

# Build map
map1 <- leaflet(height = 600, options = leafletOptions(preferCanvas = TRUE)) |> 

  # Different basemap options:
  
  # ESRI
  addProviderTiles(providers$Esri.OceanBasemap) |>
  #Esri's "reference" layer for Ocean Basemap, which adds place
  #names, land/coastline labels, and depth-contour labels (can't remove depth-contour labels however)
  #addTiles(
  # urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/{z}/{y}/{x}",
  # attribution = "Esri, GEBCO, NOAA, National Geographic, DeLorme, HERE, Geonames.org, and other contributors",
  # group = "Labels") |>

  # Planned transects 
  addPolylines(
    data = transects_shp, 
    color = ~transect_pal(complement),
    weight = 2,
    opacity = 0.5,
    group = "Planned Transects") |>
  
  # GPS Tracks / Realized transects
  addPolylines(
    data = track_lines_sf,
    color = track_lines_sf$LineColor, 
    weight = 3,
    opacity = 0.9,
    popup = ~paste("<strong>Effort Status:</strong>", Effort),
    group = "Realized Transects") |>
  
  # WEA polygons
  addPolygons(
    data = studyarea_shp,
    color = "black",           # border color
    weight = 2,
   # fillColor = "yellow",
    #fillOpacity = 0.1,         # light fill so tracks/transects underneath stay visible
    fill = FALSE,              # no fill at all -- outline only
    popup = ~paste("<strong>WEA:</strong>", WEA, "<br><strong>Area:</strong>", round(Area_km2, 1), "km²"),
    group = "WEA Boundaries") |>
  
  # Effort Legend 
   addLegend(
     position = "topright",
     colors   = legend_colors,  # Pass explicit hex colors directly
     labels   = legend_labels,  # Pass explicit text labels directly
     opacity  = 0.9,
     title    = "Effort Status") |>
  
  # Transect Legend
   addLegend(
     position = "topright",
     pal = transect_pal,
     values = unique(transects_shp$complement),
     title = "Planned Transects") |>
  
  # Title (option 1)
  # onRender("
  #   function(el, x) {
  #     // Create the full-width title bar
  #     var title = document.createElement('div');
  #     title.innerHTML = 'WEAWAVS Spring 2026';
  #     title.style.position = 'absolute';
  #     title.style.top = '0';
  #     title.style.left = '0';
  #     title.style.width = '100%';
  #     title.style.zIndex = '1000';
  #     title.style.textAlign = 'center';
  #     title.style.backgroundColor = 'white';
  #     title.style.padding = '10px 0';
  #     title.style.fontSize = '18px';
  #     title.style.fontWeight = 'bold';
  #     title.style.boxShadow = '0 1px 4px rgba(0,0,0,0.3)';
  #     el.appendChild(title);
  # 
  #     // Push the zoom control and legend down so they don't sit
  #     // underneath the new title bar
  #     var topControls = el.querySelectorAll('.leaflet-top');
  #     topControls.forEach(function(ctrl) { ctrl.style.marginTop = '44px'; });
  #   }
  # ") |>
  
  # Title (option 2)
  # addControl(
  #  html = "<h3 style='margin:0; padding:4px 10px; background-color:white; 
  #           border-radius:4px; box-shadow:0 1px 4px rgba(0,0,0,0.3);'>
  #           WEAWAVS Spring 2026
  #           </h3>",
  #  position = "topleft") |>

  addLayersControl(
    overlayGroups = c("Planned Transects", "Realized Transects"),
    options = layersControlOptions(collapsed = FALSE))

# View 
map1

# Export
saveWidget(map1, paste0(path2data, "/Data_summaries/figures/WEAWAVS_", dataset, "_gps_effort_map.html"), selfcontained = TRUE)

# Title (option 3); added after the map gets created
map_effort_titled <- browsable(
  tagList(
    div(
      style = "width:100%; text-align:center; font-size:20px; font-weight:bold; 
               font-family: 'Arial';
               padding:10px 0; background-color:white;",
      "WEAWAVS Spring 2026"),
    map1))
 
# View
map1_titled

# Export
save_html(
  map1_titled,
  file = paste0(path2data, "/Data_summaries/figures/WEAWAVS_", dataset, "_gps_effort_map.html"),
  background = "white") # saves a lib folder as well


##### TROUBLESHOOTING OPTIONS OFR EXPORT:
# To get the exact URL/attribution your installed leaflet.providers version uses for Esri.OceanBasemap (rather than guessing), the easiest path is to look at the plain-text JS file that your working non-selfcontained export already generates successfully:
#Run your working save_html() export (the one that already succeeds) once more.
#Open WEAWAVS_..._files/leaflet-providers-plugin-2.2.3/leaflet-providers-plugin.js in a text editor.
#Search for "OceanBasemap" — you'll see the literal url template, attribution string, and any options (like maxZoom) leaflet.providers uses for that basemap.

#Then hardcode it, e.g.:
# addTiles(
#   urlTemplate = "https://{s}.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
#   attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri",
#   options = tileOptions(maxZoom = 13))



# Map 2: GPS tracks showing on effort

# Keep only tracks with "On or "Bridge-On" effort segments
gps_sf <- gpseff |>
  filter(Effort %in% c("On", "Bridge-On")) |>
  filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
  arrange(datetime_utc) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"),
           crs = 4326,
           remove = FALSE)

# Filter out GPS points that fall outside both WEA polygons
studyarea_shp <- st_transform(studyarea_shp, st_crs(gps_sf))

gps_sf <- gps_sf |>
  st_filter(studyarea_shp, .predicate = st_intersects)

# Build continuous track line segments based on effort changes or time gaps
track_lines_sf <- gps_sf |>
  mutate(
    time_diff = as.numeric(difftime(datetime_adt, lag(datetime_adt), units = "mins")),
    effort_change = Effort != lag(Effort),
    new_segment = replace_na(effort_change, TRUE) | replace_na(time_diff > 10, TRUE),
    segment_id = cumsum(new_segment)) |>
  mutate(Effort = trimws(Effort)) |> 
  group_by(Effort, segment_id) |>
  summarize(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING")

# COLOR PALETTES SETUP 

effort_colors <- c(
  "Bridge-On"  = "#006400",  # Bright Green
  "On"         = "#006400" #,  # Dark Green
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
# Same-colored complements:
#transect_cols <- c("1" = "grey40", "2" = "grey40") 
transect_pal <- colorFactor(
  palette = transect_cols,
  domain  = unique(transects_shp$complement))


# Build map
map2 <- leaflet(height = 600, options = leafletOptions(preferCanvas = TRUE)) |>
  
  # Different basemap options:
  
  # ESRI
  addProviderTiles(providers$Esri.OceanBasemap) |>
  #Esri's companion "reference" layer for Ocean Basemap -- adds place
  #names, land/coastline labels, and depth-contour labels that the base
  #tile set alone doesn't include (can't remove depth-contour labels however)
  #addTiles(
  # urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Reference/MapServer/tile/{z}/{y}/{x}",
  # attribution = "Esri, GEBCO, NOAA, National Geographic, DeLorme, HERE, Geonames.org, and other contributors",
  # group = "Labels") |>

  # Planned transects 
  addPolylines(
    data = transects_shp, 
    color = ~transect_pal(complement),
    weight = 2,
    opacity = 0.5,
    group = "Planned Transects") |>
  
  # GPS Tracks / Realized transects
  addPolylines(
    data = track_lines_sf,
    color = track_lines_sf$LineColor, 
    weight = 3,
    opacity = 0.9,
    popup = ~paste("<strong>Effort Status:</strong>", Effort),
    group = "Realized Transects") |>
  
  # WEA polygons
  addPolygons(
    data = studyarea_shp,
    color = "black",           # border color
    weight = 2,
    # fillColor = "yellow",
    #fillOpacity = 0.1,         # light fill so tracks/transects underneath stay visible
    fill = FALSE,              # no fill at all -- outline only
    popup = ~paste("<strong>WEA:</strong>", WEA, "<br><strong>Area:</strong>", round(Area_km2, 1), "km²"),
    group = "WEA Boundaries") |>

  # Transect Legend
  addLegend(
    position = "topright",
    colors   = c("#006400", "#666666"),  # grey40 → its hex equivalent (CSS doesn't recognize "grey40")
    labels   = c("Realized transects", "Planned transects"),
    opacity  = 0.9) #|>

#addLayersControl(
#  overlayGroups = c("Planned Transects", "Realized Transects", "WEA Boundaries"),
#  options = layersControlOptions(collapsed = FALSE))

# View 
map2

# Export
#saveWidget(map_effort, paste0(path2data, "/Data_summaries/figures/WEAWAVS_", dataset, "_gps_effort_map.html"), selfcontained = TRUE)

# Title: added after the map gets created
map2_titled <- browsable(
  tagList(
    div(
      style = "width:100%; text-align:center; font-size:20px; font-weight:bold; 
               font-family: 'Arial';
               padding:10px 0; background-color:white;",
      "WEAWAVS Spring 2026"),
    map2))

# View
map2_titled

# Export
save_html(
  map2_titled,
  file = paste0(path2data, "/Data_summaries/figures/WEAWAVS_", dataset, "_gps_effort_map.html"),
  background = "white")



# MAP 3: Sightings color-coded by species ID

# PREPARE SIGHTINGS DATA

# Filter out any species that is dead
#sight <- sight |>
#  filter( # TO ADD: Does not include 'dead' anywhere in the species name)

sight_sf <- sight |>
  # Use lat/lon as Sgt_lat and Sgt_lon had to be adjusted within survey_data_error-checking.r
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

species_levels <- sort(unique(sight_sf$Species))

sight_sf <- sight_sf |>
  mutate(Species = factor(Species, levels = species_levels))

# Species Palettes
# [TO DO: Make these colors not rainbow, and make them specific to a given species]
species_pal <- colorFactor(
  palette = grDevices::rainbow(length(species_levels)),
  domain  = species_levels)

# Build map
map3 <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(providers$Esri.OceanBasemap) |>
  
  # GPS tracks (not color-coded by effort)
  addPolylines(
    data = track_lines_sf,
    color = "grey70",
    weight = 1.5,
    opacity = 0.4,
    group = "Realized transects") |>
  
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
      "<strong>Sgt Type:</strong> ", Sgt_type),
    group = "Sightings") |>
  
  # Species Legend
  addLegend(
    position = "bottomright",
    pal = species_pal,
    values = species_levels,
    title = "Species") |>
  
  addLayersControl(
    overlayGroups = c("Realized transects", "Sightings"),
    options = layersControlOptions(collapsed = FALSE))

# View 
map3

# Export
saveWidget(map3, paste0(path2data, "/Data_summaries/figures/WEAWAVS_", dataset, "_MMsightings_map.html"), selfcontained = TRUE)
           
           
           
           
################################################################################
# Static versions of the 3 maps above
################################################################################

# Output settings
out_dir    <- paste0(path2data, "/Data_summaries/figures/")
dpi_out    <- 300      # matches your earlier choice: PNG, 300 dpi
fig_width  <- 8        # inches (adjust as needed)
fig_height <- 6.5      # inches (adjust as needed)

save_map <- function(plot, filename) {
  ggsave(
    filename = paste0(out_dir, filename),
    plot     = plot,
    width    = fig_width, height = fig_height, units = "in",
    dpi      = dpi_out, bg = "white")
}

# Study area extent
study_bbox <- st_bbox(st_transform(studyarea_shp, 4326))

# Halifax's coordinates
halifax_lon <- -63.575
halifax_lat <- 44.649

# Padding for the *plotted* map frame (keeps tracks/points from touching the edge).
frame_pad <- 0.15
map_xlim <- c(min(study_bbox["xmin"] - frame_pad, halifax_lon - 0.3),
              study_bbox["xmax"] + frame_pad + 0.3)  # +0.3 extra east so the SB label isn't clipped
map_ylim <- c(study_bbox["ymin"] - frame_pad, study_bbox["ymax"] + frame_pad)

# Bathymetry query extent is derived from the *plotted* frame (map_xlim/
# map_ylim) rather than study_bbox directly, plus a little extra padding so
# there's no blank strip right at the plotted edges -- that way it always
# covers whatever the frame above actually ends up showing, Halifax's
# westward extension included, with no separate bookkeeping needed.
bathy_pad <- 0.35

bathy <- getNOAA.bathy(
  lon1       = map_xlim[1] - bathy_pad,
  lon2       = map_xlim[2] + bathy_pad,
  lat1       = map_ylim[1] - bathy_pad,
  lat2       = map_ylim[2] + bathy_pad,
  resolution = 1,     # arc-minutes; smaller = finer detail but slower & bigger.
  # 1 is a good regional-scale default; try 0.5 if you
  # want a sharper depth image and don't mind the wait.
  keep       = TRUE   # caches the download to a .csv in your working directory,
)                      # so re-running this script won't re-download it

bathy_df <- fortify.bathy(bathy)  # columns: x (lon), y (lat), z (depth; negative = underwater)
message("Depth range in study area (m): ", paste(range(bathy_df$z), collapse = " to "))


# Water only -- land now comes from the coastline shapefile below instead of
# the bathymetry raster, so anything at/above sea level is just dropped here.
bathy_df <- bathy_df |> mutate(depth_water = ifelse(z <= 0, z, NA))

# Coastline shapefile

sort(unique(coastal_shp$PRENAME))   # run once to see the exact names available
maritime_provinces <- c("Nova Scotia", "New Brunswick", "Prince Edward Island")
coastal_shp <- coastal_shp[coastal_shp$PRENAME %in% maritime_provinces, ]

sf::sf_use_s2(FALSE)

coastal_shp <- coastal_shp |>
  st_simplify(dTolerance = 0.0005, preserveTopology = TRUE) |>  # ~50m; plenty for this scale
  st_make_valid() |>                    # hi-res boundary files often have small topology errors
  st_transform(4326)                    # match everything else (source is NAD83, near-identical to WGS84)

sf::sf_use_s2(TRUE)   # back to the (more accurate) default

# Reusable bathymetry + coastline layers, styled to resemble the Esri Ocean
bathy_layers <- list(
  # Ocean: smooth dark-to-pale blue gradient by depth, no legend
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = depth_water), interpolate = TRUE),
  scale_fill_gradientn(
    colours  = c("#0a3161", "#0f5c8a", "#2f8dc0", "#7fc0e0", "#cfe9f5"),
    na.value = "transparent",
    guide    = "none"),
  # Land: your coastline shapefile, drawn on top of the water raster
  geom_sf(data = coastal_shp, fill = "grey85", colour = "grey40", linewidth = 0.25, inherit.aes = FALSE))

# Reusable scale bar + north arrow + consistent map frame/theme for all 3 maps
map_furniture <- list(
  annotation_scale(location = "bl", width_hint = 0.25, style = "ticks"),
  #annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()),
  coord_sf(xlim = map_xlim, ylim = map_ylim, crs = 4326, expand = FALSE),
  theme_bw(base_size = 11),
  theme(panel.grid = element_line(colour = "grey85", linewidth = 0.2),
        legend.background = element_rect(fill = alpha("white", 0.8), colour = NA),
        legend.key.width = unit(1.1, "cm"))   # longer legend line swatches
)


# =============================================================================
# MAP 1: GPS tracks colour-coded by effort status
# =============================================================================

# Prepare GPS track data
gps_sf <- gpseff |>
  filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
  arrange(datetime_utc) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE)

studyarea_shp <- st_transform(studyarea_shp, st_crs(gps_sf))
gps_sf <- gps_sf |> st_filter(studyarea_shp, .predicate = st_intersects)

track_lines_sf1 <- gps_sf |>
  # Remove any rows where Effort = NA (these should be changed to "Off")
  filter(!is.na(Effort), trimws(Effort) != "") |>
  mutate(
    time_diff     = as.numeric(difftime(datetime_adt, lag(datetime_adt), units = "mins")),
    effort_change = Effort != lag(Effort),
    new_segment   = replace_na(effort_change, TRUE) | replace_na(time_diff > 10, TRUE),
    segment_id    = cumsum(new_segment),
    Effort        = trimws(Effort)) |>
  group_by(Effort, segment_id) |>
  summarize(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING")

effort_colors <- c(
  "On"         = "#006400",
  "Bridge-On"  = "#00FF00",
  "Transit-On" = "#FFD700",
  "Off"        = "#FF0000",
  "Closing"    = "#800080")

transects_shp <- transects_shp |> mutate(complement = as.factor(complement))
transect_cols <- c("Complement 1" = "grey40", "Complement 2" = "black")

# Place-name labels (reusable -- add `+ place_labels_layer` to Map 2/3 too)

# WEA polygon labels (FMB / SB), auto-positioned at each polygon's top-right
# corner with a small offset. 
anchor_xy <- st_coordinates(st_point_on_surface(st_geometry(studyarea_shp)))

wea_labels <- studyarea_shp |>
  st_drop_geometry() |>
  mutate(
    label = case_when(
      grepl("French", WEA, ignore.case = TRUE) ~ "FMB",
      grepl("Sydney", WEA, ignore.case = TRUE) ~ "SB",
      TRUE ~ WEA
    )
  ) |>
  bind_cols(as.data.frame(t(sapply(st_geometry(studyarea_shp), function(g) st_bbox(g)[c("xmax", "ymax")])))) |>
  bind_cols(as.data.frame(anchor_xy)) |>   # adds columns X, Y -- a point guaranteed inside the polygon
  mutate(
    label_x = ifelse(label == "FMB", X + 0.5, xmax + 0.04),
    label_y = ifelse(label == "FMB", Y + 0.4, ymax + 0.02))

# City points, real-world coordinates. Halifax reuses halifax_lon/halifax_lat
# from the top of the script (that's what the map frame was widened around).
# hjust/vjust are mapped per-row via aes() below so Halifax and Sydney's
# labels can sit in different positions relative to their points.
place_points <- data.frame(
  place = c("Halifax", "Sydney"),
  lon   = c(halifax_lon, -60.183),
  lat   = c(halifax_lat, 46.135),
  hjust = c(0.5, 0.5),   # both centred horizontally over their point
  vjust = c(-1, 1.8)     # Halifax's label sits above; Sydney's sits below
)

# Region labels
place_regions <- data.frame(
  place = c("Nova Scotia", "P.E.I."),
  lon   = c(-62.6, -63.1),  
  lat   = c(45.35, 46.3))

place_labels_layer <- list(
  geom_text(data = wea_labels, aes(x = label_x, y = label_y, label = label),
            inherit.aes = FALSE, fontface = "bold", size = 4, hjust = 0),
  geom_point(data = place_points, aes(x = lon, y = lat),
             inherit.aes = FALSE, size = 2, shape = 21, fill = "black", colour = "black", stroke = 0.7),
  geom_text(data = place_points, aes(x = lon, y = lat, label = place, hjust = hjust, vjust = vjust),
            inherit.aes = FALSE, size = 3),
  geom_text(data = place_regions, aes(x = lon, y = lat, label = place),
            inherit.aes = FALSE, size = 4.2, fontface = "italic", colour = "grey25")
)

# Build map
map1 <- ggplot() +
  bathy_layers +
  geom_sf(data = studyarea_shp, fill = NA, colour = "black", linewidth = 0.6) +
  geom_sf(data = transects_shp, aes(colour = complement), linewidth = 0.6, alpha = 0.6) +
  scale_colour_manual(values = transect_cols, name = "Planned Transects",
                      guide = guide_legend(override.aes = list(linewidth = 1.2))) +
  new_scale_colour() +
  geom_sf(data = track_lines_sf1, aes(colour = Effort), linewidth = 0.8) +
  scale_colour_manual(values = effort_colors, name = "Effort Status", na.value = "grey50",
                      breaks = c("On", "Bridge-On", "Transit-On", "Off"),  # legend order
                      guide = guide_legend(override.aes = list(linewidth = 1.2))) +
  place_labels_layer +
  map_furniture +
  labs(title = paste("WEAWAVS", dataset, "— GPS Tracks by Effort"), x = NULL, y = NULL)

map1

save_map(map1, paste0("WEAWAVS_", dataset, "_gps_tracks_by_effort_map.png"))



# =============================================================================
# MAP 2: GPS tracks showing only "On" / "Bridge-On" effort within strata
# =============================================================================

gps_sf2 <- gpseff |>
  filter(Effort %in% c("On", "Bridge-On")) |>
  filter(!is.na(vessel_lon), !is.na(vessel_lat)) |>
  arrange(datetime_utc) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326, remove = FALSE) |>
  st_filter(studyarea_shp, .predicate = st_intersects)

track_lines_sf2 <- gps_sf2 |>
  mutate(
    time_diff     = as.numeric(difftime(datetime_adt, lag(datetime_adt), units = "mins")),
    effort_change = Effort != lag(Effort),
    new_segment   = replace_na(effort_change, TRUE) | replace_na(time_diff > 10, TRUE),
    segment_id    = cumsum(new_segment),
    Effort        = trimws(Effort)) |>
  group_by(Effort, segment_id) |>
  summarize(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING")

# Build map
map2 <- ggplot() +
  bathy_layers +
  geom_sf(data = studyarea_shp, fill = NA, colour = "black", linewidth = 0.6) +
  geom_sf(data = transects_shp,   aes(colour = "Planned transects"),  linewidth = 0.6, alpha = 0.6) +
  geom_sf(data = track_lines_sf2, aes(colour = "Realized transects"), linewidth = 0.8) +
  scale_colour_manual(name = NULL, values = c("Planned transects"  = "grey40",
                                              "Realized transects" = "#006400"),
                      guide = guide_legend(override.aes = list(linewidth = 1.2))) +
  place_labels_layer +
  map_furniture +
  labs(title = paste("WEAWAVS", dataset, "— On-Effort GPS Tracks"), x = NULL, y = NULL)

map2

save_map(map2, paste0("WEAWAVS_", dataset, "_gps_tracks_on_effort_map.png"))





# =============================================================================
# MAP 3: Sightings colour-coded by species
# =============================================================================

sight_sf <- sight |>
  # Remove any "...dead..." species entries
  filter(!grepl("dead", Species, ignore.case = TRUE)) |>   
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

species_levels <- sort(unique(sight_sf$Species))
sight_sf <- sight_sf |> mutate(Species = factor(Species, levels = species_levels))

# Fixed species -> colour lookup (replaces the auto-generated hue_pal()
# palette) so each species always gets the same, clearly distinct colour
# regardless of which species happen to be present in a given dataset run.
# Confirmed-ID species get vivid, maximally-distinct hues; the four "Unknown
# ..." categories get muted/neutral tones instead, so they read as a visually
# de-emphasized group rather than competing for attention with real IDs.
species_colors <- c(
  "Atlantic white-sided dolphin" = "#e6194b",  # red
  "Blue whale"                   = "#4363d8",  # blue
  "Common dolphin"               = "#f58231",  # orange
  "Fin whale"                    = "#3cb44b",  # green
  "Fin/sei whale"                = "#42d4f4",  # cyan
  "Grey seal"                    = "#911eb4",  # purple
  "Harbour porpoise"             = "#ffe119",  # yellow
  "Harbour seal"                 = "#f032e6",  # magenta
  "Humpback whale"               = "#000075",  # navy
  "Killer whale"                 = "#469990",  # teal
  "Minke whale"                  = "#bfef45",  # lime
  "Pilot whale"                  = "#800000",  # maroon
  "White-beaked dolphin"         = "#fabed4",  # pink
  "Unknown baleen whale"         = "#7d8ca3",  # blue/grey   (muted -- unknown ID)
  "Unknown cetacean"             = "#b5838d",  # dusty rose  (muted -- unknown ID)
  "Unknown dolphin"              = "#c9a227",  # mustard (muted -- unknown ID)
  "Unknown seal"                 = "#808000"   # olive  (muted -- unknown ID)
)

# If any species in the data doesn't exactly match one of the names above
# (different spelling/capitalization), it'll fall back to na.value below and
# show up as "NA" in the legend -- run this to check for mismatches:
setdiff(species_levels, names(species_colors))

# Build map
map3 <- ggplot() +
  bathy_layers +
  geom_sf(data = studyarea_shp,   fill = NA, colour = "black", linewidth = 0.6) +
  geom_sf(data = track_lines_sf2, aes(colour = "On-effort GPS Track"), linewidth = 0.5, alpha = 0.5) +
  scale_colour_manual(name = NULL, values = c("On-effort GPS Track" = "grey60"),
                      guide = guide_legend(override.aes = list(linewidth = 1, alpha = 1))) +
  new_scale_colour() +
  geom_sf(data = sight_sf, aes(colour = Species), size = 1.8, alpha = 0.85) +
  scale_colour_manual(values = species_colors, name = "Species", na.value = "grey50",
                      guide = guide_legend(override.aes = list(size = 3))) +
  place_labels_layer +
  map_furniture +
  labs(title = paste("WEAWAVS", dataset, "— MM Sightings"), x = NULL, y = NULL)

# View
map3

# Export
save_map(map3, paste0("WEAWAVS_", dataset, "_MMsightings_map.png"))


















# UPDATE CODE BELOW......

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






#----GPS----

# Mysticetus GPS files
mysti_files <- list.files(file.path(paste0(here(), "/Mysti_GPS")),
                          pattern = "csv$", full.names = TRUE)

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

# Garmin GPS files
garmin_files <- list.files(file.path(paste0(here(), "/Garmin_GPS")),
                           pattern = "\\.gpx$", full.names = TRUE)

# Combine Garmin GPS files
gps_garmin <- map_df(garmin_files, function(f){
  
  df <- st_read(f, layer = "track_points", quiet = TRUE)
  
  coords <- st_coordinates(df)
  
  df |>
    mutate(datetime_utc = force_tz(time, "UTC"),
      vessel_lon_g = coords[,1],
      vessel_lat_g = coords[,2]) |>
    st_drop_geometry() |>
    select(datetime_utc, vessel_lat_g, vessel_lon_g) |>
    arrange(datetime_utc)
  
})

# Export Garmin track only
write.csv(gps_garmin, file = paste0(here(), "/Garmin_GPS/all_garmin_tracks.csv"))

# Check time interval between successive GPS positions
time_int_check <- gps_garmin |>
  arrange(datetime_utc) |>
  mutate(time_diff_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs"))) |>
  arrange(time_diff_sec)

summary(time_int_check$time_diff_sec)


# Import ship's NAV GPS (provided by Teleost crew at end of survey)
teleost_gps <- read.csv(paste0(here(),"/Teleost_NAV_GPS/Teleost_VoyageLog20260512121847.csv"))

# Check time interval between successive GPS positions
teleost_gps1 <- teleost_gps |>
  
  ###################################################################
  ###### NEED TO FIGURE OUT WHAT TIME ZONE THE SHIP'S TRACK IS IN! ##
  ###################################################################
  
    # Create datetime column
  mutate(datetime_utc = ymd_hms(paste(Date, Time), tz = "UTC")) |>
  # Ensure rows are ordered by time
  arrange(datetime_utc) |>
  # Calculate successive time differences
  mutate(time_diff_sec = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "secs"))) 

check <- teleost_gps1 |>
  arrange(time_diff_sec)


# Import ship's NAV GPS (provided via OpenCPN run from a TeamWhale laptop)

# Teleost's NAV OpenCPN gpx files
teleost_files <- list.files(file.path(paste0(here(), "/Teleost_NAV_GPS/OpenCPN")),
                           pattern = "\\.gpx$", full.names = TRUE)

# Combine Teleost GPS files

# Read and combine
gps_teleost <- map_df(teleost_files, function(f) {
  
  df <- st_read(f, layer = "track_points",quiet = TRUE)
  
  coords <- st_coordinates(df)
  
  df |>
    st_drop_geometry() |>
    bind_cols(as.data.frame(coords)) |>
    
  ###################################################################
  ###### NEED TO FIGURE OUT WHAT TIME ZONE THE SHIP'S TRACK IS IN! ##
  ###################################################################
  
    mutate(datetime_utc = force_tz(time, "UTC"),
      vessel_lat_t = Y,
      vessel_lon_t = X) |>
    select(datetime_utc, vessel_lat_t,vessel_lon_t) |>
    arrange(datetime_utc)
  
})
    
# Combine Mysticetus and Garmin GPS
gps <- bind_rows(gps_mysti, gps_garmin) |>
  arrange(datetime_utc) |>
  # Convert to Atlantic Daylight time
  mutate(datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax")) |> # with_tz() = converts the same moment in time to another timezone)
  # Relocate to first columns
  relocate(c(datetime_utc, datetime_adt)) |>
  # There are some duplicates in the Mysticetus GPS rows
  distinct() # n = 6795 duplicate rows removed; sometimes duplicates occurred if ship was stationary for longer periods

# Check time interval between successive GPS positions
time_int_check2 <- gps |>
  arrange(datetime_adt) |>
  mutate(time_diff_sec = as.numeric(difftime(datetime_adt, lag(datetime_adt), units = "secs"))) |>
  arrange(time_diff_sec)

summary(time_int_check$time_diff_sec)




#----EFFORT----

# List all EffortEnv csv files
effort_files <- list.files(file.path(paste0(here(), "/Mysti_data")),
                           pattern = "EffortEnv\\.csv$", 
                           recursive = TRUE, full.names = TRUE)

# Import & combine all EffortEnv files
effort <- effort_files |>
  map_df(~read_csv(.,
                   col_types = cols(.default = col_character()),
                   show_col_types = FALSE)) 

# Data wrangling

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
         vessel_lon_e = if_else(lon_dir == "W", -vessel_lon_e, vessel_lon_e)) |>
  # Add start and end daily effort times, where:
  # - start of effort = first datetime_adt of the day where Effort %in% c("Bridge-On", "On", "Transit-On")
  # - end of day =  the last datetime_adt of the day
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
  # Remove rows marked to be deleted (can also check them first before deleting)
  filter(Delete_row != "True") |>
  # Remove unnecessary columns
  select(-c(datetime_missing, datetime_duplicates, lat_deg, lat_min, lon_deg, lon_min, 
            lat_dir, lon_dir, Lock_row, Delete_row)) |>
  # Relocate columns closer to start of df
  relocate(c(start.effort_adt, end.effort_adt), .after = `Datetime_UTC (UTC)`)


# ---------------------------
# JOIN GPS + EFFORT
# ---------------------------

# Combine GPS + Effort
gps_effort <- gps |>
  full_join(effort3, by = "datetime_utc") |>
  arrange(datetime_utc) |>
  # Coalesce GPS coordinates while preserving coordinate pairs
  mutate(vessel_lat = if_else(!is.na(vessel_lat_m) & !is.na(vessel_lon_m),
                           vessel_lat_m, vessel_lat_g),
         vessel_lon = if_else(!is.na(vessel_lat_m) & !is.na(vessel_lon_m),
                           vessel_lon_m, vessel_lon_g)) |>
  relocate(c(vessel_lat, vessel_lon), .after = vessel_lon_g) |>
  # NOTE: After the step above, verify that the coalesce operation worked properly
  # Now, coalesce the lat/lon from Effort/Env with the vessel_lat/vessel_lon
  mutate(vessel_lat = coalesce(vessel_lat, vessel_lat_e),
         vessel_lon = coalesce(vessel_lon, vessel_lon_e),
         datetime_adt = coalesce(datetime_adt.x, datetime_adt.y),
         Vessel = replace_na(Vessel, "Teleost"),
         # Create date variable based on Atlantic Daylight Time zone
         date_adt = as_date(datetime_adt),
         # Create "strata" column based on dates within each strata (or not in strata)
         Strata = case_when(date_adt %in% as.Date(c("2026-04-23", "2026-05-01", "2026-05-04")) ~ "Not.in.strata",
                            date_adt >= "2026-05-06" & date_adt <= "2026-05-10" ~ "SB",
                            TRUE ~ "FMB"),
         # Correct a typo in Transect ID
         Transect_ID = case_when(Transect_ID == "FBM37" ~ "FMB37",
                                 TRUE ~ Transect_ID),
         Transect_ID = na_if(Transect_ID, "Na")) |>
  # Select columns of interest
  select(datetime_utc, datetime_adt, date_adt, vessel_lat, vessel_lon,
         Vessel, Strata, start.effort_adt, end.effort_adt, Effort, 
         Action, Transect_ID, Port_obs, Stb_obs, BigEye_obs, Data_recorder,
         Sea_state, Swell, Port_vis, Stb_vis,Precipitation,
         Glare_int, Glare_left, Glare_right, sog_kt, cog_t, Notes, QAQC_notes) |>
  arrange(datetime_adt) |>
  group_by(date_adt) |>
  # Fill the blanks downward (for specific columns)
  fill(c(Strata, Effort, start.effort_adt, end.effort_adt,
         Transect_ID, Port_obs, Stb_obs, BigEye_obs, Data_recorder,
         Sea_state, Swell, Port_vis, Stb_vis, Precipitation, Glare_int,
         Glare_left, Glare_right, sog_kt, cog_t), .direction = "down") |>
  # Fill daily bounds upward
  fill(c(start.effort_adt, end.effort_adt), .direction = "up") |>
  # Define 'realized' effort window
  mutate(real_effort_window = case_when(
    date_adt %in% as.Date(c("2026-05-01", "2026-05-04", "2026-05-12")) ~ FALSE, # Entirely off-effort dates
    between(datetime_adt, start.effort_adt, end.effort_adt) ~ TRUE, # Daily effort window
    TRUE ~ FALSE)) |>
  # Define 'available' effort window, which corresponds a fixed time window each date_adt that was "available" to conduct effort
  mutate(start.avail.effort_adt = as.POSIXct(paste(date_adt, "06:30:00"), tz = "America/Halifax"), 
         end.avail.effort_adt = as.POSIXct(paste(date_adt, "19:00:00"), tz = "America/Halifax"),
         avail_effort_window = datetime_adt >= start.avail.effort_adt & datetime_adt <= end.avail.effort_adt) |>
  relocate(c(real_effort_window, avail_effort_window), .after = end.effort_adt) |>
  # Replace remaining missing effort values and in_effort_window values
  mutate(Effort = replace_na(Effort, "Off"),
         real_effort_window = replace_na(real_effort_window, FALSE),
         # Remove any Transect_IDs if in Transit-On effort
         Transect_ID = case_when(Effort == "Transit-On" ~ NA,
                                 TRUE ~ Transect_ID)) |>
  ungroup() |>
  # Set certain columns to NA if Effort = Off
  mutate(across(c(Transect_ID, Port_obs, Stb_obs, BigEye_obs, Data_recorder,
        Sea_state, Swell, Port_vis, Stb_vis, Precipitation, Glare_int,
        Glare_left, Glare_right), ~ replace(.x, Effort == "Off", NA)))


# Flag suspicious GPS points & replace with closest datetime's lat/lon IF that is appropriate; adjust as needed

# Convert to data.table
gps_dt <- as.data.table(gps_effort)

# Create a column of "suspicious_gps" positions if outside of the study area bounding box
gps_dt[, suspicious_gps :=
         vessel_lon < xmin |
         vessel_lon > xmax |
         vessel_lat < ymin |
         vessel_lat > ymax |
         is.na(vessel_lat) |
         is.na(vessel_lon)]

# create “good reference table"; keep ONLY valid GPS fixes:
good_dt <- gps_dt[
  suspicious_gps == FALSE,
  .(datetime_adt, vessel_lat, vessel_lon)]

# Rolling join (nearest time match); match each suspicious point to nearest valid timestamp
setkey(good_dt, datetime_adt)
setkey(gps_dt, datetime_adt)

gps_dt[suspicious_gps == TRUE,
         c("vessel_lat", "vessel_lon") :=
           good_dt[.SD, on = "datetime_adt", roll = "nearest",
                   .(vessel_lat, vessel_lon)] ]

# Convert back to tibble & remove 'suspicious_GPS' column
gps_effort1 <- as_tibble(gps_dt) |>
  select(-(suspicious_gps))


# Checks
summary(gps_effort1$vessel_lat)
summary(gps_effort1$vessel_lon)
unique(gps_effort1$date_adt)
unique(sort(gps_effort1$Transect_ID))
table(gps_effort1$date_adt, gps_effort1$Strata)

# Export
write_csv(gps_effort1, file = paste0(here(),"/Data_summaries/tables/spring2026_gps_track_effortenv_FINAL.csv"))

# Export different versions for easier QGIS mapping

# Available observation window (12.5h per day) only
g1 <- gps_effort1 |>
  filter(avail_effort_window == TRUE) 

write_csv(g1, file = paste0(here(),"/Data_summaries/tables/spring2026_gps_track_effortenv_FINAL_avail_effort_window_only.csv"))

# Realized effort window only
g2 <- gps_effort1 |>
  filter(real_effort_window == TRUE) 

write_csv(g2, file = paste0(here(),"/Data_summaries/tables/spring2026_gps_track_effortenv_FINAL_real_effort_window_only.csv"))

# Available observation window (12.5h per day) within FMB only
g3 <- gps_effort1 |>
  filter(avail_effort_window == TRUE) |>
  filter(Strata == "FMB")

write_csv(g3, file = paste0(here(),"/Data_summaries/tables/spring2026_gps_track_effortenv_FINAL_FMB_avail_effort_window_only.csv"))

# Available observation window (12.5h per day) within SB only
g4 <- gps_effort1 |>
  filter(avail_effort_window == TRUE) |>
  filter(Strata == "SB")

write_csv(g4, file = paste0(here(),"/Data_summaries/tables/spring2026_gps_track_effortenv_FINAL_SB_avail_effort_window_only.csv"))



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
         Vessel = "Teleost",
         # Remove the parentheses, everything inside them & any extra space before the parentheses
         Obs_platform = trimws(gsub("\\s*\\(.*\\)", "", Obs_platform)),
         # Adjust Distance_tool entries
         Distance_tool = case_when(Distance_tool %in% c("Fujinon25x150", "BigEyes") ~ "BigEyes_Fujinon25x150",
                                   TRUE ~ Distance_tool),
         # Create "strata" column based on dates within each strata (or not in strata)
         Strata = case_when(date_adt %in% as.Date(c("2026-04-23", "2026-05-01", "2026-05-04")) ~ "Not.in.strata",
                            date_adt >= "2026-05-06" & date_adt <= "2026-05-10" ~ "SB",
                            TRUE ~ "FMB")) |>
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
         # Check whether the rows without Yes/No in Photos actually had photos; if not, assign NA to "No"
         Photos_taken = replace_na(Photos_taken, "No")) |>
  relocate(c(datetime_missing, datetime_duplicates)) |>
  # Select to arrange column order
  select(Vessel, Strata, "Datetime_UTC_locked (UTC)", "Datetime_UTC (UTC)", datetime_utc, datetime_adt,
         date_adt, vessel_lat, vessel_lon, Sgt_lat, Sgt_lon,
         Sgt_type, Sgt_side, Observer, Obs_platform, Distance_tool, 
         Reticles, Distance_m, Angle_rel, Angle_num, Angle_abs, Distance_tool, PSD_m,
         Species, Min_count, Best_count, Max_count, Resight, Photos_taken, 
         Camera, Frame_first, Frame_last, Notes, QAQC_notes,
         datetime_missing, datetime_duplicates)


#...............................................................................
# QA/QC
#...............................................................................

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