################################################################################
#
# Script purpose: Generate summary metrics for survey transects
# Program: DFO Maritimes - Cetacean Research and Monitoring Program (CRMP)
# Project: Wind Energy Area Wildlife Assessment Vessel Surveys (WEAWAVS)
# Author: Katherine Gavrilchuk
# Affiliation: Fisheries and Oceans Canada
# Contact: katherine.gavrilchuk@dfo-mpo.gc.ca
# Last updated: August 5, 2026
# R version: 4.6.1
# NOTE: Dave Fifield (ECCC) created the survey design
#
################################################################################

#----SET WORKING DIRECTORY----

setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Survey Design/")


#----LOAD PACKAGES----

library(dplyr)
library(geosphere)
library(stringr)
library(sf)
library(leaflet)


#----LOAD DATA----

# Planned Transects 
transects_df <- read.csv(paste0(getwd(), "/WEAWAVS_2026-04-10_DFifield/Scenario 2 Spring 2026_transects.csv"))
transects_shp <- st_read(file.path(paste0(getwd(), "/WEAWAVS_2026-04-10_DFifield/Planned_transects_Scenario2_Spring2026.shp")),
                         quiet = TRUE)

# Inspect data
str(transects_df)
str(transects_shp)
head(transects_df)


#----INSPECT DATA----

# Produce a map of each strata to check start (x1,y1) and end (x2,y2) coordinates 

# Define color mapping for complements
complement_colors <- colorFactor(
  palette = c("Complement 1" = "grey", "Complement 2" = "black"),
  domain = transects_shp$complement)

leaflet() |>
  addProviderTiles(providers$CartoDB.Positron) %>% # providers$Esri.WorldImagery 
  addPolylines(
    data = transects_shp,
    color = ~complement_colors(complement),
    weight = 2,
    label = ~label,
    popup = ~paste("Transect:", transect, "<br>Strata:", strata, "<br>Complement:", complement)) |> 
  addCircleMarkers(
    data = transects_df1, # transects_df  transects_df1
    lng = ~x1, lat = ~y1,
    color = "green",
    radius = 5,
    stroke = FALSE, fillOpacity = 0.9,
    popup = ~paste("Start -", label)) |>
  addCircleMarkers(
    data = transects_df1,
    lng = ~x2, lat = ~y2,
    color = "red",
    radius = 5,
    stroke = FALSE, fillOpacity = 0.9,
    popup = ~paste("End -", label)) |>
  addLegend(
    position = "bottomright",
    colors = c("grey", "black", "green", "red"),
    labels = c("Complement 1", "Complement 2", "Start", "End"),
    title = "Legend")

# After inspection of the start/end coords of each transect, there are some errors;
# Corrections to be made:
# 1) French/Middle Bank - Complement 2: The x1 and y1 coordinates and x2 and y2 coordinates need to be swapped.
# 2) Sydney Bight - Complement 1: The x1 and y1 coordinates and x2 and y2 coordinates need to be swapped.

transects_df1 <- transects_df |>
  mutate(swap_flag = (strata == "French/Middle Bank" & complement == "Complement 2") |
           (strata == "Sydney Bight" & complement == "Complement 1"),
         # Temporarily hold original values
         x1_new = if_else(swap_flag, x2, x1),
         y1_new = if_else(swap_flag, y2, y1),
         x2_new = if_else(swap_flag, x1, x2),
         y2_new = if_else(swap_flag, y1, y2)) |>
  mutate(x1 = x1_new,
         y1 = y1_new,
         x2 = x2_new,
         y2 = y2_new) |>
  select(-swap_flag, -x1_new, -y1_new, -x2_new, -y2_new)

# Export corrected transects_df
write.csv(transects_df1, paste0(getwd(), "/WEAWAVS_2026-04-10_DFifield/Scenario 2 Spring 2026_transects_coords_corrected.csv"))


#----CALCULATE SURVEY METRICS----

# Speed (km/h) or 10 kt
speed_kmh <- 18.5

# Generate additional metrics
transects_df2 <- transects_df1 |>
  # Extract numeric portion of Transect_ID
  rename(Transect_ID = label,
         start_lon = x1,			
         start_lat = y1,
         end_lon = x2,
         end_lat = y2,
         Strata = strata) |>
  mutate(Transect_ID_num = as.numeric(str_extract(Transect_ID, "\\d+")),
         Direction = case_when(Strata == "French/Middle Bank" & complement == "Complement 2" ~ "West-to-East",
                               Strata == "French/Middle Bank" & complement == "Complement 1" ~ "East-to-West",
                               Strata == "Sydney Bight" & complement == "Complement 2" ~ "South-to-North",
                               Strata == "Sydney Bight" & complement == "Complement 1" ~ "North-to-South"),
         Direction_num = case_when(Direction == "West-to-East" ~ 1,
                                   Direction == "East-to-West" ~ 2,
                                   Direction == "South-to-North" ~ 3,
                                   Direction == "North-to-South" ~ 4)) |>
  arrange(Strata, Direction_num, Transect_ID_num) |>
  group_by(Strata, Direction_num) |>
  mutate(
    Transect_km = distHaversine(cbind(start_lon, start_lat), cbind(end_lon, end_lat)) / 1000,
    Transect_time_h = Transect_km / speed_kmh,
    Inter_transect_km = distHaversine(cbind(end_lon, end_lat), cbind(lead(start_lon), lead(start_lat))) / 1000,
    
    # Cumulative distance (km) per transect + transit to next transect
    Cum_dist_km = cumsum(Transect_km) + cumsum(coalesce(Inter_transect_km, 0)),
    # Cumulative time (h) per transect + transit to next transect
    Cum_time_h = cumsum(Transect_time_h) + cumsum(coalesce(Inter_transect_km, 0) / speed_kmh),
    
    Total_compl_km = sum(Transect_km, na.rm = TRUE),
    Total_with_transit_km = sum(Transect_km, na.rm = TRUE) +
      sum(Inter_transect_km, na.rm = TRUE)) |>
  mutate(across(ends_with("km"), ~ round(.x, 1)),
         across(ends_with("_h"), ~ round(.x, 1))) |>
  ungroup() |>
  select(-c(Direction_num, transect))

# Export 
write.csv(transects_df2, paste0(getwd(), "/Scenario2_13-day_transects_with_metrics.csv"),  row.names = FALSE)

      