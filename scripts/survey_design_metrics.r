#############################################
# WEAWAVeS Survey Design - additional metrics 
#############################################

# NOTE: Dave Fifield (ECCC) created the survey design

# Load packages
library(dplyr)
library(geosphere)
library(stringr)

# Load survey transects
survey <- read.csv("C:/Users/gavrilchukk/Desktop/WEAWAVeS_repo/Survey_design/FOR TELEOST_Scenario 2_13-day_Spring 2026_transects.csv")

str(survey)

# Speed (km/h)
speed_kmh <- 18.5

survey1 <- survey |>
  
  # 1. Extract numeric portion of Transect_ID
  mutate(
    Transect_ID_num = as.numeric(str_extract(Transect_ID, "\\d+")),
    
    Direction_num = case_when(Direction == "West-to-East" ~ 1,
                              Direction == "East-to-West" ~ 2,
                              Direction == "South-to-North" ~ 3,
                              Direction == "North-to-South" ~ 4)) |>
  
  # 2. Ensure proper ordering within each group
  arrange(Strata, Direction_num, Transect_ID_num) |>
  
  group_by(Strata, Direction) |>
  
  mutate(
    # Transect distance (km)
    Transect_km = distHaversine(
      cbind(start_lon, start_lat),
      cbind(end_lon, end_lat)) / 1000,
    
    # Time per transect (hours)
    Transect_time_h = Transect_km / speed_kmh,
    
    # Inter-transect distance (km)
    # Distance from END of current transect to START of next
    Inter_transect_km = distHaversine(
      cbind(end_lon, end_lat),
      cbind(lead(start_lon), lead(start_lat))) / 1000,
    
    # Total km per "complement" (Strata + Direction)
    Total_compl_km = sum(Transect_km, na.rm = TRUE),
    
    # Total survey distance including transit
    Total_with_transit_km = sum(Transect_km, na.rm = TRUE) +
      sum(Inter_transect_km, na.rm = TRUE)) |>
  
  # 3. Round values for reporting
  mutate(
    across(ends_with("km"), ~ round(.x, 1)),
    Transect_time_h = round(Transect_time_h, 1)) |>
  
  ungroup()

# # Check
# survey1 |>
#   group_by(Strata, Direction) |>
#   summarise(
#     n = n(),
#     total_km = unique(Total_compl_km),
#     .groups = "drop")

# Export 
write.csv(survey1,"Scenario 2_13-day_Spring 2026_transects_with_metrics.csv",
  row.names = FALSE)

