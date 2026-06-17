#############################################
# WEAWAVeS Survey Design - additional metrics 
#############################################

# NOTE: Dave Fifield (ECCC) created the survey design

# Load packages
library(dplyr)
library(geosphere)
library(stringr)

# Load survey transects
survey <- read.csv("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Survey design/Scenario 2_13-day_Spring 2026_transects_coords_corrected.csv")

str(survey)

# Speed (km/h)
speed_kmh <- 18.5

# Generate additional metrics
survey1 <- survey |>
  # Extract numeric portion of Transect_ID
  rename(Transect_ID = label,
         start_lon = x1,			
         start_lat = y1,
         end_lon = x2,
         end_lat = y2,
         Strata = strata) |>
  mutate(Transect_ID_num = as.numeric(str_extract(Transect_ID, "\\d+")),
         Direction = case_when(Strata =="French/Middle Bank" & complement == "Complement 2" ~  "West-to-East",
                               Strata =="French/Middle Bank" & complement == "Complement 1" ~  "East-to-West",
                               Strata =="Sydney Bight" & complement == "Complement 2" ~  "South-to-North",
                               Strata =="Sydney Bight" & complement == "Complement 1" ~  "North-to-South"),  
         Direction_num = case_when(Direction == "West-to-East" ~ 1,
                                   Direction == "East-to-West" ~ 2,
                                   Direction == "South-to-North" ~ 3,
                                   Direction == "North-to-South" ~ 4)) |>
  # Ensure proper ordering within each group
  arrange(Strata, Direction_num, Transect_ID_num) |>
  group_by(Strata, Direction_num) |>
  # Transect distance (km)
  mutate(Transect_km = distHaversine(
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
  # Round values for reporting
  mutate(across(ends_with("km"), ~ round(.x, 1)),
         Transect_time_h = round(Transect_time_h, 1)) |>
  ungroup() |>
  select(-c(Direction_num, transect))

# Export 
write.csv(survey1,"C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Survey design/Scenario2_13-day_Summer2026_transects_with_metrics.csv",
  row.names = FALSE)

str(survey1)







#Prepare ordered path
#This converts your data into a single sequential route.

library(dplyr)
library(lubridate)

speed_kmh <- 18.5
day_start <- 7
day_end <- 19

start_time <- ymd_hms("2026-06-30 07:00:00")

survey_path <- survey1 %>%
  arrange(Strata, Transect_ID)


#STEP 2 — Build movement segments
# turn each transect into:
# survey segment
#inter-transect segment

build_segments <- function(df) {
  
  segments <- list()
  
  for (i in 1:nrow(df)) {
    
    segments[[length(segments) + 1]] <- list(
      type = "survey",
      id = df$Transect_ID[i],
      km = df$Transect_km[i]
    )
    
    if (i < nrow(df)) {
      segments[[length(segments) + 1]] <- list(
        type = "transit",
        id = paste0(df$Transect_ID[i], "->", df$Transect_ID[i+1]),
        km = df$Inter_transect_km[i]
      )
    }
  }
  
  segments
}

# STEP 3 — Daylight-aware time engine
advance_daylight <- function(time, hours) {
  
  while (hours > 0) {
    
    h <- hour(time)
    
    # before day starts → jump to 07:00
    if (h < day_start) {
      time <- update(time, hour = day_start, minute = 0, second = 0)
      next
    }
    
    # after day ends → jump to next day 07:00
    if (h >= day_end) {
      time <- time + days(1)
      time <- update(time, hour = day_start, minute = 0, second = 0)
      next
    }
    
    remaining <- day_end - h
    step <- min(hours, remaining)
    
    time <- time + hours(step)
    hours <- hours - step
  }
  
  time
}


# STEP 4 — Run full simulation

#This is the key object that generates everything.

run_simulation <- function(segments, start_time) {
  
  time <- start_time
  
  results <- data.frame(
    segment = character(),
    type = character(),
    start = as.POSIXct(character()),
    midpoint = as.POSIXct(character()),
    end = as.POSIXct(character())
  )
  
  for (seg in segments) {
    
    duration_h <- seg$km / speed_kmh
    duration_time <- dhours(duration_h)
    
    start_seg <- time
    
    # overnight exception ONLY for FMB41->SB01
    if (grepl("FMB41->SB01", seg$id)) {
      
      time <- time + duration_time
      
    } else {
      
      time <- advance_daylight(time, duration_h)
    }
    
    end_seg <- time
    mid_seg <- start_seg + (end_seg - start_seg) / 2
    
    results <- rbind(results, data.frame(
      segment = seg$id,
      type = seg$type,
      start = start_seg,
      midpoint = mid_seg,
      end = end_seg
    ))
  }
  
  results
}

#STEP 5 — Build per-transect table
extract_transect_table <- function(sim) {
  
  sim %>%
    filter(type == "survey") %>%
    mutate(Transect_ID = segment) %>%
    select(Transect_ID, start, midpoint, end)
}

#STEP 6 — Build daily summary table
# This is the part that fixes your earlier inconsistency.

make_daily_summary <- function(sim) {
  
  sim %>%
    mutate(date = as.Date(start)) %>%
    group_by(date) %>%
    summarise(
      transects = paste(unique(segment[type == "survey"]), collapse = ", "),
      .groups = "drop"
    )
}

# STEP 7 — Run everything
segments <- build_segments(survey_path)

sim <- run_simulation(segments, start_time)

transect_table <- extract_transect_table(sim)

daily_table <- make_daily_summary(sim)
      