#########################################
# Marine Mammal Observer -- Scheduling
########################################

# This script does the following:
# Computes sunrise/sunset from date + lat/lon
# Builds daily time blocks (30 min) within daylight constraints
# Generates a rotating observer schedule with fair offsets
# Exports a printable PDF table (one page per day)

# Conditions:

# Observers cycle through:
# Stations → Off-duty → back to Port
# Daily offset rotation ensures:
  # Different starting stations each day
  # Different observer pairings

# You can easily change:
  
# Number of observers
# Station configuration
# Survey dates
# Lat/lon
# Time buffers (e.g., change +1 hr to +45 min)

# Uses suncalc to compute:

# Sunrise / sunset
# Then trims:
#  +1 hr after sunrise
#  −1 hr before sunset
# Rounded to nearest 30 min


# ============================
# Libraries
# ============================
library(tidyverse)
library(lubridate)
library(suncalc)
library(openxlsx)
#library(here)

# ============================
# USER INPUTS
# ============================

start_date <- as.Date("2026-04-28")
end_date   <- as.Date("2026-04-29")

# Daily schedule start time (ADT)
start_time_str <- "07:00"
end_time_str   <- "20:00"

# Max operational hours per day
max_hours <- 13  

# Approximate mid-point of study area
lat <- 45.066179959259934
lon <- -60.17219035617895

# Set the different time zones
tz_sun <- "America/Halifax"     # Study area time zone (for suncalc times)
tz_nl  <- "America/St_Johns"    # Ship time zone
tz_utc <- "UTC"                 # Data collection time zone

# Set the output directory
out_dir <- "C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVeS/MMO_Protocols/MMO_scheduling"
dir.create(out_dir, showWarnings = FALSE)

# Observers
observers <- c("Nat", "Amanda", "Mike", "Katy", "Pam", "Hilary") 

# Stations
n_stations <- 4 # 3, 4, 5

station_list <- list(
  `3` = c("Port", "Starboard", "Data"),
  `4` = c("BigEye","Port", "Starboard", "Data"),
  `5` = c("BigEye","Port", "Starboard", "Data", "IO"))

stations <- station_list[[as.character(n_stations)]]

# Colors (Excel)
station_colors <- c(
  "BigEye"    = "#D8BFD8",
  "Port"      = "#ADD8E6",
  "Starboard" = "#90EE90",
  "Data"      = "#FFD580",
  "IO"        = "#D3D3D3")

# ============================
# FUNCTIONS
# ============================

round_time <- function(x) round_date(x, "20 minutes") # 30 minutes

get_day_window <- function(date) {
  
  start_nl <- ymd_hm(paste(date, start_time_str), tz = tz_nl)
  end_nl   <- ymd_hm(paste(date, end_time_str),   tz = tz_nl)
  
  tibble(
    start = start_nl,
    end   = end_nl
  )
}

generate_times <- function(start, end) {
  seq(start, end, by = "20 min") # 30 min
}

# Constrained daily shuffle (so that the same person doesn't always start on Port)
# Maintain a fixed order of who-start-on-port, but randomly shuffle observers at other stations
generate_rotation <- function(observers, day_index) {
  
  n <- length(observers)
  
  # Deterministic Port rotation
  port_index <- ((day_index - 1) %% n) + 1
  first <- observers[port_index]
  
  # Shuffle remaining observers
  set.seed(1000 + day_index)
  remaining <- setdiff(observers, first)
  rest <- sample(remaining)
  
  c(first, rest)
}


assign_schedule <- function(times, observers, stations) {
  
  n_obs <- length(observers)
  n_stations <- length(stations)
  
  # Initialize queue (fixed daily order)
  queue <- observers
  
  map_dfr(seq_along(times), function(i) {
    
    # Assign stations
    on_duty <- queue[1:n_stations]
    
    # Off-duty = everyone else
    off_duty <- queue[(n_stations + 1):n_obs]
    
    # ---- Time handling (NL-based) ----
    time_nl  <- times[i]
    time_utc <- with_tz(time_nl, tz_utc)
    
    # ---- Split off-duty BEFORE tibble ----
    off_duty_1 <- ifelse(length(off_duty) >= 1, off_duty[1], NA)
    off_duty_2 <- ifelse(length(off_duty) >= 2, off_duty[2], NA)
    
    # ---- Build row ----
    row <- tibble(
      Time_NL = format(time_nl, "%H:%M"),
      !!!set_names(as.list(on_duty), stations),
      `Off-duty 1` = off_duty_1,
      `Off-duty 2` = off_duty_2,
      Time_UTC = format(time_utc, "%H:%M")
    )
    
    # Rotate queue (last person → front)
    queue <<- c(tail(queue, 1), head(queue, -1))
    
    row
  })
}

# ============================
# BUILD SCHEDULE
# ============================

dates <- seq(start_date, end_date, by = "day")

schedule_list <- list()

for (i in seq_along(dates)) {
  
  window <- get_day_window(dates[i])
  times <- generate_times(window$start, window$end)
  
  obs_order <- generate_rotation(observers, i)
  
  sched <- assign_schedule(times, obs_order, stations)
  
  schedule_list[[i]] <- list(
    date = dates[i],
    data = sched,
    sun  = window
  )
}


# ============================
# EXCEL: SCHEDULE
# ============================

wb <- createWorkbook()

# Base table style
base_style <- createStyle(
  fontSize = 16,
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

# Header (date) style
date_style <- createStyle(
  fontSize = 20,
  textDecoration = "bold",
  halign = "center"
)

# Column header style
header_style <- createStyle(
  fontSize = 16,
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight"
)

# Meal highlight style (Time_NL only)
meal_style <- createStyle(
  fgFill = "#D9D9D9"
)

for (x in schedule_list) {
  
  sheet <- as.character(x$date)
  addWorksheet(wb, sheet)
  
  # ----------------------------
  # 1. Add DATE HEADER (row 1)
  # ----------------------------
  
  date_label <- paste0("Date: ", format(x$date, "%B %d, %Y"))
  
  writeData(wb, sheet, date_label, startRow = 1, startCol = 1)
  
  # Merge across all columns
  mergeCells(
    wb,
    sheet,
    rows = 1,
    cols = 1:ncol(x$data)
  )
  
  addStyle(
    wb,
    sheet,
    date_style,
    rows = 1,
    cols = 1,
    gridExpand = TRUE
  )
  
  # ----------------------------
  # 2. Write TABLE (starts row 2)
  # ----------------------------
  
  writeData(wb, sheet, x$data, startRow = 2)
  
  # Bold column headers
  addStyle(
    wb,
    sheet,
    header_style,
    rows = 2,
    cols = 1:ncol(x$data),
    gridExpand = TRUE
  )
  
  # Apply base style to all data cells
  addStyle(
    wb,
    sheet,
    base_style,
    rows = 2:(nrow(x$data) + 2),
    cols = 1:ncol(x$data),
    gridExpand = TRUE
  )
  
  # Freeze pane below header row
  freezePane(wb, sheet, firstRow = TRUE)
  
  # ----------------------------
  # 3. Station colour coding
  # ----------------------------
  
  for (col in stations) {
    
    col_index <- which(names(x$data) == col)
    
    style <- createStyle(
      fgFill = station_colors[col],
      halign = "center"
    )
    
    addStyle(
      wb,
      sheet,
      style,
      rows = 3:(nrow(x$data) + 2),
      cols = col_index,
      gridExpand = TRUE
    )
  }
  
  # ----------------------------
  # 4. Meal time highlighting (Time_NL ONLY)
  # ----------------------------
  
  meal_times <- c("07:00", "11:00", "11:30", "17:00", "17:30")
  
  time_col <- which(names(x$data) == "Time_NL")
  
  meal_rows <- which(x$data$Time_NL %in% meal_times) + 2  # +2 offset
  
  if (length(meal_rows) > 0) {
    addStyle(
      wb,
      sheet,
      meal_style,
      rows = meal_rows,
      cols = time_col,
      gridExpand = TRUE
    )
  }
}

saveWorkbook(
  wb,
  file.path(out_dir, "WEAWAVeS_Spring2026_MMO_schedule_6obs_4stations.xlsx"),
  overwrite = TRUE)











# ============================
# EXCEL: SUNRISE / SUNSET TABLE
# ============================

sun_table <- map_dfr(schedule_list, function(x) {
  
  sunrise_adt <- x$sun$sunrise
  sunset_adt  <- x$sun$sunset
  
  sunrise_ndt <- with_tz(sunrise_adt, tz_nl)
  sunset_ndt  <- with_tz(sunset_adt, tz_nl)
  
  tibble(
    Date = x$date,
    Sunrise_ADT = format(sunrise_adt, "%H:%M"),
    Sunrise_NDT = format(sunrise_ndt, "%H:%M"),
    Sunset_ADT  = format(sunset_adt, "%H:%M"),
    Sunset_NDT  = format(sunset_ndt, "%H:%M")
  )
})

wb2 <- createWorkbook()
addWorksheet(wb2, "sun_times")
writeData(wb2, "sun_times", sun_table)

saveWorkbook(
  wb2,
  file.path(out_dir, "Sunrise_sunset_times_spring2026.xlsx"),
  overwrite = TRUE)

