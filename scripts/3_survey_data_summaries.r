################################################################################
#
# Script purpose: Summarize vessel survey effort and marine mammal sightings data
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

library(sf)
library(geosphere)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(ggplot2)
library(marmap)
library(ggspatial)
library(ggnewscale)
library(scales)
library(tidyverse) 
library(ggspatial)
library(ggnewscale)


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


#----LOAD COMBINED SIGHTINGS & GPS-EFFORT----

sight_raw <- read_csv(paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_mm_sightings.csv"),
                      col_types = cols(.default = col_character()))

gpseff_raw <- read_csv(paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_gps-effort.csv"),
                       col_types = cols(.default = col_character()))

# Datetimes: write.csv() stores times as plain text without a time zone, so
# datetime_utc is read as UTC and datetime_adt / date_adt are rebuilt from
# it (rather than trusting the ADT text, which R would otherwise read as UTC).
gpseff <- gpseff_raw |>
  mutate(
    datetime_utc = ymd_hms(datetime_utc, tz = "UTC", truncated = 3),
    datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
    date_adt     = as_date(datetime_adt),
    across(any_of(c("start.effort_adt", "end.effort_adt")),
           ~ ymd_hms(.x, tz = "America/Halifax", truncated = 3)),
    across(any_of(c("vessel_lat", "vessel_lon", "sog_kt", "cog_t")), as.numeric)) |>
  # Time order; an effort entry goes before a GPS fix in the same second
  arrange(datetime_utc, row_type != "effort") |>
  mutate(row_id = row_number())   # stable key for joins below

sight <- sight_raw |>
  mutate(
    datetime_utc = ymd_hms(datetime_utc, tz = "UTC", truncated = 3),
    datetime_adt = with_tz(datetime_utc, tzone = "America/Halifax"),
    date_adt     = as_date(datetime_adt),
    # Calf_count stays text ("Unknown" is a valid entry)
    across(any_of(c("vessel_lat", "vessel_lon", "Sgt_lat", "Sgt_lon", "map_lat", "map_lon",
                    "Reticles", "Distance_m", "Angle_num", "Angle_abs", "PSD_m",
                    "Min_count", "Best_count", "Max_count")), as.numeric))

# Checks: no unparsed times or positions (all should be 0)
sum(is.na(gpseff$datetime_utc)); sum(is.na(gpseff$vessel_lat))
sum(is.na(sight$datetime_utc))
table(gpseff$row_type, gpseff$gps_source)
table(gpseff$Effort, useNA = "ifany")

# Resights are kept in the exported file; exclude them from all sighting
# summaries to avoid double-counting
sight_nores <- sight |>
  filter(!Resight %in% c("Yes", "Possible"))

nrow(sight); nrow(sight_nores)


#----LOAD TRANSECTS AND OTHER SHAPEFILES----

# Planned transects (shapefile)
transects_shp <- st_read(file.path(paste0(getwd(), "/Survey design/WEAWAVS_2026-04-10_DFifield/Planned_transects_Scenario2_Spring2026.shp")), quiet = TRUE)

# Planned transects with metrics (csv)
transects_df <- read.csv(paste0(getwd(), "/Survey design/Scenario2_13-day_transects_with_metrics.csv")) |>
  as_tibble()

# Study area polygons (shapefile)
studyarea_shp <- st_read(file.path(paste0(getwd(), "/Survey design/WEAWAVS_2026-04-10_DFifield/Study_area_buf_prelim.shp")), quiet = TRUE)

# Coastal shapefile
coastal_shp <- st_read(file.path("C:/Users/GavrilchukK/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/Mapping/Shapefiles/coastline/canada/gpr_000b11a_e_hires/gpr_000b11a_e.shp"), quiet = TRUE)

# Designated wind energy areas 
wea_shp <- st_read("C:/Users/GavrilchukK/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/Mapping/Shapefiles/Designated-WEA/Designated_WEAs_25_07_29.shp")


#----DEFINE STUDY AREA----

# V2 (adapted after the Summer2026 survey since they had a distant SAR call)
study.bbox <- c(-65.742188, 41.508577, -52.163086, 47.279229)

xmin <- study.bbox[1]
ymin <- study.bbox[2]
xmax <- study.bbox[3]
ymax <- study.bbox[4]


#----SUMMARY TABLES----

# ALL EFFORT SUMMARIES USE DAYTIME SURVEY HOURS ONLY: for each day, from the
# first to the last entry in the effort datasheet. Night-time track data
# (and any day with no effort entries) is not summarized.
#
# Tables
#   Table 1a  Distance and hours in each effort state, by date
#   Table 1b  Daily On/Bridge-On effort and transects visited
#   Table 1c  Distance and hours in each effort state, by strata
#   Table 2a  Planned-line coverage (On/Bridge-On), by date and transect
#   Table 2b  Planned-line coverage (On/Bridge-On), by strata and overall
#   Table 2c  Transect completion, On/Bridge-On vs any effort state
#   Table 3a  Sightings by date and species (resights excluded)
#   Table 3b  Sightings by species, with totals (resights excluded)
#   Table 4a  Hours in each sea state (BFT)
#   Table 4b  Hours in each visibility category (worst of port/starboard)
#   (+)       Survey time lost to bad weather
# Suggested additional tables (keep or drop):
#   Table 5   Encounter rates (on-effort sightings per 100 km), by species and strata
#   Table 6   Group size by species
#   Table 7   Perpendicular sighting distances by species (on-effort)
#   Table 8   On-effort distance and hours by sea state


#----PREPARE DATA: DAYTIME SURVEY WINDOW----

# Daily survey window = first to last entry in the effort datasheet that day.
# Only GPS rows inside this window are summarized; night-time track data,
# and days with no effort entries, are left out of every effort table.
survey_window <- gpseff |>
  filter(row_type == "effort") |>
  group_by(date_adt) |>
  summarize(win_start = min(datetime_utc),
            win_end   = max(datetime_utc),
            .groups = "drop") |>
  mutate(window_h = round(as.numeric(difftime(win_end, win_start, units = "hours")), 2))

survey_window |>
  mutate(across(c(win_start, win_end), ~ with_tz(.x, "America/Halifax"))) |>
  print(n = Inf)

gpseff_day <- gpseff |>
  inner_join(survey_window, by = "date_adt") |>
  filter(datetime_utc >= win_start, datetime_utc <= win_end) |>
  select(-c(win_start, win_end, window_h))

# Rows kept vs. excluded (night-time / no effort entries)
nrow(gpseff); nrow(gpseff_day)


#----PREPARE DATA: TRACK SEGMENTS (daytime only)----

# One row per segment between consecutive rows (GPS fixes and effort
# entries, in time order), within each day's survey window. Each segment
# takes the effort state, conditions, strata and Transect_ID in force at its
# START. The last row of a window (the last effort entry) ends the day, so
# no segment extends into the night.
# Notes:
#  - GPS spikes were removed during error-checking, so no speed filter is
#    needed here.
#  - Effort rows sit exactly on the track (track position at the event
#    time), so they add no extra distance.
gps_seg <- gpseff_day |>
  mutate(Effort = coalesce(Effort, "Off")) |>   # safety only; none expected
  group_by(date_adt) |>
  mutate(next_lon   = lead(vessel_lon),
         next_lat   = lead(vessel_lat),
         next_time  = lead(datetime_adt),
         dist_km    = distHaversine(cbind(vessel_lon, vessel_lat), cbind(next_lon, next_lat)) / 1000,
         duration_h = as.numeric(difftime(next_time, datetime_adt, units = "hours")),
         # Effort blocks: a new block at every change of effort state.
         # (consecutive_id() replaces cumsum(Effort != lag(Effort)), which
         # returned NA for the rest of a day if a comparison was NA)
         effort_block = consecutive_id(Effort)) |>
  ungroup() |>
  filter(!is.na(dist_km))   # last row of each day has no next point

# Effort blocks: start/end, hours and distance of each continuous block
effort_blocks <- gps_seg |>
  group_by(date_adt, effort_block, Effort) |>
  summarize(start_time_adt = min(datetime_adt),
            end_time_adt   = max(next_time),
            duration_h     = sum(duration_h),
            distance_km    = sum(dist_km),
            .groups = "drop")


#----PREPARE DATA: POSITION ALONG THE PLANNED TRANSECTS----

# For each On/Bridge-On GPS row with a Transect_ID:
#  - dist_to_transect_m: distance (m) from the vessel to ITS planned line
#  - frac_along: where the vessel is along that line (0 = start, 1 = end)
# These are used for planned-line coverage (Table 2) instead of track
# distance, so wandering, zig-zagging or doubling back can't inflate it.

# One line per planned transect (WGS84). The ID is in the shapefile's
# "label" column.
transect_lines <- transects_shp |>
  st_transform(4326) |>
  select(Transect_ID = label) |>
  mutate(Transect_ID = as.character(Transect_ID)) |>
  group_by(Transect_ID) |>
  summarise(.groups = "drop") |>          # merge multi-part transects
  st_cast("MULTILINESTRING") |>
  st_line_merge() |>
  st_cast("LINESTRING")

# Planned length per transect (km)
planned <- transects_df |> select(Transect_ID, Planned_km = Transect_km)

# Checks: Transect_IDs logged but not in the design (should be empty)
setdiff(unique(na.omit(gpseff$Transect_ID)), transect_lines$Transect_ID)
setdiff(unique(na.omit(gpseff$Transect_ID)), planned$Transect_ID)

on_tr <- gpseff_day |>
  filter(Effort %in% c("On", "Bridge-On"), !is.na(Transect_ID)) |>
  select(row_id, Transect_ID, vessel_lon, vessel_lat) |>
  inner_join(as_tibble(transect_lines) |> rename(tr_geom = geometry), by = "Transect_ID")

on_pts <- on_tr |>
  select(-tr_geom) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326)

# Geodesic distance (m) from each fix to its own transect line
on_tr$dist_to_transect_m <- round(as.numeric(
  st_distance(on_pts, on_tr$tr_geom, by_element = TRUE)), 1)

# Position along the line, as a fraction of its length. Calculated in a
# projected CRS (Statistics Canada Lambert, EPSG:3347); being a fraction,
# it is scaled by the planned length from transects_df.
on_tr$frac_along <- st_line_project(
  st_transform(on_tr$tr_geom, 3347),
  st_transform(st_geometry(on_pts), 3347),
  normalized = TRUE)

# Added to the daytime rows (used for coverage). On/Bridge-On rows are
# always inside the survey window, so nothing is lost.
gpseff_day <- gpseff_day |>
  left_join(on_tr |> select(row_id, dist_to_transect_m, frac_along), by = "row_id")

# Distance from the planned line, per transect (large maximums = manoeuvring
# or a Transect_ID carried somewhere it shouldn't be)
gpseff_day |>
  filter(!is.na(dist_to_transect_m)) |>
  group_by(Transect_ID) |>
  summarize(n_fixes = n(), median_m = median(dist_to_transect_m),
            max_m = max(dist_to_transect_m), .groups = "drop") |>
  arrange(desc(max_m))

# Survey dates
# gpseff_day |>
#   filter(!is.na(Transect_ID)) |>
#   group_by(Survey) |>
#   summarize(start = min(date_adt), end = max(date_adt))
# Error: Column `Survey` is not found.


#----TABLE 1a: DISTANCE AND HOURS BY EFFORT STATE AND DATE----

# Daytime survey window only
table1 <- gps_seg |>
  group_by(date_adt, Effort) |>
  summarize(Distance_km = round(sum(dist_km), 1),
            Duration_h  = round(sum(duration_h), 1),
            .groups = "drop") |>
  rename(Date_adt = date_adt)

# Wide version (one row per date), with the survey window length
table1_wide <- table1 |>
  pivot_wider(names_from = Effort, values_from = c(Distance_km, Duration_h)) |>
  left_join(survey_window |> select(Date_adt = date_adt, Survey_window_h = window_h),
            by = "Date_adt")

# Check: hours per day add up to the survey window (differences = rounding)
table1 |>
  group_by(Date_adt) |>
  summarize(sum_h = sum(Duration_h)) |>
  left_join(survey_window |> select(Date_adt = date_adt, window_h), by = "Date_adt")


#----TABLE 1b: DAILY ON-EFFORT SUMMARY AND TRANSECTS VISITED----

table1b <- gps_seg |>
  filter(Effort %in% c("On", "Bridge-On")) |>
  group_by(Date_adt = date_adt) |>
  summarize(OnEffort_km  = round(sum(dist_km), 1),
            OnEffort_h   = round(sum(duration_h), 1),
            n_transects  = n_distinct(Transect_ID, na.rm = TRUE),
            Transects    = paste(sort(unique(na.omit(Transect_ID))), collapse = ", "),
            .groups = "drop")


#----TABLE 1c: DISTANCE AND HOURS BY EFFORT STATE AND STRATA----

# Strata = position-based (FMB, SB, NONE = outside both WEAs), daytime only
table1c <- gps_seg |>
  group_by(Strata = strata, Effort) |>
  summarize(Distance_km = round(sum(dist_km), 1),
            Duration_h  = round(sum(duration_h), 1),
            .groups = "drop") |>
  arrange(Strata, Effort)


#----TABLE 2: PLANNED-LINE COVERAGE (On / Bridge-On)----

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

# Stretch of planned line covered between consecutive rows that are both
# On/Bridge-On on the same transect (frac_along only exists for those rows,
# so pairs crossing into another effort state or transect drop out).
cover_int <- gpseff_day |>
  group_by(date_adt) |>
  mutate(next_frac = lead(frac_along),
         next_tr   = lead(Transect_ID)) |>
  ungroup() |>
  filter(!is.na(frac_along), !is.na(next_frac), Transect_ID == next_tr) |>
  transmute(date_adt, Transect_ID,
            s = pmin(frac_along, next_frac),
            e = pmax(frac_along, next_frac))

# Table 2a: coverage by date and transect (each day separately)
table2a <- cover_int |>
  group_by(date_adt, Transect_ID) |>
  summarize(Prop_completed = union_length(s, e), .groups = "drop") |>
  left_join(planned, by = "Transect_ID") |>
  mutate(Realized_km = Prop_completed * Planned_km) |>
  rename(Date_adt = date_adt) |>
  select(Date_adt, Transect_ID, Realized_km, Planned_km, Prop_completed)

# Coverage per transect across ALL dates (union, so a stretch surveyed on
# two days is counted once)
realized_by_transect <- cover_int |>
  group_by(Transect_ID) |>
  summarize(frac = union_length(s, e), .groups = "drop") |>
  left_join(planned, by = "Transect_ID") |>
  transmute(Transect_ID, Realized_km = frac * Planned_km)

# Table 2b: by strata, joined onto the FULL transect list so transects never
# surveyed still count their planned km
table2b_strata <- transects_df |>
  select(Strata, Transect_ID, Planned_km = Transect_km) |>
  left_join(realized_by_transect, by = "Transect_ID") |>
  mutate(Realized_km = replace_na(Realized_km, 0)) |>
  group_by(Strata) |>
  summarize(Realized_km = sum(Realized_km),
            Planned_km  = sum(Planned_km),
            .groups = "drop") |>
  mutate(Prop_completed = Realized_km / Planned_km)

table2b_total <- table2b_strata |>
  summarize(Realized_km = sum(Realized_km),
            Planned_km  = sum(Planned_km)) |>
  mutate(Prop_completed = Realized_km / Planned_km)

# Transect completion (On/Bridge-On)
# A transect counts as complete at >= 99% of its planned line covered
# (effort rarely starts/ends exactly on the transect end points).
completion_threshold <- 0.99
n_transects_planned  <- n_distinct(transects_df$Transect_ID)

transect_completion_on <- planned |>
  left_join(realized_by_transect, by = "Transect_ID") |>
  mutate(Realized_km    = replace_na(Realized_km, 0),
         Prop_completed = Realized_km / Planned_km)

n_transects_completed <- sum(transect_completion_on$Prop_completed >= completion_threshold)


#----TRANSECT COMPLETION IN ANY EFFORT STATE----


# Distance from the planned line during On/Bridge-On effort (m)
quantile(gpseff_day$dist_to_transect_m, c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE)
# Future checks: inspect the cases where we deviated > 500m off the line 
# but were still in 'On' or 'Bridge-On' effort.

# A GPS row counts toward EVERY planned transect within max_dist_any_m of it
# (rows near the zig-zag vertices can count for two lines), in any effort
# state, within each day's survey window. This value sets how close the vessel 
# must be to a planned transect line (in metres) for a GPS position to count as 
# covering that line, whatever the effort state.
max_dist_any_m <- 500

fix_any <- gpseff_day |>
  mutate(fix_id = row_number()) |>                  # time order = row order
  select(fix_id, date_adt, datetime_utc, vessel_lon, vessel_lat) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326)

# All (row, transect) pairs within the distance limit
near_list <- st_is_within_distance(fix_any, transect_lines, dist = max_dist_any_m)
pairs <- tibble(fix_id = rep(fix_any$fix_id, lengths(near_list)),
                tr_idx = unlist(near_list))

pairs$frac <- st_line_project(
  st_transform(st_geometry(transect_lines)[pairs$tr_idx], 3347),
  st_transform(st_geometry(fix_any)[pairs$fix_id], 3347),
  normalized = TRUE)

fix_info <- fix_any |> st_drop_geometry()

# Covered intervals: consecutive rows (in time, same day) both near the same
# transect. The On/Bridge-On intervals are added so any-state >= on-effort.
cover_any <- pairs |>
  mutate(Transect_ID = transect_lines$Transect_ID[tr_idx]) |>
  left_join(fix_info, by = "fix_id") |>
  arrange(Transect_ID, fix_id) |>
  group_by(Transect_ID) |>
  mutate(next_id   = lead(fix_id),
         next_frac = lead(frac),
         next_date = lead(date_adt)) |>
  ungroup() |>
  filter(next_id == fix_id + 1, next_date == date_adt) |>
  transmute(Transect_ID,
            s = pmin(frac, next_frac),
            e = pmax(frac, next_frac)) |>
  bind_rows(cover_int |> select(Transect_ID, s, e))

completion_any_state <- planned |>
  left_join(cover_any |>
              group_by(Transect_ID) |>
              summarize(Prop_completed = union_length(s, e), .groups = "drop"),
            by = "Transect_ID") |>
  mutate(Prop_completed = replace_na(Prop_completed, 0))

n_transects_completed_any <- sum(completion_any_state$Prop_completed >= completion_threshold)

# Compare (any-state should be >= On/Bridge-On)
n_transects_planned; n_transects_completed; n_transects_completed_any

# Table 2c: completion per transect, On/Bridge-On vs any effort state
table2c <- transects_df |>
  select(Strata, Transect_ID, Planned_km = Transect_km) |>
  left_join(transect_completion_on |> select(Transect_ID, Prop_on_effort = Prop_completed),
            by = "Transect_ID") |>
  left_join(completion_any_state |> select(Transect_ID, Prop_any_effort = Prop_completed),
            by = "Transect_ID") |>
  mutate(across(c(Prop_on_effort, Prop_any_effort), ~ round(replace_na(.x, 0), 3)),
         Complete_on_effort  = Prop_on_effort  >= completion_threshold,
         Complete_any_effort = Prop_any_effort >= completion_threshold) |>
  arrange(Strata, Transect_ID)

# Transects not yet completed (any effort state)
completion_any_state |>
  filter(Prop_completed < completion_threshold) |>
  arrange(Transect_ID) |>
  print(n = Inf)


#----TABLE 3: SIGHTINGS (resights excluded)----

# n_sightings   = sighting events; n_individuals = sum of Best_count.
# On-effort and Incidental sightings are also counted separately.
survey_vessel <- paste(sort(unique(na.omit(sight$Vessel))), collapse = ", ")

summarize_sightings <- function(df) {
  df |>
    summarize(n_sightings   = n(),
              n_on_effort   = sum(Sgt_type %in% "On-effort"),
              n_incidental  = sum(Sgt_type %in% "Incidental"),
              n_individuals = sum(Best_count, na.rm = TRUE),
              .groups = "drop")
}

# Table 3a: by date and species
table3a <- sight_nores |>
  group_by(Date_adt = date_adt, Species) |>
  summarize_sightings() |>
  mutate(Survey = dataset, Vessel = survey_vessel, .before = 1)

# Table 3b: by species, with a totals row
table3b <- sight_nores |>
  group_by(Species) |>
  summarize_sightings() |>
  arrange(desc(n_sightings)) |>
  mutate(Survey = dataset, Vessel = survey_vessel, .before = 1)

table3b_display <- bind_rows(
  table3b,
  table3b |>
    summarize(across(starts_with("n_"), sum)) |>
    mutate(Survey = "", Vessel = "", Species = "Totals"))

# Check: totals match the number of sightings (should be TRUE)
sum(table3b$n_sightings) == nrow(sight_nores)


#----TABLE 4: ENVIRONMENTAL CONDITIONS----

# Hours in each sea state and visibility category, within each day's survey
# window, all effort states. Summed per day, then across days. Time with no
# recorded value is excluded, so proportions are of the time with conditions
# recorded.
bft_levels <- c(as.character(0:6), ">6")
vis_levels <- c("Excellent", "Moderate", "Poor", "Restricted")   # best -> worst

env_seg <- gps_seg |>   # already limited to the daytime survey window
  mutate(Sea_state = trimws(Sea_state),
         # Worst of port/starboard visibility (one side missing -> the other)
         Vis_worst = vis_levels[pmax(match(trimws(Port_vis), vis_levels),
                                     match(trimws(Stb_vis),  vis_levels),
                                     na.rm = TRUE)])

summarize_env <- function(df, var, levels) {
  df |>
    filter(!is.na({{ var }})) |>
    group_by(date_adt, Category = {{ var }}) |>
    summarize(Duration_h = sum(duration_h), .groups = "drop") |>   # per day
    group_by(Category) |>
    summarize(Duration_h = sum(Duration_h), .groups = "drop") |>   # all days
    mutate(Category = factor(Category, levels = c(levels, setdiff(Category, levels)))) |>
    arrange(Category) |>
    mutate(Prop_time  = round(Duration_h / sum(Duration_h), 2),
           Duration_h = round(Duration_h, 1))
}

table4a <- summarize_env(env_seg, Sea_state, bft_levels) |> rename(Sea_state_BFT = Category)
table4b <- summarize_env(env_seg, Vis_worst, vis_levels) |> rename(Visibility = Category)

# Values outside the expected categories (should be empty)
setdiff(unique(env_seg$Sea_state), c(bft_levels, NA))
setdiff(unique(c(env_seg$Port_vis, env_seg$Stb_vis)), c(vis_levels, NA))

# Survey time lost to bad weather: Off effort while sea state >= 6 OR
# visibility Restricted (each segment counted once, even if both applied),
# expressed in survey days of survey_day_h hours.
survey_day_h <- 12

weather_lost_h <- env_seg |>
  filter(Effort == "Off",
         Sea_state %in% c("6", ">6") | Vis_worst %in% "Restricted") |>
  summarize(h = sum(duration_h)) |>
  pull(h)

weather_lost_days <- weather_lost_h / survey_day_h

# Breakdown by cause (should add up to weather_lost_h)
env_seg |>
  filter(Effort == "Off") |>
  mutate(cause = case_when(
    Sea_state %in% c("6", ">6") & Vis_worst %in% "Restricted" ~ "Both",
    Sea_state %in% c("6", ">6")                               ~ "Sea state",
    Vis_worst %in% "Restricted"                               ~ "Visibility")) |>
  filter(!is.na(cause)) |>
  group_by(cause) |>
  summarize(hours = round(sum(duration_h), 1))


#----SUGGESTED ADDITIONAL TABLES----

# On-effort = On, Bridge-On or Transit-On: the same states in which a
# sighting can be labelled "On-effort". All tables: resights excluded,
# daytime survey window only.
on_states <- c("On", "Bridge-On", "Transit-On")

# On-effort distance (km) per strata, and overall
effort_km_strata <- gps_seg |>
  filter(Effort %in% on_states) |>
  group_by(strata) |>
  summarize(OnEffort_km = sum(dist_km), .groups = "drop") |>
  bind_rows(gps_seg |>
              filter(Effort %in% on_states) |>
              summarize(OnEffort_km = sum(dist_km)) |>
              mutate(strata = "All"))

sight_on <- sight_nores |> filter(Sgt_type %in% "On-effort")


# Table 5: Encounter rates by species and strata
# Encounter rate = on-effort sightings per X km of on-effort track
# (individuals per X km also given). Strata from the vessel position.

er_unit_km <- 100   # encounter rate expressed per this many km of on-effort track

table5 <- bind_rows(
  sight_on |> group_by(strata, Species) |> summarize_sightings(),
  sight_on |> group_by(Species) |> summarize_sightings() |> mutate(strata = "All")) |>
  select(strata, Species, n_sightings, n_individuals) |>
  left_join(effort_km_strata, by = "strata") |>
  mutate(Sightings_rate   = round(er_unit_km * n_sightings / OnEffort_km, 2),
         Individuals_rate = round(er_unit_km * n_individuals / OnEffort_km, 2),
         OnEffort_km = round(OnEffort_km, 1)) |>
  rename(Strata = strata) |>
  arrange(factor(Strata, levels = c("FMB", "SB", "NONE", "All")), desc(n_sightings))


# Table 6: Group size by species (Best_count; all sightings, resights excluded)
table6 <- sight_nores |>
  filter(!is.na(Best_count)) |>
  group_by(Species) |>
  summarize(n_sightings = n(),
            Mean   = round(mean(Best_count), 1),
            SD     = round(sd(Best_count), 1),
            Median = median(Best_count),
            Min    = min(Best_count),
            Max    = max(Best_count),
            n_with_calves = sum(suppressWarnings(as.numeric(Calf_count)) > 0, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(n_sightings))


# Table 7: On-effort distance and hours by sea state (context for detection:
# detectability of most species drops at higher Beaufort)
table7 <- gps_seg |>
  filter(Effort %in% on_states) |>
  mutate(Sea_state = trimws(Sea_state)) |>
  group_by(Sea_state_BFT = Sea_state) |>
  summarize(OnEffort_km = round(sum(dist_km), 1),
            OnEffort_h  = round(sum(duration_h), 1),
            .groups = "drop") |>
  mutate(Prop_km = round(OnEffort_km / sum(OnEffort_km), 2),
         Sea_state_BFT = factor(Sea_state_BFT,
                                levels = c(bft_levels, setdiff(Sea_state_BFT, bft_levels)))) |>
  arrange(Sea_state_BFT)


#----CHECKS----

# Track km vs planned-line km per transect (On/Bridge-On). Ratio slightly
# above 1 is normal (small deviations from the line); much higher = a lot of
# wandering or doubling back; below 1 = a problem (e.g. GPS gap).
gps_seg |>
  filter(Effort %in% c("On", "Bridge-On"), !is.na(Transect_ID)) |>
  group_by(Transect_ID) |>
  summarize(Track_km = sum(dist_km), .groups = "drop") |>
  left_join(realized_by_transect, by = "Transect_ID") |>
  mutate(ratio = round(Track_km / Realized_km, 2)) |>
  arrange(desc(ratio)) |>
  print(n = Inf)

# TO DO: Add nuance about if a transect is re-done, then the "On" effort state
# that it was completed in should be kept (not the off-effort state) for summaries


#----EXPORT TABLES----

tables_dir <- file.path(path2data, "Data_summaries", "tables")
dir.create(tables_dir, showWarnings = FALSE)

save_table <- function(df, name) {
  write.csv(df, file.path(tables_dir, paste0("WEAWAVS_", dataset, "_", name, ".csv")),
            row.names = FALSE)
}

save_table(table1_wide,            "T1a_distance-hours-by-effort-date")
save_table(table1b,                "T1b_daily-on-effort")
save_table(table1c,                "T1c_distance-hours-by-effort-strata")
save_table(table2a,                "T2a_transect-coverage-by-date")
save_table(bind_rows(table2b_strata,
                     table2b_total |> mutate(Strata = "Overall")),
           "T2b_transect-coverage-by-strata")
save_table(table2c,                "T2c_transect-completion")
save_table(table3a,                "T3a_MM-sightings-by-date-species")
save_table(table3b_display,        "T3b_MM-sightings-by-species")
save_table(table4a,                "T4a_sea-state-hours")
save_table(table4b,                "T4b_visibility-hours")
save_table(table5,                 "T5_encounter-rates")
save_table(table6,                 "T6_group-size")
save_table(table7,                 "T7_on-effort-by-sea-state")











#----SUMMARY MAPS----
#
# Map 1  Survey design: planned transects, study area, designated WEAs
# Map 2  Daytime GPS track, colour-coded by effort state
# Map 3  On / Bridge-On effort only (realized vs planned transects)
# Map 4  Sightings by species (marine mammals + leatherback turtles;
#        dead animals as open symbols; resights excluded)



#----MAPS: SETTINGS----

out_dir <- file.path(path2data, "Data_summaries", "figures")
dir.create(out_dir, showWarnings = FALSE)

dpi_out     <- 300    # PNG resolution
fig_width   <- 8      # inches
fig_height  <- 6.5    # inches
show_titles <- TRUE   # FALSE = no title on the figure (if captions are in the report)

save_map <- function(plot, filename) {
  ggsave(filename = file.path(out_dir, filename), plot = plot,
         width = fig_width, height = fig_height, units = "in",
         dpi = dpi_out, bg = "white")
}

map_title <- function(txt) if (show_titles) paste("WEAWAVS", dataset, "—", txt) else NULL

# Track lines are broken wherever consecutive GPS rows are more than this many
# minutes apart, so gaps aren't bridged by a straight line
gap_break_min <- 10

# TRUE = tracks are clipped to the WEA polygons (strata); FALSE = full
# daytime track, including transits between and outside the WEAs
clip_to_strata <- TRUE


#----MAPS: EXTENT----

studyarea_shp <- st_transform(studyarea_shp, 4326)
study_bbox    <- st_bbox(studyarea_shp)

# Halifax (the frame is widened to include it)
halifax_lon <- -63.575
halifax_lat <- 44.649

# Map frame: study area + padding (keeps tracks/points off the edge);
# +0.3 extra east so the SB label isn't clipped
frame_pad <- 0.15
map_xlim <- c(min(study_bbox[["xmin"]] - frame_pad, halifax_lon - 0.5),   # room for the Halifax label
              study_bbox[["xmax"]] + frame_pad + 0.45)                    # room for the SB label
map_ylim <- c(study_bbox[["ymin"]] - frame_pad, study_bbox[["ymax"]] + frame_pad)


#----MAPS: BASEMAP (bathymetry + coastline)----

# Bathymetry (NOAA ETOPO, 1 arc-minute), slightly larger than the frame so
# there is no blank strip at the edges. keep = TRUE caches the download in
# the working directory, so later runs don't re-download it.
bathy_pad <- 0.35
bathy <- marmap::getNOAA.bathy(lon1 = map_xlim[1] - bathy_pad, lon2 = map_xlim[2] + bathy_pad,
                               lat1 = map_ylim[1] - bathy_pad, lat2 = map_ylim[2] + bathy_pad,
                               resolution = 1, keep = TRUE)

# Water only (land comes from the coastline shapefile).
# Colour is based on log10(depth): with a linear scale, everything on the
# shelf (0-200 m, where the study area is) falls in the palest shade because
# the deep basin (~4000-5000 m) takes up most of the colour range. On a log
# scale, 10, 100, 1000 m are evenly spaced, so the banks, basins and the
# shelf edge stand out.
bathy_df <- marmap::fortify.bathy(bathy) |>   # x = lon, y = lat, z = elevation (negative = water)
  mutate(depth_m   = if_else(z <= 0, -z, NA_real_),
         depth_log = log10(depth_m + 1))

# Optional depth contour lines (m); off by default (depth is shown by the
# smooth colour shading alone). Set to TRUE to add them.
show_contours  <- FALSE
contour_depths <- c(100, 200, 1000)

# Coastline: cropped to the map frame (all provinces inside it, e.g. also
# Newfoundland or the Magdalen Islands if the frame reaches them), then
# simplified (~50 m; plenty at this scale)
sf_use_s2(FALSE)
coast_map <- coastal_shp |>
  st_transform(4326) |>
  st_make_valid() |>                   # hi-res boundary files often have small topology errors
  st_crop(xmin = map_xlim[1] - 0.5, xmax = map_xlim[2] + 0.5,
          ymin = map_ylim[1] - 0.5, ymax = map_ylim[2] + 0.5) |>
  st_simplify(dTolerance = 0.0005, preserveTopology = TRUE)
sf_use_s2(TRUE)

# Reusable background layers: bathymetry + land
basemap_bg <- list(
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = depth_log), interpolate = TRUE),
  # Medium blues, smooth gradient (shallow = very light blue, deep = medium
  # blue): enough contrast to show the banks and shelf edge, light enough for
  # tracks and transects to stay visible. Darken/lighten the ends to taste.
  #scale_fill_gradientn(colours  = c("#eef6fc", "#cfe5f4", "#a9cfea", "#7fb4dc", "#5a97c9", "#3f7db5"),
  #                     na.value = "transparent", guide = "none"),
  #making it lighter...
  scale_fill_gradientn(colours  = c("#f3f9fd", "#dbecf7", "#bddcf0", "#9cc8e6", "#8fc0e2", "#76abd6"),
                       na.value = "transparent", guide = "none"),
  if (show_contours)
    geom_contour(data = bathy_df |> filter(!is.na(depth_m)), aes(x = x, y = y, z = depth_m),
                 breaks = contour_depths, colour = "#7f9fb8", linewidth = 0.2, alpha = 0.8),
  geom_sf(data = coast_map, fill = "grey85", colour = "grey40", linewidth = 0.25, inherit.aes = FALSE)
)

# Background + study area outline (no legend entry), used by Maps 2-4
basemap_layers <- c(basemap_bg, list(
  geom_sf(data = studyarea_shp, fill = NA, colour = "black", linewidth = 0.6, inherit.aes = FALSE)
))

# Reusable scale bar, frame and theme
map_furniture <- list(
  annotation_scale(location = "bl", width_hint = 0.25, style = "ticks"),
  # annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering()),
  coord_sf(xlim = map_xlim, ylim = map_ylim, crs = 4326, expand = FALSE),
  theme_bw(base_size = 11),
  theme(plot.title.position = "plot",         # title spans the whole figure, not just the map
        panel.grid = element_line(colour = "grey85", linewidth = 0.2),
        # Legends BELOW the map (outside it), laid out in rows, so they take
        # height rather than width and the map keeps the full figure width.
        # Several legends (e.g. Map 2) are stacked one above the other.
        legend.position   = "bottom",
        legend.box        = "vertical",
        legend.box.just   = "left",
        legend.direction  = "horizontal",
        legend.title.position = "left",
        legend.title      = element_text(size = 9, face = "bold"),
        legend.text       = element_text(size = 9),
        legend.key        = element_rect(fill = NA),
        legend.key.width  = unit(0.9, "cm"),     # line swatches in the legend
        legend.spacing.y  = unit(0.1, "cm"),
        legend.margin     = margin(0, 0, 0, 0)),
  labs(x = NULL, y = NULL)
)


#----MAPS: PLACE LABELS----

# WEA labels (FMB / SB), positioned relative to each polygon
anchor_xy <- st_coordinates(st_point_on_surface(st_geometry(studyarea_shp)))

wea_labels <- studyarea_shp |>
  st_drop_geometry() |>
  mutate(label = case_when(grepl("French", WEA, ignore.case = TRUE) ~ "FMB",
                           grepl("Sydney", WEA, ignore.case = TRUE) ~ "SB",
                           TRUE ~ WEA)) |>
  bind_cols(as.data.frame(t(sapply(st_geometry(studyarea_shp),
                                   function(g) st_bbox(g)[c("xmax", "ymax")])))) |>
  bind_cols(as.data.frame(anchor_xy)) |>   # X, Y = a point inside the polygon
  mutate(label_x = ifelse(label == "FMB", X + 0.5, xmax + 0.04),
         label_y = ifelse(label == "FMB", Y + 0.4, ymax + 0.02))

# Cities (point + label; Halifax's label above, Sydney's below)
place_points <- data.frame(
  place = c("Halifax", "Sydney"),
  lon   = c(halifax_lon, -60.183),
  lat   = c(halifax_lat, 46.135),
  hjust = c(0.5, 0.5),
  vjust = c(-1, 1.8))

# Region labels
place_regions <- data.frame(
  place = c("Nova Scotia", "P.E.I."),
  lon   = c(-62.6, -63.1),
  lat   = c(45.35, 46.3))

place_labels_layer <- list(
  geom_text(data = wea_labels, aes(x = label_x, y = label_y, label = label),
            inherit.aes = FALSE, fontface = "bold", size = 4, hjust = 0),
  geom_point(data = place_points, aes(x = lon, y = lat), inherit.aes = FALSE,
             size = 2, shape = 21, fill = "black", colour = "black", stroke = 0.7),
  geom_text(data = place_points, aes(x = lon, y = lat, label = place, hjust = hjust, vjust = vjust),
            inherit.aes = FALSE, size = 3),
  geom_text(data = place_regions, aes(x = lon, y = lat, label = place),
            inherit.aes = FALSE, size = 4.2, fontface = "italic", colour = "grey25")
)


#----MAPS: TRACK LINES (daytime survey window only)----

# Consecutive rows are joined into one line while the effort state stays the
# same, on the same day, with no gap > gap_break_min. Night-time track data
# is not used (gpseff_day only covers each day's survey window).
track_pts <- gpseff_day |>
  arrange(datetime_utc, row_type != "effort") |>
  mutate(gap_min = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")),
         new_run = coalesce(Effort != lag(Effort), TRUE) |
           coalesce(date_adt != lag(date_adt), TRUE) |
           coalesce(gap_min > gap_break_min, TRUE),
         run_id  = cumsum(new_run)) |>
  group_by(run_id) |>
  filter(n() >= 2) |>                      # a line needs at least 2 points
  ungroup()

track_lines <- track_pts |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326) |>
  group_by(run_id, Effort, date_adt) |>
  summarize(do_union = FALSE, .groups = "drop") |>   # keeps time order
  st_cast("LINESTRING")

# Clip to the WEA polygons. Lines are clipped (rather than dropping the GPS
# points outside the polygons first), so a track that leaves and re-enters a
# WEA is not joined by a straight line across the gap.
if (clip_to_strata) {
  track_lines <- suppressWarnings(st_intersection(track_lines, st_union(studyarea_shp))) |>
    filter(st_geometry_type(geometry) %in% c("LINESTRING", "MULTILINESTRING")) |>
    # One geometry type: a mixed-type column makes ggplot draw legend keys
    # as boxes instead of lines
    st_cast("MULTILINESTRING")
}

table(track_lines$Effort)


#----MAPS: PLANNED TRANSECTS----

# All planned transects drawn in one colour (complements not distinguished)
transects_map <- transects_shp |>
  st_transform(4326)

planned_col <- "grey40"


#----MAP 1: SURVEY DESIGN (planned transects, study area, designated WEAs)----

# Designated WEAs: all except Sydney Bank (French Bank, Middle Bank and
# Sydney Bight are kept). The column holding the WEA names is found
# automatically: the text column that contains "Sydney Bank".
wea_name_col <- names(wea_shp)[sapply(st_drop_geometry(wea_shp), function(x)
  is.character(x) && any(grepl("Sable Island Bank", x, ignore.case = TRUE)))][1]

wea_map <- wea_shp |>
  st_transform(4326) |>
  st_make_valid() |>
  filter(!grepl("Sable Island Bank", .data[[wea_name_col]], ignore.case = TRUE))

wea_map[[wea_name_col]]   # check: French Bank, Middle Bank, Sydney Bight

# Layers, bottom to top: designated WEAs (filled), study area outline,
# planned transects. Each has its own legend entry with a matching symbol
# (filled box, outlined box, line).
map1 <- ggplot() +
  basemap_bg +
  new_scale_fill() +                       # separate fill scale from the bathymetry
  geom_sf(data = wea_map, aes(fill = "Designated wind energy areas"),
          colour = "#b35806", linewidth = 0.4, alpha = 0.35) +
  scale_fill_manual(name = NULL, values = c("Designated wind energy areas" = "#f1a340"),
                    guide = guide_legend(order = 1)) +
  geom_sf(data = studyarea_shp, aes(colour = "Study area (survey strata)"),
          fill = NA, linewidth = 0.6, key_glyph = "polygon") +
  scale_colour_manual(name = NULL, values = c("Study area (survey strata)" = "black"),
                      guide = guide_legend(order = 2, override.aes = list(fill = NA))) +
  new_scale_colour() +
  geom_sf(data = transects_map, aes(colour = "Planned transects"),
          linewidth = 0.6, alpha = 0.9, key_glyph = "path") +
  scale_colour_manual(name = NULL, values = c("Planned transects" = planned_col),
                      guide = guide_legend(order = 3, override.aes = list(linewidth = 1.2))) +
  place_labels_layer +
  map_furniture +
  labs(title = if (show_titles) "WEAWAVS: Survey Design" else NULL)

map1
save_map(map1, "WEAWAVS_map1_survey-design.png")


#----MAP 2: DAYTIME GPS TRACK BY EFFORT STATE----

effort_colors <- c(
  "On"         = "#006400",  # dark green
  "Bridge-On"  = "#00FF00",  # bright green
  "Transit-On" = "#FFD700",  # gold
  "Off"        = "#FF0000",  # red
  "Closing"    = "#800080")  # purple

# Legend: only the states present, in palette order
effort_present <- intersect(names(effort_colors), unique(track_lines$Effort))

map2 <- ggplot() +
  basemap_layers +
  geom_sf(data = transects_map, aes(colour = "Planned transects"), linewidth = 0.6, alpha = 0.9,
          show.legend = "line") +
  scale_colour_manual(values = c("Planned transects" = planned_col), name = NULL,
                      guide = guide_legend(override.aes = list(linewidth = 1.2), order = 1)) +
  new_scale_colour() +
  geom_sf(data = track_lines, aes(colour = Effort), linewidth = 0.8, show.legend = "line") +
  scale_colour_manual(values = effort_colors, breaks = effort_present,
                      name = "Effort status", na.value = "grey50",
                      guide = guide_legend(override.aes = list(linewidth = 1.2))) +
  place_labels_layer +
  map_furniture +
  labs(title = map_title("GPS tracks by effort status"))

map2
save_map(map2, paste0("WEAWAVS_", dataset, "_map2_gps-tracks-by-effort.png"))


#----MAP 3: ON / BRIDGE-ON EFFORT (realized vs planned transects)----

track_lines_on <- track_lines |>
  filter(Effort %in% c("On", "Bridge-On"))

map3 <- ggplot() +
  basemap_layers +
  geom_sf(data = transects_map,  aes(colour = "Planned transects"),  linewidth = 0.6, alpha = 0.9,
          show.legend = "line") +
  geom_sf(data = track_lines_on, aes(colour = "Realized transects (On / Bridge-On)"), linewidth = 0.8,
          show.legend = "line") +
  scale_colour_manual(name = NULL,
                      values = c("Planned transects" = planned_col,
                                 "Realized transects (On / Bridge-On)" = "#006400"),
                      guide = guide_legend(override.aes = list(linewidth = 1.2))) +
  place_labels_layer +
  map_furniture +
  labs(title = map_title("On-effort GPS tracks"))

map3
save_map(map3, paste0("WEAWAVS_", dataset, "_map3_on-effort-tracks.png"))


#----MAP 4: SIGHTINGS BY SPECIES----

# Leatherback turtles come from the all-sightings file (the marine mammal
# file excludes them). Resights excluded.
turtles <- read_csv(paste0(path2data, "/Data_summaries/data_combined_cleaned/WEAWAVS_", dataset, "_all_sightings.csv"),
                    col_types = cols(.default = col_character())) |>
  filter(str_remove(Species, " \\(dead\\)$") == "Leatherback turtle",
         !Resight %in% c("Yes", "Possible")) |>
  mutate(across(c(map_lat, map_lon, Best_count), as.numeric))

# Map position = map_lat/map_lon from error-checking (animal position for
# On-effort sightings with a distance, otherwise the vessel position).
# Dead animals keep their species colour but get an open symbol.
sight_map <- bind_rows(
  sight_nores |> select(Species, Sgt_type, Best_count, map_lat, map_lon),
  turtles     |> select(Species, Sgt_type, Best_count, map_lat, map_lon)) |>
  mutate(Status        = if_else(str_detect(Species, "\\(dead\\)$"), "Dead", "Live"),
         Species_group = str_remove(Species, " \\(dead\\)$"))

# Sightings without a map position (should be 0)
sum(is.na(sight_map$map_lat) | is.na(sight_map$map_lon))

sight_sf <- sight_map |>
  filter(!is.na(map_lat), !is.na(map_lon)) |>
  arrange(Status == "Dead") |>              # dead animals drawn on top
  st_as_sf(coords = c("map_lon", "map_lat"), crs = 4326)

# Fixed species colours: vivid, distinct hues for confirmed species; muted
# tones for the "Unknown ..." groups so they read as secondary.
species_colors <- c(
  "Atlantic white-sided dolphin" = "#e6194b",  # red
  "Blue whale"                   = "#4363d8",  # blue
  "Bottlenose dolphin"           = "#9a6324",  # brown
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
  "Unknown baleen whale"         = "#7d8ca3",  # blue-grey  (muted: unknown ID)
  "Unknown cetacean"             = "#b5838d",  # dusty rose (muted: unknown ID)
  "Unknown small cetacean"       = "#a9a9a9",  # grey       (muted: unknown ID)
  "Unknown dolphin"              = "#c9a227",  # mustard    (muted: unknown ID)
  "Unknown seal"                 = "#808000",  # olive      (muted: unknown ID)
  "Leatherback turtle"           = "#000000"   # black
)

# Species without a colour (should be empty; add them above)
setdiff(unique(sight_sf$Species_group), names(species_colors))

# Legend order: confirmed species (A-Z), then "Unknown ...", then turtles;
# only species present in the data
present <- unique(sight_sf$Species_group)
species_levels <- c(sort(present[!grepl("^Unknown|turtle", present)]),
                    sort(present[grepl("^Unknown", present)]),
                    present[grepl("turtle", present)])

# Plain data frame of sighting coordinates for geom_point()
sight_pts <- sight_sf |>
  mutate(lon = st_coordinates(geometry)[, 1],
         lat = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

# Title mentions leatherback turtles only if any are on the map
sight_title <- if (any(sight_sf$Species_group == "Leatherback turtle")) {
  "Marine mammal and leatherback turtle sightings"
} else {
  "Marine mammal sightings"
}

map4 <- ggplot() +
  basemap_layers +
  # key_glyph = "path": the legend key is drawn as a plain line only
  geom_sf(data = track_lines_on, aes(colour = "On-effort GPS track"), linewidth = 0.5, alpha = 0.5,
          key_glyph = "path") +
  scale_colour_manual(name = NULL, values = c("On-effort GPS track" = "grey40"),
                      guide = guide_legend(override.aes = list(linewidth = 1, alpha = 1, shape = NA),
                                           order = 3)) +
  new_scale_colour() +
  # Sightings drawn with geom_point() on lon/lat (same CRS as the map,
  # EPSG:4326) rather than geom_sf(): geom_point always uses a plain point
  # as its legend key, so no line is drawn through the legend symbols.
  geom_point(data = sight_pts, aes(x = lon, y = lat, colour = Species_group, shape = Status),
             size = 1.3, stroke = 0.7, alpha = 0.85) +
  scale_colour_manual(values = species_colors, breaks = species_levels, name = "Species",
                      na.value = "grey50",
                      guide = guide_legend(override.aes = list(size = 3, shape = 16), order = 1)) +
  scale_shape_manual(values = c("Live" = 16, "Dead" = 1), name = NULL,
                     labels = c("Live" = "Live", "Dead" = "Dead (open symbol)"),
                     guide = if (any(sight_sf$Status == "Dead")) guide_legend(order = 2) else "none") +
  place_labels_layer +
  map_furniture +
  # Many species: arrange the species legend in 3 columns, title on top
  guides(colour = guide_legend(ncol = 3, title.position = "top",
                               override.aes = list(size = 2.5, shape = 16), order = 1)) +
  labs(title = map_title(sight_title))

map4
save_map(map4, paste0("WEAWAVS_", dataset, "_map4_sightings-by-species.png"))








#----WEAWAVS 2026: PRESENTATION FIGURES----
#
# Figures for a short slide deck for offshore wind energy managers.
# Minimal, consistent styling; each figure is saved at the size it will
# occupy on a 16:9 slide (13.33 x 7.5 in), so text appears at true size.


#----PACKAGES----

library(tidyverse)
library(sf)
library(geosphere)
library(patchwork)    # combine figures
library(showtext)     # custom font (Lato)


#----SETTINGS----

surveys <- c("Spring2026", "Summer2026", "Fall2026")

# Display names and one colour per survey (used on every slide)
survey_labels <- c(Spring2026 = "Spring 2026", Summer2026 = "Summer 2026", Fall2026 = "Fall 2026")
survey_cols   <- c("Spring 2026" = "#5b9a68", "Summer 2026" = "#e0a526", "Fall 2026" = "#c0573e")

# The presentation covers the survey areas only, so effort and sightings are
# limited to effort ON THE TRANSECTS (On, Bridge-On). Transit-On (observing
# off-transect or outside the study areas) is excluded throughout.
transect_states <- c("On", "Bridge-On")

# FALSE = only on-effort sightings made on the transects (default).
# TRUE  = also include incidental sightings made inside the survey areas
#         (bubble grid, headline numbers and maps; never in encounter rates).
include_incidental <- FALSE

# Output folder (shared across surveys)
fig_dir <- file.path(getwd(), "Working data", "presentation_figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Font: Lato from Google Fonts (needs internet the first time); falls back
# to the default sans-serif font if it can't be loaded
font_family <- tryCatch({ font_add_google("Lato", "Lato"); "Lato" },
                        error = function(e) "sans")
showtext_auto()
showtext_opts(dpi = 300)   # so text size matches in saved PNGs

# Save a figure at its size on the slide (inches)
save_slide <- function(plot, name, width, height) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), plot,
         width = width, height = height, units = "in", dpi = 300, bg = "white")
}

# Shared theme for charts
theme_slide <- function(base = 16) {
  theme_minimal(base_size = base, base_family = font_family) +
    theme(panel.grid   = element_blank(),
          axis.title   = element_blank(),
          plot.title   = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(),
          strip.text   = element_text(face = "bold", size = base))
}

# Shared theme for maps (no axes, gridlines or coordinates)
theme_map_slide <- function(base = 14) {
  theme_void(base_size = base, base_family = font_family) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          strip.text   = element_text(face = "bold", size = base + 2, margin = margin(b = 6)),
          panel.background = element_rect(fill = "#eef3f7", colour = NA))   # light sea
}


#----LOAD AND PREPARE DATA (all surveys)----

load_survey <- function(ds) {
  p <- data_paths[[ds]]
  list(
    gpseff = read_csv(file.path(p, "Data_summaries/data_combined_cleaned",
                                paste0("WEAWAVS_", ds, "_gps-effort.csv")),
                      col_types = cols(.default = "c")) |> mutate(Survey = ds),
    sight  = read_csv(file.path(p, "Data_summaries/data_combined_cleaned",
                                paste0("WEAWAVS_", ds, "_mm_sightings.csv")),
                      col_types = cols(.default = "c")) |> mutate(Survey = ds))
}

all_data <- map(surveys, load_survey)

# Types set explicitly (all columns were read as text); ADT rebuilt from UTC
gpseff_all <- map_dfr(all_data, "gpseff") |>
  mutate(datetime_utc = ymd_hms(datetime_utc, tz = "UTC", truncated = 3),
         datetime_adt = with_tz(datetime_utc, "America/Halifax"),
         date_adt     = as_date(datetime_adt),
         across(c(vessel_lat, vessel_lon), as.numeric),
         Survey_lab   = factor(survey_labels[Survey], levels = survey_labels)) |>
  arrange(Survey, datetime_utc, row_type != "effort")

sight_all <- map_dfr(all_data, "sight") |>
  filter(!Resight %in% c("Yes", "Possible")) |>          # resights excluded
  mutate(across(c(map_lat, map_lon, Best_count), as.numeric),
         Survey_lab = factor(survey_labels[Survey], levels = survey_labels))

# Daytime survey window only (first to last effort entry of each day),
# as in the summary tables: night-time track data is not used
window_all <- gpseff_all |>
  filter(row_type == "effort") |>
  group_by(Survey, date_adt) |>
  summarize(win_start = min(datetime_utc), win_end = max(datetime_utc), .groups = "drop")

gpseff_day <- gpseff_all |>
  inner_join(window_all, by = c("Survey", "date_adt")) |>
  filter(datetime_utc >= win_start, datetime_utc <= win_end)

# Track segments (distance and duration between consecutive rows)
seg_all <- gpseff_day |>
  group_by(Survey, date_adt) |>
  mutate(dist_km    = distHaversine(cbind(vessel_lon, vessel_lat),
                                    cbind(lead(vessel_lon), lead(vessel_lat))) / 1000,
         duration_h = as.numeric(difftime(lead(datetime_utc), datetime_utc, units = "hours"))) |>
  ungroup() |>
  filter(!is.na(dist_km))


#----SPECIES GROUPS----

# Broad groups for a non-specialist audience (species detail stays in the
# technical report). "(dead)" labels are removed before grouping.
species_groups <- c(
  "Blue whale" = "Baleen whales", "Fin whale" = "Baleen whales",
  "Fin/sei whale" = "Baleen whales", "Humpback whale" = "Baleen whales",
  "Minke whale" = "Baleen whales", "Unknown baleen whale" = "Baleen whales",
  "Atlantic white-sided dolphin" = "Dolphins", "Bottlenose dolphin" = "Dolphins",
  "Common dolphin" = "Dolphins", "White-beaked dolphin" = "Dolphins",
  "Pilot whale" = "Dolphins", "Killer whale" = "Dolphins",
  "Unknown dolphin" = "Dolphins", "Unknown small cetacean" = "Dolphins",
  "Harbour porpoise" = "Porpoises",
  "Grey seal" = "Seals", "Harbour seal" = "Seals", "Unknown seal" = "Seals",
  "Unknown cetacean" = "Unidentified whales/dolphins")

group_levels <- c("Baleen whales", "Dolphins", "Porpoises", "Seals", "Unidentified whales/dolphins")

group_cols <- c("Baleen whales" = "#1f4e79", "Dolphins" = "#e07b39", "Porpoises" = "#7b5ea7",
                "Seals" = "#4c9a8a", "Unidentified whales/dolphins" = "#9e9e9e")

sight_all <- sight_all |>
  mutate(Species_base = str_remove(Species, " \\(dead\\)$"),
         Group = factor(unname(species_groups[Species_base]), levels = group_levels),
         # On-effort sighting made on a transect (effort On/Bridge-On at the
         # time of the sighting; Effort_at_sgt comes from error-checking)
         on_transect = Sgt_type %in% "On-effort" & Effort_at_sgt %in% transect_states,
         # Incidental sighting inside one of the survey areas (strata)
         incidental_in_strata = Sgt_type %in% "Incidental" & strata %in% c("FMB", "SB"))

# How sightings split (before filtering)
count(sight_all, Survey, Sgt_type, Effort_at_sgt)

# Sightings used on the slides
sight_slides <- sight_all |>
  filter(on_transect | (include_incidental & incidental_in_strata))

count(sight_slides, Survey_lab)

# Species without a group (should be empty; add them to species_groups)
setdiff(unique(sight_all$Species_base), names(species_groups))


#----SHAPEFILES AND MAP EXTENT----

transects_shp <- st_read(file.path(getwd(), "Survey design/WEAWAVS_2026-04-10_DFifield/Planned_transects_Scenario2_Spring2026.shp"), quiet = TRUE) |>
  st_transform(4326)
transects_df  <- read.csv(file.path(getwd(), "Survey design/Scenario2_13-day_transects_with_metrics.csv"))
studyarea_shp <- st_read(file.path(getwd(), "Survey design/WEAWAVS_2026-04-10_DFifield/Study_area_buf_prelim.shp"), quiet = TRUE) |>
  st_transform(4326)
wea_shp       <- st_read("C:/Users/GavrilchukK/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/Mapping/Shapefiles/Designated-WEA/Designated_WEAs_25_07_29.shp", quiet = TRUE)
coastal_shp   <- st_read("C:/Users/GavrilchukK/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/Mapping/Shapefiles/coastline/canada/gpr_000b11a_e_hires/gpr_000b11a_e.shp", quiet = TRUE)

# Designated WEAs, without Sable Island Bank (name column found automatically)
wea_name_col <- names(wea_shp)[sapply(st_drop_geometry(wea_shp), function(x)
  is.character(x) && any(grepl("Sable Island Bank", x, ignore.case = TRUE)))][1]
wea_map <- wea_shp |>
  st_transform(4326) |> st_make_valid() |>
  filter(!grepl("Sable Island Bank", .data[[wea_name_col]], ignore.case = TRUE)) |>
  mutate(wea_label = .data[[wea_name_col]])

# Map extent: study area + padding (incl. Halifax)
bb <- st_bbox(studyarea_shp)
map_xlim <- c(min(bb[["xmin"]] - 0.2, -63.9), bb[["xmax"]] + 0.4)
map_ylim <- c(bb[["ymin"]] - 0.2, bb[["ymax"]] + 0.2)

# Land, cropped to the extent and simplified (minimal style: no bathymetry).
# The planar-geometry warnings here are expected and harmless at this scale
# (crop box and ~200 m simplification in degrees), so they are silenced.
sf_use_s2(FALSE)
land <- suppressWarnings(suppressMessages(
  coastal_shp |>
    st_transform(4326) |> st_make_valid() |>
    st_crop(xmin = map_xlim[1] - 1, xmax = map_xlim[2] + 1,
            ymin = map_ylim[1] - 1, ymax = map_ylim[2] + 1) |>
    st_simplify(dTolerance = 0.002, preserveTopology = TRUE)))
sf_use_s2(TRUE)

land_layer <- geom_sf(data = land, fill = "#dcdcd7", colour = NA, inherit.aes = FALSE)
map_coords <- coord_sf(xlim = map_xlim, ylim = map_ylim, crs = 4326, expand = FALSE)


#----SLIDE 1: STUDY DESIGN----

# (a) Map: WEAs as the only strong colour, labelled directly on the map
# Label point inside each WEA, calculated in a projected CRS (EPSG:3347)
# for an accurate result, then converted back to lon/lat
wea_lab_pts <- wea_map |>
  st_transform(3347) |>
  mutate(geometry = st_point_on_surface(geometry)) |>
  st_transform(4326) |>
  mutate(lon = st_coordinates(geometry)[, 1], lat = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

slide1_map <- ggplot() +
  land_layer +
  geom_sf(data = transects_shp, colour = "grey55", linewidth = 0.35) +
  geom_sf(data = wea_map, fill = "#f1a340", colour = "#b35806", alpha = 0.45, linewidth = 0.4) +
  geom_sf(data = studyarea_shp, fill = NA, colour = "grey20", linewidth = 0.6) +
  geom_label(data = wea_lab_pts, aes(lon, lat, label = wea_label), family = font_family,
             size = 4.2, fill = alpha("white", 0.8), linewidth = 0, label.padding = unit(0.15, "lines")) +
  annotate("text", x = -63.1, y = 45.25, label = "Nova Scotia", family = font_family,
           fontface = "italic", colour = "grey35", size = 5) +
  map_coords +
  theme_map_slide()

# (b) Locator inset: where the study area is, for an international audience.
# Needs the rnaturalearth package (already installed).
na_land <- rnaturalearth::ne_countries(scale = 50, country = c("Canada", "United States of America"),
                                       returnclass = "sf")
locator <- ggplot() +
  geom_sf(data = na_land, fill = "#dcdcd7", colour = "white", linewidth = 0.2) +
  annotate("rect", xmin = map_xlim[1], xmax = map_xlim[2], ymin = map_ylim[1], ymax = map_ylim[2],
           fill = NA, colour = "#c0392b", linewidth = 0.8) +
  coord_sf(xlim = c(-82, -50), ylim = c(36, 58), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#eef3f7", colour = "grey60", linewidth = 0.4))

slide1_map_inset <- slide1_map +
  inset_element(locator, left = 0.0, bottom = 0.62, right = 0.28, top = 1.0, align_to = "panel")

# (c) Key numbers (big text, no chart)
key_numbers <- tibble(
  value = c("2", as.character(n_distinct(transects_df$Transect_ID)),
            format(round(sum(transects_df$Transect_km), -1), big.mark = ","), "3"),
  label = c("survey areas", "planned transects", "km of planned transects", "seasonal surveys (2026)"))

slide1_numbers <- ggplot(key_numbers |> mutate(y = rev(seq_along(value)))) +
  geom_text(aes(x = 0, y = y + 0.12, label = value), hjust = 0, family = font_family,
            fontface = "bold", size = 13, colour = "#b35806") +
  geom_text(aes(x = 0, y = y - 0.25, label = label), hjust = 0, family = font_family,
            size = 5, colour = "grey30") +
  xlim(0, 1) + ylim(0.4, nrow(key_numbers) + 0.6) +
  theme_void()

slide1 <- slide1_map_inset + slide1_numbers + plot_layout(widths = c(3, 1))
slide1
save_slide(slide1, "slide1_study-design", width = 12.3, height = 6)


#----SLIDE 2: SURVEY EFFORT----

# (a) On-transect track lines per survey. A line runs only through
# consecutive rows that are ALL On/Bridge-On: it is broken at every change to
# another effort state (Off, Transit-On, Closing), at a new day, and at GPS
# gaps > 10 min. The run IDs are built BEFORE filtering to On/Bridge-On, so
# an Off (or other) period is never bridged by a straight line, however short.
gap_break_min <- 10

track_lines_all <- gpseff_day |>
  group_by(Survey) |>
  mutate(on_tr   = Effort %in% transect_states,
         gap_min = as.numeric(difftime(datetime_utc, lag(datetime_utc), units = "mins")),
         run_id  = cumsum(coalesce(on_tr != lag(on_tr), TRUE) |
                            coalesce(gap_min > gap_break_min, TRUE) |
                            coalesce(date_adt != lag(date_adt), TRUE))) |>
  ungroup() |>
  filter(on_tr) |>
  group_by(Survey, Survey_lab, run_id) |>
  filter(n() >= 2) |>
  ungroup() |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326) |>
  group_by(Survey, Survey_lab, run_id) |>
  summarize(do_union = FALSE, .groups = "drop") |>
  st_cast("LINESTRING")

slide2_maps <- ggplot() +
  land_layer +
  geom_sf(data = transects_shp, colour = "grey75", linewidth = 0.3) +
  geom_sf(data = studyarea_shp, fill = NA, colour = "grey45", linewidth = 0.4) +
  geom_sf(data = track_lines_all, aes(colour = Survey_lab), linewidth = 0.6, show.legend = FALSE) +
  scale_colour_manual(values = survey_cols) +
  facet_wrap(~Survey_lab, nrow = 1) +
  map_coords +
  theme_map_slide()

# (b) Stat cards: 4 big numbers per survey
# Transect completion (On/Bridge-On, all strata) from the summary script's
# Table 2b export; NA if that table hasn't been exported for a survey
read_completion <- function(ds) {
  f <- file.path(data_paths[[ds]], "Data_summaries", "tables",
                 paste0("WEAWAVS_", ds, "_T2b_transect-coverage-by-strata.csv"))
  if (!file.exists(f)) return(NA_real_)
  read_csv(f, show_col_types = FALSE) |> filter(Strata == "Overall") |> pull(Prop_completed)
}

# transect_km / transect_h: On + Bridge-On only (on the planned transects);
# shown on the stat cards and used for encounter rates
effort_stats <- seg_all |>
  group_by(Survey) |>
  # Survey days = days on which a transect was surveyed (any row that day
  # with a Transect_ID filled in)
  summarize(days        = n_distinct(date_adt[!is.na(Transect_ID)]),
            transect_km = sum(dist_km[Effort %in% transect_states]),
            transect_h  = sum(duration_h[Effort %in% transect_states]),
            .groups = "drop") |>
  mutate(completion = map_dbl(Survey, read_completion),
         Survey_lab = factor(survey_labels[Survey], levels = survey_labels))

# Days on transect and km on transect per survey:
effort_stats |> 
  select(Survey, days, transect_km, transect_h, completion)

cards <- effort_stats |>
  transmute(Survey_lab,
            `1` = paste0(days, "|survey days"),
            `2` = paste0(format(round(transect_km), big.mark = ","), "|km surveyed on transect"),
            `3` = paste0(round(transect_h), "|hours on transect"),
            `4` = paste0(ifelse(is.na(completion), "–", scales::percent(completion, accuracy = 1)),
                         "|of transect line covered")) |>
  pivot_longer(-Survey_lab, names_to = "item", values_to = "txt") |>
  separate(txt, into = c("value", "label"), sep = "\\|") |>
  mutate(item = as.integer(item),
         x = c(1, 2, 1, 2)[item],      # 2 x 2 layout per card
         y = c(2, 2, 1, 1)[item])

slide2_cards <- ggplot(cards) +
  geom_text(aes(x, y + 0.15, label = value, colour = Survey_lab), family = font_family,
            fontface = "bold", size = 9, show.legend = FALSE) +
  geom_text(aes(x, y - 0.25, label = label), family = font_family, size = 4, colour = "grey35") +
  scale_colour_manual(values = survey_cols) +
  facet_wrap(~Survey_lab, nrow = 1) +
  scale_x_continuous(limits = c(0.4, 2.6)) +
  scale_y_continuous(limits = c(0.5, 2.6)) +
  theme_void() +
  theme(strip.text = element_blank())

slide2 <- slide2_maps / slide2_cards + plot_layout(heights = c(2.6, 1))
slide2
save_slide(slide2, "slide2_survey-effort", width = 12.3, height = 6)

# (c) Optional: how survey time was used (share of daytime hours)
time_use <- seg_all |>
  mutate(Sea_state = trimws(Sea_state),
         weather = Sea_state %in% c("6", ">6") | Port_vis %in% "Restricted" | Stb_vis %in% "Restricted",
         Use = case_when(Effort %in% c("On", "Bridge-On") ~ "On transect",
                         Effort == "Transit-On"            ~ "Observing in transit",
                         Effort == "Off" & weather          ~ "Off – weather",
                         TRUE                               ~ "Off – other"),
         Use = factor(Use, levels = c("On transect", "Observing in transit", "Off – weather", "Off – other")),
         Survey_lab = factor(survey_labels[Survey], levels = rev(survey_labels))) |>
  group_by(Survey_lab, Use) |>
  summarize(h = sum(duration_h), .groups = "drop_last") |>
  mutate(share = h / sum(h)) |>
  ungroup()

slide2_timeuse <- ggplot(time_use, aes(share, Survey_lab, fill = Use)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = ifelse(share >= 0.06, scales::percent(share, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), family = font_family, colour = "white", size = 4.5) +
  scale_fill_manual(values = c("On transect" = "#2e7d4f", "Observing in transit" = "#8fbf9f",
                               "Off – weather" = "#c0573e", "Off – other" = "#c9c9c4")) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  theme_slide(14) +
  theme(axis.text.x = element_blank())

slide2_timeuse
save_slide(slide2_timeuse, "slide2_time-use", width = 12.3, height = 2.2)


#----SLIDE 3: SIGHTINGS----

# Layout (3 parts, aligned by patchwork):
#   left:   silhouette + group name for each row
#   centre: bubble grid (species group x survey; bubble size = animals counted)
#   bottom: per-survey numbers (animals counted, species identified)

# Rows of the bubble grid: all groups except "Unidentified whales/dolphins"
# ("Unknown cetacean": too few, and not assignable to a group). Those
# sightings are still included in the per-survey "animals counted" totals.
bubble_groups <- setdiff(group_levels, "Unidentified whales/dolphins")
group_rows    <- rev(bubble_groups)   # baleen whales on top

# (a) Bubble grid
bubbles <- sight_slides |>
  filter(Group %in% bubble_groups) |>
  mutate(Group = factor(as.character(Group), levels = group_rows)) |>
  group_by(Survey_lab, Group) |>
  summarize(Individuals = sum(Best_count, na.rm = TRUE), Groups = n(), .groups = "drop") |>
  complete(Survey_lab, Group, fill = list(Individuals = 0, Groups = 0))

# Bubble size: scale_size() with a minimum size, so even small counts get a
# bubble big enough to hold their number (sizes still increase with the
# count, but are no longer strictly proportional to it)
slide3_bubbles <- ggplot(bubbles, aes(Survey_lab, Group)) +
  geom_point(data = filter(bubbles, Individuals > 0),
             aes(size = Individuals, colour = Survey_lab), alpha = 0.85) +
  geom_text(aes(label = ifelse(Individuals > 0, format(Individuals, big.mark = ","), "–")),
            family = font_family, fontface = "bold", size = 5.5, colour = "grey10") +
  scale_size(range = c(13, 30), guide = "none") +
  scale_colour_manual(values = survey_cols, guide = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(drop = FALSE) +
  theme_slide(16) +
  theme(axis.text.x = element_text(face = "bold", size = 21, margin = margin(b = 8)),
        axis.text.y = element_blank())

# (b) Silhouettes (PhyloPic, via the rphylopic package; needs internet).
# One representative species per group; "Unidentified" has no icon.
# PhyloPic images have individual licences (mostly CC0 / CC-BY): the
# attribution for each is printed below; credit them on the slide if required.
# Per icon: the species, a rotation (degrees; the fin whale image is drawn
# head-up, so it is turned to lie horizontally like the others; use 90
# instead of -90 if it ends up facing the wrong way) and a width (in x-axis
# units; the panel runs from 0 to 5)
icon_specs <- tribble(
  ~Group,          ~taxon,                  ~angle, ~width,
  "Baleen whales", "Balaenoptera physalus",    -90,    1.5,
  "Dolphins",      "Delphinus delphis",          0,    1.2,
  "Porpoises",     "Phocoena phocoena",          0,    1.2,
  "Seals",         "Halichoerus grypus",         0,    1.2)

icons <- icon_specs |>
  mutate(uuid = map_chr(taxon, ~ tryCatch(rphylopic::get_uuid(name = .x, n = 1),
                                          error = function(e) NA_character_)),
         Group = factor(Group, levels = group_rows))

icons                                                     # check: a uuid for each
map(na.omit(icons$uuid), ~ tryCatch(rphylopic::get_attribution(uuid = .x),
                                    error = function(e) NULL))   # credits

# Download each image and rotate it if needed
icons <- icons |>
  filter(!is.na(uuid)) |>
  mutate(img = map2(uuid, angle, function(u, a) {
    img <- rphylopic::get_phylopic(uuid = u)
    if (a != 0) img <- rphylopic::rotate_phylopic(img, angle = a)
    img
  }))

group_labels <- tibble(Group = factor(group_rows, levels = group_rows),
                       label = str_wrap(group_rows, 15))

icon_x  <- 0.8    # icon centre
label_x <- 1.7    # group names start here

# One layer per icon, so each can have its own width
icon_layers <- pmap(icons, function(Group, img, width, ...) {
  rphylopic::geom_phylopic(data = tibble(x = icon_x, y = Group), aes(x = x, y = y),
                           img = img, width = width, fill = "grey35", colour = "grey35")
})

slide3_icons <- ggplot() +
  icon_layers +
  geom_text(data = group_labels, aes(x = label_x, y = Group, label = label), hjust = 0,
            family = font_family, size = 7, colour = "grey20", lineheight = 0.9) +
  scale_x_continuous(limits = c(0, 5), expand = c(0, 0)) +
  scale_y_discrete(limits = group_rows, drop = FALSE) +
  coord_cartesian(clip = "off") +
  theme_void()

# (c) Per-survey numbers under the bubbles
survey_numbers <- sight_slides |>
  group_by(Survey_lab) |>
  summarize(animals = sum(Best_count, na.rm = TRUE),
            species = n_distinct(Species_base[!grepl("^Unknown|/", Species_base)]),  # identified to species
            .groups = "drop") |>
  pivot_longer(c(animals, species), names_to = "stat", values_to = "value") |>
  mutate(y     = if_else(stat == "animals", 2, 1),
         label = if_else(stat == "animals", "animals counted", "species identified"))

slide3_numbers <- ggplot(survey_numbers, aes(Survey_lab, y)) +
  geom_text(aes(label = format(value, big.mark = ","), colour = Survey_lab), nudge_x = -0.05, hjust = 1,
            family = font_family, fontface = "bold", size = 8, show.legend = FALSE) +
  geom_text(aes(label = label), nudge_x = 0.02, hjust = 0,
            family = font_family, size = 4.5, colour = "grey35") +
  scale_colour_manual(values = survey_cols) +
  scale_y_continuous(limits = c(0.5, 2.5)) +
  theme_void()

# Combine: icons | bubbles, numbers under the bubbles only
slide3 <- slide3_icons + slide3_bubbles + plot_spacer() + slide3_numbers +
  plot_layout(ncol = 2, widths = c(1.5, 4), heights = c(4, 1)) +
  # Small note at the bottom of the slide
  plot_annotation(
    caption = paste("Number of animals from on-effort sightings made while surveying transects in the WEAs"),
    theme = theme(plot.caption = element_text(family = font_family, size = 11, colour = "grey45",
                                              hjust = 0, margin = margin(t = 10))))
slide3
save_slide(slide3, "slide3_sightings", width = 12.3, height = 6.2)

# (c) Optional: encounter rates (groups per 100 km on transect), the fair
# comparison between surveys with different amounts of effort. Always
# on-transect sightings / on-transect km (incidentals never included).
enc_rates <- sight_all |>
  filter(on_transect, !is.na(Group)) |>
  count(Survey, Survey_lab, Group, name = "n_groups") |>
  complete(nesting(Survey, Survey_lab), Group, fill = list(n_groups = 0)) |>
  left_join(effort_stats |> select(Survey, transect_km), by = "Survey") |>
  mutate(rate = 100 * n_groups / transect_km,
         Group = factor(Group, levels = rev(group_levels)))

slide3_rates <- ggplot(enc_rates, aes(rate, Group, colour = Survey_lab)) +
  geom_line(aes(group = Group), colour = "grey80", linewidth = 1) +
  geom_point(size = 5) +
  scale_colour_manual(values = survey_cols) +
  labs(x = "Groups sighted per 100 km on transect") +
  theme_slide(15) +
  theme(axis.title.x = element_text(size = 13, colour = "grey30"),
        panel.grid.major.x = element_line(colour = "grey90"))

slide3_rates
save_slide(slide3_rates, "slide3_encounter-rates", width = 8, height = 4.5)


#----SLIDE 4: SIGHTINGS MAPS----

# Three panels: faint on-transect track, sightings coloured by species group
# (all points the same size). Drawn with geom_point() so legend keys are
# plain points.
# Same four groups as the slide 3 bubble grid ("Unidentified whales/dolphins"
# left out)
sight_pts <- sight_slides |>
  filter(!is.na(map_lat), !is.na(map_lon), Group %in% bubble_groups)

slide4 <- ggplot() +
  land_layer +
  geom_sf(data = studyarea_shp, fill = NA, colour = "grey55", linewidth = 0.4) +
  geom_sf(data = track_lines_all, colour = "grey70", linewidth = 0.3) +
  geom_point(data = sight_pts, aes(map_lon, map_lat, colour = Group),
             size = 2, alpha = 0.8) +
  scale_colour_manual(values = group_cols, breaks = bubble_groups, drop = TRUE,
                      guide = guide_legend(override.aes = list(size = 4, alpha = 1))) +
  facet_wrap(~Survey_lab, nrow = 1) +
  map_coords +
  theme_map_slide() +
  theme(legend.key  = element_rect(fill = NA, colour = NA),   # no grey box behind legend points
        # l = space between each point and its label; r = space before the next item
        legend.text = element_text(size = 19, margin = margin(l = 8, r = 22)),
        strip.text  = element_text(face = "bold", size = 21, margin = margin(b = 8)))

slide4 <- slide4 +
  # Small note at the bottom of the slide
  plot_annotation(
    caption = paste("On-effort sightings made while surveying transects in the WEAs"),
    theme = theme(plot.caption = element_text(family = font_family, size = 11, colour = "grey45",
                                              hjust = 0, margin = margin(t = 10))))
slide4
save_slide(slide4, "slide4_sightings-maps", width = 12.3, height = 6)





#####################################################################
# ADDITIONAL SLIDES
#
# Slide 5   Hotspot hex map (sightings per 100 km of effort, per hexagon)
# Slide 6   French/Middle Bank vs Sydney Bight: encounter rates by group
# Slide 7   Seasonal composition (share of sightings by species group)
# Slide 8   Presence calendar (species x season, encounter rates)
# Slide 9   Species at risk (SARA-listed species): maps + counts
# Slide 10  Encounter rates by depth zone
# Slide 11  Dominant species per group and season (top 2)
#
# Encounter rate = on-effort sightings made on transect / km on transect
# (x 100). Only the four species groups of slides 3-4 are used.
#####################################################################


#----ADDITIONAL SLIDES: SHARED PREP----

# Vessel position of each sighting (used to match sightings to the effort
# at the same place: hexagons, strata, depth)
sight_all    <- sight_all    |> mutate(across(c(vessel_lat, vessel_lon), as.numeric))
sight_slides <- sight_slides |> mutate(across(c(vessel_lat, vessel_lon), as.numeric))

# On-transect sightings in the four groups (numerator of encounter rates)
sight_tr <- sight_all |>
  filter(on_transect, Group %in% bubble_groups) |>
  mutate(Group = factor(as.character(Group), levels = bubble_groups))

# On-transect effort segments (denominator). Each ~10 s segment is placed at
# its start position (segments are ~50 m long, so this is precise enough
# for hexagons and depth zones).
seg_tr <- seg_all |>
  filter(Effort %in% transect_states)

strata_labels <- c(FMB = "French/Middle Bank", SB = "Sydney Bight")


#----SLIDE 5: HOTSPOT HEX MAP----

# Hexagons across the two WEAs. In each hexagon:
#   rate = on-transect sightings / on-transect km x 100
# Hexagons with little effort give unreliable rates, so they are drawn in
# grey instead of coloured.
hex_km        <- 10    # hexagon size (km, distance across flat sides)
min_effort_km <- 10    # hexagons with less on-transect effort are greyed out
crs_m         <- 32620 # UTM zone 20N (metres), for an even hexagon grid

strata_m <- st_transform(studyarea_shp, crs_m)

hex <- st_make_grid(strata_m, cellsize = hex_km * 1000, square = FALSE) |>
  st_sf() |>
  mutate(hex_id = row_number())
hex <- hex[lengths(st_intersects(hex, strata_m)) > 0, ]   # only hexagons touching a WEA

# Effort per hexagon and survey
eff_hex <- seg_tr |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326) |>
  st_transform(crs_m) |>
  st_join(hex, join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(hex_id)) |>
  group_by(Survey_lab, hex_id) |>
  summarize(effort_km = sum(dist_km), .groups = "drop")

# Sightings per hexagon and survey (vessel position at the sighting)
sgt_hex <- sight_tr |>
  filter(!is.na(vessel_lat), !is.na(vessel_lon)) |>
  st_as_sf(coords = c("vessel_lon", "vessel_lat"), crs = 4326) |>
  st_transform(crs_m) |>
  st_join(hex, join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(hex_id)) |>
  count(Survey_lab, hex_id, name = "n_sgt")

hex_map <- hex |>
  inner_join(eff_hex |>
               left_join(sgt_hex, by = c("Survey_lab", "hex_id")) |>
               mutate(n_sgt = replace_na(n_sgt, 0L),
                      rate  = 100 * n_sgt / effort_km,
                      low_effort = effort_km < min_effort_km),
             by = "hex_id") |>
  st_transform(4326)

slide5 <- ggplot() +
  land_layer +
  geom_sf(data = filter(hex_map, low_effort), fill = "grey85", colour = "white", linewidth = 0.15) +
  geom_sf(data = filter(hex_map, !low_effort), aes(fill = rate), colour = "white", linewidth = 0.15) +
  geom_sf(data = studyarea_shp, fill = NA, colour = "grey40", linewidth = 0.4) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "sqrt",
                       name = "Sightings per 100 km of survey effort",
                       guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                               barwidth = unit(9, "cm"), barheight = unit(0.4, "cm"))) +
  facet_wrap(~Survey_lab, nrow = 1) +
  map_coords +
  theme_map_slide() +
  theme(legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12),
        strip.text   = element_text(face = "bold", size = 21, margin = margin(b = 8))) +
  labs(caption = paste0("Grey: less than ", min_effort_km, " km of effort in the hexagon (", hex_km,
                        " km across); on-effort sightings on transect, all species groups.")) +
  theme(plot.caption = element_text(family = font_family, size = 11, colour = "grey45", hjust = 0))

slide5
save_slide(slide5, "slide5_hotspots", width = 12.3, height = 6)


#----SLIDE 6: FRENCH/MIDDLE BANK vs SYDNEY BIGHT----

eff_strata <- seg_tr |>
  filter(strata %in% names(strata_labels)) |>
  group_by(Survey_lab, strata) |>
  summarize(effort_km = sum(dist_km), .groups = "drop")

rates_strata <- sight_tr |>
  filter(strata %in% names(strata_labels)) |>
  count(Survey_lab, strata, Group, name = "n") |>
  complete(Survey_lab, strata = names(strata_labels), Group, fill = list(n = 0)) |>
  left_join(eff_strata, by = c("Survey_lab", "strata")) |>
  mutate(rate = 100 * n / effort_km,
         Area = factor(strata_labels[strata], levels = strata_labels))

strata_cols <- c("French/Middle Bank" = "#2a7f87", "Sydney Bight" = "#7a5195")

slide6 <- ggplot(rates_strata, aes(Survey_lab, rate, colour = Area, group = Area)) +
  geom_line(linewidth = 1.2, alpha = 0.6) +
  geom_point(size = 4.5) +
  scale_colour_manual(values = strata_cols) +
  scale_x_discrete(labels = function(x) str_remove(x, " 2026")) +   # "Spring", "Summer", "Fall"
  facet_wrap(~Group, nrow = 1, scales = "free_y") +
  labs(y = "Sightings per 100 km") +
  theme_slide(16) +
  theme(axis.title.y = element_text(size = 14, colour = "grey30"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.spacing = unit(1.2, "cm"),
        strip.text = element_text(face = "bold", size = 17),
        legend.text = element_text(size = 16))

slide6
save_slide(slide6, "slide6_FMB-vs-SB", width = 12.3, height = 5.5)


#----SLIDE 7: SEASONAL COMPOSITION----

# Share of SIGHTINGS (groups encountered) by species group in each season.
# Sightings rather than animals, so a few very large dolphin pods don't hide
# the rest of the community.
comp <- sight_slides |>
  filter(Group %in% bubble_groups) |>
  mutate(Group = factor(as.character(Group), levels = bubble_groups)) |>
  count(Survey_lab, Group) |>
  group_by(Survey_lab) |>
  arrange(Group, .by_group = TRUE) |>
  mutate(share = n / sum(n),
         x_mid = cumsum(share) - share / 2) |>     # centre of each segment (groups left to right)
  ungroup() |>
  mutate(Survey_lab = factor(Survey_lab, levels = rev(survey_labels)),   # Spring on top
         y_num = as.numeric(Survey_lab),
         pct   = scales::percent(share, accuracy = 1))

# Labels: inside the bar when the segment is wide enough (>= 5%); otherwise
# just above the bar, in the group's colour (ggrepel spreads neighbouring
# small labels apart so they don't overlap)
small_cut <- 0.05

slide7 <- ggplot(comp, aes(share, Survey_lab, fill = Group)) +
  geom_col(width = 0.65, position = position_stack(reverse = TRUE)) +
  geom_text(data = filter(comp, share >= small_cut), aes(x = x_mid, y = Survey_lab, label = pct),
            inherit.aes = FALSE, family = font_family, fontface = "bold", colour = "white", size = 5.5) +
  ggrepel::geom_text_repel(data = filter(comp, share < small_cut),
                           aes(x = x_mid, y = y_num + 0.33, label = pct, colour = Group),
                           inherit.aes = FALSE, family = font_family, fontface = "bold", size = 4.5,
                           direction = "x", nudge_y = 0.12, min.segment.length = 0,
                           segment.size = 0.3, box.padding = 0.15, show.legend = FALSE) +
  scale_colour_manual(values = group_cols, guide = "none") +
  scale_fill_manual(values = group_cols, breaks = bubble_groups) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_slide(18) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face = "bold", size = 20),
        legend.text = element_text(size = 17, margin = margin(l = 6, r = 18)),
        legend.key.size = unit(0.6, "cm")) +
  labs(caption = "Share of on-effort sightings on transect (groups encountered), by species group.") +
  theme(plot.caption = element_text(family = font_family, size = 11, colour = "grey45", hjust = 0))

slide7
save_slide(slide7, "slide7_seasonal-composition", width = 12.3, height = 4.5)


#----SLIDE 8: PRESENCE CALENDAR----

# Species identified to species (unknowns and "Fin/sei whale" excluded),
# encounter rate per season; rows grouped by species group, most frequent
# first within each group.
cal <- sight_tr |>
  filter(!grepl("^Unknown|/", Species_base)) |>
  count(Survey, Survey_lab, Group, Species_base, name = "n") |>
  complete(nesting(Survey, Survey_lab), nesting(Group, Species_base), fill = list(n = 0)) |>
  left_join(effort_stats |> select(Survey, transect_km), by = "Survey") |>
  mutate(rate = 100 * n / transect_km)

sp_order <- cal |>
  group_by(Group, Species_base) |>
  summarize(total = sum(rate), .groups = "drop") |>
  arrange(Group, desc(total)) |>
  pull(Species_base)

cal <- cal |>
  mutate(Species_base = factor(Species_base, levels = rev(unique(sp_order))),
         txt_col = if_else(rate > 0.6 * max(rate), "white", "grey15"))

slide8 <- ggplot(cal, aes(Survey_lab, Species_base, fill = rate)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = ifelse(n > 0, sprintf("%.1f", rate), "–"), colour = txt_col),
            family = font_family, size = 4.8, show.legend = FALSE) +
  scale_colour_identity() +
  scale_fill_gradient(low = "#f1f5f9", high = "#1f4e79",
                      name = "Sightings per 100 km on transect",
                      guide = guide_colourbar(title.position = "top", title.hjust = 0.5,
                                              barwidth = unit(8, "cm"), barheight = unit(0.4, "cm"))) +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  facet_grid(Group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme_slide(15) +
  theme(axis.text.x = element_text(face = "bold", size = 18),
        axis.text.y = element_text(size = 14),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 15, hjust = 1),
        panel.spacing = unit(0.3, "cm"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11))

slide8
save_slide(slide8, "slide8_presence-calendar", width = 12.3, height = 6.5)


#----SLIDE 9: SPECIES AT RISK----

# Marine mammals listed on Schedule 1 of Canada's Species at Risk Act
# (Atlantic populations; checked Oct 2026 on canada.ca). Only listed species
# that might occur here are included; extend as needed. Note: harbour
# porpoise, killer whale and sei whale have no Atlantic Schedule 1 listing,
# and "Fin/sei whale" is not counted as a fin whale.
sar_species <- tribble(
  ~Species_base,                 ~SARA_status,
  "Blue whale",                  "Endangered",
  "North Atlantic right whale",  "Endangered",
  "Northern bottlenose whale",   "Endangered",
  "Fin whale",                   "Special Concern",
  "Sowerby's beaked whale",      "Special Concern")

sar_cols <- c("Blue whale" = "#2b6cb0", "North Atlantic right whale" = "#b83280",
              "Northern bottlenose whale" = "#2f855a", "Fin whale" = "#d0743c",
              "Sowerby's beaked whale" = "#6b46c1")

# All sightings of these species in the survey areas (on transect, or
# incidental inside a WEA): for species at risk, every record matters.
sar_sight <- sight_all |>
  filter(on_transect | incidental_in_strata) |>
  inner_join(sar_species, by = "Species_base") |>
  mutate(Species_lab = paste0(Species_base, " (", SARA_status, ")"))

count(sar_sight, Survey_lab, Species_base)   # check (names must match exactly)

sar_present <- sar_species |> filter(Species_base %in% sar_sight$Species_base)
sar_labs    <- setNames(paste0(sar_present$Species_base, " (", sar_present$SARA_status, ")"),
                        sar_present$Species_base)

slide9_maps <- ggplot() +
  land_layer +
  geom_sf(data = studyarea_shp, fill = NA, colour = "grey55", linewidth = 0.4) +
  geom_sf(data = track_lines_all, colour = "grey75", linewidth = 0.3) +
  geom_point(data = filter(sar_sight, !is.na(map_lat)),
             aes(map_lon, map_lat, colour = Species_base), size = 2.6, alpha = 0.85) +
  scale_colour_manual(values = sar_cols, breaks = names(sar_labs), labels = sar_labs,
                      guide = guide_legend(override.aes = list(size = 4))) +
  facet_wrap(~Survey_lab, nrow = 1, drop = FALSE) +
  map_coords +
  theme_map_slide() +
  theme(legend.key  = element_rect(fill = NA, colour = NA),
        legend.text = element_text(size = 16, margin = margin(l = 6, r = 18)),
        strip.text  = element_text(face = "bold", size = 21, margin = margin(b = 8)))

# Counts under each map: sightings (and animals) per species
sar_counts <- sar_sight |>
  group_by(Survey_lab, Species_base) |>
  summarize(n = n(), animals = sum(Best_count, na.rm = TRUE), .groups = "drop") |>
  complete(Survey_lab, Species_base = sar_present$Species_base, fill = list(n = 0, animals = 0)) |>
  mutate(row   = match(Species_base, rev(sar_present$Species_base)),
         label = if_else(n > 0,
                         paste0(Species_base, ": ", n, if_else(n == 1, " sighting", " sightings"),
                                " (", animals, if_else(animals == 1, " animal)", " animals)")),
                         paste0(Species_base, ": none")))

slide9_counts <- ggplot(sar_counts, aes(x = 0.05, y = row, label = label, colour = Species_base)) +
  geom_text(hjust = 0, family = font_family, size = 5, show.legend = FALSE) +
  scale_colour_manual(values = sar_cols) +
  facet_wrap(~Survey_lab, nrow = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0.5, nrow(sar_present) + 0.5)) +
  theme_void() +
  theme(strip.text = element_blank())

slide9 <- slide9_maps / slide9_counts +
  plot_layout(heights = c(3, max(0.6, 0.35 * nrow(sar_present)))) +
  plot_annotation(caption = "Species listed under Canada's Species at Risk Act (Schedule 1). All sightings in the survey areas (on transect and incidental); repeat sightings excluded.",
                  theme = theme(plot.caption = element_text(family = font_family, size = 11,
                                                            colour = "grey45", hjust = 0)))

slide9
save_slide(slide9, "slide9_species-at-risk", width = 12.3, height = 6.5)


#----SLIDE 10: ENCOUNTER RATES BY DEPTH ZONE----

# Water depth at each effort segment and each sighting (vessel position),
# from NOAA ETOPO bathymetry (1 arc-minute; cached after the first download).
# Rates are calculated per depth zone, so zones with more effort don't look
# "busier" just because the ship spent more time there.
depth_breaks <- c(0, 50, 100, 200, Inf)
depth_labels <- c("< 50 m", "50–100 m", "100–200 m", "> 200 m")
min_zone_km  <- 25   # depth zones with less effort in a season are not plotted

bathy_slide <- marmap::getNOAA.bathy(lon1 = map_xlim[1] - 0.2, lon2 = map_xlim[2] + 0.2,
                                     lat1 = map_ylim[1] - 0.2, lat2 = map_ylim[2] + 0.2,
                                     resolution = 1, keep = TRUE)
bathy_r <- marmap::as.raster(bathy_slide)

depth_zone <- function(lon, lat) {
  depth <- -raster::extract(bathy_r, cbind(lon, lat))   # elevation -> depth (m)
  cut(depth, breaks = depth_breaks, labels = depth_labels, right = FALSE)
}

eff_depth <- seg_tr |>
  mutate(zone = depth_zone(vessel_lon, vessel_lat)) |>
  filter(!is.na(zone)) |>
  group_by(Survey_lab, zone) |>
  summarize(effort_km = sum(dist_km), .groups = "drop")

eff_depth   # check: effort per depth zone and season

rates_depth <- sight_tr |>
  filter(!is.na(vessel_lat)) |>
  mutate(zone = depth_zone(vessel_lon, vessel_lat)) |>
  filter(!is.na(zone)) |>
  count(Survey_lab, zone, Group, name = "n") |>
  complete(Survey_lab, zone, Group, fill = list(n = 0)) |>
  inner_join(eff_depth, by = c("Survey_lab", "zone")) |>
  filter(effort_km >= min_zone_km) |>
  mutate(rate = 100 * n / effort_km)

slide10 <- ggplot(rates_depth, aes(zone, rate, colour = Survey_lab, group = Survey_lab)) +
  geom_line(linewidth = 1.1, alpha = 0.6) +
  geom_point(size = 4) +
  scale_colour_manual(values = survey_cols) +
  facet_wrap(~Group, nrow = 1, scales = "free_y") +
  labs(y = "Sightings per 100 km", x = "Water depth") +
  theme_slide(15) +
  theme(axis.title.y = element_text(size = 14, colour = "grey30"),
        axis.title.x = element_text(size = 14, colour = "grey30", margin = margin(t = 6)),
        axis.text.x  = element_text(size = 11),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.spacing = unit(1, "cm"),
        strip.text = element_text(face = "bold", size = 17),
        legend.text = element_text(size = 16, margin = margin(l = 6, r = 18))) +
  labs(caption = paste0("Depth at the vessel position (NOAA ETOPO, 1 arc-minute). Depth zones with less than ",
                        min_zone_km, " km of effort in a season are not shown.")) +
  theme(plot.caption = element_text(family = font_family, size = 11, colour = "grey45", hjust = 0))

slide10
save_slide(slide10, "slide10_depth-zones", width = 12.3, height = 5.5)


#----SLIDE 11: DOMINANT SPECIES PER GROUP AND SEASON----

# Top 2 species (identified to species) in each group and season, by number
# of sightings; share = % of that group's identified sightings that season.
# Same rows and silhouettes as slide 3.
top_sp <- sight_slides |>
  filter(Group %in% bubble_groups, !grepl("^Unknown|/", Species_base)) |>
  mutate(Group = factor(as.character(Group), levels = group_rows)) |>
  count(Survey_lab, Group, Species_base) |>
  group_by(Survey_lab, Group) |>
  mutate(share = n / sum(n)) |>
  slice_max(n, n = 2, with_ties = FALSE) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  mutate(label = paste0(Species_base, "  ", scales::percent(share, accuracy = 1)))

cells <- expand_grid(Survey_lab = factor(survey_labels, levels = survey_labels),
                     Group = factor(group_rows, levels = group_rows))
empty_cells <- anti_join(cells, top_sp, by = c("Survey_lab", "Group"))

slide11_grid <- ggplot() +
  geom_tile(data = cells, aes(Survey_lab, Group), fill = "#f5f7f9", width = 0.94, height = 0.86) +
  geom_text(data = filter(top_sp, rank == 1), aes(Survey_lab, Group, label = label, colour = Group),
            position = position_nudge(y = 0.14), family = font_family, fontface = "bold",
            size = 6, show.legend = FALSE) +
  geom_text(data = filter(top_sp, rank == 2), aes(Survey_lab, Group, label = label),
            position = position_nudge(y = -0.17), family = font_family, size = 4.8, colour = "grey35") +
  geom_text(data = empty_cells, aes(Survey_lab, Group), label = "–", colour = "grey60", size = 6) +
  scale_colour_manual(values = group_cols) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = group_rows) +
  theme_slide(16) +
  theme(axis.text.x = element_text(face = "bold", size = 21, margin = margin(b = 8)),
        axis.text.y = element_blank())

slide11 <- slide3_icons + slide11_grid +
  plot_layout(widths = c(1.5, 4)) +
  plot_annotation(caption = "Most frequently sighted species in each group (% of the group's sightings identified to species); on-effort sightings on transect.",
                  theme = theme(plot.caption = element_text(family = font_family, size = 11,
                                                            colour = "grey45", hjust = 0)))

slide11
save_slide(slide11, "slide11_dominant-species", width = 12.3, height = 6)


#----SLIDE 12: LEATHERBACK TURTLE SIGHTINGS (all surveys on one map)----

# Leatherbacks are not in the marine mammal files, so they are read from each
# survey's all-sightings export. Turtles are always recorded as Incidental,
# so every sighting is shown (resights excluded); dead animals as open circles.
# turtles_in_areas_only = TRUE keeps only sightings inside the WEAs.
turtles_in_areas_only <- FALSE

read_all_sightings <- function(ds) {
  f <- file.path(data_paths[[ds]], "Data_summaries/data_combined_cleaned",
                 paste0("WEAWAVS_", ds, "_all_sightings.csv"))
  if (!file.exists(f)) { message("No all-sightings file for ", ds); return(NULL) }
  read_csv(f, col_types = cols(.default = "c")) |> mutate(Survey = ds)
}

turtles <- map_dfr(surveys, read_all_sightings) |>
  filter(!Resight %in% c("Yes", "Possible"),
         str_remove(Species, " \\(dead\\)$") == "Leatherback turtle") |>
  mutate(across(c(map_lat, map_lon, Best_count), as.numeric),
         Survey_lab = factor(survey_labels[Survey], levels = survey_labels),
         Status     = if_else(str_detect(Species, "\\(dead\\)$"), "Dead", "Live"))

if (turtles_in_areas_only) turtles <- filter(turtles, strata %in% c("FMB", "SB"))

count(turtles, Survey_lab, Status, .drop = FALSE)   # check

# Legend labels with the number of sightings per survey, e.g. "Summer 2026 (5)"
n_turtles   <- count(turtles, Survey_lab, .drop = FALSE)
turtle_labs <- setNames(paste0(n_turtles$Survey_lab, " (", n_turtles$n, ")"), n_turtles$Survey_lab)

# To show/hide the tracklines from all surveys:
show_turtle_tracks <- TRUE   # TRUE = show the on-transect GPS tracks

slide12 <- ggplot() +
  land_layer +
  geom_sf(data = studyarea_shp, fill = NA, colour = "grey55", linewidth = 0.4) +
  { if (show_turtle_tracks) geom_sf(data = track_lines_all, colour = "grey80", linewidth = 0.25) } +
  geom_point(data = filter(turtles, !is.na(map_lat), !is.na(map_lon)) |> arrange(Status == "Dead"),
             aes(map_lon, map_lat, colour = Survey_lab, shape = Status),
             size = 3.5, stroke = 1.1, alpha = 0.9) +
  scale_colour_manual(values = survey_cols, labels = turtle_labs, drop = FALSE,
                      guide = guide_legend(ncol = 1, override.aes = list(size = 4, shape = 16), order = 1)) +
  scale_shape_manual(values = c("Live" = 16, "Dead" = 1),
                     labels = c("Live" = "Live", "Dead" = "Dead (open circle)"),
                     guide = if (any(turtles$Status == "Dead")) guide_legend(ncol = 1, order = 2) else "none") +
  map_coords +
  theme_map_slide() +
  theme(legend.position = "right",
        legend.box      = "vertical",
        legend.box.just = "left",
        legend.key      = element_rect(fill = NA, colour = NA),
        legend.text     = element_text(size = 15, margin = margin(l = 6)),
        legend.spacing.y = unit(0.4, "cm")) +
  labs(caption = str_wrap(paste(
    "WEAWAVS - Leatherback sea turtle sightings.",
    if (show_turtle_tracks)
      "Grey lines: transects surveyed during 'On' or 'Bridge-On' effort (all three surveys combined)."
    else ""), width = 110)) +
  theme(plot.caption.position = "plot",
        plot.margin  = margin(t = 25, r = 10, b = 10, l = 10),
        plot.caption = element_text(family = font_family, size = 11, colour = "grey45",
                                    hjust = 0, lineheight = 1.1, margin = margin(t = 30)))
slide12
save_slide(slide12, "slide12_leatherback-turtles", width = 10, height = 6.5)



#----SLIDE 13: SIGHTINGS AND ANIMALS PER SPECIES AND SEASON----

# Every species (unknown categories included) per season, plus a Total
# column: number of sightings, with the number of animals (sum of best
# counts) in brackets. Resights excluded. Reads the all-sightings exports
# (uses read_all_sightings() from slide 12).
#
# Three versions:
#   13A: on-effort sightings made on the transects (On / Bridge-On), in the
#        WEAs; same definition as slides 3-11
#   13B: all sightings, within and outside the WEAs (incl. transits),
#        all sighting types (on-effort and incidental)
#   13C: all sighting types (on-effort and incidental), within the WEAs
#        (FMB + SB) only; matches the written summary

spp_base <- map_dfr(surveys, read_all_sightings) |>
  filter(!Resight %in% c("Yes", "Possible")) |>
  mutate(Best_count   = as.numeric(Best_count),
         Species_base = str_remove(Species, " \\(dead\\)$"),
         Survey_lab   = factor(survey_labels[Survey], levels = survey_labels),
         Group = coalesce(unname(species_groups[Species_base]),
                          if_else(Species_base == "Leatherback turtle", "Sea turtles", NA_character_)))

# Species not shown (fish, sharks, "Other"...): check nothing important is dropped
spp_base |> filter(is.na(Group)) |> count(Species_base)
spp_base <- spp_base |> filter(!is.na(Group))

# How sightings split by type and effort (check Sgt_type / Effort_at_sgt exist)
count(spp_base, Survey, Sgt_type, Effort_at_sgt)

# Notes on filters...
# Slides 3–11 count a sighting as on-transect using effort alone. 13A also requires 
# the sighting’s strata to be FMB or SB, so a sighting made from a transect near a WEA boundary, 
# with the animal positioned just outside it, counts on slides 3–11 but not on 13A.
# This one line shows whether any such sightings exist:
sight_all |> filter(on_transect) |> count(Survey, in_WEA = strata %in% c("FMB", "SB"))
# If every row says in_WEA = TRUE, 13A matches the other slides. If some say FALSE, there are two options:
# - Keep the strata filter: 13A is then strictly “within the WEAs”.
# - Drop strata %in% c("FMB", "SB") from spp_A: 13A then matches slides 3–11 and the encounter rates.

spp_A <- spp_base |>
  filter(#strata %in% c("FMB", "SB"),
         Sgt_type %in% "On-effort",
         Effort_at_sgt %in% transect_states)

spp_B <- spp_base

# For the written summary: within the WEAs, all sighting types
spp_wea_all <- spp_base |> filter(strata %in% c("FMB", "SB"))

spp_groups <- c(group_levels, "Sea turtles")

# Builds the counts table and the figure for one set of sightings
make_spp_slide <- function(d, caption_txt) {
  
  # Counts per season, plus a Total column
  counts <- d |>
    group_by(Survey_lab, Group, Species_base) |>
    summarize(n = n(), animals = sum(Best_count, na.rm = TRUE), .groups = "drop") |>
    complete(Survey_lab, nesting(Group, Species_base), fill = list(n = 0, animals = 0)) |>
    mutate(Survey_lab = as.character(Survey_lab))
  
  counts <- bind_rows(
    counts,
    counts |>
      group_by(Group, Species_base) |>
      summarize(n = sum(n), animals = sum(animals), .groups = "drop") |>
      mutate(Survey_lab = "Total"))
  
  # Row order: by group, then most-sighted species first, unknown categories last
  sp_order <- counts |>
    filter(Survey_lab == "Total") |>
    mutate(Group = factor(Group, levels = spp_groups),
           unknown = grepl("^Unknown|/", Species_base)) |>
    arrange(Group, unknown, desc(n)) |>
    pull(Species_base)
  
  counts <- counts |>
    mutate(Group        = factor(Group, levels = spp_groups),
           Species_base = factor(Species_base, levels = rev(sp_order)),
           Column       = factor(Survey_lab, levels = c(survey_labels, "Total")),
           label = if_else(n > 0, paste0(n, "  (", format(animals, big.mark = ",", trim = TRUE), ")"), "–"),
           is_total     = Column == "Total")
  
  # Check: totals match the number of sightings
  stopifnot(sum(counts$n[counts$is_total]) == nrow(d))
  
  p <- ggplot(counts, aes(Column, Species_base)) +
    # Season cells shaded by number of sightings; Total column in plain grey
    geom_tile(data = filter(counts, !is_total), aes(fill = n), colour = "white", linewidth = 1.2) +
    geom_tile(data = filter(counts, is_total), fill = "#e9edf1", colour = "white", linewidth = 1.2) +
    geom_text(aes(label = label, fontface = if_else(is_total, "bold", "plain")),
              family = font_family, size = 4.2, colour = "grey15") +
    scale_fill_gradient(low = "#f4f7fa", high = "#8fb3d6", guide = "none") +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    # Group names on one line; "Unidentified whales/dolphins" shortened so it
    # fits its single-row band
    facet_grid(Group ~ ., scales = "free_y", space = "free_y", switch = "y",
               labeller = labeller(Group = replace(setNames(spp_groups, spp_groups),
                                                   "Unidentified whales/dolphins", "Unidentified"))) +
    theme_slide(14) +
    theme(axis.text.x = element_text(face = "bold", size = 17),
          axis.text.y = element_text(size = 13),
          strip.placement = "outside",
          strip.clip = "off",
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 13, hjust = 1),
          panel.spacing = unit(0.25, "cm")) +
    labs(caption = str_wrap(caption_txt, width = 140)) +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(family = font_family, size = 11, colour = "grey45",
                                      hjust = 0, lineheight = 1.1, margin = margin(t = 10)))
  
  list(counts = counts, plot = p)
}

slide13a <- make_spp_slide(
  spp_A,
  paste("Number of sightings (number of animals) per species. On-effort sightings made on the",
      "transects in the wind energy areas (French/Middle Bank and Sydney Bight);",
      "repeat sightings excluded."))

slide13b <- make_spp_slide(
  spp_B,
  paste("Number of sightings (number of animals) per species. All sighting types (on-effort and",
        "incidental), within and outside the wind energy areas, including transits;",
        "repeat sightings excluded."))

slide13c <- make_spp_slide(
  spp_wea_all,
  paste("Number of sightings (number of animals) per species. All sighting types (on-effort and",
        "incidental), within the wind energy areas (French/Middle Bank and Sydney Bight);",
        "repeat sightings excluded."))

slide13a$plot
save_slide(slide13a$plot, "slide13a_species-totals_WEA-on-effort", width = 12.3, height = 7)

slide13b$plot
save_slide(slide13b$plot, "slide13b_species-totals_all", width = 12.3, height = 7)

slide13c$plot
save_slide(slide13c$plot, "slide13c_species-totals_WEA-all", width = 12.3, height = 7)





# ---- Numbers for the written summary ----

# Settings (two independent filters):
#   sum_area:  where the sighting was (sighting position, strata)
#              "WEA" = sighting positioned inside FMB or SB
#              "all" = no spatial filter
#   sum_types: what the vessel was doing at the time
#              "on_transect" = on-effort sightings during On / Bridge-On
#                              (transits already excluded); slides 3-11, 13A
#              "on_effort"   = all on-effort sightings, incl. Transit-On
#              "all"         = all sighting types (on-effort and incidental),
#                              incl. those during transits
# Note: with "on_transect", "all" vs "WEA" differs only by sightings seen
# from a transect but positioned just outside a WEA boundary.

sum_area  <- "all"
sum_types <- "on_transect"

stopifnot(sum_area  %in% c("WEA", "all"),
          sum_types %in% c("on_transect", "on_effort", "all"))

spp_all <- spp_base |>
  filter(sum_area == "all" | strata %in% c("FMB", "SB"),
         case_when(sum_types == "on_transect" ~ Sgt_type %in% "On-effort" &
                     Effort_at_sgt %in% transect_states,
                   sum_types == "on_effort"   ~ Sgt_type %in% "On-effort",
                   TRUE                       ~ TRUE))

spp_counts <- make_spp_slide(spp_all, "")$counts

cat("\nArea:", sum_area, "| Sighting types:", sum_types,
    "| Sightings:", nrow(spp_all), "\n\n")

# 1. Totals per season and overall
spp_all |>
  group_by(Survey_lab) |>
  summarize(sightings = n(), animals = sum(Best_count, na.rm = TRUE)) |>
  bind_rows(summarize(spp_all, Survey_lab = "Total", sightings = n(),
                      animals = sum(Best_count, na.rm = TRUE))) |>
  print()

# 2. By group per season, with share of sightings
spp_all |>
  count(Survey_lab, Group, name = "sightings") |>
  left_join(spp_all |> group_by(Survey_lab, Group) |>
              summarize(animals = sum(Best_count, na.rm = TRUE), .groups = "drop"),
            by = c("Survey_lab", "Group")) |>
  group_by(Survey_lab) |>
  mutate(pct_sightings = round(100 * sightings / sum(sightings))) |>
  ungroup() |> as_tibble() |> print(n = Inf)

# 3. Species table (sightings [animals]) by season and total
spp_counts |>
  mutate(cell = paste0(n, " [", animals, "]")) |>
  select(Group, Species_base, Survey_lab, cell) |>
  pivot_wider(names_from = Survey_lab, values_from = cell) |>
  arrange(Group, desc(Species_base)) |> as_tibble() |> print(n = Inf)

# 4. Species identified (excludes "Unknown ..." and "x/y" categories;
#    leatherback is included in this list when present)
spp_all |>
  filter(!grepl("^Unknown|/", Species_base)) |>
  distinct(Survey_lab, Species_base) |>
  count(Survey_lab, name = "n_species") |> print()
spp_all |> filter(!grepl("^Unknown|/", Species_base)) |>
  distinct(Species_base) |> pull() |> sort()

# 5. Species at risk, plus dead animals, by season and area
#    (strata = NA or other values are sightings outside the WEAs)
spp_all |>
  filter(Species_base %in% c("Blue whale", "Fin whale", "Leatherback turtle") |
           grepl("\\(dead\\)", Species)) |>
  group_by(Survey_lab, Species, strata) |>
  summarize(sightings = n(), animals = sum(Best_count, na.rm = TRUE), .groups = "drop") |>
  as_tibble() |> print(n = Inf)

# 6. Split by area per season (for the spatial bullets)
spp_all |>
  group_by(Survey_lab, strata, Group) |>
  summarize(sightings = n(), animals = sum(Best_count, na.rm = TRUE), .groups = "drop") |>
  as_tibble() |> print(n = Inf)
