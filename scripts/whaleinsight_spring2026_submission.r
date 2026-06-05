################################################################################
# Retroactive submission of WEAWAVS Spring survey GPS tracks + Sightings
# to WhaleInsight
################################################################################

# Steps:
# 1) Import all Sighting csv files and add a "Calves_count" column next to "Max_count" and set all values to NA 
#   - What add "Calves_count"? -- This is because WhaleInsight would like to know whether there were calves present or not (and how many; hence the count rather than just presence/absence), as this may influence management decisions particularly for NARW.
#   - Why set all values to "NA"? -- This is because we didn't record information about group composition (or just calves) in the WEAWAVS Spring 2026 survey. Thus, setting values to "0" would be misleading.
# 2) Export each individual csv back to their original date (YYYYMMDD) folder 
# 3) Re-name each Sighting and GPS csv file to comply with the nomenclature requested by WhaleInsight.

# Load packages
library(dplyr)
library(here)
library(purrr)
library(tidyverse)
library(stringr)

# Set wd
setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVS/Working data/WEAWAVS_Data_Directory_Spring2026_2026-05-12_FINAL_WORKING")

# List all Sighting csv files
sight_files <- list.files(file.path(paste0(here(), "/WhaleInsight_submissions")),
                          pattern = "Sighting\\.csv$", 
                          recursive = TRUE, full.names = TRUE)

# Import files while retaining source file path
sight <- map_df(sight_files, 
                ~ read_csv(.x, col_types = cols(.default = col_character()),
                           show_col_types = FALSE) |>
                  mutate(source_file = .x))

# Add "Calves_count" column & remove duplicates
sight2 <- sight |>
  mutate(Calves_count = NA)|>
  relocate(Calves_count, .after = Max_count) |>
  # Remove any possible or confident resights
  filter(!Resight %in% c("Possible", "Yes"))

table(sight2$Resight)

# Split back into original files
sight_split <- split(sight2, sight2$source_file)

# Write each file back to the "WhaleInsight_submissions" folder
iwalk(sight_split, 
      ~ {cat("Writing:", .y, "\n")
    write_csv(select(.x, -source_file), .y, na = "") })


# Rename both Sighting and GPS files to:
# 2026-MM-DD_DFO_WEAWAVS_Sighting.csv  
# 2026-MM-DD_DFO_WEAWAVS_GPS_track.csv

# Find all csv files
files <- list.files(
  file.path(here(), "WhaleInsight_submissions"),
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE)

for (f in files) {
  
  # Folder name (e.g. "20260423")
  folder_date <- basename(dirname(f))
  
  # Convert YYYYMMDD -> YYYY-MM-DD
  formatted_date <- str_replace(
    folder_date,
    "^(\\d{4})(\\d{2})(\\d{2})$",
    "\\1-\\2-\\3")
  
  old_name <- basename(f)
  
  # Determine new filename
  if (old_name == "Sighting.csv") {
    
    new_name <- paste0(formatted_date, "_DFO_WEAWAVS_Sighting.csv")
    
  } else if (str_detect(old_name,
                        "^Mysticetus_GPS_\\d{8}\\.csv$")) {
    
    new_name <- paste0(formatted_date, "_DFO_WEAWAVS_GPS_track.csv")
    
  } else {
    
    # Skip any other files
    next
  }
  
  new_path <- file.path(dirname(f), new_name)
  
  success <- file.rename(f, new_path)
  
  cat(
    if (success) "✓" else "✗",
    basename(f),
    "->",
    basename(new_path),
    "\n")
}





