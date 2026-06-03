#################################################
## Download an offline basemap (GeoTIFF or tiles)
#################################################

# Notes
# Run this once before fieldwork
# Store in /spatial/
# Works fully offline afterward

setwd("C:/Users/gavrilchukk/OneDrive - DFO-MPO/DFO MARITIMES_CRMP/WEAWAVeS/End-of-day R scripts")

# Load packages
library(maptiles)
library(sf)

# Load study area
study_area <- st_read("spatial/Study_area_orig.shp")

# Option A: {maptiles} 

# Download tiles (OpenStreetMap)
tiles <- get_tiles(
  study_area,
  provider = "OpenStreetMap",
  zoom = 8,
  crop = TRUE)

# Save as GeoTIFF
terra::writeRaster(tiles, "spatial/basemap.tif", overwrite = TRUE)


# Option B: Higher-quality basemap (satellite)
tiles <- get_tiles(
  study_area,
  provider = "Esri.WorldImagery",
  zoom = 8,
  crop = TRUE)

# Save as GeoTIFF
terra::writeRaster(tiles, "spatial/basemap_higher_res.tif", overwrite = TRUE)

