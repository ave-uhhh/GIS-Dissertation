# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# ------------------------------------------------ LANDCOVER DATA PROCESSING ------------------------------------------------

# Note: landcover rasters for 2011, 2013, 2015, 2017, 2019, and 2021 were obtained from SERVIR-Mekong and saved in a folder called "servir_landcover"

# Install new libraries and load all libraries
install.packages("terra")
install.packages("tmap")
library(dplyr)
library(sf) 
library(terra) # For spatial analysis
library(tmap) # Mapping and spatial visualization

# ---------- Check the extent of LMB and landcover raster ----------

# Load example landcover raster for plotting
landcover_21 <- rast("spatial_data/servir_landcover/servir_2021.tif")

# Load shapefiles as vectors
mekong <- vect("spatial_data/River_Mekong/River_Mekong.shp")
lmb <- vect("spatial_data/LMB/LMB.shp")

# Check crs and extent
crs(landcover_21)
crs(mekong)
crs(lmb)
ext(landcover_21)
ext(mekong)
ext(lmb)

# Plot using the extent of the LMB shapefile
plot(landcover_21, main = "Full LMB with Raster Overlay" )
lines(mekong, col = "blue")
lines(lmb, col = "orange")


# ---------- Determine Catchment Landcover -----------

# Load catchment shapefile
catchments <- st_read("spatial_data/Catchment/Catchment.shp")

# Define years and corresponding raster file paths
years <- c(2011, 2013, 2015, 2017, 2019, 2021)
raster_paths <- paste0("spatial_data/servir_landcover/servir_", years, ".tif")

# Output folders
output_folder <- "servir_landcover_output/catchment_landcover/"
catchment_raster_folder <- paste0(output_folder, "catchment_rasters/")
dir.create(catchment_raster_folder, recursive = TRUE, showWarnings = FALSE)

# Loop through each year and process land use data
for (j in seq_along(years)) {
  year <- years[j]
  raster_path <- raster_paths[j]
  
  # Load and reproject raster
  lu_raster <- rast(raster_path)
  lu_raster <- project(lu_raster, "EPSG:32648", res=c(30, 30))
  
  # Get cell area
  cell_area <- prod(res(lu_raster))
  print(res(lu_raster))  # Ensure it's still (30, 30)
  
  zone_summaries <- list()
  
  # Iterate over catchments
  for (i in seq_len(nrow(catchments))) {
    zone_geom <- catchments[i, ]  # Select catchment geometry
    zone_name <- zone_geom$Ctchmn_
    zone_safe <- gsub("[^a-zA-Z0-9_]", "_", zone_name)
    
    # Convert sf object to SpatVector for terra functions
    zone_geom_vect <- vect(zone_geom)
    
    # Check if extents overlap before cropping
    if (!relate(ext(lu_raster), ext(zone_geom_vect), "intersects")) {
      message("Skipping ", zone_name, " for year ", year, " - No overlap with raster")
      next
    }
    
    # Mask landcover raster by catchment
    lc_zone <- mask(crop(lu_raster, zone_geom_vect), zone_geom_vect)
    
    # Save masked raster
    #out_raster_path <- paste0(catchment_raster_folder, zone_safe, "_", year, ".tif")
    #writeRaster(lc_zone, out_raster_path, overwrite = TRUE)
    
    # Frequency table
    freq_df <- as.data.frame(freq(lc_zone)) %>%
      filter(!is.na(value))
    
    if (nrow(freq_df) == 0) next
    
    freq_df <- freq_df %>%
      mutate(
        area_m2 = count * cell_area,
        Catchment = zone_name,
        Year = year
      ) %>%
      rename(land_cover_class = value)
    
    zone_summaries[[i]] <- freq_df
  }
  
  # Combine summaries for the year
  catchment_landcover_summary <- bind_rows(zone_summaries)
  
  # Save as CSV file
  csv_path <- paste0(output_folder, "landcover_", year, "_catchment_summary.csv")
  write.csv(catchment_landcover_summary, csv_path, row.names = FALSE)
  
  message("Saved land cover summary for ", year, " at ", csv_path)
}

message("All years processed successfully!")

# Ensure calculated and actual catchment area are similar / the same
## Filter for Nam Ou catchment
nam_ou <- catchments %>% filter(Ctchmn_ == "NAM OU")

## Calculate area in square meters
nam_ou_area_m2 <- st_area(nam_ou)

## Print results
print(paste("Nam Ou Catchment Area:", round(nam_ou_area_m2, 2), "m²")) # Compare result to that of csv


# ---------- Determine Mekong / BioRA Landcover ----------

# Load BioRA river zones and apply buffer once (15km)
biora <- st_read("spatial_data/BioRA_Zone/BioRA_Zone.shp") %>%
  st_transform(32648)

biora <- biora %>%
  group_by(Zone) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

biora_buffered <- st_buffer(biora, dist = 15000)  # Apply buffer ONCE before loop
biora_buf_vect <- vect(biora_buffered)  # Convert to SpatVector for terra functions

# Define years and raster file paths
years <- c(2011, 2013, 2015, 2017, 2019, 2021)
raster_paths <- paste0("spatial_data/servir_landcover/servir_", years, ".tif")

# Output folder
output_folder <- "servir_landcover_output/biora_landcover/"
biora_raster_folder <- paste0(output_folder, "biora_rasters/")
dir.create(biora_raster_folder, recursive = TRUE, showWarnings = FALSE)

# Loop through each year
for (j in seq_along(years)) {
  year <- years[j]
  raster_path <- raster_paths[j]
  
  # Load and reproject raster
  lu_raster <- rast(raster_path)
  lu_raster <- project(lu_raster, "EPSG:32648", res = c(30, 30))
  
  # Get cell area
  cell_area <- prod(res(lu_raster))
  
  zone_summaries <- list()
  
  # Iterate over buffered BioRA zones
  for (i in seq_len(nrow(biora_buffered))) {
    zone_geom <- biora_buf_vect[i]  # Select buffered river geometry
    zone_name <- biora$Zone[i]  # Use original zone name
    zone_safe <- gsub("[^a-zA-Z0-9_]", "_", zone_name)
    
    # Check if extents overlap before cropping
    if (!relate(ext(lu_raster), ext(zone_geom), "intersects")) {
      message("Skipping ", zone_name, " for year ", year, " - No overlap with raster")
      next
    }
    
    # Mask landcover raster by buffered BioRA zone
    lc_zone <- mask(crop(lu_raster, zone_geom), zone_geom)
    
    # Save masked raster
    # out_raster_path <- paste0(output_folder, "biora_rasters", zone_safe, "_", year, ".tif")
    # writeRaster(lc_zone, out_raster_path, overwrite = TRUE)
    
    # Frequency table
    freq_df <- as.data.frame(freq(lc_zone)) %>%
      filter(!is.na(value))
    
    if (nrow(freq_df) == 0) next
    
    freq_df <- freq_df %>%
      mutate(
        area_m2 = count * cell_area,
        Zone = zone_name,
        Year = year
      ) %>%
      rename(land_cover_class = value)
    
    zone_summaries[[i]] <- freq_df
  }
  
  # Combine summaries for the year
  landcover_summary <- bind_rows(zone_summaries)
  
  # Save as CSV file
  csv_path <- paste0(output_folder, "landcover_", year, "_biora_summary.csv")
  write.csv(landcover_summary, csv_path, row.names = FALSE)
  
  message("Saved land cover summary for ", year, " at ", csv_path)
}

message("All years processed successfully!") 



