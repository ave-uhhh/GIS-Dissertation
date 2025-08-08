# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# ------------------------------------------------ DATA EXTRACTION FROM MRC WFS ------------------------------------------------

# Install and load libraries
install.packages("sf")
install.packages("httr")
library(sf) # For working with spatial data using simple features
library(httr) # For handling web APIs

# -------------- MRC WFS Extraction -------------

# Define the WMS URL and layer
wfs_base <- "https://geo.mrcmekong.org/geo/mrc/ows?"

# Target layers
layers <- c("EHM_station_CRMN", "BioRA_Zone", "River_Mekong", "River_maintributary", "Catchment", "LMB")

# Specify output directory
base_output_dir <- "spatial_data"

# Loop through layers
for (layer_name in layers) {
  message("Downloading: ", layer_name)
  
  # Construct full WFS URL for this layer
  wfs_url <- paste0(
    wfs_base,
    "service=WFS&version=1.1.0&request=GetFeature&typeName=", layer_name, "&outputFormat=application/json"
  )
  
  # Read as GeoJSON from WFS
  vec <- st_read(wfs_url, quiet = TRUE)
  
  # Reproject to ESPG 32648
  vec_espg <- st_transform(vec, 32648) 
  
  # Clean layer name for file paths
  safe_layer_name <- gsub("[:% ]", "_", layer_name)  # Replace colons, spaces, and percent signs with underscores
  
  # Create a subfolder for each layer
  layer_dir <- file.path(base_output_dir, layer_name)
  if (!dir.exists(layer_dir)) {
    dir.create(layer_dir, recursive = TRUE)
  }
  
  # Save as shapefile inside its respective folder
  out_path <- file.path(layer_dir, paste0(layer_name, ".shp"))
  st_write(vec_espg, out_path, delete_layer = TRUE, quiet = TRUE)
  
  message("Saved: ", out_path)
}



# -------------- Extract Country Boundaries Separately --------------
# (would not work in the loop due to naming conventions)

wfs_base <- "https://geo.mrcmekong.org/geo/mrc/ows?"

# Target layer for country boundaries
layer_name <- URLencode("mrc:Country Boundary", reserved = TRUE)

# Construct full WFS request
wfs_url <- paste0(
  wfs_base,
  "service=WFS&version=1.1.0&request=GetFeature",
  "&typeName=", layer_name,
  "&outputFormat=application/json"
)

# Read the layer
country_boundary <- st_read(wfs_url)

# Transform to ESPG 32648
country_boundary_proj <- st_transform(country_boundary, 4326)

# Drop Z (and M if present) dimensions
country_boundary_2d <- st_zm(country_boundary_proj)

# Create the output folder and save the shapefile
dir.create("spatial_data/countries", recursive = TRUE)
st_write(country_boundary_2d, "spatial_data/countries/countries.shp")




# -------------- Extract WQM separately --------------

wfs_base <- "https://geo.mrcmekong.org/geo/mrc/ows?"

# Helper function to create output directories
create_output_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

# Save Water Quality Stations
extract_wqm <- function(output_dir) {
  create_output_dir(output_dir)
  
  layer_name <- URLencode("Water Quality Stations", reserved = TRUE)
  
  wfs_url <- paste0(
    wfs_base,
    "service=WFS&version=1.1.0&request=GetFeature",
    "&typeName=", layer_name,
    "&outputFormat=application/json"
  )
  
  message("Fetching WQM data...")
  wqm <- st_read(wfs_url)
  
  st_write(wqm, file.path(output_dir, "wqm_stations.shp"), delete_dsn = TRUE)
  
  return(wqm)
}

# Prepare and save country boundaries
save_wqm <- function(wqm, output_dir, crs = 32648) {
  create_output_dir(output_dir)
  
  wqm_proj <- st_transform(wqm, crs)
  wqm_2d <- st_zm(wqm_proj)
  
  st_write(wqm_2d, file.path(output_dir, "wqm.shp"), delete_dsn = TRUE)
}

# Run the full pipeline
output_dir <- "spatial_data/wqm"

wqm <- extract_wqm(output_dir)
save_wqm(wqm, output_dir)

