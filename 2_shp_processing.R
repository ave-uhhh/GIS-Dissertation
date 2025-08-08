# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# ------------------------------------------------ PROCESS SHAPEFILES ------------------------------------------------

# Install new libraries and load all libraries
install.packages("dplyr")
install.packages("tidyr")
library(dplyr) # Data manipulation and transformation
library(sf)
library(tidyr) # Data tidying and reshaping

# Load shapefiles
lmb <- st_read("spatial_data/LMB/LMB.shp") %>%
  st_transform("EPSG:32648")

catchment <- st_read("spatial_data/Catchment/Catchment.shp") %>%
  st_transform("EPSG:32648")

country <- st_read("spatial_data/countries/countries.shp") %>%
  st_transform("EPSG:32648")

mekong <- st_read("spatial_data/River_Mekong/River_Mekong.shp") %>%
  st_transform("EPSG:32648")

biora <- st_read("spatial_data/BioRA_Zone/BioRA_Zone.shp") %>%
  st_transform("EPSG:32648")

tributary <- st_read("spatial_data/River_maintributary/River_maintributary.shp") %>%
  st_transform("EPSG:32648")

ehm <- st_read("spatial_data/EHM_station_CRMN/EHM_station_CRMN.shp") %>%
  st_transform("EPSG:32648")

wqm <- st_read("spatial_data/wqm/wqm.shp") %>%
  st_transform("EPSG:32648")

# Load data
eh_data <- read.csv("ecological_health_data/eh_siteranking.csv")
wq_data <- read.csv("water_quality_data/wq_data_cleaned.csv")
# Load metadata for joins
select_stations_md <- read.csv("join_metadata/wq_corresponding_eh_final.csv")
select_catchments_md <- read.csv("join_metadata/wq_catchments_for_landcover_final.csv")


# -------------- EXTRACT RELEVANT EHM -------------

# Extract selected EHM
sf_select_ehm <- ehm[ehm$Site_name %in% select_stations_md$eh_sf_name, ]

# Check match
length(unique(select_stations_md$eh_sf_name))
length(unique(sf_select_ehm$Site_name))

# Create a new folder (if it doesn't already exist)
dir.create("spatial_data/select_ehm", showWarnings = FALSE)

# Save the shapefile and read for later
st_write(sf_select_ehm, "spatial_data/select_ehm/select_ehm.shp", delete_layer = TRUE)
select_ehm <- st_read("spatial_data/select_ehm/select_ehm.shp")

# -------------- EXTRACT RELEVANT WQM -------------

# Extract selected WQM
sf_select_wqm <- wqm[wqm$Sttn_nm %in% select_stations_md$wq_station_name, ]

# Check match
length(unique(select_stations_md$wq_station_name))
length(unique(sf_select_wqm$Sttn_nm))

# Create a new folder (if it doesn't already exist)
dir.create("spatial_data/select_wqm", showWarnings = FALSE)

# Save the shapefile and read for later
st_write(sf_select_wqm, "spatial_data/select_wqm/select_wqm.shp", delete_layer = TRUE)
select_wqm <- st_read("spatial_data/select_wqm/select_wqm.shp")

# -------------- EXTRACT RELEVANT CATCHMENTS -------------

# Gather all catchment names into one column
catchment_names <- select_catchments_md %>%
  select(catchment, catchment_2, catchment_3) %>%
  pivot_longer(everything(), names_to = "source", values_to = "catchment_name") %>%
  filter(!is.na(catchment_name)) %>%
  distinct(catchment_name) %>%
  pull(catchment_name)

# Extract seleected catchments
sf_select_catchments <- catchment %>%
  filter(Ctchmn_ %in% catchment_names)

# Create a new folder (if it doesn't already exist)
dir.create("spatial_data/select_catchments", showWarnings = FALSE)

# Save the shapefile and read for later
st_write(sf_select_catchments, "spatial_data/select_catchments/select_catchments.shp", delete_layer = TRUE)
select_catchments <- st_read("spatial_data/select_catchments/select_catchments.shp")


# -------------- EXTRACT RELEVANT TRIBUTARIES -------------

# Extract tributaries in selected catchments
sf_select_tribs <- tributary %>%
  st_filter(sf_select_catchments, .predicate = st_intersects)

## Create a new folder (if it doesn't already exist)
dir.create("spatial_data/select_tribs", showWarnings = FALSE)

## Save the shapefile and read for later
st_write(sf_select_tribs, "spatial_data/select_tribs/select_tribs.shp", delete_layer = TRUE)
select_tribs <- st_read("spatial_data/select_tribs/select_tribs.shp")


# Extract major LMB tributaries
## Create a vector of target tributary names
major_trib_names <- c("Bassac", "Nam Khan", "Nam Mae Ing", "Nam Mae Kok", "Nam Mun",
                      "Nam Ngum", "Nam Ou", "Nam Song", "Nam Tha", "Se Bang Fai",
                      "Se Bang Hieng", "Se Kong", "Se San", "Sre Pok", "Tonle Sap",
                      "Nam Ka Dinh", "Nam Songkhram")

## Check column names in tributary layer
sort(unique(tributary$River_Name))
names(tributary)

## Filter based on correct column
tribs_major <- tributary %>%
  filter(River_Name %in% major_trib_names)

## Create a new folder (if it doesn't already exist)
dir.create("spatial_data/major_tribs", showWarnings = FALSE)

## Save the shapefile and read for later
st_write(tribs_major, "spatial_data/major_tribs/tribs_major.shp", delete_layer = TRUE)
major_tribs <- st_read("spatial_data/major_tribs/tribs_major.shp")


# -------------- CREATE LU STUDY AREA BOUNDARY -------------

# Buffer Mekong
biora_buffered <- biora %>%
  summarise(geometry = st_union(geometry)) %>%
  st_buffer(dist = 15000)

# Combine & simplify study area
combined_area <- rbind(
  select_catchments %>% select(geometry),
  biora_buffered %>% select(geometry)
)
study_region <- st_union(combined_area)  # merge catchments and buffer into single shape
study_region_clipped <- st_intersection(study_region, lmb)  # clip to LMB

# Create a new folder (if it doesn't already exist)
dir.create("spatial_data/lu_study_area", showWarnings = FALSE)

# Save the shapefile and load for later
st_write(study_region_clipped, "spatial_data/lu_study_area/lu_study_area.shp", delete_layer = TRUE)
lu_study_area <- st_read("spatial_data/lu_study_area/lu_study_area.shp")

