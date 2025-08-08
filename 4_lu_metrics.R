# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# ------------------------------------------------ CALCULATE LAND USE METRICS ------------------------------------------------

# Install new libraries and load all libraries
install.packages("purr")
install.packages("readr")
install.packages("stringr")
library(dplyr)
library(purrr)  # Map functions
library(readr)  # Reading text files
library(readxl) 
library(stringr)  # String manipulation
library(terra)
library(tidyr)


# Load polygon
study_area <- vect("spatial_data/lu_study_area/lu_study_area.shp")

# Load rasters
years <- c(2011, 2013, 2015, 2017, 2019, 2021)
lulc_files <- paste0("spatial_data/servir_landcover/servir_", years, ".tif")
lulc_rasters <- lapply(lulc_files, rast)
names(lulc_rasters) <- years

# Define land use class groupings
class_groups <- list(
  forest = c(5, 6, 7, 10, 11, 18),
  agriculture = c(1, 3, 4, 12, 13, 14),
  urban = c(2, 16),
  semi_natural = c(8, 9, 15, 17)
)


# -------------- CALCULATE LU PER CATEGORY WITHIN STUDY AREA -------------

# Function to extract and process values
extract_grouped_percents <- function(raster, year) {
  # Mask the raster using the polygon
  masked <- mask(crop(raster, study_area), study_area)
  
  # Get all pixel values as vector
  values <- values(masked, mat = FALSE)
  values <- na.omit(values)  # Remove NA values
  
  # Tabulate counts
  freq_table <- as.data.frame(table(values))
  colnames(freq_table) <- c("Class", "Count")
  freq_table$Class <- as.integer(as.character(freq_table$Class))
  
  # Group into categories and compute percent
  freq_table <- freq_table %>%
    mutate(Category = case_when(
      Class %in% class_groups$forest ~ "forest",
      Class %in% class_groups$agriculture ~ "agriculture",
      Class %in% class_groups$urban ~ "urban",
      Class %in% class_groups$semi_natural ~ "semi_natural",
      TRUE ~ "other"
    )) %>%
    group_by(Category) %>%
    summarise(Total = sum(Count), .groups = "drop") %>%
    mutate(Year = year, Percent = 100 * Total / sum(Total)) %>%
    select(Year, Category, Percent)
}

# Apply to all years
lulc_summary <- lapply(seq_along(lulc_rasters), function(i) {
  extract_grouped_percents(lulc_rasters[[i]], years[i])
}) %>% bind_rows()

# Save
write.csv(lulc_summary, "servir_landcover_output/lulc_percent_summary.csv", row.names = FALSE)


# -------------- REFORMAT DATA -------------

# Define years to process
years <- c(2011, 2013, 2015, 2017, 2019, 2021)

# Initialize empty list to store ALL catchment data
catchment_list <- list()

# Loop through each year and read data
for (year in years) {
  file_path <- paste0("servir_landcover_output/catchment_landcover/landcover_", year, "_catchment_summary.csv")
  
  if (file.exists(file_path)) {
    df_catchment <- read_csv(file_path) %>%
      mutate(Year = year)  # Add Year column for tracking
    
    # Pivot to restructure data
    df_catchment_transformed <- df_catchment %>%
      select(Catchment, land_cover_class, count, Year) %>%
      pivot_wider(names_from = land_cover_class, values_from = count, names_prefix = "Class_") %>%
      replace(is.na(.), 0)  # Fill missing classes with zeros
    
    catchment_list[[as.character(year)]] <- df_catchment_transformed
  } else {
    print(paste("File not found:", file_path))
  }
}

# Combine all years into one dataset
catchment_combined <- bind_rows(catchment_list)

# Save to CSV
write_csv(catchment_combined, "servir_landcover_output/catchment_landcover/catchment_landcover_all_years.csv")


# Initialize empty list to store ALL BioRA data
biora_list <- list()

# Loop through each year and read data
for (year in years) {
  file_path <- paste0("servir_landcover_output/biora_landcover/landcover_", year, "_biora_summary.csv")
  
  if (file.exists(file_path)) {
    df_biora <- read_csv(file_path) %>%
      mutate(Year = year)  # Add Year column for tracking
    
    # Pivot to restructure data
    df_biora_transformed <- df_biora %>%
      select(Zone, land_cover_class, count, Year) %>%
      pivot_wider(names_from = land_cover_class, values_from = count, names_prefix = "Class_") %>%
      replace(is.na(.), 0)  # Fill missing classes with zeros
    
    biora_list[[as.character(year)]] <- df_biora_transformed
  } else {
    print(paste("File not found:", file_path))
  }
}

# Combine all years into one dataset
biora_combined <- bind_rows(biora_list)

# Save to CSV
write_csv(biora_combined, "servir_landcover_output/biora_landcover/biora_landcover_all_years.csv")


# -------------- SUMMARIZE LU FOR EACH WQ SITE -------------

# Load station metadata and clean
station_meta <- read.csv("join_metadata/wq_catchments_for_landcover.csv") %>%
  mutate(across(everything(), ~str_trim(as.character(.))))
wq_sites <- read_csv("water_quality_data/selected_wq_sites.csv") %>%
  mutate(code = str_trim(as.character(code)))

# Define years
years <- c(2011, 2013, 2015, 2017, 2019, 2021)

# Load land use files
load_landuse <- function(years, folder_path, file_suffix, level) {
  map_dfr(years, function(yr) {
    file <- paste0(folder_path, "landcover_", yr, file_suffix)
    read_csv(file) %>%
      mutate(Year = yr, Level = level)
  })
}

catchment_lu <- load_landuse(
  years,
  "servir_landcover_output/catchment_landcover/",
  "_catchment_summary.csv",
  "Catchment"
)

biora_lu <- load_landuse(
  years,
  "servir_landcover_output/biora_landcover/",
  "_biora_summary.csv",
  "Biora"
)

# Summarize land use by unit
summarize_area <- function(df, zone_col) {
  df %>%
    group_by(across(all_of(c(zone_col, "Year", "land_cover_class")))) %>%
    summarise(area_m2 = sum(area_m2), .groups = "drop")
}

catchment_area <- summarize_area(catchment_lu, "Catchment")
biora_area     <- summarize_area(biora_lu, "Zone")

# Expand station metadata across all years and normalize
station_years <- expand_grid(station_meta, Year = years) %>%
  mutate(across(everything(), ~str_trim(as.character(.)))) %>%
  mutate(
    biora.zone = str_to_upper(na_if(biora.zone, "")),
    catchment  = na_if(catchment, ""),
    Year = as.numeric(Year)
  )

biora_area <- biora_area %>%
  mutate(Zone = str_to_upper(str_trim(Zone))) # clean and standardize zone

# -------------- 4.4.1 Aggregate raw areas by station-year

# Tributary-only stations
trib_stations <- station_years %>%
  filter(is.na(biora.zone)) %>%
  pivot_longer(cols = starts_with("catchment"), names_to = NULL, values_to = "Catchment_ID") %>%
  filter(!is.na(Catchment_ID)) %>%
  left_join(catchment_area, by = c("Catchment_ID" = "Catchment", "Year")) %>%
  group_by(code, Year, land_cover_class) %>%
  summarise(area_m2 = sum(area_m2), .groups = "drop")

# Biora-only stations
biora_only <- station_years %>%
  filter(!is.na(biora.zone) & is.na(catchment)) %>%
  left_join(biora_area, by = c("biora.zone" = "Zone", "Year")) %>%
  group_by(code, Year, land_cover_class) %>%
  summarise(area_m2 = sum(area_m2), .groups = "drop")

# Mixed stations (zone + catchment)
biora_zone_data <- station_years %>%
  filter(!is.na(biora.zone) & !is.na(catchment)) %>%
  select(code, Year, biora.zone) %>%
  distinct() %>%
  left_join(biora_area, by = c("biora.zone" = "Zone", "Year"))

catchment_data <- station_years %>%
  filter(!is.na(biora.zone) & !is.na(catchment)) %>%
  pivot_longer(cols = starts_with("catchment"), names_to = NULL, values_to = "Catchment_ID") %>%
  filter(!is.na(Catchment_ID)) %>%
  left_join(catchment_area, by = c("Catchment_ID" = "Catchment", "Year"))

biora_catch_combined <- bind_rows(
  biora_zone_data %>% select(code, Year, land_cover_class, area_m2),
  catchment_data  %>% select(code, Year, land_cover_class, area_m2)
) %>%
  filter(!is.na(area_m2)) %>%
  group_by(code, Year, land_cover_class) %>%
  summarise(area_m2 = sum(area_m2), .groups = "drop")

# -------------- LU PERCENT CALCULATION -------------

# Note: 4.4 must be run beofre this section

# Convert areas to percent cover
convert_to_percent <- function(df) {
  df %>%
    filter(!is.na(land_cover_class)) %>%
    group_by(code, Year) %>%
    mutate(Total = sum(area_m2, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Percent = (area_m2 / Total) * 100) %>%
    select(-area_m2, -Total) %>%
    pivot_wider(
      names_from = land_cover_class,
      values_from = Percent,
      names_prefix = "Class_",
      values_fill = list(Percent = 0)
    )
}

trib_pct   <- convert_to_percent(trib_stations)
biora_pct2 <- convert_to_percent(biora_only)
mixed_pct  <- convert_to_percent(biora_catch_combined)

# Merge all station-year cover summaries
station_lu_final <- bind_rows(
  trib_pct   %>% mutate(Source = "Catchment"),
  biora_pct2 %>% mutate(Source = "Biora"),
  mixed_pct  %>% mutate(Source = "Zone + Catchment")
)

# Add station ids
station_lu_final <- station_lu_final %>%
  left_join(wq_sites, by = "code")
# Remove unnecessay columns from wq_sites join
station_lu_final <- station_lu_final %>%
  select(-Longitude, -Latitude, -River_Names_og, -Country)

# Export
write_csv(station_lu_final, "servir_landcover_output/wqm_landcover_percent_by_year.csv")

# -------------- LU AREA M2 CALCULATION -------------

# Note: 4.4 must be run beofore this section

# Pivot area data to wide format by land cover class (same structure, just with area_m2)
pivot_area_data <- function(df) {
  df %>%
    filter(!is.na(land_cover_class)) %>%
    pivot_wider(
      names_from = land_cover_class,
      values_from = area_m2,
      names_prefix = "Class_",
      values_fill = list(area_m2 = 0)
    )
}

trib_area   <- pivot_area_data(trib_stations)
biora_area2 <- pivot_area_data(biora_only)
mixed_area  <- pivot_area_data(biora_catch_combined)

# Add Source label and combine
station_area_final <- bind_rows(
  trib_area   %>% mutate(Source = "Catchment"),
  biora_area2 %>% mutate(Source = "Biora"),
  mixed_area  %>% mutate(Source = "Zone + Catchment")
)

# Export to CSV
write_csv(station_area_final, "servir_landcover_output/wqm_landcover_area_by_year.csv")



# -------------- LU AREA CHANGE -------------

# Note: 4.4 must be run beofore this section

# Pivot and calculate percent of each class per year
regional_percents <- station_area_final %>%
  filter(Year %in% c(2011, 2013, 2015, 2017, 2019, 2021)) %>%
  pivot_longer(cols = starts_with("Class_"), names_to = "land_cover_class", values_to = "area_m2") %>%
  group_by(Year, land_cover_class) %>%
  summarise(total_area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  mutate(percent_of_total = (total_area_m2 / sum(total_area_m2, na.rm = TRUE)) * 100) %>%
  ungroup() %>%
  select(-total_area_m2) %>%
  pivot_wider(names_from = Year, values_from = percent_of_total, names_prefix = "Year_") %>%
  mutate(relative_percent_change_2011_2021 = ((Year_2021 - Year_2011) / Year_2011) * 100)

# Export
write_csv(regional_percents, "servir_landcover_output/wqm_landcover_change_by_class.csv")

# land use change for the whole region by aggregated type
## define class groups
class_grouping <- station_area_final %>%
  mutate(
    forest = Class_5 + Class_6 + Class_7 + Class_10 + Class_11,
    agriculture = Class_3 + Class_4 + Class_12 + Class_13 + Class_14 + Class_1,
    urban = Class_16 + Class_2,
    semi_natural = Class_8 + Class_9 + Class_15 + Class_17
  ) %>%
  select(code, Year, forest, agriculture, urban, semi_natural) %>%
  rename(year_collected = Year)

# Calculate total area by land use category and year
regional_group_percents <- class_grouping %>%
  filter(year_collected %in% c(2011, 2013, 2015, 2017, 2019, 2021)) %>%
  pivot_longer(cols = c(forest, agriculture, urban, semi_natural),
               names_to = "land_use", values_to = "area_m2") %>%
  group_by(year_collected, land_use) %>%
  summarise(total_area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop") %>%
  group_by(year_collected) %>%
  mutate(percent_of_total = (total_area_m2 / sum(total_area_m2)) * 100) %>%
  ungroup() %>%
  select(-total_area_m2) %>%
  pivot_wider(names_from = year_collected, values_from = percent_of_total, names_prefix = "Year_") %>%
  mutate(relative_percent_change_2011_2021 = ((Year_2021 - Year_2011) / Year_2011) * 100)

# Export
write_csv(regional_group_percents, "servir_landcover_output/wqm_landcover_change_by_group.csv")
