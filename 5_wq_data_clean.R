# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# ---------------------------------- CLEAN WATER QUALITY DATA -------------------------------------

# Install and load packages
install.packages("lubridate")
install.packages("readxl")
install.packages("tidyverse")
library(lubridate) # For formatting
library(readxl) # For reading Excel files
library(tidyverse) # Loads full tidyverse collection: dplyr, ggplot2, tidyr, readr, purrr, tibble, stringr, and forcats

# Read in the data shared by the MRC
data <- read.csv("water_quality_data/WQMN_data_for_Ava.csv")

# View basic structure
glimpse(data)
summary(data)

# Clean column names
data <- data %>%
  rename_with(tolower)

colnames(data) # check new column names

# Remove 2024 data (its in a different format, ugly, and unnecessary)
data <- data %>%
  filter(year_collected != 2024)

# Remove data for three unnecessary stations
data <- data %>%
  filter(!(name %in% c("Tan Thanh")))

# Check class type and convert
str(data)

## See nonnumeric data in each column that will cause conversion issues
lapply(data[, c("tidehl", "flow_m3s", "temp_c", "ph", "tss_mgl", "cond_msm", "totn_mgl", "totp_mgl", "do_mgl", "codmn_mgl", "fc_mpn_100ml", "bod_mgl")], function(x) unique(x[!grepl("^[-0-9\\.]+$", x)]))

## Convert and remove problematic data
data <- data %>%
  mutate(across(c(tidehl, flow_m3s, temp_c, ph, tss_mgl, cond_msm, totn_mgl, totp_mgl, do_mgl, codmn_mgl, fc_mpn_100ml, bod_mgl),
                ~ifelse(grepl("^[-0-9\\.]+$", .), as.numeric(.), NA)))

## Verify conversion
sapply(data, class)

# Look at unique station name combinations
unique_combos <- data %>%
  distinct(name, statid) %>%
  arrange(statid, name)

print(unique_combos)

# Change wrong station ids
data <- data %>%
  mutate(statid = case_when(
    name == "Backprea" ~ "H020107",
    name == "Chrouy Changvar" ~ "H019801",
    name == "Kampong Loung" ~ "H020106",
    name == "Koh Thom" ~ "H033403",
    name == "Kaorm Samnor" ~ "H019807",
    name == "Prek Kdam" ~ "H020102",
    name == "Angdoung Meas" ~ "H0440103",
    TRUE ~ statid  # Keep original values for others
  ))

# Add three digit station code
selected_sites <- read.csv("water_quality_data/selected_wq_sites.csv")

## rename column from wq data
data <- data %>% rename(station_name = name)

## add relevant columns
data <- data %>%
  left_join(selected_sites %>% select(statid, station_name, code, river_name_sf), 
            by = c("statid", "station_name"))

## rename code column
data <- data %>% rename(station_code = code)

glimpse(data)

# Save as new clean file to use
write_csv(data, "water_quality_data/wq_data_cleaned.csv")

# Filter for relevant years and dry season months
dry_season_years_data <- data %>%
  filter(year_collected %in% c(2011, 2013, 2015, 2017, 2019, 2021)) %>%
  filter(month_collected %in% c(12, 1, 2, 3))

# Save as relevant time series file to use
write_csv(dry_season_years_data, "water_quality_data/wq_data_relevant_year_month.csv")


