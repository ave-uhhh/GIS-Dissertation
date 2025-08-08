# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# All statistical analysis codes must be run after running this code

# --------------------------------------------------------------- STATISTICAL ANALYSIS ---------------------------------------------------------------

# Install new libraries and load all libraries needed for statistical analysis and related plots
install.packages("car")
install.packages("corrplot")
install.packages("broom")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("gratia")
install.packages("mgcv")
install.packages("patchwork")
install.packages("RColorBrewer")
install.packages("vegan")
library(car) # Regression tools
library(corrplot) # Visualizes correlation matrices
library(broom) # Tidies model outputs into data frames
library(ggplot2) # Data visualization
library(ggrepel) # Label placement in ggplot2 plots
library(gratia) # Visualization and diagnostics for GAMs
library(mgcv) # Fits GAMs
library(patchwork) # Combines multiple plots into a layout
library(RColorBrewer) # Color palettes for plots
library(tidyverse)
library(vegan) # Multivariate analysis

# Load datasets
wq_data <- read_csv("water_quality_data/wq_data_cleaned.csv")
landcover_data <- read.csv("servir_landcover_output/wqm_landcover_percent_by_year.csv")
abundance <- read.csv("ecological_health_data/abundance.csv")
richness <- read.csv("ecological_health_data/richness.csv")
aspt <- read.csv("ecological_health_data/aspt.csv")
site_match <- read.csv("join_metadata/wq_corresponding_eh_final.csv")
predictor_site_match <- read.csv("join_metadata/wq_corresponding_eh.csv")

# Create folder for stats outputs
dir.create("statistical_analysis")
dir.create("figures")
dir.create("statistical_analysis/gams")

# -------------------------- PREP WQ DATA --------------------------
# Define numeric WQ variables (categorical/predictor variables are sites, land use type, and year)
water_quality_vars <- c(
  "temp_c", "ph", "tss_mgl", "cond_msm", "totn_mgl",
  "totp_mgl", "do_mgl", "codmn_mgl", "fc_mpn_100ml", "bod_mgl"
)

# Filter for dry season months (December, January, February & March) and take median value
dry_median <- wq_data %>%
  filter(month_collected %in% c(12, 1, 2, 3)) %>%
  filter(year_collected %in% c(2011, 2013, 2015, 2017, 2019, 2021)) %>%
  group_by(statid, year_collected, country_code) %>%
  summarise(across(all_of(water_quality_vars), median, na.rm = TRUE), .groups = "drop")

# Check missingness
missing_summary <- dry_median %>%
  summarise(across(all_of(water_quality_vars), ~mean(is.na(.)) * 100))

print(missing_summary)

# Drop heavily missing variables
reduced_vars <- setdiff(water_quality_vars, c("bod_mgl", "fc_mpn_100ml"))

# -------------------------- PREP EH DATA --------------------------
# Pivot eh data to long format
abund_long <- abundance %>%
  pivot_longer(cols = starts_with("X"), names_to = "year_collected", values_to = "abundance") %>%
  mutate(
    year_collected = as.numeric(sub("X", "", year_collected)),
  ) %>%
  select(code, year_collected, abundance)

rich_long <- richness %>%
  pivot_longer(cols = starts_with("X"), names_to = "year_collected", values_to = "richness") %>%
  mutate(
    year_collected = as.numeric(sub("X", "", year_collected)),
  ) %>%
  select(code, year_collected, richness)

aspt_long <- aspt %>%
  pivot_longer(cols = starts_with("X"), names_to = "year_collected", values_to = "aspt") %>%
  mutate(
    year_collected = as.numeric(sub("X", "", year_collected)),
  ) %>%
  select(code, year_collected, aspt)

# Join eh metrics together
macro_metrics <- abund_long %>%
  left_join(rich_long, by = c("code", "year_collected")) %>%
  left_join(aspt_long, by = c("code", "year_collected"))


# -------------------------- JOIN WQ AND EH --------------------------
# Drop NA's and join data
wq_eh_merge <- site_match %>%
  left_join(select(dry_median, statid, year_collected),
            by = c("wq_statid" = "statid")) %>%
  left_join(macro_metrics, by = c("eh_code" = "code", "year_collected")) %>%
  left_join(select(dry_median, statid, year_collected, all_of(reduced_vars)),
            by = c("wq_statid" = "statid", "year_collected")) %>%
  filter(!is.na(aspt))

# Ensure joins worked cleanly
glimpse(wq_eh_merge)

# -------------------------- PREP LAND USE DATA --------------------------
# Aggregate land use categories
land_use <- landcover_data %>%
  mutate(
    forest = Class_5 + Class_6 + Class_7 + Class_10 + Class_11 + Class_18,
    agriculture = Class_3 + Class_4 + Class_12 + Class_13 + Class_14 + Class_1,
    urban = Class_16 + Class_2,
    semi_natural = Class_8 + Class_9 + Class_15 + Class_17
  ) %>%
  select(statid, Year, forest, agriculture, urban, semi_natural) %>%
  rename(year_collected = Year)

# -------------------------- JOIN WQ AND LU --------------------------
wq_lu_merge <- dry_median %>%
  left_join(land_use, by = c("statid", "year_collected")) %>%
  drop_na(forest, agriculture, urban, semi_natural)

# -------------------------- JOIN WQ, EH, LU --------------------------
wq_eh_lu_merge <- wq_eh_merge %>%
  left_join(select(land_use, statid, year_collected, urban, agriculture, forest),
            by = c("wq_statid" = "statid", "year_collected")) %>%
  select(year_collected, all_of(reduced_vars), aspt, richness, abundance, urban, agriculture, forest, type) %>%
  drop_na()
