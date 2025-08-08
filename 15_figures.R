# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# ------------------------------------------- VISUALISATIONS ----------------------------------  

# Install new libraries and load all libraries
install.packages("cowplot")
install.packages("forcats")
install.packages("ggspatial")
install.packages("grid")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("scales")
library(corrplot)
library(cowplot) # Enhances ggplot2
library(forcats) # Tools for working with categorical variables
library(ggplot2)
library(ggspatial)  # Adds scale bars, north arrows, and other spatial elements to ggplots
library(grid) # Graphics system
library(patchwork)
library(rnaturalearth)  # Access to Natural Earth vector map data
library(rnaturalearthdata) # Provides Natural Earth dataset
library(scales) # Customize scales and labels in visualizations
library(sf) 
library(terra)
library(tidyverse)
library(tmap)

# Load shapefiles and transform to consistent CRS
lmb <- st_read("spatial_data/LMB/LMB.shp") 
catchment <- st_read("spatial_data/Catchment/Catchment.shp") 
mekong <- st_read("spatial_data/River_Mekong/River_Mekong.shp") 
biora <- st_read("spatial_data/BioRA_Zone/BioRA_Zone.shp") 
tributary <- st_read("spatial_data/River_maintributary/River_maintributary.shp")
select_ehm <- st_read("spatial_data/select_ehm/select_ehm.shp") 
select_wqm <- st_read("spatial_data/select_wqm/select_wqm.shp")
select_catchments <- st_read("spatial_data/select_catchments/select_catchments.shp") 
select_tribs <- st_read("spatial_data/select_tribs/select_tribs.shp")
tribs_major <- st_read("spatial_data/major_tribs/tribs_major.shp")
study_area <- st_read("spatial_data/lu_study_area/lu_study_area.shp") 

# ------------------------------------------- OVERVIEW MAP ----------------------------------  

# Transform CRS of relevant layers
lmb_3857 <- st_transform(lmb, 3857)
mekong_3857 <- st_transform(mekong, 3857)
tribs_major_3857 <- st_transform(tribs_major, 3857)
select_wqm_3857 <- st_transform(select_wqm, 3857)
select_ehm_3857 <- st_transform(select_ehm, 3857)

# Load country outlines with R Natural Earth Data
countries <- ne_countries(scale = "medium", returnclass = "sf")

# Define bounding box (xmin, ymin, xmax, ymax) in lon/lat
bbox <- st_bbox(c(xmin = 98, ymin = 8, xmax = 110, ymax = 24), crs = st_crs(4326))

# Crop country outlines to bounding box
countries_clipped <- st_crop(countries, bbox)

# Reproject to match your study layers
countries_clipped_3857 <- st_transform(countries_clipped, 3857)

# Load ocean boundaries from R Natural Earth and transform CRS
oceans <- ne_download(scale = "medium",
                      type  = "ocean",
                      category = "physical",
                      returnclass = "sf") %>%
  st_transform(crs = st_crs(countries_clipped_3857))

# Define bounding box from your countries layer
bbox_extent <- st_bbox(countries_clipped_3857)

# Clip ocean layer to bounding box
oceans_clipped <- st_crop(oceans, bbox_extent)

# Assign custom country fill colors
countries_clipped_3857 <- countries_clipped_3857 %>%
  mutate(fill_type = case_when(
    name == "Thailand" ~ "#FFE785",
    name == "Vietnam"  ~ "#FFEEAA",
    name == "Cambodia" ~ "#F5D34C",
    name == "Laos"     ~ "#FFF7D6",
    TRUE               ~ "#E5E5E5"
  ))

#  Add country label coordinates
countries_clipped_3857 <- countries_clipped_3857 %>%
  mutate(label_x = st_coordinates(st_centroid(geometry))[, 1],
         label_y = st_coordinates(st_centroid(geometry))[, 2])

# Tag feature types for the legend
mekong_3857$type <- "Mekong River"
tribs_major_3857$type <- "Major Tributaries"
select_wqm_3857$type <- "WQM Station"
select_ehm_3857$type <- "EHM Station"
lmb_3857$type <- "LMB Boundary"

# Create a bounding box with matching CRS
bbox <- st_as_sfc(st_bbox(countries_clipped_3857), crs = st_crs(countries_clipped_3857))

# Make map
overview_map <- ggplot() +
  geom_sf(data = oceans_clipped, fill = "#deebf7", color = NA) +
  
  # COUNTRY FILLS (using identity scale with no legend conflict)
  geom_sf(data = countries_clipped_3857, aes(fill = fill_type), color = "grey30", size = 0.3, show.legend = FALSE) +
  scale_fill_identity() +
  
  # Mekong & tributaries
  geom_sf(data = mekong_3857, aes(color = "Mekong River"),      size = 0.4) +
  geom_sf(data = tribs_major_3857, aes(color = "Major Tributaries"), size = 0.4) +
  
  # LMB outline mapped for legend
  geom_sf(data = lmb_3857, aes(color = "LMB Boundary"), fill = NA, linewidth = 0.9) +
  
  # WQM & EHM stations
  geom_sf(data = select_wqm_3857, aes(shape = "WQM Station"), fill = "#E090FF",
          color = "black", size = 2) +
  geom_sf(data = select_ehm_3857, aes(shape = "EHM Station"), fill = "#FFA928",
          color = "black", size = 1) +
  
  # unified legend scales
  scale_color_manual(name = "Feature", values = c(
    "Mekong River"      = "#0788C9",
    "Major Tributaries" = "#84B8D2",
    "LMB Boundary"      = "black"
  )) +
  scale_shape_manual(name = "Station Type", values = c(
    "WQM Station" = 21,
    "EHM Station" = 24
  )) +
  
  # Styling
  annotation_scale(location = "br", width_hint = 0.3, pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm")) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_fancy_orienteering,
                         pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm")) +
  coord_sf(expand = FALSE) + # Prevents ggplot from adding margin space around the map
  
  theme_minimal() +
  theme(
    plot.margin     = margin(5, 5, 5, 5),
    plot.title      = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x    = element_blank(),
    axis.title.y    = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.grid      = element_blank(),
    panel.border    = element_blank()  # this removes the bounding box frame
  ) +
  labs(title = "Lower Mekong Basin Overview") +
  theme(
    plot.title = element_text(hjust = 0.5),  # centers the title over the plot panel
  )

print(overview_map)

# Save map
ggsave("figures/overview_map.png", overview_map, width = 12, height = 6, dpi = 300)


# ------------------------------------------- LAND USE FIGURES  ----------------------------------  

# Load and prepare spatial data
landuse_2011 <- rast("spatial_data/servir_landcover/servir_2011.tif")
landuse_2021 <- rast("spatial_data/servir_landcover/servir_2021.tif")

# Reclassify and group land use classes
rcl <- matrix(c(
  5, 1, 6, 1, 7, 1, 10, 1, 11, 1, 18, 1,
  3, 2, 4, 2, 12, 2, 13, 2, 14, 2, 1, 2,
  16, 3, 2, 3,
  8, 4, 9, 4, 15, 4, 17, 4
), byrow = TRUE, ncol = 2)

# Create color palette 
palette <- c("darkgreen", "goldenrod1", "red", "skyblue")
labels <- c("Forest", "Agriculture", "Urban", "Semi-natural")

# Clip study area to lmb
study_region_clipped <- st_intersection(study_area, lmb)

# Reproject Study Area shapefile
study_region_clipped <- transform(study_region_clipped, "EPSG:32648")

# convert to SpatVector
study_vect <- vect(st_sf(geometry = study_region_clipped))
lmb_vect <- vect(lmb)

# Clip and reclassify 2011 land use
cookie_2011 <- mask(crop(landuse_2011, study_vect), study_vect)
cookie_2011_reclass <- classify(cookie_2011, rcl, others = NA)

# Clip and reclassify 2021 land use
cookie_2021 <- mask(crop(landuse_2021, study_vect), study_vect)
cookie_2021_reclass <- classify(cookie_2021, rcl, others = NA)

# Plot 2011
tmap_mode("plot")

lu_2011 <- 
  tm_shape(lmb) +
  tm_borders(col = "darkred", lwd = 2) +   # main borders
  tm_shape(cookie_2011_reclass, raster.downsample = FALSE) +
  tm_raster(
    palette = palette,
    title = "Land Use Categories",
    labels = labels
  ) +
  tm_shape(study_region_clipped) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(lmb) + 
  tm_polygons(alpha = 0) +                 # keep it if needed for layering
  tm_compass(type = "arrow", size = 5.5, position = c("left", "bottom")) +
  tm_scale_bar(
    position = c("left", "bottom"),
    breaks = c(0, 100, 200, 300, 400),
    text.size = 1.6,
    lwd = 1.5,
    color.dark = "black",
    color.light = "white"
  ) +
  tm_layout(
    legend.show = FALSE,
    frame = TRUE,
    title = "2011 Land Use",
    title.size = 3,
    title.position = c("right", "top")
  )

print(lu_2011)

# Save
tmap_save(tm = lu_2011, filename = "figures/2011_landuse.png", width = 8, height = 11, dpi = 300)

# Plot 2021
lu_2021 <- 
  tm_shape(lmb) +
  tm_borders(col = "darkred", lwd = 2) +   # main borders
  tm_shape(cookie_2021_reclass, raster.downsample = FALSE) +
  tm_raster(
    palette = palette,
    title = "Land Use Categories",
    labels = labels
  ) +
  tm_shape(study_region_clipped) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(lmb) + 
  tm_polygons(alpha = 0) +                 # keep it if needed for layering
  tm_compass(type = "arrow", size = 5.5, position = c("left", "bottom")) +
  tm_scale_bar(
    position = c("left", "bottom"),
    breaks = c(0, 100, 200, 300, 400),
    text.size = 1.6,
    lwd = 1.5,
    color.dark = "black",
    color.light = "white"
  ) +
  tm_layout(
    legend.show = FALSE,
    frame = TRUE,
    title = "2021 Land Use",
    title.size = 3,
    title.position = c("right", "top")
  )

# Save
tmap_save(tm = lu_2021, filename = "figures/2021_landuse.png", width = 8, height = 11, dpi = 300)


# -------------------------- NATURAL LAND LOST MAP --------------------------

# create change raster
## Combine 2011 and 2021 into a single raster stack
lu_stack <- c(cookie_2011_reclass, cookie_2021_reclass)

# Create a change raster: encode transitions as "from_to" values
change_raster <- app(lu_stack, fun = function(x) {
  if (is.na(x[1]) | is.na(x[2])) {
    return(NA)
  } else {
    return(x[1] * 10 + x[2])  # e.g. 1→3 becomes 13
  }
})

# Define codes for forest and semi-natural loss
loss_codes <- c(12, 13, 14, 42, 43)  # Forest → Agriculture/Urban/Semi-natural; Semi-natural → Agriculture/Urban

# loss raster
loss_raster <- classify(change_raster, rcl = matrix(c(loss_codes, rep(1, length(loss_codes))), ncol = 2), others = NA)

# Convert raster to data frame for ggplot
loss_df <- as.data.frame(loss_raster, xy = TRUE, na.rm = TRUE)
colnames(loss_df)[3] <- "loss"

# Convert vector layers to sf
lmb_sf <- st_transform(st_as_sf(lmb), crs = 32648)
cookie_sf <- st_transform(st_as_sf(study_region_clipped), crs = 32648)

# Get bounding box
bbox_poly <- st_as_sfc(st_bbox(cookie_sf))

# Add a new column to distinguish boundaries
lmb_sf_clean <- st_sf(
  boundary_type = "LMB Boundary",
  geometry = st_geometry(lmb_sf)
)

cookie_sf_clean <- st_sf(
  boundary_type = "Study Region",
  geometry = st_geometry(cookie_sf)
)

# Combine safely
boundary_sf <- rbind(lmb_sf_clean, cookie_sf_clean)

# Map
lu_change <- ggplot() +
  
  # Fill Study Region with grey
  geom_sf(data = cookie_sf_clean, fill = "grey90", color = NA) +
  
  geom_raster(data = loss_df, aes(x = x, y = y), fill = "firebrick") +
  
  # Outline Study Region for legend
  geom_sf(data = cookie_sf_clean, aes(color = boundary_type), fill = NA, size = 0.8) +
  
  # Plot LMB Boundary second (on top)
  geom_sf(data = lmb_sf_clean, aes(color = boundary_type), fill = NA, size = 0.8) +
  
  # Bounding box
  geom_sf(data = bbox_poly, fill = NA, color = "black", size = 0.8) +
  
  # Scale bar and north arrow
  annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.8) +
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0, "cm"),
    pad_y = unit(1.2, "cm")
  ) +
  
  # Legend
  scale_color_manual(
    values = c("LMB Boundary" = "black", "Study Region" = "darkgrey"),
    name = "Boundary"
  ) +
  
  # Layout
  coord_sf(
    xlim = st_bbox(cookie_sf)[c("xmin", "xmax")],
    ylim = st_bbox(cookie_sf)[c("ymin", "ymax")],
    expand = FALSE
  ) +
  labs(
    title = "Forest & Semi-natural Land Loss (2011–2021)",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("figures/lu_change.png", lu_change, width = 12, height = 6, dpi = 300)


# -------------------------- LAND USE GRAPH --------------------------

# Load base data
lulc_summary <- read_csv("servir_landcover_output/lulc_percent_summary.csv")

# make sure Year is a factor for discrete bars
lulc_summary_long <- lulc_summary %>%
  filter(Category %in% c("forest", "agriculture", "urban", "semi_natural")) %>%
  mutate(
    Category = factor(Category, levels = c("urban", "agriculture", "forest", "semi_natural")),
    Year = factor(Year)
  )

# Reformat names
lulc_summary_long$Category <- recode(
  lulc_summary_long$Category,
  "urban" = "Urban",
  "agriculture" = "Agriculture",
  "forest" = "Forest",
  "semi_natural" = "Semi-natural"
)

# Calculate height of bar
bar_tops <- lulc_summary_long %>%
  group_by(Year) %>%
  summarise(Top = sum(Percent))

# Merge with bar tops
urban_labels <- lulc_summary_long %>%
  filter(Category == "Urban") %>%
  left_join(bar_tops, by = "Year") %>%
  mutate(Label = paste0(round(Percent, 2), "%"))

# Stacked bar plot with arrow over urban land 
lu_change_graph <- ggplot(lulc_summary_long, aes(x = Year, y = Percent, fill = Category)) +
  geom_bar(stat = "identity", width = 0.7) +
  
  # Labels for other categories inside the bars
  geom_text(
    data = filter(lulc_summary_long, Category != "Urban"),
    aes(label = paste0(round(Percent, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 5.5,
    color = "#000000"
  ) +
  
  # Urban labels at top of each bar
  geom_text(
    data = urban_labels,
    aes(x = Year, y = Top + 2, label = Label),
    inherit.aes = FALSE,
    size = 5.5,
    color = "#000000",
  ) +
  
  scale_fill_manual(values = c(
    "Urban" = "#e31a1c",  
    "Agriculture" = "#F7CF72",
    "Forest" = "#A6C46A",
    "Semi-natural" = "#8DCBBD"
  )) +
  
  labs(
    title = "Land Use Composition in Lower Mekong Basin (2011–2021)",
    x = "Year",
    y = "Percentage of Total Area",
    fill = "Land Use Category"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

print(lu_change_graph)

ggsave("figures/lu_change_graph.png", lu_change_graph, width = 12, height = 6, dpi = 300)


# ------------------------------------------- STUDY METHODS MAP  ----------------------------------  

# Ensure Zone is a factor
biora$Zone <- as.factor(biora$Zone)

# Dissolve BioRA zones
biora_dissolved <- biora %>%
  group_by(Zone) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Buffer each zone
biora_buffered <- st_buffer(biora_dissolved, dist = 15000)

# Add group labels for legend mapping
catchment$group <- "Catchments"
select_catchments$group <- "Selected Catchments"
tributary$group <- "Tributaries"
select_tribs$group <- "Relevant Tributaries"
select_wqm$Station <- "WQM Stations"

# Color lookup table
zone_colors <- c(
  "BioRA Zone 1" = "#66a61e",
  "BioRA Zone 2" = "#1b9e77",
  "BioRA Zone 3" = "#0042DC",
  "BioRA Zone 4" = "#3F18B5",
  "BioRA Zone 5" = "#e7298a",
  "BioRA Zone 6" = "#AE0020",
  "BioRA Zone 7" = "#d95f02",
  "BioRA Zone 8" = "#a6761d"
)

# Plot
study_methods_map <- ggplot() +
  # Catchments
  geom_sf(data = catchment, aes(fill = group), color = "#666666", size = 0.2) +
  geom_sf(data = select_catchments, aes(fill = group), color = "#666666", size = 0.3) +
  
  # Tributaries
  #geom_sf(data = tributary, aes(color = group), size = 0.3) +
  geom_sf(data = select_tribs, aes(color = group), size = 0.3) +
  
  # BioRA buffer with semi-transparent fill
  geom_sf(data = biora_buffered, aes(fill = Zone), color = NA, alpha = 0.3) +
  
  # BioRA river segments
  geom_sf(data = biora, aes(color = Zone), size = 1) +
  
  # Monitoring stations
  geom_sf(data = select_wqm, aes(shape = Station), fill = "#C875FF", color = "black", size = 2) +
  
  # LMB outline
  geom_sf(data = lmb, fill = NA, color = "black", size = 0.6) +
  
  # Manual scales
  scale_fill_manual(
    name = "Map Features",
    values = c(zone_colors, "Catchments" = "#E5E5E5", "Selected Catchments" = "#FFF0A5"),
    labels = c(
      "BioRA Zone 1" = "BioRA Zone 1 / Buffer",
      "BioRA Zone 2" = "BioRA Zone 2 / Buffer",
      "BioRA Zone 3" = "BioRA Zone 3 / Buffer",
      "BioRA Zone 4" = "BioRA Zone 4 / Buffer",
      "BioRA Zone 5" = "BioRA Zone 5 / Buffer",
      "BioRA Zone 6" = "BioRA Zone 6 / Buffer",
      "BioRA Zone 7" = "BioRA Zone 7 / Buffer",
      "BioRA Zone 8" = "BioRA Zone 8 / Buffer"
    )
  ) +
  scale_color_manual(
    name = "Rivers",
    values = c(zone_colors, "Relevant Tributaries" = "#6AA9C8")
  ) +
  scale_shape_manual(
    name = "Monitoring Stations",
    values = c("WQM Stations" = 21, "EHM Station" = 24)
  ) +
  
  # North arrow and scale bar
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "bl", which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(0, "cm"),
    pad_y = unit(1.2, "cm")
  ) +
  
  # Labels and theme
  labs(
    title = "Areas Selected for Land Cover Analysis",
    subtitle = "Catchments and Buffered BioRA Zones"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title.x    = element_blank(),
    axis.title.y    = element_blank(),
    axis.text       = element_blank(),
    axis.ticks      = element_blank(),
    panel.grid      = element_blank()
  ) +
  coord_sf()

ggsave("figures/study_methods_map.png", study_methods_map, width = 12, height = 6, dpi = 300)

# Save map without legend
map_only <- study_methods_map + theme(legend.position = "none")
ggsave("figures/study_methods_map_nolegend.png", map_only, width = 12, height = 6, dpi = 300)

# Save legend without map
legend_only <- get_legend(study_methods_map)
legend_plot <- ggdraw(legend_only)
ggsave("figures/study_methods_legend.png", legend_plot, width = 4, height = 10, dpi = 300)


# ------------------------------------------ WATER QUALITY W LAND USE PLOT --------------------------------------

# Load CSV files with data
lu_data_percents <- read_csv("servir_landcover_output/wqm_landcover_percent_by_year.csv")
wq_info <- read_csv("water_quality_data/selected_wq_sites.csv")
wq_data <- read_csv("water_quality_data/wq_data_cleaned.csv")

# Remove unnecessary cite
wq_info <- wq_info %>%
  filter(!statid %in% c("H380104", "H310102"))

# Select relevant years and dry season months
wq_data <- wq_data %>%
  filter(month_collected %in% c(12, 1, 2, 3)) %>%
  filter(year_collected %in% c(2011, 2013, 2015, 2017, 2019, 2021))

# Group land use
lu_percent_grouped <- lu_data_percents %>%
  mutate(
    forest = Class_5 + Class_6 + Class_7 + Class_10 + Class_11 + Class_18,
    agriculture = Class_3 + Class_4 + Class_12 + Class_13 + Class_14 + Class_1,
    urban = Class_16 + Class_2,
    semi_natural = Class_8 + Class_9 + Class_15 + Class_17
  ) %>%
  select(code, Year, forest, agriculture, urban, semi_natural)

# Join land use with link table via `code`
land_use_joined <- left_join(wq_info, lu_percent_grouped, by = "code")

# Create combined_data first
combined_data <- left_join(wq_data, land_use_joined, by = "statid")

# compute dominant land use and prep fields
combined_data <- combined_data %>%
  rowwise() %>%
  mutate(
    dominant_land_use = if (any(is.na(c(forest, agriculture, urban, semi_natural)))) {
      NA_character_
    } else {
      c("forest", "agriculture", "urban", "semi_natural")[which.max(c(forest, agriculture, urban, semi_natural))]
    }
  ) %>%
  ungroup() %>%
  mutate(
    Site = factor(station_name.x, levels = sort(unique(station_name.x))),
    Country = as.factor(Country),
    dominant_land_use = as.factor(dominant_land_use)
  )

# Create and alphabetize site
combined_data <- combined_data %>%
  mutate(Site = station_name.x) %>%
  arrange(Site)

# Sanity check
glimpse(combined_data)
table(combined_data$dominant_land_use)  # if already assigned
unique(combined_data$Site)              # check alphabetical ordering

# reorder site factor and clean labels
combined_data <- combined_data %>%
  mutate(
    Site = factor(code, levels = sort(unique(code))),  # alphabetized by code
    Country = as.factor(Country),
    dominant_land_use = as.factor(dominant_land_use)
  )

# Define color palettes
land_use_palette <- c(
  urban = "#e31a1c",  
  agriculture = "#F7CF72",
  forest = "#A6C46A",
  semi_natural = "#8DCBBD"
)

# define metric labels
metric_labels <- c(
  "codmn_mgl" = "Chemical Oxygen Demand (mg/L)",
  "cond_msm" = "Electrical Conductivity (µS/cm)",
  "do_mgl" = "Dissolved Oxygen (mg/L)",
  "ph" = "pH",
  "temp_c" = "Temperature (C)",
  "totn_mgl" = "Total Nitrogen (mg/L)",
  "totp_mgl" = "Total Phosphorus (mg/L)",
  "tss_mgl" = "Total Suspended Solids (mg/L)"
)

# Choose wq metrics to plot
params_to_plot <- c("ph", "do_mgl", "totp_mgl", "cond_msm", "tss_mgl", "codmn_mgl", "totn_mgl", "temp_c")

# establish mrc guidelines
wqgh_guidelines <- tibble( 
  parameter = c("ph", "totn_mgl", "codmn_mgl", "bod_mgl", "do_mgl", "cond_msm", "tss_mgl", "temp_c", "totp_mgl"), 
  min = c(6, NA, NA, NA, 6, 70, NA, NA, NA), max = c(9, 5, 5, 4, NA, 150, 500, 29.7, 0.1) 
)

# set soft y-axis limits for specific metrics
y_limits <- list(
  ph = c(6, 9),
  do_mgl = c(2.5, 10),
  totp_mgl = c(0, 0.4),
  cond_msm = c(0, 150),
  tss_mgl = c(0, 800),
  codmn_mgl = c(0, 10),
  totn_mgl = c(0, 8),
  temp_c = c(15, 35)
)

# Letters for plot titles
plot_letters <- letters[1:length(params_to_plot)]

# Split parameters into groups of 4 per page
metric_chunks <- split(params_to_plot, ceiling(seq_along(params_to_plot) / 4))

for (i in seq_along(metric_chunks)) {
  plots <- list()
  
  for (j in seq_along(metric_chunks[[i]])) {
    param <- metric_chunks[[i]][[j]]
    letter <- letters[(i - 1) * 4 + j]
    
    # Extract guideline values
    guide <- wqgh_guidelines %>% filter(parameter == param)
    min_val <- guide$min
    max_val <- guide$max
    
    # Diagnostics
    if (is.na(min_val) & is.na(max_val)) {
      message(paste("🔍 No MRC guidelines defined for:", param))
    } else {
      if (!is.na(min_val) && !(min_val >= min(y_limits[[param]]) && min_val <= max(y_limits[[param]]))) {
        message(paste("⚠️ MRC minimum for", param, "is outside the visible y-axis range"))
      }
      if (!is.na(max_val) && !(max_val >= min(y_limits[[param]]) && max_val <= max(y_limits[[param]]))) {
        message(paste("⚠️ MRC maximum for", param, "is outside the visible y-axis range"))
      }
    }
    
    df_param <- combined_data %>% filter(is.finite(.data[[param]]))
    
    p <- ggplot(df_param, aes(x = Site, y = .data[[param]])) +
      geom_boxplot(aes(fill = dominant_land_use), outlier.shape = NA, show.legend = FALSE) +
      {if (!is.na(min_val)) geom_hline(yintercept = min_val, 
                                       color = "forestgreen", linetype = "dashed", linewidth = 0.7, show.legend = FALSE) else NULL} +
      {if (!is.na(max_val)) geom_hline(yintercept = max_val, 
                                       color = "firebrick", linetype = "dashed", linewidth = 0.7, show.legend = FALSE) else NULL} +
      scale_fill_manual(values = land_use_palette, guide = "none") +
      coord_cartesian(ylim = y_limits[[param]]) +
      labs(
        title = paste0("(", letter, ")"),
        x = "Station Code",
        y = metric_labels[[param]]
      ) +
      theme_minimal(base_size = 16) +
      theme(
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0),
        legend.position = "none"
      )
    
    plots[[j]] <- p
  }
  
  page_plot <- wrap_plots(plots, ncol = 2)
  
  print(page_plot)
  
  ggsave(paste0("figures/wq_metrics_page_", i, ".png"), page_plot,
         width = 12, height = 8, dpi = 300)
}

# ------------------------------------------ SITE MATCHING MAPS --------------------------------------

# Transform CRS
select_wqm_3857 <- st_transform(select_wqm, 3857)
mekong_3857 <- st_transform(mekong, 3857)
tribs_major_3857 <- st_transform(tribs_major, 3857)
select_ehm_3857 <- st_transform(select_ehm, 3857)
select_catchments_3857 <- st_transform(select_catchments, 3857)
lmb_3857 <- st_transform(lmb, 3857)

# Load Example Station and Create Buffer
target_station <- select_wqm_3857 %>%
  filter(Sttn_nm == "Chiang Sean")
station_buffer <- st_buffer(target_station, dist = 2000)  # 2km

# Crop Layers to Buffer
mekong_zoom       <- st_crop(mekong_3857, station_buffer)
tribs_zoom        <- st_crop(tribs_major_3857, station_buffer)
wqm_zoom          <- st_crop(select_wqm_3857, station_buffer)
ehm_zoom          <- st_crop(select_ehm_3857, station_buffer)
catchments_zoom   <- st_crop(select_catchments_3857, station_buffer)
lmb_zoom          <- st_crop(lmb_3857, station_buffer)

# Make line between wq and eh
## Filter the WQM station
wqm_point <- select_wqm_3857 %>% 
  filter(Sttn_nm == "Chiang Sean")

# Filter the EHM station named Chiang Sean
ehm_point <- select_ehm_3857 %>% 
  filter(grepl("Chiang Saen", Site_name, ignore.case = TRUE))

connection_line <- st_sf(
  geometry = st_sfc(st_cast(st_union(st_geometry(wqm_point), st_geometry(ehm_point)), "LINESTRING")),
  crs = st_crs(wqm_point)
)

# Calculate distance between points
distance_m <- st_distance(wqm_point, ehm_point) %>% as.numeric()
distance_km <- round(distance_m / 1000, 2)  # Round to 2 decimal places

# Add label to line and adjust position
label_point <- st_line_sample(connection_line$geometry[[1]], sample = 0.5) %>%
  st_sf(geometry = ., crs = st_crs(connection_line))
label_point$label <- paste0(distance_km, " km")

coords <- st_coordinates(label_point)[1, ]  # Get x and y of the midpoint
offset <- 250  # adjust this distance (in meters) as needed for display

# Create a new point shifted right/up
label_point_offset <- st_point(c(coords["X"] + offset, coords["Y"] + offset)) %>%
  st_sfc(crs = st_crs(label_point)) %>%
  st_sf(label = paste0(distance_km, " km"), geometry = .)

# Convert Mekong linestring to polygon
mekong_poly <- st_buffer(mekong_zoom, dist = 290)  # Creates a filled polygon around the lines
mekong_dissolved <- st_union(mekong_poly)
mekong_dissolved <- st_sf(geometry = mekong_dissolved, crs = st_crs(mekong_poly))

# Build Zoomed-In Map
zoom_map <- ggplot() +
  
  # Rivers and basin outline
  geom_sf(data = mekong_dissolved, aes(fill = factor("Mekong River", levels = c("Mekong River", "WQM Station", "EHM Station"))), color = "#61A9BD", linewidth = 0.8) +
  
  geom_sf(data = connection_line, color = "black", linetype = "dashed", size = 0.6) +
  geom_sf_text(data = label_point_offset, aes(label = label),
               color = "black", size = 6, fontface = "italic") +
  
  # Station points
  geom_sf(data = wqm_zoom, aes(fill = factor("WQM Station", levels = c("Mekong River", "WQM Station", "EHM Station"))), shape = 21, color = "black", size = 5) +
  geom_sf(data = ehm_zoom, aes(fill = factor("EHM Station", levels = c("Mekong River", "WQM Station", "EHM Station"))), shape = 24, color = "black", size = 5) +
  
  # Legends
  scale_fill_manual(
    name = "Features",
    values = c(
      "Mekong River" = "lightblue",
      "WQM Station" = "#E090FF",
      "EHM Station" = "#FFA928"
    )
  ) +
  
  # Map styling
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", style = north_arrow_fancy_orienteering, pad_y = unit(0.75, "cm")) +
  coord_sf(
    xlim = st_bbox(station_buffer)[c("xmin", "xmax")],
    ylim = st_bbox(station_buffer)[c("ymin", "ymax")],
    expand = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.05, 0.5),  # left edge, vertically centered
    legend.justification = c(0, 0.5),  # anchor the box’s left side
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.background = element_rect(color = "black"),
    legend.box.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  ) +
  labs(title = "WQM and Downstream EHM Site Pair Example")

print(zoom_map)

# Export Map
ggsave("figures/site_pair.png", zoom_map, width = 8, height = 8, dpi = 300)


# ------------------------------------------ LU AREA MAPS --------------------------------------
# -------------------------- TRIBUTARY AREA MAP --------------------------

# select wqm for example
wqm_point <- select_wqm %>%
  filter(Sttn_nm == "Backprea")

# filter and merge catchments
target_catchments <- select_catchments %>%
  filter(Ctchmn_ %in% c("ST.MONGKOL BOREY", "ST.BATTAMBANG"))

target_catchments$category <- case_when(
  target_catchments$Ctchmn_ == "ST.MONGKOL BOREY" ~ "Primary Catchment",
  target_catchments$Ctchmn_ == "ST.BATTAMBANG" ~ "Secondary Catchment",
  TRUE ~ "Other Catchment"  # or NA if you'd prefer to exclude others
)

# create bounding box from the catchments
catchment_bbox <- st_bbox(target_catchments) %>%
  st_as_sfc() %>%
  st_sf(crs = st_crs(target_catchments))

catchment_bbox<- catchment_bbox %>%
  st_buffer(dist = 50000)  # 50 km buffer in meters

# Plot trib example
trib_map <- ggplot() +
  # Base catchment fill
  geom_sf(data = catchment, aes(fill = "Catchments"), color = "darkgrey", linewidth = 0.3) +
  
  # lmb outline
  geom_sf(data = lmb, aes(color = "LMB Outline"), fill = NA, linewidth = 0.8) +
  
  # highlight target catchments
  geom_sf(data = target_catchments, aes(fill = category), color = "grey30", linewidth = 0.6) +
  
  # Rivers & tributaries
  geom_sf(data = tributary, aes(color = "Tributaries"), linewidth = 0.6) +
  
  # WQM site
  geom_sf(data = wqm_point, aes(fill = "WQM Site"), shape = 21, size = 5, color = "black") +
  
  # Legend setup
  scale_fill_manual(name = "Map Features", values = c(
    "Catchments" = "grey90",
    "WQM Site" = "#E090FF",
    "Primary Catchment" = "#FFD700",      # darker yellow
    "Secondary Catchment" = "#FFEE99"    # lighter yellow
  )) + 
  scale_color_manual(name = "Features", values = c(
    "LMB Outline" = "black",
    "Tributaries" = "#84B8D2"
  )) +
  
  # Extras
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", style = north_arrow_fancy_orienteering, pad_y = unit(0.75, "cm")) +
  coord_sf(xlim = st_bbox(catchment_bbox)[c("xmin", "xmax")],
           ylim = st_bbox(catchment_bbox)[c("ymin", "ymax")],
           expand = TRUE) +
  
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.margin = margin(5, 5, 5, 5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(title = "Catchment Selection for Tributary Site Land Use Example")

ggsave("figures/trib_land_area.png", trib_map, width = 8, height = 8, dpi = 300)


# -------------------------- Plot same tributary map zoomed in
# Create 2 km buffer around WQM site
wqm_buffer <- st_buffer(wqm_point, dist = 2000)  # Distance in meters

# convert tribs linestring to polygon
tribs_poly <- st_buffer(tributary, dist = 100)  # Creates a filled polygon around the lines
tribs_dissolved <- st_union(tribs_poly)
tribs_dissolved <- st_sf(geometry = tribs_dissolved, crs = st_crs(tribs_poly))

# Extract bounding box from the buffer
zoom_bbox <- st_bbox(wqm_buffer)

# Ensure category is a factor for legend ordering
target_catchments$category <- factor(target_catchments$category, levels = c(
  "Primary Catchment", "Secondary Catchment"
))

# Plot zoomed in map
trib_map_zoom <- ggplot() +
  # Base catchment fill
  geom_sf(data = catchment, aes(fill = "Catchments"), color = "darkgrey", linewidth = 0.3) +
  
  # Highlight categorized target catchments
  geom_sf(data = target_catchments, aes(fill = category), color = "grey30", linewidth = 0.6) +
  
  # Tributaries
  geom_sf(data = tribs_dissolved, aes(fill = "Tributaries"), linewidth = 0.6) +
  
  # WQM site as point
  geom_sf(data = wqm_point, aes(fill = "WQM Site"), shape = 21, size = 7, color = "black") +
  
  # Unified fill legend
  scale_fill_manual(name = "Map Features", values = c(
    "Catchments" = "grey90",
    "Primary Catchment" = "#FFD700",      # darker yellow
    "Secondary Catchment" = "#FFEE99",    # lighter yellow
    "WQM Site" = "#E090FF",
    "Tributaries" = "#84B8D2"
  )) +
  
  # Extras
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "br", style = north_arrow_fancy_orienteering, pad_y = unit(0.75, "cm")) +
  
  # Zoom to 2 km buffer
  coord_sf(xlim = c(zoom_bbox["xmin"], zoom_bbox["xmax"]),
           ylim = c(zoom_bbox["ymin"], zoom_bbox["ymax"]),
           expand = TRUE) +
  
  # Styling
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.margin = margin(5, 5, 5, 5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(title = "Catchment Selection for Tributary Site Land Use Example")

# Save map
ggsave("figures/trib_land_area_zoom.png", trib_map_zoom, width = 8, height = 8, dpi = 300)

# -------------------------- MAINSTEM AREA MAP --------------------------

# select wq site
wqm_mainstem_point <- select_wqm %>%
  filter(Sttn_nm == "Vientiane")

# select biora
biora$Zone <- as.factor(biora$Zone)

# Dissolve BioRA zones
biora_dissolved <- biora %>%
  group_by(Zone) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Buffer each zone
biora_buffered <- st_buffer(biora_dissolved, dist = 15000)

# select zone 4
target_biora <- biora_buffered %>%
  filter(Zone == "BioRA Zone 2")
target_biora$category <- "Primary Buffer"

# create bounding box from the buffer
bbox_biora <- st_bbox(target_biora)

# convert to sf for plotting
bbox_sf <- st_as_sfc(bbox_biora) %>%
  st_sf(crs = st_crs(target_catchment))
bbox_sf_buffered <- st_buffer(bbox_sf, dist = 15000)  # buffer

# fill in mekong river for style
mekong_filled <- st_buffer(st_union(mekong), dist = 500)  # tweak distance based on your scale

# Plot
main_map <- ggplot() +
  # Base catchment fill
  geom_sf(data = catchment, aes(fill = "Catchments"), color = "darkgrey", linewidth = 0.3) +
  
  # lmb outline
  geom_sf(data = lmb, aes(color = "LMB Outline"), fill = NA, linewidth = 0.8) +
  
  # highlight target buffer/catchments
  geom_sf(data = target_biora, aes(fill = category), color = "grey30", linewidth = 0.6) +
  
  # Rivers & tributaries
  geom_sf(data = tributary, aes(color = "Tributaries"), linewidth = 0.6) +
  geom_sf(data = mekong_filled, aes(fill = "Mekong"), color = NA) +
  
  # WQM site
  geom_sf(data = wqm_mainstem_point, aes(fill = "WQM Site"), shape = 21, size = 5, color = "black") +
  
  # Legend setup
  scale_fill_manual(name = "Map Features", values = c(
    "Catchments" = "grey90",
    "WQM Site" = "#E090FF",
    "Primary Buffer" = "#FFD700",     
    "Mekong" = "#0788C9"
  )) + 
  scale_color_manual(name = "Features", values = c(
    "LMB Outline" = "black",
    "Tributaries" = "#84B8D2"
  )) +
  
  # Extras
  annotation_scale(location = "tl", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering, pad_y = unit(0.75, "cm")) +
  coord_sf(xlim = st_bbox(bbox_sf_buffered)[c("xmin", "xmax")],
           ylim = st_bbox(bbox_sf_buffered)[c("ymin", "ymax")],
           expand = TRUE) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.margin = margin(5, 5, 5, 5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) +
  labs(title = "BioRA Buffer Selection for Mainstem Site Land Use Example")

print(main_map)

# Save
ggsave("figures/main_land_area.png", main_map, width = 8, height = 8, dpi = 300)


# -------------------------- Plot same mainstem map zoomed in
# Create 15 km buffer around WQM site
wqm_mainstem_buffer <- st_buffer(wqm_mainstem_point, dist = 15000)

# Extract bounding box from the buffer
zoom_main_bbox <- st_bbox(wqm_mainstem_buffer)

# Plot zoomed in map
main_zoom_map <- ggplot() +
  # Base catchment fill
  geom_sf(data = catchment, aes(fill = "Catchments"), color = "darkgrey", linewidth = 0.3) +
  
  # lmb outline
  geom_sf(data = lmb, aes(color = "LMB Outline"), fill = NA, linewidth = 0.8) +
  
  # highlight target buffer/catchments
  geom_sf(data = target_biora, aes(fill = category), color = "grey30", linewidth = 0.6) +
  
  # Rivers & tributaries
  geom_sf(data = tributary, aes(color = "Tributaries"), linewidth = 0.6) +
  geom_sf(data = mekong_filled, aes(fill = "Mekong"), color = "darkgrey") +
  
  # WQM site
  geom_sf(data = wqm_mainstem_point, aes(fill = "WQM Site"), shape = 21, size = 7, color = "black") +
  
  # Legend setup
  scale_fill_manual(name = "Map Features", values = c(
    "Catchments" = "grey90",
    "WQM Site" = "#E090FF",
    "Primary Buffer" = "#FFD700",     
    "Mekong" = "#0788C9"
  )) + 
  scale_color_manual(name = "Features", values = c(
    "LMB Outline" = "black",
    "Tributaries" = "#84B8D2"
  )) +
  
  # Extras
  annotation_scale(location = "tl", width_hint = 0.5, line_width = 3, text_cex = 1.5) +
  annotation_north_arrow(location = "tl", style = north_arrow_fancy_orienteering, pad_y = unit(0.75, "cm"), height = unit(2, "cm")) +
  coord_sf(xlim = st_bbox(zoom_main_bbox)[c("xmin", "xmax")],
           ylim = st_bbox(zoom_main_bbox)[c("ymin", "ymax")],
           expand = TRUE) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.justification = "center",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.box.margin = margin(5, 5, 5, 5),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) +
  labs(title = "Buffer and Catchment Selection for Mainstem Site Land Use Example")

print(main_zoom_map)

# Save
ggsave("figures/main_land_area_zoom.png", main_zoom_map, width = 8, height = 8, dpi = 300)


# ------------------------- R2 SUMMARY BAR CHART -------------------------

# R2 data as a wide-format data frame
r2_matrix <- tribble(
  ~Response, ~Mainstem, ~Tributary, ~Macro, ~Land_Use, ~Full,
  "temp_c",    0.636,     0.562,     0.132,   0.292,    0.403,
  "ph",        0.305,     0.392,     0.045,   0.245,    0.298,
  "tss_mgl",   0.287,     0.889,     0.802,   0.098,    0.853,
  "cond_msm",  0.554,     0.696,    -0.005,   0.520,    0.515,
  "totn_mgl",  0.222,     0.452,     0.043,   0.280,    0.315,
  "totp_mgl",  0.030,     0.473,     0.225,   0.114,    0.260,
  "do_mgl",    0.585,     0.456,     0.028,   0.362,    0.414,
  "codmn_mgl", 0.530,     0.453,     0.030,   0.374,    0.396
)

# Reshape to long format for plotting
r2_long <- r2_matrix %>%
  pivot_longer(cols = -Response, names_to = "Model", values_to = "R2_Value")

# Plot residuals
r2_plot <- ggplot(r2_long, aes(x = Response, y = R2_Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  labs(title = "Model Performance by Water Quality Variable",
       x = "Water Quality Parameter",
       y = expression(paste("Adjusted ", R^2, " Value")),
       fill = "GAM Model") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 20) +  # Increase base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )

# Save plot
ggsave("figures/gams_all_r2_compare.png", r2_plot, width = 8, height = 8, dpi = 300)
