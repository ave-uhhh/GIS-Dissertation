# ------------------------------------------------------ LU GAM -------------------------------------------------------

# ---------------- LU GAM POOLED -----------------

# Define land use predictors
lu_vars <- c("urban", "agriculture", "forest")

# GAM function
run_gam_lu <- function(response_var) {
  formula <- as.formula(paste(response_var, "~", paste0("s(", lu_vars, ")", collapse = " + ")))
  
  model <- gam(formula, data = wq_eh_lu_merge, method = "REML")
  summary_model <- summary(model)
  
  term_table <- summary_model$s.table
  
  tibble(
    Response = response_var,
    Adj_R2 = summary_model$r.sq,
    Dev_Explained = summary_model$dev.expl * 100,
    Urban_p = term_table["s(urban)", "p-value"],
    Agriculture_p = term_table["s(agriculture)", "p-value"],
    Forest_p = term_table["s(forest)", "p-value"]
  )
}

# Apply GAM function across multiple response variables and combine output into a single tibble
gam_lu_pooled <- map_dfr(reduced_vars, run_gam_lu)

# Save
write.csv(gam_lu_pooled, "statistical_analysis/gams/gam_lu_pool.csv", row.names = FALSE)

# ---------------- LU GAM PLOT -----------------

# Fit separate GAMs
mod_urban  <- gam(cond_msm ~ s(urban), data = wq_eh_lu_merge, method = "REML")
mod_agric  <- gam(cond_msm ~ s(agriculture), data = wq_eh_lu_merge, method = "REML")
mod_forest <- gam(cond_msm ~ s(forest), data = wq_eh_lu_merge, method = "REML")

# Generate prediction grids
pred_grid_urban <- tibble(urban = seq(min(wq_eh_lu_merge$urban, na.rm = TRUE),
                                      max(wq_eh_lu_merge$urban, na.rm = TRUE),
                                      length.out = 200))
pred_grid_agric <- tibble(agriculture = seq(min(wq_eh_lu_merge$agriculture, na.rm = TRUE),
                                            max(wq_eh_lu_merge$agriculture, na.rm = TRUE),
                                            length.out = 200))
pred_grid_forest <- tibble(forest = seq(min(wq_eh_lu_merge$forest, na.rm = TRUE),
                                        max(wq_eh_lu_merge$forest, na.rm = TRUE),
                                        length.out = 200))

# Predict with SEs
preds_urban <- predict(mod_urban, newdata = pred_grid_urban, se.fit = TRUE)
preds_agric <- predict(mod_agric, newdata = pred_grid_agric, se.fit = TRUE)
preds_forest <- predict(mod_forest, newdata = pred_grid_forest, se.fit = TRUE)

# Attach predictions to grids
pred_grid_urban <- pred_grid_urban %>%
  mutate(fit = preds_urban$fit, se = preds_urban$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

pred_grid_agric <- pred_grid_agric %>%
  mutate(fit = preds_agric$fit, se = preds_agric$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

pred_grid_forest <- pred_grid_forest %>%
  mutate(fit = preds_forest$fit, se = preds_forest$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

# change labels
wq_eh_lu_merge <- wq_eh_lu_merge %>%
  mutate(River_Type = case_when(
    type == "m" ~ "Mainstem",
    type == "t" ~ "Tributary",
    TRUE ~ type
  ))

# Core plot styling shared across all
base_theme <- theme_minimal() +
  theme(legend.position = "bottom")

# Build each plot
plot_urban <- ggplot() +
  geom_ribbon(data = pred_grid_urban, aes(x = urban, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = pred_grid_urban, aes(x = urban, y = fit), color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = urban, y = cond_msm, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Urban Land Use vs. Conductivity", x = "Urban (%)", y = "Conductivity", color = "River Type") +
  base_theme

plot_agric <- ggplot() +
  geom_ribbon(data = pred_grid_agric, aes(x = agriculture, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = pred_grid_agric, aes(x = agriculture, y = fit), color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = agriculture, y = cond_msm, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Agricultural Land Use vs. Conductivity", x = "Agriculture (%)", y = "Conductivity", color = "River Type") +
  base_theme

plot_forest <- ggplot() +
  geom_ribbon(data = pred_grid_forest, aes(x = forest, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = pred_grid_forest, aes(x = forest, y = fit), color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = forest, y = cond_msm, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Forest Land Use vs. Conductivity", x = "Forest (%)", y = "Conductivity", color = "River Type") +
  base_theme

# Combine plots with unified legend
combined_plot <- (plot_urban / plot_agric / plot_forest) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_plot <- (plot_urban / plot_agric / plot_forest) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 12)
  )

# Add title
final_gam_pool <- combined_plot + 
  plot_annotation(title = "Land Use Effects on Electrical Conductivity",
                  theme = theme(plot.title = element_text(size = 18, face = "bold")))

# Display the result
final_gam_pool

ggsave("figures/gam_lu_cond_pool.png", final_gam_pool, width = 8, height = 11, dpi = 300)


# ---------------- LU GAM YEARLY -----------------

# Loop through each year
run_gam_lu_by_year <- function(year, response_var) {
  data_subset <- wq_eh_lu_merge %>%
    filter(year_collected == year) %>%
    select(all_of(c(response_var, lu_vars))) %>%
    drop_na()
  
  if (nrow(data_subset) < 10) return(NULL)
  
  formula <- as.formula(paste(response_var, "~", paste0("s(", lu_vars, ")", collapse = " + ")))
  model <- gam(formula, data = data_subset, method = "REML")
  summary_model <- summary(model)
  
  tibble(
    Year = year,
    Response = response_var,
    Adj_R2 = summary_model$r.sq,
    Dev_Explained = summary_model$dev.expl * 100,
    Urban_p = summary_model$s.table["s(urban)", "p-value"],
    Agriculture_p = summary_model$s.table["s(agriculture)", "p-value"],
    Forest_p = summary_model$s.table["s(forest)", "p-value"]
  )
}

# loop across all years / variables
gam_lu_by_year <- map_dfr(unique(wq_eh_lu_merge$year_collected), function(y) {
  map_dfr(reduced_vars, ~run_gam_lu_by_year(y, .x))
})

# Save
write.csv(gam_lu_by_year, "statistical_analysis/gams/gam_lu_yearly.csv", row.names = FALSE)


# ---------------- LU GAM PLOT -----------------

# Subset data for 2021
wq_2021 <- wq_eh_lu_merge %>%
  filter(year_collected == 2021) %>%
  select(totn_mgl, urban, type) %>%
  drop_na()

# Fit GAM
mod_urban_totn_2021 <- gam(totn_mgl ~ s(urban), data = wq_2021, method = "REML")

# generate prediction
pred_grid_urban_21 <- tibble(urban = seq(min(wq_2021$urban, na.rm = TRUE),
                                         max(wq_2021$urban, na.rm = TRUE),
                                         length.out = 200))

# predict with SE's
preds_urban_21 <- predict(mod_urban_totn_2021, newdata = pred_grid_urban_21, se.fit = TRUE)

# Attach predictions to grids
pred_grid_urban_21 <- pred_grid_urban_21 %>%
  mutate(fit = preds_urban_21$fit, se = preds_urban_21$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

# change labels
wq_2021 <- wq_2021 %>%
  mutate(River_Type = case_when(
    type == "m" ~ "Mainstem",
    type == "t" ~ "Tributary",
    TRUE ~ type
  ))

# Build plot
plot_urban_totn_21 <- ggplot() +
  geom_ribbon(data = pred_grid_urban_21, aes(x = urban, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = pred_grid_urban_21, aes(x = urban, y = fit), color = "black", size = 1) +
  geom_point(data = wq_2021, aes(x = urban, y = totn_mgl, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Urban Land Use vs. Total Nitrogen 2021", x = "Urban (%)", y = "TN (mg/L)", color = "River Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold"),
  )

print(plot_urban_totn_21)

# Save plot
ggsave("figures/gam_lu_urban_totn_2021.png", plot_urban_totn_21, width = 8, height = 6, dpi = 300)


# Subset data for 2015
wq_2015 <- wq_eh_lu_merge %>%
  filter(year_collected == 2015) %>%
  select(codmn_mgl, forest, type) %>%
  drop_na()

# Fit GAM
mod_forest_cod_2015 <- gam(codmn_mgl ~ s(forest), data = wq_2015, method = "REML")

# generate prediction
pred_grid_forest_15 <- tibble(forest = seq(min(wq_2015$forest, na.rm = TRUE),
                                           max(wq_2015$forest, na.rm = TRUE),
                                           length.out = 200))

# predict with SE's
preds_forest_15 <- predict(mod_forest_cod_2015, newdata = pred_grid_forest_15, se.fit = TRUE)

# Attach predictions to grids
pred_grid_forest_15 <- pred_grid_forest_15 %>%
  mutate(fit = preds_forest_15$fit, se = preds_forest_15$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

# change labels
wq_2015 <- wq_2015 %>%
  mutate(River_Type = case_when(
    type == "m" ~ "Mainstem",
    type == "t" ~ "Tributary",
    TRUE ~ type
  ))

# Build plot
plot_forest_cod_15 <- ggplot() +
  geom_ribbon(data = pred_grid_forest_15, aes(x = forest, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = pred_grid_forest_15, aes(x = forest, y = fit), color = "black", size = 1) +
  geom_point(data = wq_2015, aes(x = forest, y = codmn_mgl, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Forest Land Use vs. Chemical Oxygen Demand 2011", x = "Forest (%)", y = "COD (mg/L)", color = "River Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold"),
  )

print(plot_forest_cod_15)

ggsave("figures/gam_for_cod_2015.png", plot_forest_cod_15, width = 8, height = 6, dpi = 300)


# ---------------- LU GAM HEATMAP -----------------
# heat map set up
gam_lu_pooled_plot <- gam_lu_pooled %>%
  select(Response, Adj_R2) %>%
  mutate(Year = "Pooled")

# Yearly data: Keep as-is
gam_lu_yearly_plot <- gam_lu_by_year %>%
  select(Response, Year, Adj_R2) %>%
  mutate(Year = as.character(Year))

# Combine both
gam_lu_r2_plot_data <- bind_rows(gam_lu_yearly_plot, gam_lu_pooled_plot)

# Define labels
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

# Apply labels
gam_lu_r2_plot_data <- gam_lu_r2_plot_data %>%
  mutate(Response_Label = metric_labels[Response])

# Add a dummy row with NA Adj_R2 for spacing
gam_lu_r2_plot_data <- gam_lu_r2_plot_data %>%
  bind_rows(gam_lu_r2_plot_data %>%
              distinct(Response_Label) %>%
              mutate(Year = " ", Adj_R2 = NA)) %>%
  arrange(Response_Label, Year)

#update factor levels to keep order
gam_lu_r2_plot_data$Year <- factor(
  gam_lu_r2_plot_data$Year,
  levels = c(sort(unique(gam_lu_yearly_plot$Year)), " ", "Pooled")
)

# plot
gam_lu_heatmap <- ggplot(gam_lu_r2_plot_data, aes(x = Year, y = Response_Label, fill = Adj_R2)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(9, "YlGnBu"),
    na.value = "#f0f0f0"
  ) +
  labs(
    title = "Model Fit Over Time",
    subtitle = "Adjusted R² from Full GAMs",
    x = "Year", y = "Water Quality Metric", fill = "Adj R²"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    panel.grid = element_blank()
  )

print(gam_lu_heatmap)

# Save figure
ggsave("figures/gam_lu_heatmap.png", gam_lu_heatmap, width = 10, height = 6, dpi = 300)

