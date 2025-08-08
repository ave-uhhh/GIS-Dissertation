# ------------------------------------------------------ FULL GAM -------------------------------------------------------

# -------------------------- FULL GAM POOLED --------------------------

# Define predictors
eh_vars <- c("aspt", "abundance", "richness")
lu_vars <- c("urban", "agriculture", "forest")
all_predictors <- c(eh_vars, lu_vars)

# Fit gam model to return model object (necessary for predicting sites)
fit_gam_model <- function(response_var) {
  formula <- as.formula(paste(response_var, "~", paste0("s(", all_predictors, ")", collapse = " + ")))
  gam(formula, data = wq_eh_lu_merge, method = "REML")
}

# Summarise model performance
summarise_gam_model <- function(model, response_var) {
  summary_model <- summary(model)
  term_table <- summary_model$s.table
  
  tibble(
    Response = response_var,
    Adj_R2 = summary_model$r.sq,
    Dev_Explained = summary_model$dev.expl * 100,
    ASPT_p = term_table["s(aspt)", "p-value"],
    Richness_p = term_table["s(richness)", "p-value"],
    Abundance_p = term_table["s(abundance)", "p-value"],
    Urban_p = term_table["s(urban)", "p-value"],
    Agriculture_p = term_table["s(agriculture)", "p-value"],
    Forest_p = term_table["s(forest)", "p-value"]
  )
}

# Run model and export summary
gam_pooled_results <- map_dfr(reduced_vars, function(response_var) {
  model <- fit_gam_model(response_var)
  summarise_gam_model(model, response_var)
})

# Save
write.csv(gam_pooled_results, "statistical_analysis/gams/gam_full_pooled.csv", row.names = FALSE)

# -------------------------- PREDICT WQ FROM FULL MODEL --------------------------

# Rerun joins with predictor_site_match
# ------ Join WQ and EH
# Drop NA's and join data
wq_eh_merge_predictor <- predictor_site_match %>%
  left_join(select(dry_median, statid, year_collected),
            by = c("wq_statid" = "statid")) %>%
  left_join(macro_metrics, by = c("eh_code" = "code", "year_collected")) %>%
  left_join(select(dry_median, statid, year_collected, all_of(reduced_vars)),
            by = c("wq_statid" = "statid", "year_collected")) %>%
  filter(!is.na(aspt))

# ----- Join WQ and LU
wq_lu_merge_predictor <- dry_median %>%
  left_join(land_use, by = c("statid", "year_collected")) %>%
  drop_na(forest, agriculture, urban, semi_natural)

# ----- Join WQ, EH, LU
wq_eh_lu_merge_predictor <- wq_eh_merge_predictor %>%
  left_join(select(land_use, statid, year_collected, urban, agriculture, forest),
            by = c("wq_statid" = "statid", "year_collected")) %>%
  select(wq_statid, year_collected, all_of(reduced_vars), aspt, richness, abundance, urban, agriculture, forest, type) %>%
  drop_na()

# Define withheld sites
model_sites <- c("H011200", "H013401", "H019806", "H910108")

# Filter data for prediction
new_sites_data <- wq_eh_lu_merge_predictor %>% filter(wq_statid %in% model_sites)

# Predict loop
prediction_results <- map_dfr(reduced_vars, function(response_var) {
  model <- fit_gam_model(response_var)
  
  preds <- predict(model, newdata = new_sites_data, type = "response")
  
  tibble(
    wq_statid = new_sites_data$wq_statid,
    year_collected = new_sites_data$year_collected,
    Response = response_var,
    Prediction = preds,
    Actual = new_sites_data[[response_var]]
  )
})

# Save predictions
write.csv(prediction_results, "statistical_analysis/gams/gam_full_predictions.csv", row.names = FALSE)

# Calculate residual
prediction_results <- prediction_results %>%
  mutate(Residual = Actual - Prediction)

# Calculate RMSE per metric
rmse_summary <- prediction_results %>%
  group_by(Response) %>%
  summarise(
    RMSE = sqrt(mean(Residual^2, na.rm = TRUE)),
    Mean_Residual = mean(Residual, na.rm = TRUE),
    SD_Residual = sd(Residual, na.rm = TRUE),
    .groups = "drop"
  )

# View RMSE per metric and save
print(rmse_summary)
write.csv(rmse_summary, "statistical_analysis/gams/gam_full_predictions_rmse.csv", row.names = FALSE)

# ------------ PLOT PREDICTIONS --------------

# Plot actual vs predicted
ggplot(prediction_results, aes(x = Actual, y = Prediction)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~ Response, scales = "free") +
  labs(title = "GAM Predictions vs Actual Observations",
       x = "Observed",
       y = "Predicted")

# plot residuals
wq_metric_labels <- c(
  "codmn_mgl" = "COD",
  "cond_msm" = "Conductivity",
  "do_mgl" = "DO",
  "ph" = "pH",
  "temp_c" = "Temp",
  "totn_mgl" = "TN",
  "totp_mgl" = "TP",
  "tss_mgl" = "TSS"
)

resids_plot <- ggplot(prediction_results, aes(x = Actual, y = Residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ Response, scales = "free", labeller = as_labeller(wq_metric_labels)) +
  labs(title = "Residuals vs Observed Values",
       x = "Observed",
       y = "Residuals") +
  theme(
    plot.title = element_text(size = 22, face = "bold"),        
    axis.title.x = element_text(size = 18),                    
    axis.title.y = element_text(size = 18), 
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    strip.text = element_text(size = 16, face = "bold")         
  )

# View and save plot
print(resids_plot)
ggsave("figures/gam_full_prediction_residual_plot.png", resids_plot, width = 10, height = 8, dpi = 300)

# -------------------------- EXAMINE OUTLIER SITES --------------------------

# Rerun EH, WQ, LU join once more with statid and eh_code
wq_eh_lu_merge_outlier <- wq_eh_merge %>%
  left_join(select(land_use, statid, year_collected, urban, agriculture, forest),
            by = c("wq_statid" = "statid", "year_collected")) %>%
  select(wq_statid, eh_code, year_collected, all_of(reduced_vars), aspt, richness, abundance, urban, agriculture, forest, type) %>%
  drop_na()

# Define predictors and metrics
eh_vars <- c("aspt", "abundance", "richness")
lu_vars <- c("urban", "agriculture", "forest")
all_predictors <- c(eh_vars, lu_vars)

wq_metrics <- c("temp_c", "ph", "tss_mgl", "cond_msm",
                "totn_mgl", "totp_mgl", "do_mgl", "codmn_mgl")

# Extract outliers
check_combined_outliers <- function(response_var) {
  formula <- as.formula(paste(response_var, "~", paste0("s(", all_predictors, ")", collapse = " + ")))
  model <- gam(formula, data = wq_eh_lu_merge_outlier, method = "REML")
  
  preds <- predict(model, type = "response", se.fit = TRUE)
  upper <- preds$fit + (1.96 * preds$se.fit)
  lower <- preds$fit - (1.96 * preds$se.fit)
  
  wq_eh_lu_merge_outlier %>%
    mutate(response = .data[[response_var]],
           predicted = preds$fit,
           lower_CI = lower,
           upper_CI = upper,
           outside_CI = response < lower_CI | response > upper_CI,
           metric = response_var) %>%
    select(eh_code, year_collected, metric, response, predicted, lower_CI, upper_CI, outside_CI)
}

# Run accross metrics and combine
eh_lu_outliers <- map_dfr(wq_metrics, check_combined_outliers)

# Summarize per site
eh_lu_site_summary <- eh_lu_outliers %>%
  group_by(eh_code) %>%
  summarise(
    total_outliers = sum(outside_CI, na.rm = TRUE),
    total_checks = n(),
    outlier_pct = round((total_outliers / total_checks) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(outlier_pct))

# Save 
write.csv(eh_lu_site_summary, "statistical_analysis/gams/gam_full_outliers.csv", row.names = FALSE)

# Classify outliers by zone
zonation <- read_csv("join_metadata/wq_corresponding_eh_final.csv")  # contains eh_code, zone, river_system

eh_lu_site_summary_zoned <- eh_lu_site_summary %>%
  left_join(zonation, by = "eh_code")

# Summarize outliers by zone
zone_summary <- eh_lu_site_summary_zoned %>%
  group_by(type) %>%
  summarise(
    avg_outlier_pct = round(mean(outlier_pct, na.rm = TRUE), 1),
    high_outlier_sites = sum(outlier_pct > 70),
    total_sites = n()
  )

# Save results
write.csv(zone_summary, "statistical_analysis/gams/gam_full_outliers_zone.csv")

# Summarize outliers by river system
river_summary <- eh_lu_site_summary_zoned %>%
  group_by(wq_River_Names_og) %>%
  summarise(
    avg_outlier_pct = round(mean(outlier_pct, na.rm = TRUE), 1),
    high_outlier_sites = sum(outlier_pct > 70),
    total_sites = n()
  )

# Save results
write.csv(river_summary, "statistical_analysis/gams/gam_full_outliers_river.csv")

# Summarize outliers by spatial bins
coords_summary <- eh_lu_site_summary_zoned %>%
  mutate(
    lat_bin = round(wq_lat, 1),
    lon_bin = round(wq_long, 1)
  ) %>%
  group_by(lat_bin, lon_bin) %>%
  summarise(
    avg_outlier_pct = mean(outlier_pct, na.rm = TRUE),
    total_sites = n(),
    .groups = "drop"
  )

# Save results
write.csv(coords_summary, "statistical_analysis/gams/gam_full_outlier_bins.csv")


# ---------------- FULL GAM SIGNIFICANCE MATRIX PLOT -----------------

# Reshape to long format for plotting
sig_long <- gam_pooled_results %>%
  select(Response, ends_with("_p")) %>%
  pivot_longer(-Response, names_to = "Predictor", values_to = "p_value") %>%
  mutate(Predictor = gsub("_p", "", Predictor),
         Significance = case_when(
           p_value < 0.001 ~ "<0.001",
           p_value < 0.01 ~ "<0.01",
           p_value < 0.05 ~ "<0.05",
           TRUE ~ "NS"
         ))

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

# apply metric labels
sig_long <- sig_long %>%
  mutate(Response_Label = metric_labels[Response])

# plot
p_val_heat <- ggplot(sig_long, aes(x = Predictor, y = Response_Label, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("<0.001" = "#0D2880", "<0.01" = "#67C2C2", "<0.05" = "#F8EEA0", "NS" = "#f0f0f0")) +
  labs(x = "Predictor", y = "Water Quality Variable",
       fill = "Significance Level", title = "Significance of Predictors Across GAMs") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold"),
        panel.grid = element_blank())

ggsave("figures/gam_full_pval.png", p_val_heat, width = 10, height = 6, dpi = 300)


# ---------------- FULL GAM PLOT -----------------

# Create River_Type label
wq_eh_lu_merge <- wq_eh_lu_merge %>%
  mutate(River_Type = case_when(
    type == "m" ~ "Mainstem",
    type == "t" ~ "Tributary",
    TRUE ~ type
  ))

# Fit models
mod_tss <- gam(tss_mgl ~ s(aspt) + s(urban), data = wq_eh_lu_merge, method = "REML")
mod_cond  <- gam(cond_msm ~ s(agriculture) + s(forest), data = wq_eh_lu_merge, method = "REML")

# Prediction grids
grid_aspt <- tibble(aspt = seq(min(wq_eh_lu_merge$aspt, na.rm = TRUE),
                               max(wq_eh_lu_merge$aspt, na.rm = TRUE), length.out = 200),
                    urban = mean(wq_eh_lu_merge$urban, na.rm = TRUE))  # hold urban constant

grid_urban <- tibble(urban = seq(min(wq_eh_lu_merge$urban, na.rm = TRUE),
                                 max(wq_eh_lu_merge$urban, na.rm = TRUE), length.out = 200),
                     aspt = mean(wq_eh_lu_merge$aspt, na.rm = TRUE))

grid_ag <- tibble(agriculture = seq(min(wq_eh_lu_merge$agriculture, na.rm = TRUE),
                                   max(wq_eh_lu_merge$agriculture, na.rm = TRUE), length.out = 200),
                    forest = mean(wq_eh_lu_merge$forest, na.rm = TRUE))

grid_forest <- tibble(forest = seq(min(wq_eh_lu_merge$forest, na.rm = TRUE),
                                   max(wq_eh_lu_merge$forest, na.rm = TRUE), length.out = 200),
                      agriculture = mean(wq_eh_lu_merge$agriculture, na.rm = TRUE))

# Predictions
pred_aspt <- predict(mod_tss, newdata = grid_aspt, se.fit = TRUE)
pred_urban <- predict(mod_tss, newdata = grid_urban, se.fit = TRUE)
pred_ag <- predict(mod_cond, newdata = grid_ag, se.fit = TRUE)
pred_forest <- predict(mod_cond, newdata = grid_forest, se.fit = TRUE)

# Attach confidence intervals
grid_aspt <- grid_aspt %>% mutate(fit = pred_aspt$fit, se = pred_aspt$se.fit,
                                  lower = fit - 1.96 * se, upper = fit + 1.96 * se)
grid_urban <- grid_urban %>% mutate(fit = pred_urban$fit, se = pred_urban$se.fit,
                                    lower = fit - 1.96 * se, upper = fit + 1.96 * se)
grid_ag <- grid_ag %>% mutate(fit = pred_ag$fit, se = pred_ag$se.fit,
                                  lower = fit - 1.96 * se, upper = fit + 1.96 * se)
grid_forest <- grid_forest %>% mutate(fit = pred_forest$fit, se = pred_forest$se.fit,
                                      lower = fit - 1.96 * se, upper = fit + 1.96 * se)

# Core plot theme
base_theme <- theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))

# Panel A: TSS partial effects
plot_aspt <- ggplot() +
  geom_ribbon(data = grid_aspt, aes(x = aspt, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = grid_aspt, aes(x = aspt, y = fit), color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = aspt, y = tss_mgl, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Partial Effect of ASPT on TSS", x = "ASPT Score", y = "TSS (mg/L)", color = "River Type") +
  base_theme

plot_urban <- ggplot() +
  geom_ribbon(data = grid_urban, aes(x = urban, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = grid_urban, aes(x = urban, y = fit), color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = urban, y = tss_mgl, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Partial Effect of Urban Land on TSS", x = "Urban (%)", y = "TSS (mg/L)", color = "River Type") +
  base_theme

panel_a <- (plot_aspt / plot_urban) + plot_annotation(tag_levels = "A")

# Panel B: COND partial effects
plot_ag <- ggplot() +
  geom_ribbon(data = grid_ag, aes(x = agriculture, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = grid_ag, aes(x = agriculture, y = fit), color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = agriculture, y = cond_msm, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Partial Effect of Agricultural Land on Conductivity", x = "Agriculture", y = "COND (msm)", color = "River Type") +
  base_theme

plot_forest <- ggplot() +
  geom_ribbon(data = grid_forest, aes(x = forest, ymin = lower, ymax = upper), fill = "darkgrey", alpha = 0.2) +
  geom_line(data = grid_forest, aes(x = forest, y = fit), color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = forest, y = cond_msm, color = River_Type), alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Partial Effect of Forest Cover on Conductivity", x = "Forest (%)", y = "COND (msm)", color = "River Type") +
  base_theme

panel_b <- (plot_ag / plot_forest) + plot_annotation(tag_levels = "B")

# Combine Panels A + B
full_gam_plots <- (panel_a | panel_b) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Title + Save
final_plot <- full_gam_plots + plot_annotation(title = "Effects of Macroinvertebrate and Land Use Predictors on Water Quality",
                                               theme = theme(plot.title = element_text(size = 18, face = "bold")))
print(final_plot)
ggsave("figures/gam_full_plots.png", final_plot, width = 12, height = 10, dpi = 300)

# -------- RESIDUALS PLOT ---------
# Fit models and store both summaries and model objects
gam_models_list <<- list()  # Declare globally

gam_pooled_results <- map_dfr(reduced_vars, function(response_var) {
  model <- fit_gam_model(response_var)
  gam_models_list[[response_var]] <<- model  # Use super-assignment to modify outer variable
  summarise_gam_model(model, response_var)
})

# Loop through all variables and build dataframe
residual_df <- map_dfr(names(gam_models_list), function(resp) {
  model <- gam_models_list[[resp]]
  tibble(
    Response = resp,
    Fitted = fitted(model),
    Residual = residuals(model)
  )
})

glimpse(residual_df)

# Plot
residuals_plot <- ggplot(residual_df, aes(x = Fitted, y = Residual)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Response, scales = "free") +
  labs(title = "Residuals vs Fitted Values by Response Variable",
       x = "Fitted Values",
       y = "Residuals") +
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
ggsave("figures/gam_full_residuals_plot.png", residuals_plot, width = 8, height = 8, dpi = 300)

# -------------------------- FULL GAM YEARLY --------------------------

# RUN !!
run_gam_wq_eh_lu_by_year <- function(year, response_var) {
  data_subset <- wq_eh_lu_merge %>%
    filter(year_collected == year) %>%
    select(all_of(c(response_var, all_predictors))) %>%
    drop_na()
  
  if (nrow(data_subset) < 10) return(NULL)
  
  formula <- as.formula(paste(response_var, "~", paste0("s(", all_predictors, ")", collapse = " + ")))
  model <- gam(formula, data = data_subset, method = "REML")
  summary_model <- summary(model)
  
  term_table <- summary_model$s.table
  
  tibble(
    Year = year,
    Response = response_var,
    Adj_R2 = summary_model$r.sq,
    Dev_Explained = summary_model$dev.expl * 100,
    ASPT_p = term_table["s(aspt)", "p-value"],
    Richness_p = term_table["s(richness)", "p-value"],
    Abundance_p = term_table["s(abundance)", "p-value"],
    Urban_p = term_table["s(urban)", "p-value"],
    Agriculture_p = term_table["s(agriculture)", "p-value"],
    Forest_p = term_table["s(forest)", "p-value"]
  )
}

gam_wq_eh_lu_by_year <- map_dfr(unique(wq_eh_lu_merge$year_collected), function(y) {
  map_dfr(reduced_vars, ~run_gam_wq_eh_lu_by_year(y, .x))
})

# Save 
write.csv(gam_wq_eh_lu_by_year, "statistical_analysis/gams/gam_full_yearly.csv", row.names = FALSE)

# ---------------- FULL GAM HEATMAP -----------------

# Pooled data: Tag year as "Pooled"
gam_pooled_plot <- gam_pooled_results %>%
  select(Response, Adj_R2) %>%
  mutate(Year = "Pooled")

# Yearly data: Keep as-is
gam_yearly_plot <- gam_wq_eh_lu_by_year %>%
  select(Response, Year, Adj_R2) %>%
  mutate(Year = as.character(Year))

# Combine both
gam_r2_plot_data <- bind_rows(gam_yearly_plot, gam_pooled_plot)

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
gam_r2_plot_data <- gam_r2_plot_data %>%
  mutate(Response_Label = metric_labels[Response])

# Add a dummy row with NA Adj_R2 for spacing
gam_r2_plot_data <- gam_r2_plot_data %>%
  bind_rows(gam_r2_plot_data %>%
              distinct(Response_Label) %>%
              mutate(Year = " ", Adj_R2 = NA)) %>%
  arrange(Response_Label, Year)

#update factor levels to keep order
gam_r2_plot_data$Year <- factor(
  gam_r2_plot_data$Year,
  levels = c(sort(unique(gam_yearly_plot$Year)), " ", "Pooled")
)

# plot
gam_heatmap <- ggplot(gam_r2_plot_data, aes(x = Year, y = Response_Label, fill = Adj_R2)) +
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

ggsave("figures/gam_full_heatmap.png", gam_heatmap, width = 12, height = 6, dpi = 300)


