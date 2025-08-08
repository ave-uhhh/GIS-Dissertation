# ------------------------------------------------------ MACRO GAM -------------------------------------------------------

# ---------------- MACRO GAM POOLED -----------------

# Function to run GAM and extract results
run_gam_model <- function(response_var) {
  formula <- as.formula(paste(response_var, "~ s(aspt) + s(richness) + s(abundance)"))
  
  # Fit GAM
  model <- gam(formula, data = wq_eh_lu_merge, method = "REML")
  
  # Summary stats
  gam_summary <- summary(model)
  smooth_terms <- gam_summary$s.table
  
  tibble(
    Response = response_var,
    Adj_R2 = gam_summary$r.sq,
    Dev_Explained = gam_summary$dev.expl * 100,
    ASPT_p = smooth_terms["s(aspt)", "p-value"],
    Richness_p = smooth_terms["s(richness)", "p-value"],
    Abundance_p = smooth_terms["s(abundance)", "p-value"]
  )
}

# Fit a GAM model
gam_model <- gam(totp_mgl ~ s(aspt) + s(richness) + s(abundance), 
                 data = wq_eh_lu_merge, method = "REML")

# Run across all WQ variables
gam_macro_pooled <- map_dfr(reduced_vars, run_gam_model)

# Save results
write.csv(gam_macro_pooled, "statistical_analysis/gams/gam_macro_pool.csv", row.names = FALSE)


# ---------------- MACRO GAM SMOOTHER PLOT -----------------

# Fit pooled GAM model for TSS
gam_model <- gam(tss_mgl ~ s(aspt) + s(richness) + s(abundance),
                 data = wq_eh_lu_merge, method = "REML")

# Compute median values for non-focal predictors
ref_vals <- wq_eh_lu_merge %>%
  summarise(
    aspt_median = median(aspt, na.rm = TRUE),
    richness_median = median(richness, na.rm = TRUE),
    abundance_median = median(abundance, na.rm = TRUE)
  )

# Create prediction grids
grid_aspt <- tibble(
  aspt = seq(min(wq_eh_lu_merge$aspt, na.rm = TRUE),
             max(wq_eh_lu_merge$aspt, na.rm = TRUE), length.out = 200),
  richness = ref_vals$richness_median,
  abundance = ref_vals$abundance_median
)

grid_richness <- tibble(
  richness = seq(min(wq_eh_lu_merge$richness, na.rm = TRUE),
                 max(wq_eh_lu_merge$richness, na.rm = TRUE), length.out = 200),
  aspt = ref_vals$aspt_median,
  abundance = ref_vals$abundance_median
)

grid_abundance <- tibble(
  abundance = seq(min(wq_eh_lu_merge$abundance, na.rm = TRUE),
                  max(wq_eh_lu_merge$abundance, na.rm = TRUE), length.out = 200),
  aspt = ref_vals$aspt_median,
  richness = ref_vals$richness_median
)

# Predict fits and confidence intervals
pred_aspt <- predict(gam_model, newdata = grid_aspt, se.fit = TRUE)
pred_rich  <- predict(gam_model, newdata = grid_richness, se.fit = TRUE)
pred_abund <- predict(gam_model, newdata = grid_abundance, se.fit = TRUE)

# Attach prediction results to grids
grid_aspt <- grid_aspt %>%
  mutate(fit = pred_aspt$fit, se = pred_aspt$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

grid_richness <- grid_richness %>%
  mutate(fit = pred_rich$fit, se = pred_rich$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

grid_abundance <- grid_abundance %>%
  mutate(fit = pred_abund$fit, se = pred_abund$se.fit,
         lower = fit - 1.96 * se, upper = fit + 1.96 * se)

# Define River_Type for color coding (if not already done)
wq_eh_lu_merge <- wq_eh_lu_merge %>%
  mutate(River_Type = case_when(
    type == "m" ~ "Mainstem",
    type == "t" ~ "Tributary",
    TRUE ~ type
  ))

# Plot ASPT effect
plot_aspt <- ggplot() +
  geom_ribbon(data = grid_aspt, aes(x = aspt, ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.3) +
  geom_line(data = grid_aspt, aes(x = aspt, y = fit),
            color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = aspt, y = tss_mgl, color = River_Type),
             alpha = 0.7) +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  labs(title = "Effect of ASPT on TSS", x = "ASPT", y = "Predicted TSS (mg/L)", color = "River Type") +
  theme_minimal(base_size = 16)

# Save
ggsave("figures/gam_macro_aspt_tss.png", plot_aspt, width = 8, height = 8, dpi = 300)

# Setup for combined plot
## Plot: ASPT
plot_aspt <- ggplot() +
  geom_ribbon(data = grid_aspt, aes(x = aspt, ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.3) +
  geom_line(data = grid_aspt, aes(x = aspt, y = fit),
            color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = aspt, y = tss_mgl, color = River_Type),
             alpha = 0.7) +
  labs(x = "ASPT", y = "Predicted TSS (mg/L)", title = "ASPT") +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  theme_minimal(base_size = 12)

## Plot: Richness
plot_richness <- ggplot() +
  geom_ribbon(data = grid_richness, aes(x = richness, ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.3) +
  geom_line(data = grid_richness, aes(x = richness, y = fit),
            color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = richness, y = tss_mgl, color = River_Type),
             alpha = 0.7) +
  labs(x = "Richness", y = "Predicted TSS (mg/L)", title = "Richness") +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  theme_minimal(base_size = 12)

## Plot: Abundance
plot_abundance <- ggplot() +
  geom_ribbon(data = grid_abundance, aes(x = abundance, ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.3) +
  geom_line(data = grid_abundance, aes(x = abundance, y = fit),
            color = "black", size = 1) +
  geom_point(data = wq_eh_lu_merge, aes(x = abundance, y = tss_mgl, color = River_Type),
             alpha = 0.7) +
  labs(x = "Abundance", y = "Predicted TSS (mg/L)", title = "Abundance") +
  scale_color_manual(values = c("Mainstem" = "#1f77b4", "Tributary" = "#2ca02c")) +
  theme_minimal(base_size = 12)

# Combine plots with shared legend
combined_plot <- (plot_aspt / plot_richness / plot_abundance) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 12)
  )

# Add global title
final_gam_plot <- combined_plot +
  plot_annotation(
    title = "Macroinvertebrate Metric Effects on TSS",
    subtitle = "Pooled GAMs | Observations Colored by River Type",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  )

# Display
final_gam_plot

ggsave("figures/gam_macro_aspt_tss_FULL.png", final_gam_plot, width = 8, height = 8, dpi = 300)



# ---------------- MACRO GAM YEARLY -----------------

# Function to loop through all years
run_gam_by_year <- function(year, response_var) {
  data_subset <- wq_eh_lu_merge %>%
    filter(year_collected == year) %>%
    select(aspt, richness, abundance, all_of(response_var)) %>%
    drop_na()
  
  if (nrow(data_subset) < 10) return(NULL)
  
  formula <- as.formula(paste(response_var, "~ s(aspt) + s(richness) + s(abundance)"))
  model <- gam(formula, data = data_subset, method = "REML")
  summary_model <- summary(model)
  
  tibble(
    Year = year,
    Response = response_var,
    Adj_R2 = summary_model$r.sq,
    Dev_Explained = summary_model$dev.expl * 100,
    ASPT_p = summary_model$s.table["s(aspt)", "p-value"],
    Richness_p = summary_model$s.table["s(richness)", "p-value"],
    Abundance_p = summary_model$s.table["s(abundance)", "p-value"]
  )
}

# Loop across years and WQ variables
gam_macro_by_year <- map_dfr(unique(wq_eh_lu_merge$year_collected), function(y) {
  map_dfr(reduced_vars, ~run_gam_by_year(y, .x))
})

# Save
write.csv(gam_macro_by_year, "statistical_analysis/gams/gam_macro_yearly.csv", row.names = FALSE)


# ---------------- MACRO GAM PLOT -----------------

# Filter 2021 data
data_2021 <- wq_eh_lu_merge %>%
  filter(year_collected == 2021) %>%
  select(richness, do_mgl, totp_mgl) %>%
  drop_na()

# Fit GAMs for DO and TP
mod_richness_do_2021 <- gam(do_mgl ~ s(richness), data = data_2021, method = "REML")
mod_richness_tp_2021 <- gam(totp_mgl ~ s(richness), data = data_2021, method = "REML")

# Richness vs. DO (2021)
rich_do_gam <- draw(mod_richness_do_2021, residuals = TRUE) +
  labs(
    title = "Richness vs. Dissolved Oxygen (2021)",
    x = "Macroinvertebrate Richness",
    y = "Effect on DO (mg/L)"
  ) +
  theme_minimal()

ggsave("figures/rich_do_gam.png", rich_do_gam, width = 12, height = 6, dpi = 300)

# Richness vs. TP (2021)
rich_tp_gam <- draw(mod_richness_tp_2021, residuals = TRUE) +
  labs(
    title = "Richness vs. Total Phosphorus (2021)",
    x = "Macroinvertebrate Richness",
    y = "Effect on TP (mg/L)"
  ) +
  theme_minimal()

ggsave("figures/rich_tp_gam.png", rich_tp_gam, width = 12, height = 6, dpi = 300)


# ---------------- MACRO GAM HEATMAP -----------------

# Set up
gam_macro_pooled_plot <- gam_macro_pooled %>%
  select(Response, Adj_R2) %>%
  mutate(Year = "Pooled")

# Yearly data: Keep as-is
gam_macro_yearly_plot <- gam_macro_by_year %>%
  select(Response, Year, Adj_R2) %>%
  mutate(Year = as.character(Year))

# Combine both
gam_macro_r2_plot_data <- bind_rows(gam_macro_yearly_plot, gam_macro_pooled_plot)

# Degine labels
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
gam_macro_r2_plot_data <- gam_macro_r2_plot_data %>%
  mutate(Response_Label = metric_labels[Response])

# Add a dummy row with NA Adj_R2 for spacing
gam_macro_r2_plot_data <- gam_macro_r2_plot_data %>%
  bind_rows(gam_macro_r2_plot_data %>%
              distinct(Response_Label) %>%
              mutate(Year = " ", Adj_R2 = NA)) %>%
  arrange(Response_Label, Year)

# Update factor levels to keep order
gam_macro_r2_plot_data$Year <- factor(
  gam_macro_r2_plot_data$Year,
  levels = c(sort(unique(gam_macro_yearly_plot$Year)), " ", "Pooled")
)

# Plot
gam_macro_heatmap <- ggplot(gam_macro_r2_plot_data, aes(x = Year, y = Response_Label, fill = Adj_R2)) +
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

print(gam_macro_heatmap)

ggsave("figures/gam_macro_heatmap.png", gam_macro_heatmap, width = 10, height = 6, dpi = 300)


# ------------------------------------------------------ MACRO GAM W/O RICHNESS -------------------------------------------------------

# Set up model
compare_gam_models <- function(response_var, data = wq_eh_lu_merge) {
  # Full model with all predictors
  full_formula <- as.formula(paste(response_var, "~ s(aspt) + s(richness) + s(abundance)"))
  full_model <- gam(full_formula, data = data, method = "REML")
  full_summary <- summary(full_model)
  
  # Reduced model without richness
  reduced_formula <- as.formula(paste(response_var, "~ s(aspt) + s(abundance)"))
  reduced_model <- gam(reduced_formula, data = data, method = "REML")
  reduced_summary <- summary(reduced_model)
  
  # Combine results
  tibble(
    Response = response_var,
    Full_Adj_R2 = full_summary$r.sq,
    Full_Dev_Explained = full_summary$dev.expl * 100,
    Reduced_Adj_R2 = reduced_summary$r.sq,
    Reduced_Dev_Explained = reduced_summary$dev.expl * 100,
    Δ_Adj_R2 = round(reduced_summary$r.sq - full_summary$r.sq, 3),
    Δ_Dev_Explained = round((reduced_summary$dev.expl - full_summary$dev.expl) * 100, 2)
  )
}

# Apply across all reduced_vars
gam_comparison_results <- map_dfr(reduced_vars, ~compare_gam_models(.x))

# View or export
print(gam_comparison_results)
write.csv(gam_comparison_results, "statistical_analysis/gams/gam_macro_no_richness.csv", row.names = FALSE)

