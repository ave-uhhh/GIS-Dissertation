# ------------------------------------------------------ LU RDA -------------------------------------------------------

# ---------------- LU RDA POOLED -----------------

# Select reduced water quality variables
wq_matrix <- wq_lu_merge %>%
  select(all_of(reduced_vars)) %>%
  drop_na()

# Remove na's
wq_lu_merge <- wq_lu_merge %>%
  drop_na(all_of(c(reduced_vars, "forest", "agriculture", "urban")))

# Select land use predictors
lu_vars <- c("urban", "agriculture", "forest")

# Select WQ and LU matrices from the same rows
wq_matrix <- wq_lu_merge %>% select(all_of(reduced_vars))
lu_matrix <- wq_lu_merge %>% select(all_of(lu_vars)) %>% drop_na()

# Run the RDA: WQ as response, LU as predictor with wq data normalized
wq_matrix_scaled <- scale(wq_matrix)
rda_model_scaled <- rda(wq_matrix_scaled ~ ., data = lu_matrix)

# Significance tests
anova(rda_model_scaled, permutations = 999)
anova(rda_model_scaled, by = "term", permutations = 999)  # Tests individual predictors

# Export the results
## Overall RDA test
rda_overall <- anova(rda_model_scaled, permutations = 999)
dir.create("statistical_analysis/rda_results")
write.csv(as.data.frame(rda_overall), "statistical_analysis/rda_results/rda_lu_pooled_overall.csv")

## Term-specific RDA test
rda_terms <- anova(rda_model_scaled, by = "term", permutations = 999)
write.csv(as.data.frame(rda_terms), "statistical_analysis/rda_results/rda_lu_pooled_terms.csv")


# ---------------- LU RDA PLOT -----------------

# Extract scores for plotting
scores_rda <- scores(rda_model_scaled, display = c("species", "sites", "bp"), scaling = 2)

# Convert to data frames
species_scores <- as.data.frame(scores_rda$species) %>% rownames_to_column("WQ_Var")
biplot_scores <- as.data.frame(scores_rda$biplot) %>% rownames_to_column("Land_Use")

# Define metric labels
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

# Apply metric_labels to species_scores
species_scores <- species_scores %>%
  mutate(Label = metric_labels[WQ_Var])

# Update ggplot text aesthetic
rda_biplot <- ggplot() +
  geom_segment(
    data = biplot_scores,
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2, color = Land_Use),
    arrow = arrow(length = unit(0.2, "cm")), size = 2
  ) +
  geom_text_repel(
    data = species_scores,
    aes(x = RDA1, y = RDA2, label = Label),
    color = "darkblue", size = 3.5,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c(
    "urban" = "#E63946",
    "agriculture" = "#F4A261",
    "forest" = "#2A9D8F"
  )) +
  coord_equal() +
  labs(
    title = "RDA Biplot: Scaled WQ Variables and Land Use Predictors",
    x = "RDA 1",
    y = "RDA 2",
    color = "Land Use"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# Save the plot
ggsave("figures/rda_lu_pooled.png", rda_biplot, width = 10, height = 6, dpi = 300)



# ---------------- LU RDA YEARLY -----------------

# Select years
years <- c(2011, 2013, 2015, 2017, 2019, 2021)

# Run yearly RDA to loop through years
run_yearly_rda <- function(year) {
  subset_data <- wq_lu_merge %>% filter(year_collected == year)
  
  # Ensure complete cases across both sets of variables
  complete_rows <- complete.cases(subset_data[, c(reduced_vars, lu_vars)])
  subset_data <- subset_data[complete_rows, ]
  
  # Extract matrices
  wq_matrix <- subset_data %>% select(all_of(reduced_vars)) %>% scale()
  lu_matrix <- subset_data %>% select(all_of(lu_vars))
  
  # Escape if data is sparse
  if (nrow(lu_matrix) < 2 || ncol(lu_matrix) == 0 ||
      nrow(wq_matrix) < 2 || ncol(wq_matrix) == 0) {
    warning("Insufficient data for year: ", year)
    return(tibble(Year = year, R2 = NA, AdjR2 = NA, F = NA, p_value = NA))
  }
  
  tryCatch({
    rda_model <- rda(wq_matrix ~ ., data = lu_matrix)
    
    rsq <- RsquareAdj(rda_model)
    r2 <- rsq$r.squared
    adj_r2 <- rsq$adj.r.squared
    
    overall_test <- anova(rda_model, permutations = 999)
    term_tests <- anova(rda_model, by = "term", permutations = 999)
    term_df <- as.data.frame(term_tests) %>%
      mutate(Year = year, Term = rownames(term_tests))
    
    write.csv(as.data.frame(overall_test),
              paste0("statistical_analysis/rda_results/rda_lu_yearly_overall", year, ".csv"),
              row.names = FALSE)
    
    write.csv(term_df,
              paste0("statistical_analysis/rda_results/rda_lu_yearly_terms", year, ".csv"),
              row.names = FALSE)
    
    tibble(
      Year = year,
      R2 = r2,
      AdjR2 = adj_r2,
      F = overall_test$F[1],
      p_value = overall_test$`Pr(>F)`[1]
    )
  }, error = function(e) {
    warning("Error for year ", year, ": ", e$message)
    tibble(Year = year, R2 = NA, AdjR2 = NA, F = NA, p_value = NA)
  })
}

yearly_results <- purrr::map_dfr(years, run_yearly_rda)

# Save output summary
write.csv(yearly_results, "statistical_analysis/rda_results/rda_lu_yearly_summary.csv", row.names = FALSE)


# ------------------------------------------------------ MACRO RDA -------------------------------------------------------
# ---------------- MACRO RDA POOLED -----------------

# Select reduced water quality variables
wq_matrix <- wq_eh_merge %>%
  select(all_of(reduced_vars)) %>%
  drop_na()

# Remove na's
wq_eh_merge <- wq_eh_merge %>%
  drop_na(all_of(c(reduced_vars, "richness", "abundance", "aspt")))

# Select land use predictors
macro_vars <- c("richness", "abundance", "aspt")

# Select WQ and EH matrices from the same rows
wq_matrix <- wq_eh_merge %>% select(all_of(reduced_vars))
eh_matrix <- wq_eh_merge %>% select(all_of(macro_vars)) %>% drop_na()

# Run the RDA: EH as response, WQ as predictor with wq data normalized
wq_matrix_scaled <- scale(wq_matrix)
rda_model_scaled <- rda(eh_matrix ~ ., data = as.data.frame(wq_matrix_scaled))

# Significance tests
anova(rda_model_scaled, permutations = 999)
anova(rda_model_scaled, by = "term", permutations = 999)  # Tests individual predictors

# Export the results
## Overall RDA test
rda_overall <- anova(rda_model_scaled, permutations = 999)
write.csv(as.data.frame(rda_overall), "statistical_analysis/rda_results/rda_macro_pooled_overall.csv")

## Term-specific RDA test
rda_terms <- anova(rda_model_scaled, by = "term", permutations = 999)
write.csv(as.data.frame(rda_terms), "statistical_analysis/rda_results/rda_macro_pooled_terms.csv")


# ---------------- MACRO RDA PLOT -----------------

# Extract scores for plotting
scores_rda <- scores(rda_model_scaled, display = c("species", "sites", "bp"), scaling = 2)

# Convert to data frames
species_scores <- as.data.frame(scores_rda$species) %>% rownames_to_column("Metric")
biplot_scores <- as.data.frame(scores_rda$biplot) %>% rownames_to_column("WQ_VAR")

# Define plot labels and colors
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

wq_colors <- c(
  "codmn_mgl" = "#1b9e77",
  "cond_msm" = "#d95f02",
  "do_mgl" = "#7570b3",
  "ph" = "#e7298a",
  "temp_c" = "#66a61e",
  "totn_mgl" = "#e6ab02",
  "totp_mgl" = "#a6761d",
  "tss_mgl" = "#666666"
)

metric_labels <- c(
  "aspt" = "ASPT",
  "abundance" = "Abundance",
  "richness" = "Richness"
)

# Apply metric_labels to species_scores
species_scores <- species_scores %>%
  mutate(Label = metric_labels[Metric])

# Make arrows larger
stretch_factor <- 20

biplot_scores_stretched <- biplot_scores %>%
  mutate(RDA1 = RDA1 * stretch_factor,
         RDA2 = RDA2 * stretch_factor)

# Plot biplot
rda_biplot <- ggplot() +
  geom_segment(
    data = biplot_scores_stretched,
    aes(x = 0, y = 0, xend = RDA1, yend = RDA2, color = WQ_VAR),
    arrow = arrow(length = unit(0.2, "cm")), size = 2
  ) +
  geom_text_repel(
    data = species_scores,
    aes(x = RDA1, y = RDA2, label = Label),
    color = "darkblue", size = 7,
    max.overlaps = Inf
  ) +
  scale_color_manual(
    values = wq_colors,  
    labels = wq_metric_labels,
    breaks = names(wq_metric_labels),
    name = "Water Quality"
  ) +
  coord_equal() +
  labs(
    title = "RDA Biplot: Scaled WQ Variables and Macroinvertebrate Responses",
    x = "RDA 1",
    y = "RDA 2",
    color = "Water Quality Parameters"
  ) +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.position = "bottom", 
    legend.text = element_text(size = 18)
  )


print(rda_biplot)

# Save plot
ggsave("figures/rda_macro_pooled.png", rda_biplot, width = 10, height = 6, dpi = 300)






