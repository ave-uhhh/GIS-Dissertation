
# ----------------------------------------------------- MACRO MULTIPLE LINEAR REGRESSION -------------------------------------------------------

# ---------------- MACRO MLR POOLED -----------------

# Define macro predictor variables
macro_vars <- c("aspt", "abundance", "richness")

# Initialize list to store results
mlr_results <- list()

# Loop through each WQ variable in reduced_vars
for (wq_var in reduced_vars) {
  # Create modeling dataset
  model_df <- wq_eh_merge %>%
    select(all_of(c(wq_var, macro_vars))) %>%
    drop_na()
  
  # Build formula and fit model
  mlr_formula <- as.formula(paste(wq_var, "~", paste(macro_vars, collapse = " + ")))
  mlr_model <- lm(mlr_formula, data = model_df)
  
  # Check for colinearity and print results
  vif_values <- vif(mlr_model)
  print(paste("VIFs for", wq_var))
  print(vif_values)
  
  # Tidy output and add WQ variable as ID
  mlr_output <- tidy(mlr_model) %>%
    mutate(response_variable = wq_var)
  
  # Store in list
  mlr_results[[wq_var]] <- mlr_output
}

# Combine all outputs into a single dataframe
mlr_all_output <- bind_rows(mlr_results)

# Save to CSV
dir.create("statistical_analysis/mlr_macro")
write.csv(mlr_all_output, "statistical_analysis/mlr_macro/mlr_macro_pooled.csv", row.names = FALSE)


# ---------------- MACRO MLR YEARLY -----------------

# Define macro predictor variables
macro_vars <- c("aspt", "abundance", "richness")

# Define years of interest
years <- c(2011, 2013, 2015, 2017, 2019, 2021)

# Initialize list to store all results
mlr_yearly_results <- list()

# Loop through each year
for (yr in years) {
  # Filter data for the year
  yearly_data <- wq_eh_merge %>% filter(year_collected == yr)
  
  # Loop through each WQ variable
  for (wq_var in reduced_vars) {
    
    model_df <- yearly_data %>% 
      select(all_of(c(wq_var, macro_vars))) %>%
      drop_na()
    
    # Skip model if not enough data
    if (nrow(model_df) < 3) next
    
    # Create formula and fit model
    mlr_formula <- as.formula(paste(wq_var, "~", paste(macro_vars, collapse = " + ")))
    mlr_model <- lm(mlr_formula, data = model_df)
    
    # Tidy output and add identifiers
    mlr_output <- tidy(mlr_model) %>%
      mutate(
        response_variable = wq_var,
        year = yr
      )
    
    # Store in list
    mlr_yearly_results[[paste(wq_var, yr, sep = "_")]] <- mlr_output
  }
}

# Combine and export
mlr_yearly_output <- bind_rows(mlr_yearly_results)
write.csv(mlr_yearly_output, "statistical_analysis/mlr_macro/mlr_macro_yearly.csv", row.names = FALSE)



