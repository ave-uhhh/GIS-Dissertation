# ------------------------------------------------------ VARIANCE PARTITIONING -------------------------------------------------------

# ---------------- VARIANCE PARTITIONING POOLED -----------------

# Define matrices
wq_matrix_scaled <- wq_eh_lu_merge %>% select(all_of(reduced_vars)) %>% scale()  # Response (standardized WQ)
landuse_block <- wq_eh_lu_merge %>% select(urban, agriculture, forest)           # Predictor set 1: Land use
eh_block <- wq_eh_lu_merge %>% select(aspt, richness, abundance)                 # Predictor set 2: Macro metrics

# Run variance partitioning
vp_result <- varpart(wq_matrix_scaled, landuse_block, eh_block)

# Fit partial RDA models for permutation testing
rda_land_partial <- rda(wq_matrix_scaled, landuse_block, eh_block)  # Land use conditioned on macro
rda_eh_partial <- rda(wq_matrix_scaled, eh_block, landuse_block)    # Macro conditioned on land use

# Permutation tests for unique fractions
anova_land <- anova(rda_land_partial, permutations = 999)
anova_eh <- anova(rda_eh_partial, permutations = 999)

# Export results
## Variance fractions
vp_fractions <- as.data.frame(vp_result$part$indfract)
dir.create("statistical_analysis/vp")
write.csv(vp_fractions, "statistical_analysis/vp/vp_pooled_fractions.csv", row.names = TRUE)

## Significance tests for unique fractions
write.csv(as.data.frame(anova_land), "statistical_analysis/vp/vp_pooled_unique_landuse.csv", row.names = FALSE)
write.csv(as.data.frame(anova_eh), "statistical_analysis/vp/vp_pooled_unique_macroinverts.csv", row.names = FALSE)


# ---------------- VARIANCE PARTITIONING YEARLY -----------------

# Define variables
reduced_vars <- c("temp_c", "ph", "tss_mgl", "cond_msm", "totn_mgl", "totp_mgl", "do_mgl", "codmn_mgl")
lu_vars <- c("urban", "agriculture", "forest")
eh_vars <- c("aspt", "richness", "abundance")
years <- c(2011, 2013, 2015, 2017, 2019, 2021)

# Function to run VP per year
run_variance_partitioning <- function(year) {
  data_subset <- wq_eh_lu_merge %>% filter(year_collected == year)
  
  # Define all required variables
  vars_needed <- c(reduced_vars, lu_vars, eh_vars)
  vars_available <- intersect(vars_needed, colnames(data_subset))
  
  # If no variables are available at all, exit early
  if (length(vars_available) == 0) {
    warning("No predictor variables found for year: ", year)
    return(tibble(
      Year = year,
      LU_Unique = NA,
      EH_Unique = NA,
      Shared = NA,
      Residual = NA,
      Total_Explained = NA
    ))
  }
  
  # Filter to complete rows only (only among available vars)
  data_subset <- data_subset %>% drop_na(all_of(vars_available))
  
  if (nrow(data_subset) < 5) {
    warning("Too few complete rows for year: ", year)
    return(tibble(
      Year = year,
      LU_Unique = NA,
      EH_Unique = NA,
      Shared = NA,
      Residual = NA,
      Total_Explained = NA
    ))
  }
  
  print(paste("Running year:", year, "with", nrow(data_subset), "complete cases"))
  
  # Define predictor sets
  wq_vars <- intersect(reduced_vars, colnames(data_subset))
  lu_vars_present <- intersect(lu_vars, colnames(data_subset))
  eh_vars_present <- intersect(eh_vars, colnames(data_subset))
  
  # Double-check blocks are non-empty
  if (length(wq_vars) == 0 || length(lu_vars_present) == 0 || length(eh_vars_present) == 0) {
    warning("Missing variable block(s) for year: ", year)
    return(tibble(
      Year = year,
      LU_Unique = NA,
      EH_Unique = NA,
      Shared = NA,
      Residual = NA,
      Total_Explained = NA
    ))
  }
  
  print("Available columns:")
  print(colnames(data_subset))
  
  print("WQ vars used:")
  print(wq_vars)
  
  print("LU vars used:")
  print(lu_vars_present)
  
  print("EH vars used:")
  print(eh_vars_present)
  
  # Build matrices
  wq_scaled <- scale(data_subset[, wq_vars, drop = FALSE])
  lu_block  <- data_subset[, lu_vars_present, drop = FALSE]
  eh_block  <- data_subset[, eh_vars_present, drop = FALSE]
  
  # Run variance partitioning
  vp_result <- tryCatch({
    varpart(wq_scaled, lu_block, eh_block)
  }, error = function(e) {
    warning("varpart() failed for year: ", year)
    return(NULL)
  })
  
  if (is.null(vp_result)) {
    return(tibble(
      Year = year,
      LU_Unique = NA,
      EH_Unique = NA,
      Shared = NA,
      Residual = NA,
      Total_Explained = NA
    ))
  }
  
  # Fit partial RDAs for significance testing
  rda_land <- rda(wq_scaled, lu_block, eh_block)
  rda_eh   <- rda(wq_scaled, eh_block, lu_block)
  
  # Export results
  write.csv(as.data.frame(vp_result$part$indfract),
            paste0("statistical_analysis/vp/vp_fractions_", year, ".csv"), row.names = TRUE)
  
  write.csv(as.data.frame(anova(rda_land, permutations = 999)),
            paste0("statistical_analysis/vp/vp_landuse_", year, ".csv"), row.names = FALSE)
  
  write.csv(as.data.frame(anova(rda_eh, permutations = 999)),
            paste0("statistical_analysis/vp/vp_macros_", year, ".csv"), row.names = FALSE)
  
  # Extract fractions safely
  adj_r2 <- vp_result$part$indfract$Adj.R.squared
  names(adj_r2) <- c("[a]", "[b]", "[ab]", "Residual")
  
  tibble(
    Year = year,
    LU_Unique = adj_r2["[a]"],
    EH_Unique = adj_r2["[b]"],
    Shared    = adj_r2["[ab]"],
    Residual  = adj_r2["Residual"],
    Total_Explained = sum(adj_r2[c("[a]", "[b]", "[ab]")], na.rm = TRUE)
  )
}

# Run across all years
vp_yearly_results <- map_dfr(years, run_variance_partitioning)

# Export yearly summary
write.csv(vp_yearly_results, "statistical_analysis/vp/vp_yearly_summary.csv", row.names = FALSE)

