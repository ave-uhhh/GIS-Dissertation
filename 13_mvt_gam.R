# ------------------------------------------------------ FULL GAM MAINSTEM V TRIBUTARY -------------------------------------------------------

# Sort rivers into mainstem or tributary
mainstem_rivers <- c("Mekong River", "Mekong  River")
tributary_rivers <- c("Sekong River ", "Se San River", "Srepork", "Se Bangfai River", 
                      "Nam Ou River", "Mae Kok River", "Mun", "Song Khram River", 
                      "Tonle Sap Lake","Tonle Sap River", "Bassac River")

# same merge as in section 1 with additional column
wq_eh_lu_merge <- wq_eh_merge %>%
  left_join(select(land_use, statid, year_collected, urban, agriculture, forest),
            by = c("wq_statid" = "statid", "year_collected")) %>%
  select(wq_statid, year_collected, all_of(reduced_vars), wq_River_Names_og, aspt, richness, abundance, urban, agriculture, forest, type) %>%
  drop_na()

# Create new column based on river name
wq_eh_lu_merge$site_type <- ifelse(wq_eh_lu_merge$wq_River_Names_og %in% mainstem_rivers, 
                                   "mainstem", 
                                   "tributary")

# Subset data into site groups
mainstem_data <- filter(wq_eh_lu_merge, site_type == "mainstem")
tributary_data <- filter(wq_eh_lu_merge, site_type == "tributary")

# Extract statid-year pairs from each data subset
mainstem_keys <- mainstem_data %>%
  select(wq_statid, year_collected)
tributary_keys <- tributary_data %>% 
  select(wq_statid, year_collected)

# Choose predictors
lu_vars <- c("urban", "agriculture", "forest")
eh_vars <- c("aspt", "abundance", "richness")
all_predictors <- c(eh_vars, lu_vars)

# Mainstem
wq_main <- mainstem_data %>% select(all_of(reduced_vars))
predictors_main <- mainstem_data %>% select(all_of(all_predictors))

# Tributary
wq_trib <- tributary_data %>% select(all_of(reduced_vars))
predictors_trib <- tributary_data %>% select(all_of(all_predictors))

# ---------------- GAM POOLED M v. T -----------------
# mainstem
run_gam_pooled_main <- function(response_var) {
  formula <- as.formula(paste(response_var, "~", paste0("s(", colnames(predictors_main), ")", collapse = " + ")))
  
  model <- gam(formula, data = mainstem_data, method = "REML")
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

# trib
run_gam_pooled_trib <- function(response_var) {
  formula <- as.formula(paste(response_var, "~", paste0("s(", colnames(predictors_trib), ")", collapse = " + ")))
  
  model <- gam(formula, data = tributary_data, method = "REML")
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

# Run for all WQ variables
gam_pooled_main <- map_dfr(reduced_vars, run_gam_pooled_main)
gam_pooled_trib <- map_dfr(reduced_vars, run_gam_pooled_trib)

# save results
write.csv(gam_pooled_main, "statistical_analysis/gams/gam_pooled_mainstem.csv", row.names = FALSE)
write.csv(gam_pooled_trib, "statistical_analysis/gams/gam_pooled_triburaty.csv", row.names = FALSE)
