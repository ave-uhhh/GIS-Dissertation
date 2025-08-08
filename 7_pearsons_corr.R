# Set relevant working directory; all other file paths in the code will be correct
setwd("/home/s2502571/dissfinal")

# ------------------------------------------------------ PEARSON'S CORRELATION ---------------------------------------------------------------

# Select numeric variables except 'year_collected'
numeric_vars <- wq_eh_lu_merge %>%
  select(where(is.numeric)) %>%
  select(-year_collected)

# Run Pearson's correlation
cor_matrix <- cor(numeric_vars, method = "pearson")

# Convert matrix to data frame
cor_df <- as.data.frame(cor_matrix)

# Write to CSV
write_csv(cor_df, "statistical_analysis/pearson_correlations.csv")

# Select only the predictor variables
selected_vars <- wq_eh_lu_merge %>%
  select(aspt, richness, abundance, urban, agriculture, forest)

# Run Pearson's correlation
cor_matrix <- cor(selected_vars, method = "pearson")

# Plot heatmap and save to pdf
pdf("figures/pearson_heatmap.pdf", width = 8, height = 8)

corrplot(cor_matrix, method = "color", type = "lower",
         tl.pos = "d",      
         tl.cex = 0.8, tl.col = "black", number.cex = 0.7,
         addCoef.col = "black", col = colorRampPalette(c("red", "white", "blue"))(200),
         title = "Pearson correlation matrix", mar = c(0, 0, 2, 0)) 

dev.off()
