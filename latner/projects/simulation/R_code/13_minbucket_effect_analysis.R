# ============================================================
# 13_minbucket_effect_analysis.R
# Test effect of CART minbucket parameter on structure preservation
#
# Purpose: Empirically verify that increasing minbucket reduces
# structure preservation (generates more novel combinations)
#
# Authors: Jonathan Latner, Marcel Neunhoeffer, Jorg Drechsler
# Date: 2025-01-21
# ============================================================

rm(list = ls())

library(synthpop)
library(tidyverse)

set.seed(1237)

# Paths
base_path <- "/Users/marcelneunhoeffer/Dropbox/Apps/Overleaf/JOS-privacy-measures/"
base_path = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
tables_path <- paste0(base_path, "tables/")
output_path <- paste0(base_path, "R_code/output/")

# ============================================================
# 1. Load Data
# ============================================================

cat("========================================\n")
cat("MINBUCKET EFFECT ANALYSIS\n")
cat("========================================\n\n")

data(SD2011)

# Use same variables as structure preservation analysis
ods <- SD2011[, c("sex", "age", "region", "placesize", "depress")]

# Create key combination in original
ods$key <- paste(ods$sex, ods$age, ods$region, ods$placesize, sep = "_")
original_keys <- unique(ods$key)

cat("Original data:", nrow(ods), "records\n")
cat("Unique key combinations:", length(original_keys), "\n\n")

# ============================================================
# 2. Define minbucket values to test
# ============================================================

# minbucket controls minimum number of observations in terminal nodes
# Default in synthpop is 5
# Higher values = more generalization = less structure preservation

minbucket_values <- c(1, 5, 10, 20, 50, 100, 200)

cat("Testing minbucket values:", paste(minbucket_values, collapse = ", "), "\n\n")

# ============================================================
# 3. Generate synthetic data for each minbucket value
# ============================================================

results <- data.frame(
  minbucket = integer(),
  synth_id = integer(),
  n_records = integer(),
  unique_keys_synth = integer(),
  keys_in_original = integer(),
  novel_keys = integer(),
  pct_novel = numeric(),
  stringsAsFactors = FALSE
)

m_synths <- 5  # Number of synthetic datasets per minbucket value

for (mb in minbucket_values) {
  cat("Processing minbucket =", mb, "...\n")

  # Set CART parameters
  # minbucket applies to all variables synthesized with CART
  cart_params <- list(minbucket = mb)

  # Generate synthetic data
  sds <- tryCatch({
    syn(ods[, 1:5],
        m = m_synths,
        seed = 1237,
        cart.minbucket = mb,
        print.flag = FALSE)
  }, error = function(e) {
    cat("  Error with minbucket =", mb, ":", e$message, "\n")
    NULL
  })

  if (is.null(sds)) next

  # Analyze each synthetic dataset
  for (i in 1:m_synths) {
    synth_df <- sds$syn[[i]]
    synth_df$key <- paste(synth_df$sex, synth_df$age,
                          synth_df$region, synth_df$placesize, sep = "_")

    synth_keys <- unique(synth_df$key)
    novel_keys <- setdiff(synth_keys, original_keys)

    results <- rbind(results, data.frame(
      minbucket = mb,
      synth_id = i,
      n_records = nrow(synth_df),
      unique_keys_synth = length(synth_keys),
      keys_in_original = sum(synth_keys %in% original_keys),
      novel_keys = length(novel_keys),
      pct_novel = 100 * length(novel_keys) / length(synth_keys),
      stringsAsFactors = FALSE
    ))
  }
}

cat("\nSynthesis complete.\n\n")

# ============================================================
# 4. Summarize results by minbucket value
# ============================================================

summary_results <- results %>%
  group_by(minbucket) %>%
  summarise(
    n_synths = n(),
    avg_unique_keys = mean(unique_keys_synth),
    avg_novel_keys = mean(novel_keys),
    avg_pct_novel = mean(pct_novel),
    sd_pct_novel = sd(pct_novel),
    min_pct_novel = min(pct_novel),
    max_pct_novel = max(pct_novel),
    .groups = 'drop'
  )

cat("========================================\n")
cat("RESULTS BY MINBUCKET VALUE\n")
cat("========================================\n\n")

print(as.data.frame(summary_results))

# ============================================================
# 5. Compute disclosure risk measures for each minbucket
# ============================================================

cat("\n\nComputing disclosure risk measures...\n\n")

disclosure_results <- data.frame(
  minbucket = integer(),
  repU_mean = numeric(),
  DiSCO_mean = numeric(),
  stringsAsFactors = FALSE
)

for (mb in minbucket_values) {
  cat("Computing disclosure for minbucket =", mb, "...\n")

  # Regenerate synthetic data (or use cached)
  sds <- tryCatch({
    syn(ods[, 1:5],
        m = m_synths,
        seed = 1237,
        cart.minbucket = mb,
        print.flag = FALSE)
  }, error = function(e) NULL)

  if (is.null(sds)) next

  # Compute disclosure measures
  disc <- tryCatch({
    disclosure(sds, ods[, 1:5],
               keys = c("sex", "age", "region", "placesize"),
               target = "depress",
               print.flag = FALSE)
  }, error = function(e) NULL)

  if (!is.null(disc)) {
    disclosure_results <- rbind(disclosure_results, data.frame(
      minbucket = mb,
      repU_mean = mean(disc$ident$repU, na.rm = TRUE),
      DiSCO_mean = mean(disc$attrib$DiSCO, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  }
}

# Merge with structure preservation results
combined_results <- summary_results %>%
  left_join(disclosure_results, by = "minbucket")

cat("\n========================================\n")
cat("COMBINED RESULTS: STRUCTURE & DISCLOSURE\n")
cat("========================================\n\n")

print(as.data.frame(combined_results[, c("minbucket", "avg_pct_novel", "repU_mean", "DiSCO_mean")]))

# ============================================================
# 6. Analyze relationships
# ============================================================

cat("\n========================================\n")
cat("RELATIONSHIP ANALYSIS\n")
cat("========================================\n\n")

# Correlation between minbucket and novel combination rate
cor_mb_novel <- cor(combined_results$minbucket, combined_results$avg_pct_novel)
cat("Correlation(minbucket, pct_novel):", round(cor_mb_novel, 3), "\n")

# Correlation between novel rate and disclosure measures
cor_novel_repU <- cor(combined_results$avg_pct_novel, combined_results$repU_mean, use = "complete.obs")
cor_novel_DiSCO <- cor(combined_results$avg_pct_novel, combined_results$DiSCO_mean, use = "complete.obs")
cat("Correlation(pct_novel, repU):", round(cor_novel_repU, 3), "\n")
cat("Correlation(pct_novel, DiSCO):", round(cor_novel_DiSCO, 3), "\n")

# ============================================================
# 7. Generate LaTeX table
# ============================================================

cat("\n========================================\n")
cat("GENERATING LATEX TABLE\n")
cat("========================================\n\n")

latex_table <- paste0(
  "% Generated by 13_minbucket_effect_analysis.R\n",
  "% ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
  "\\begin{table}[h]\n",
  "\\footnotesize\\sf\\centering\n",
  "\\caption{Effect of CART \\texttt{minbucket} parameter on structure preservation and disclosure risk. Higher \\texttt{minbucket} values reduce structure preservation (more novel combinations) and measured disclosure risk.\\label{tab:minbucket_effect}}\n",
  "\\begin{tabular}{@{}rrrrrr@{}}\n",
  "\\toprule\n",
  "\\texttt{minbucket} & Unique Keys & Novel Keys & \\% Novel & $repU$ & $DiSCO$ \\\\\n",
  "\\midrule\n"
)

for (i in 1:nrow(combined_results)) {
  latex_table <- paste0(latex_table,
    combined_results$minbucket[i],
    " & ", round(combined_results$avg_unique_keys[i]),
    " & ", round(combined_results$avg_novel_keys[i]),
    " & ", sprintf("%.1f", combined_results$avg_pct_novel[i]),
    " & ", sprintf("%.1f", combined_results$repU_mean[i]),
    " & ", sprintf("%.1f", combined_results$DiSCO_mean[i]),
    " \\\\\n"
  )
}

latex_table <- paste0(latex_table,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\end{table}\n"
)

table_file <- paste0(tables_path, "table_minbucket_effect.tex")
writeLines(latex_table, table_file)
cat("LaTeX table saved to:", table_file, "\n")

# ============================================================
# 8. Key findings
# ============================================================

cat("\n========================================\n")
cat("KEY FINDINGS: MINBUCKET EFFECT\n")
cat("========================================\n\n")

# Find default (minbucket=5) and compare to extremes
default_row <- combined_results %>% filter(minbucket == 5)
min_row <- combined_results %>% filter(minbucket == min(minbucket))
max_row <- combined_results %>% filter(minbucket == max(minbucket))

cat("1. STRUCTURE PRESERVATION:\n")
cat("   - minbucket=1 (minimum): ", sprintf("%.1f%%", min_row$avg_pct_novel), " novel combinations\n")
cat("   - minbucket=5 (default): ", sprintf("%.1f%%", default_row$avg_pct_novel), " novel combinations\n")
cat("   - minbucket=", max_row$minbucket, " (maximum): ", sprintf("%.1f%%", max_row$avg_pct_novel), " novel combinations\n\n")

cat("2. DISCLOSURE RISK MEASURES:\n")
cat("   - minbucket=1: repU=", sprintf("%.1f%%", min_row$repU_mean), ", DiSCO=", sprintf("%.1f%%", min_row$DiSCO_mean), "\n")
cat("   - minbucket=5: repU=", sprintf("%.1f%%", default_row$repU_mean), ", DiSCO=", sprintf("%.1f%%", default_row$DiSCO_mean), "\n")
cat("   - minbucket=", max_row$minbucket, ": repU=", sprintf("%.1f%%", max_row$repU_mean), ", DiSCO=", sprintf("%.1f%%", max_row$DiSCO_mean), "\n\n")

cat("3. INTERPRETATION:\n")
if (cor_mb_novel > 0.5) {
  cat("   - CONFIRMED: Higher minbucket increases novel combination rate\n")
  cat("   - Structure preservation DECREASES as minbucket increases\n")
} else if (cor_mb_novel < -0.5) {
  cat("   - UNEXPECTED: Higher minbucket decreases novel combination rate\n")
} else {
  cat("   - WEAK EFFECT: minbucket has limited impact on structure preservation\n")
}

cat("\n4. IMPLICATION FOR PRACTITIONERS:\n")
cat("   - Default minbucket=5 produces ", sprintf("%.1f%%", default_row$avg_pct_novel), " novel combinations\n")
cat("   - Increasing minbucket can reduce structure preservation\n")
cat("   - This may reduce false negative risk but also affects utility\n")

# ============================================================
# 9. Save results
# ============================================================

save(results, summary_results, combined_results, disclosure_results,
     file = paste0(output_path, "minbucket_effect_results.RData"))

cat("\nResults saved to:", paste0(output_path, "minbucket_effect_results.RData"), "\n")
