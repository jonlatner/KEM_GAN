# ============================================================
# 11_structure_preservation_table.R
# Generate table for Section 5.3: Structure Preservation Analysis
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

# ============================================================
# Load and Prepare Data
# ============================================================

data(SD2011)
ods <- SD2011[, c("sex", "age", "region", "placesize", "depress")]

# Create key combination
ods$key <- paste(ods$sex, ods$age, ods$region, ods$placesize, sep = "_")
original_keys <- unique(ods$key)

cat("Original data:\n")
cat("  Total records:", nrow(ods), "\n")
cat("  Unique key combinations:", length(original_keys), "\n\n")

# ============================================================
# Generate Synthetic Data and Analyze Structure Preservation
# ============================================================

m_synths <- 5
sds <- syn(ods[, 1:5], m = m_synths, seed = 1237, print.flag = FALSE)

# Analyze each synthetic dataset
results <- data.frame(
  synth_id = 1:m_synths,
  unique_keys = integer(m_synths),
  keys_in_original = integer(m_synths),
  novel_keys = integer(m_synths),
  pct_novel = numeric(m_synths)
)

all_synth_keys <- c()

for (i in 1:m_synths) {
  synth_df <- sds$syn[[i]]
  synth_df$key <- paste(synth_df$sex, synth_df$age, synth_df$region, synth_df$placesize, sep = "_")

  synth_keys <- unique(synth_df$key)
  novel_keys <- setdiff(synth_keys, original_keys)

  results$unique_keys[i] <- length(synth_keys)
  results$keys_in_original[i] <- sum(synth_keys %in% original_keys)
  results$novel_keys[i] <- length(novel_keys)
  results$pct_novel[i] <- 100 * length(novel_keys) / length(synth_keys)

  all_synth_keys <- c(all_synth_keys, synth_keys)
}

# Summary statistics
all_unique_synth_keys <- unique(all_synth_keys)
all_novel_keys <- setdiff(all_unique_synth_keys, original_keys)

cat("Results per synthetic dataset:\n")
print(results)

cat("\nSummary:\n")
cat("  Average unique keys per synthetic:", round(mean(results$unique_keys)), "\n")
cat("  Average novel keys per synthetic:", round(mean(results$novel_keys)), "\n")
cat("  Average percentage novel:", round(mean(results$pct_novel), 1), "%\n")
cat("  P(key in original | key in synthetic):",
    round(100 * sum(all_unique_synth_keys %in% original_keys) / length(all_unique_synth_keys), 1), "%\n")

# ============================================================
# Generate LaTeX Table
# ============================================================

# The table is embedded directly in the paper, but we output the key numbers
# for verification

cat("\n\n========================================\n")
cat("VALUES FOR TABLE IN PAPER:\n")
cat("========================================\n")
cat("Total records (original): 5,000\n")
cat("Total records (synthetic): 5,000\n")
cat("Unique key combinations (original):", length(original_keys), "\n")
cat("Unique key combinations (synthetic avg):", round(mean(results$unique_keys)), "\n")
cat("Number of novel keys (avg):", round(mean(results$novel_keys)), "\n")
cat("Percentage of synthetic keys that are novel:", round(mean(results$pct_novel), 1), "%\n")
cat("P(key in orig | key in synth):",
    round(100 * sum(all_unique_synth_keys %in% original_keys) / length(all_unique_synth_keys), 1), "%\n")

# ============================================================
# Save Results
# ============================================================

save(results, original_keys, all_unique_synth_keys, all_novel_keys,
     file = paste0(base_path, "R_code/output/structure_preservation_results.RData"))

cat("\nResults saved to output/structure_preservation_results.RData\n")
