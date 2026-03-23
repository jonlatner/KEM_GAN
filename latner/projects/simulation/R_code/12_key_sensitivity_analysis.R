# ============================================================
# 12_key_sensitivity_analysis.R
# Test sensitivity of repU and DiSCO to key variable specification
#
# Purpose: Demonstrate that the same synthetic data can appear
# "safe" or "risky" depending on arbitrary analyst choices about
# which variables to use as keys.
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
# 1. Load Data and Generate Synthetic Data ONCE
# ============================================================

cat("========================================\n")
cat("KEY SENSITIVITY ANALYSIS\n")
cat("========================================\n\n")

data(SD2011)

# Use more variables to allow different key specifications
# Include: sex, age, region, placesize, edu, marital, income, depress
ods <- SD2011[, c("sex", "age", "region", "placesize", "edu", "marital", "income", "depress")]

cat("Original data:", nrow(ods), "records,", ncol(ods), "variables\n\n")

# Generate synthetic data ONCE - we'll measure risk with different key specs
cat("Generating synthetic data (m=5)...\n")
sds <- syn(ods, m = 5, seed = 1237, print.flag = FALSE)
cat("Done.\n\n")

# ============================================================
# 2. Define All Key Specifications to Test
# ============================================================

# Available key variables (excluding target 'depress')
key_vars <- c("sex", "age", "region", "placesize", "edu", "marital", "income")

# Generate all combinations of 1, 2, 3, and 4 keys
all_key_specs <- list()

# Single keys
for (k1 in key_vars) {
  all_key_specs[[length(all_key_specs) + 1]] <- k1
}

# Pairs of keys
for (i in 1:(length(key_vars)-1)) {
  for (j in (i+1):length(key_vars)) {
    all_key_specs[[length(all_key_specs) + 1]] <- c(key_vars[i], key_vars[j])
  }
}

# Triples of keys
for (i in 1:(length(key_vars)-2)) {
  for (j in (i+1):(length(key_vars)-1)) {
    for (k in (j+1):length(key_vars)) {
      all_key_specs[[length(all_key_specs) + 1]] <- c(key_vars[i], key_vars[j], key_vars[k])
    }
  }
}

# Quadruples of keys
for (i in 1:(length(key_vars)-3)) {
  for (j in (i+1):(length(key_vars)-2)) {
    for (k in (j+1):(length(key_vars)-1)) {
      for (l in (k+1):length(key_vars)) {
        all_key_specs[[length(all_key_specs) + 1]] <- c(key_vars[i], key_vars[j], key_vars[k], key_vars[l])
      }
    }
  }
}

cat("Testing", length(all_key_specs), "different key specifications:\n")
cat("  - Single keys:", choose(7, 1), "\n")
cat("  - Key pairs:", choose(7, 2), "\n")
cat("  - Key triples:", choose(7, 3), "\n")
cat("  - Key quadruples:", choose(7, 4), "\n\n")

# ============================================================
# 3. Compute repU and DiSCO for Each Key Specification
# ============================================================

cat("Computing disclosure measures for each specification...\n\n")

results <- data.frame(
  spec_id = integer(),
  n_keys = integer(),
  keys = character(),
  repU_mean = numeric(),
  repU_min = numeric(),
  repU_max = numeric(),
  DiSCO_mean = numeric(),
  DiSCO_min = numeric(),
  DiSCO_max = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(all_key_specs)) {
  keys <- all_key_specs[[i]]

  # Compute disclosure with this key specification
  disc <- tryCatch({
    disclosure(sds, ods,
               keys = keys,
               target = "depress",
               print.flag = FALSE)
  }, error = function(e) NULL)

  if (!is.null(disc)) {
    repU_vals <- disc$ident$repU
    DiSCO_vals <- disc$attrib$DiSCO

    results <- rbind(results, data.frame(
      spec_id = i,
      n_keys = length(keys),
      keys = paste(keys, collapse = ", "),
      repU_mean = mean(repU_vals, na.rm = TRUE),
      repU_min = min(repU_vals, na.rm = TRUE),
      repU_max = max(repU_vals, na.rm = TRUE),
      DiSCO_mean = mean(DiSCO_vals, na.rm = TRUE),
      DiSCO_min = min(DiSCO_vals, na.rm = TRUE),
      DiSCO_max = max(DiSCO_vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    ))
  }

  # Progress indicator
  if (i %% 20 == 0) cat("  Processed", i, "of", length(all_key_specs), "specifications\n")
}

cat("\nDone. Successfully computed", nrow(results), "specifications.\n\n")

# ============================================================
# 4. Analyze Results
# ============================================================

cat("========================================\n")
cat("RESULTS: KEY SENSITIVITY\n")
cat("========================================\n\n")

# Summary by number of keys
summary_by_nkeys <- results %>%
  group_by(n_keys) %>%
  summarise(
    n_specs = n(),
    repU_min = min(repU_mean),
    repU_max = max(repU_mean),
    repU_range = max(repU_mean) - min(repU_mean),
    DiSCO_min = min(DiSCO_mean),
    DiSCO_max = max(DiSCO_mean),
    DiSCO_range = max(DiSCO_mean) - min(DiSCO_mean),
    .groups = 'drop'
  )

cat("Summary by number of keys:\n")
print(as.data.frame(summary_by_nkeys))

# Overall range
cat("\n\nOVERALL SENSITIVITY:\n")
cat("-------------------\n")
cat("repU ranges from", round(min(results$repU_mean), 2), "% to",
    round(max(results$repU_mean), 2), "%\n")
cat("  Range:", round(max(results$repU_mean) - min(results$repU_mean), 2),
    "percentage points\n")
cat("  Ratio (max/min):", round(max(results$repU_mean) / max(min(results$repU_mean), 0.01), 1), "x\n\n")

cat("DiSCO ranges from", round(min(results$DiSCO_mean), 2), "% to",
    round(max(results$DiSCO_mean), 2), "%\n")
cat("  Range:", round(max(results$DiSCO_mean) - min(results$DiSCO_mean), 2),
    "percentage points\n")
cat("  Ratio (max/min):", round(max(results$DiSCO_mean) / max(min(results$DiSCO_mean), 0.01), 1), "x\n")

# Find extreme cases
cat("\n\nEXTREME CASES:\n")
cat("--------------\n")

# Lowest repU
lowest_repU <- results %>% arrange(repU_mean) %>% head(5)
cat("\nLowest repU (appears safest):\n")
for (i in 1:nrow(lowest_repU)) {
  cat("  ", lowest_repU$keys[i], ": repU =", round(lowest_repU$repU_mean[i], 2), "%\n")
}

# Highest repU
highest_repU <- results %>% arrange(desc(repU_mean)) %>% head(5)
cat("\nHighest repU (appears riskiest):\n")
for (i in 1:nrow(highest_repU)) {
  cat("  ", highest_repU$keys[i], ": repU =", round(highest_repU$repU_mean[i], 2), "%\n")
}

# Lowest DiSCO
lowest_DiSCO <- results %>% arrange(DiSCO_mean) %>% head(5)
cat("\nLowest DiSCO (appears safest):\n")
for (i in 1:nrow(lowest_DiSCO)) {
  cat("  ", lowest_DiSCO$keys[i], ": DiSCO =", round(lowest_DiSCO$DiSCO_mean[i], 2), "%\n")
}

# Highest DiSCO
highest_DiSCO <- results %>% arrange(desc(DiSCO_mean)) %>% head(5)
cat("\nHighest DiSCO (appears riskiest):\n")
for (i in 1:nrow(highest_DiSCO)) {
  cat("  ", highest_DiSCO$keys[i], ": DiSCO =", round(highest_DiSCO$DiSCO_mean[i], 2), "%\n")
}

# ============================================================
# 5. Demonstrate Manipulation Potential
# ============================================================

cat("\n\n========================================\n")
cat("MANIPULATION POTENTIAL\n")
cat("========================================\n\n")

cat("The SAME synthetic data can be reported as:\n\n")

cat("'LOW RISK' by choosing keys =", lowest_repU$keys[1], "\n")
cat("   repU =", round(lowest_repU$repU_mean[1], 2), "%, DiSCO =",
    round(results$DiSCO_mean[results$keys == lowest_repU$keys[1]], 2), "%\n\n")

cat("'HIGH RISK' by choosing keys =", highest_repU$keys[1], "\n")
cat("   repU =", round(highest_repU$repU_mean[1], 2), "%, DiSCO =",
    round(results$DiSCO_mean[results$keys == highest_repU$keys[1]], 2), "%\n\n")

cat("This demonstrates that key specification is an arbitrary analyst choice\n")
cat("that can dramatically affect risk assessment conclusions.\n")

# ============================================================
# 6. Generate LaTeX Table
# ============================================================

cat("\n\n========================================\n")
cat("GENERATING LATEX TABLE\n")
cat("========================================\n\n")

# Create summary table for paper
latex_table <- paste0(
  "% Generated by 12_key_sensitivity_analysis.R\n",
  "% ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
  "\\begin{table}[h]\n",
  "\\footnotesize\\sf\\centering\n",
  "\\caption{Sensitivity of disclosure risk measures to key specification. The same synthetic data yields dramatically different risk estimates depending on which variables are designated as keys.\\label{tab:key_sensitivity}}\n",
  "\\begin{tabular}{@{}lrrrrrr@{}}\n",
  "\\toprule\n",
  " & \\multicolumn{3}{c}{Identity Risk ($repU$)} & \\multicolumn{3}{c}{Attribute Risk ($DiSCO$)} \\\\\n",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n",
  "Number of keys & Min & Max & Range & Min & Max & Range \\\\\n",
  "\\midrule\n"
)

for (i in 1:nrow(summary_by_nkeys)) {
  latex_table <- paste0(latex_table,
    summary_by_nkeys$n_keys[i], " key", ifelse(summary_by_nkeys$n_keys[i] > 1, "s", ""),
    " (n=", summary_by_nkeys$n_specs[i], ")",
    " & ", sprintf("%.1f", summary_by_nkeys$repU_min[i]),
    " & ", sprintf("%.1f", summary_by_nkeys$repU_max[i]),
    " & ", sprintf("%.1f", summary_by_nkeys$repU_range[i]),
    " & ", sprintf("%.1f", summary_by_nkeys$DiSCO_min[i]),
    " & ", sprintf("%.1f", summary_by_nkeys$DiSCO_max[i]),
    " & ", sprintf("%.1f", summary_by_nkeys$DiSCO_range[i]),
    " \\\\\n"
  )
}

latex_table <- paste0(latex_table,
  "\\midrule\n",
  "Overall",
  " & ", sprintf("%.1f", min(results$repU_mean)),
  " & ", sprintf("%.1f", max(results$repU_mean)),
  " & ", sprintf("%.1f", max(results$repU_mean) - min(results$repU_mean)),
  " & ", sprintf("%.1f", min(results$DiSCO_mean)),
  " & ", sprintf("%.1f", max(results$DiSCO_mean)),
  " & ", sprintf("%.1f", max(results$DiSCO_mean) - min(results$DiSCO_mean)),
  " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\end{table}\n"
)

# Write table to file
table_file <- paste0(tables_path, "table_key_sensitivity.tex")
writeLines(latex_table, table_file)
cat("LaTeX table saved to:", table_file, "\n")

# ============================================================
# 7. Additional Analysis: Correlation with Uniqueness
# ============================================================

cat("\n\n========================================\n")
cat("ADDITIONAL ANALYSIS\n")
cat("========================================\n\n")

# Does risk correlate with the number of unique combinations?
results$n_unique_orig <- NA

for (i in 1:nrow(results)) {
  keys <- strsplit(results$keys[i], ", ")[[1]]
  key_combo <- apply(ods[, keys, drop = FALSE], 1, paste, collapse = "_")
  results$n_unique_orig[i] <- length(unique(key_combo))
}

cat("Correlation between number of unique key combinations and risk measures:\n")
cat("  Cor(n_unique, repU):", round(cor(results$n_unique_orig, results$repU_mean), 3), "\n")
cat("  Cor(n_unique, DiSCO):", round(cor(results$n_unique_orig, results$DiSCO_mean), 3), "\n")

# ============================================================
# 8. Save Full Results
# ============================================================

save(results, summary_by_nkeys, all_key_specs,
     file = paste0(output_path, "key_sensitivity_results.RData"))

cat("\nFull results saved to:", paste0(output_path, "key_sensitivity_results.RData"), "\n")

# ============================================================
# 9. Final Summary
# ============================================================

cat("\n\n========================================\n")
cat("SUMMARY: KEY SENSITIVITY FINDINGS\n")
cat("========================================\n\n")

cat("1. DRAMATIC VARIATION IN RISK ESTIMATES:\n")
cat("   - repU varies by", round(max(results$repU_mean) - min(results$repU_mean), 1),
    "percentage points across key specifications\n")
cat("   - DiSCO varies by", round(max(results$DiSCO_mean) - min(results$DiSCO_mean), 1),
    "percentage points across key specifications\n\n")

cat("2. SAME DATA, DIFFERENT CONCLUSIONS:\n")
cat("   - With keys = '", lowest_repU$keys[1], "': repU = ",
    round(lowest_repU$repU_mean[1], 1), "% (LOW RISK)\n", sep = "")
cat("   - With keys = '", highest_repU$keys[1], "': repU = ",
    round(highest_repU$repU_mean[1], 1), "% (HIGH RISK)\n\n", sep = "")

cat("3. IMPLICATION FOR PRACTICE:\n")
cat("   Risk estimates from repU and DiSCO are highly sensitive to arbitrary\n")
cat("   analyst choices. A dataset can be made to appear safe or risky simply\n")
cat("   by choosing different key variables. This undermines the measures'\n")
cat("   utility for principled release decisions.\n\n")

cat("This validates Failure Mode 3 (Key Sensitivity) described in the paper.\n")
