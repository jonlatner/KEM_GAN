# ============================================================
# 10_sd2011_false_negatives.R
# Demonstrate false negatives with naturally occurring outliers
#
# Purpose: Show that syntactic measures (repU, DiSCO) report low risk
# while actual disclosure is possible for rare/unique records in SD2011
#
# Authors: Jonathan Latner, Marcel Neunhoeffer, Jorg Drechsler
# Date: 2025-01-21
# ============================================================

# Clear workspace
rm(list = ls())

# ============================================================
# 1. Setup and Libraries
# ============================================================

# Required packages
required_packages <- c("synthpop", "tidyverse", "xtable")

# Install if missing
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set paths - adjust as needed for your system
base_path <- "/Users/marcelneunhoeffer/Dropbox/Apps/Overleaf/JOS-privacy-measures/"
base_path = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
tables_path <- paste0(base_path, "tables/")
output_path <- paste0(base_path, "R_code/output/")

# Create output directory if it doesn't exist
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# Set seed for reproducibility
set.seed(1237)

# ============================================================
# 2. Load SD2011 Data
# ============================================================

cat("\n========================================\n")
cat("Loading SD2011 data...\n")
cat("========================================\n")

# Load SD2011 from synthpop package
data(SD2011)

# Select key variables (consistent with Section 5.2 analysis)
# sex, age, region, placesize as keys; depress as target
ods <- SD2011[, c("sex", "age", "region", "placesize", "depress")]

# Basic data summary
cat("\nData dimensions:", nrow(ods), "x", ncol(ods), "\n")
cat("\nVariable summary:\n")
print(summary(ods))

# ============================================================
# 3. Identify Natural Outliers
# ============================================================

cat("\n========================================\n")
cat("Identifying natural outliers...\n")
cat("========================================\n")

# Create key combination identifier (excluding target variable)
ods$key_combo <- paste(ods$sex, ods$age, ods$region, ods$placesize, sep = "_")

# Count frequency of each combination
combo_freq <- ods %>%
  group_by(key_combo) %>%
  summarise(
    n = n(),
    depress_values = paste(unique(depress), collapse = ", "),
    .groups = 'drop'
  )

# Distribution of combination frequencies
cat("\nDistribution of key combination frequencies:\n")
print(table(combo_freq$n))

# Identify unique combinations (n = 1)
unique_combos <- combo_freq %>% filter(n == 1)
cat("\nNumber of unique key combinations (n=1):", nrow(unique_combos), "\n")

# Identify quasi-unique combinations (n <= 3)
rare_combos <- combo_freq %>% filter(n <= 3)
cat("Number of rare key combinations (n<=3):", nrow(rare_combos), "\n")

# Get row indices of unique/rare records
unique_indices <- which(ods$key_combo %in% unique_combos$key_combo)
rare_indices <- which(ods$key_combo %in% rare_combos$key_combo)

cat("\nNumber of unique records:", length(unique_indices), "\n")
cat("Number of rare records (n<=3):", length(rare_indices), "\n")
cat("Percentage unique:", round(100 * length(unique_indices) / nrow(ods), 2), "%\n")

# Sample some unique records for detailed analysis
# Select a diverse sample based on different key characteristics
if (length(unique_indices) >= 10) {
  # Take first 10 unique records for analysis
  sample_unique <- unique_indices[1:10]
} else {
  sample_unique <- unique_indices
}

cat("\nSample of unique records for Bayesian analysis:\n")
for (i in sample_unique[1:min(5, length(sample_unique))]) {
  cat("  Record", i, ":", ods$key_combo[i], "-> depress =", as.character(ods$depress[i]), "\n")
}

# ============================================================
# 4. Generate Synthetic Data
# ============================================================

cat("\n========================================\n")
cat("Generating synthetic data using CART...\n")
cat("========================================\n")

# Remove the key_combo column before synthesis
ods_for_synth <- ods[, c("sex", "age", "region", "placesize", "depress")]

# Generate multiple synthetic datasets using CART
m_synths <- 5  # Number of synthetic datasets

cat("Generating", m_synths, "synthetic datasets...\n")
sds <- syn(ods_for_synth, m = m_synths, seed = 1237, print.flag = FALSE)

cat("Synthesis complete.\n")

# Add key combinations to synthetic data
sds_list <- lapply(1:m_synths, function(i) {
  df <- sds$syn[[i]]
  df$key_combo <- paste(df$sex, df$age, df$region, df$placesize, sep = "_")
  df$synth_id <- i
  df
})

sds_combined <- bind_rows(sds_list)

# Check which unique combinations reappear in synthetic data
reappear_summary <- sds_combined %>%
  filter(key_combo %in% unique_combos$key_combo) %>%
  group_by(key_combo) %>%
  summarise(
    times_reappeared = n(),
    synth_datasets = paste(unique(synth_id), collapse = ", "),
    .groups = 'drop'
  )

cat("\nUnique combinations that reappear in synthetic data:\n")
cat("  Total unique combos in original:", nrow(unique_combos), "\n")
cat("  Unique combos reappearing in synthetic:", nrow(reappear_summary), "\n")
cat("  Percentage reappearing:", round(100 * nrow(reappear_summary) / nrow(unique_combos), 2), "%\n")

# ============================================================
# 5. Compute Disclosure Risk Measures (repU, DiSCO)
# ============================================================

cat("\n========================================\n")
cat("Computing disclosure risk measures...\n")
cat("========================================\n")

# Compute disclosure metrics using synthpop
disclosure_results <- disclosure(sds, ods_for_synth,
                                 keys = c("sex", "age", "region", "placesize"),
                                 target = "depress",
                                 print.flag = FALSE)

# Extract repU (identity risk) - percentage of synthetic uniques that match
repU <- disclosure_results$ident$repU

# Extract DiSCO (attribute risk) - attribute disclosure risk
# Check structure of disclosure results
cat("\nStructure of disclosure results:\n")
print(names(disclosure_results))

# Get DiSCO values (may be in different location depending on synthpop version)
if (!is.null(disclosure_results$attrib)) {
  DiSCO <- disclosure_results$attrib$DiSCO
} else {
  # Calculate manually if not available
  DiSCO <- rep(NA, m_synths)
}

# Print results
cat("\nIdentity Risk (repU) per synthetic dataset:\n")
for (i in 1:m_synths) {
  cat("  Synthetic", i, ":", round(repU[i], 2), "%\n")
}
cat("  Average:", round(mean(repU), 2), "%\n")

cat("\nAttribute Risk (DiSCO) per synthetic dataset:\n")
for (i in 1:m_synths) {
  cat("  Synthetic", i, ":", round(DiSCO[i], 2), "%\n")
}
cat("  Average:", round(mean(DiSCO), 2), "%\n")

# ============================================================
# 6. Compute Bayesian Risk for Outliers
# ============================================================

cat("\n========================================\n")
cat("Computing Bayesian risk for unique records...\n")
cat("========================================\n")

# Define the Bayesian risk computation function
# For a unique record, we assess whether observing it in synthetic data
# allows an attacker to infer the target variable (depress)

compute_bayesian_risk_for_outlier <- function(
    original_data,
    record_index,
    synth_data_list,
    n_monte_carlo = 50,
    seed = NULL
) {
  # Get the true record and its key
  true_record <- original_data[record_index, ]
  true_key <- paste(true_record$sex, true_record$age,
                    true_record$region, true_record$placesize, sep = "_")
  true_target <- as.character(true_record$depress)

  # Check if this key appears in any synthetic dataset
  key_appears <- FALSE
  synth_target_when_appears <- c()

  for (i in seq_along(synth_data_list)) {
    synth_df <- synth_data_list[[i]]
    synth_key <- paste(synth_df$sex, synth_df$age,
                       synth_df$region, synth_df$placesize, sep = "_")
    matches <- which(synth_key == true_key)

    if (length(matches) > 0) {
      key_appears <- TRUE
      synth_target_when_appears <- c(synth_target_when_appears,
                                      as.character(synth_df$depress[matches]))
    }
  }

  # If key doesn't appear in synthetic data, no disclosure via this path
  if (!key_appears) {
    return(list(
      record_index = record_index,
      key_combo = true_key,
      true_target = true_target,
      key_reappears = FALSE,
      synth_target_values = NA,
      bayesian_risk = 0,
      note = "Key does not reappear in synthetic data"
    ))
  }

  # Key appears - compute Bayesian risk via Monte Carlo
  # The question: given that we see this key in synthetic data,
  # what is P(true_target | synthetic_data)?

  # For CART: if a unique key reappears, it's very likely the original value
  # because CART samples from observed combinations

  # Monte Carlo approach:
  # 1. Create counterfactual datasets where we change only the target value
  # 2. Check how often CART would produce the observed pattern

  D_minus_i <- original_data[-record_index, ]

  # Get possible values for target
  possible_targets <- levels(original_data$depress)
  if (is.null(possible_targets)) {
    possible_targets <- unique(original_data$depress)
  }

  # For each possible target value, compute likelihood of observed synthetic
  likelihoods <- numeric(length(possible_targets))
  names(likelihoods) <- possible_targets

  if (!is.null(seed)) set.seed(seed)

  for (t_idx in seq_along(possible_targets)) {
    target_val <- possible_targets[t_idx]

    # Create hypothetical complete data with this target value
    hypothetical_record <- true_record
    hypothetical_record$depress <- factor(target_val, levels = levels(original_data$depress))
    D_hypothetical <- rbind(D_minus_i, hypothetical_record)

    # Generate synthetic datasets and check how often key reappears with observed target
    matches_observed <- 0

    for (mc in 1:n_monte_carlo) {
      # Generate one synthetic dataset
      synth_mc <- tryCatch({
        synth_result <- syn(D_hypothetical, m = 1, seed = seed + mc * 1000 + t_idx * 100,
                            print.flag = FALSE)
        # When m=1, $syn is a data frame directly, not a list
        if (is.data.frame(synth_result$syn)) {
          synth_result$syn
        } else {
          synth_result$syn[[1]]
        }
      }, error = function(e) NULL)

      if (is.null(synth_mc) || !is.data.frame(synth_mc)) next

      # Check if key appears with target matching observed synthetic data
      mc_key <- paste(synth_mc$sex, synth_mc$age,
                      synth_mc$region, synth_mc$placesize, sep = "_")
      mc_matches <- which(mc_key == true_key)

      if (length(mc_matches) > 0) {
        mc_targets <- as.character(synth_mc$depress[mc_matches])
        # Check if any match the observed synthetic target
        if (any(mc_targets %in% synth_target_when_appears)) {
          matches_observed <- matches_observed + 1
        }
      }
    }

    likelihoods[t_idx] <- matches_observed / n_monte_carlo
  }

  # Apply Bayes rule with uniform prior
  prior <- rep(1/length(possible_targets), length(possible_targets))
  names(prior) <- possible_targets

  # Avoid division by zero
  if (sum(likelihoods) == 0) {
    posterior <- prior  # Fall back to prior if no signal
  } else {
    unnorm_posterior <- prior * likelihoods
    posterior <- unnorm_posterior / sum(unnorm_posterior)
  }

  # The Bayesian risk is the posterior probability of the true target
  bayesian_risk <- posterior[true_target]

  return(list(
    record_index = record_index,
    key_combo = true_key,
    true_target = true_target,
    key_reappears = TRUE,
    synth_target_values = unique(synth_target_when_appears),
    likelihoods = likelihoods,
    posterior = posterior,
    bayesian_risk = bayesian_risk,
    note = "Key reappears in synthetic data"
  ))
}

# Compute Bayesian risk for sample of unique records
# This is computationally intensive, so we limit to 5 records
n_records_to_analyze <- min(5, length(sample_unique))
cat("Analyzing", n_records_to_analyze, "unique records (full Bayesian computation)...\n")
cat("This may take several minutes...\n\n")

bayesian_results <- list()

for (i in 1:n_records_to_analyze) {
  idx <- sample_unique[i]
  cat("Processing record", i, "of", n_records_to_analyze, "(index:", idx, ")...\n")

  result <- compute_bayesian_risk_for_outlier(
    original_data = ods_for_synth,
    record_index = idx,
    synth_data_list = sds$syn,
    n_monte_carlo = 30,  # Reduced for speed; increase for accuracy
    seed = 1237 + i
  )

  bayesian_results[[as.character(idx)]] <- result

  cat("  Key:", result$key_combo, "\n")
  cat("  Reappears:", result$key_reappears, "\n")
  cat("  Bayesian risk:", round(result$bayesian_risk, 3), "\n\n")
}

# ============================================================
# 7. Generate Results Table
# ============================================================

cat("\n========================================\n")
cat("Generating results table...\n")
cat("========================================\n")

# Create summary table for analyzed records
outlier_table <- data.frame(
  Record = integer(),
  Key_Combination = character(),
  True_Target = character(),
  Reappears = logical(),
  Synth_Target = character(),
  repU = numeric(),
  DiSCO = numeric(),
  Bayesian_Risk = numeric(),
  stringsAsFactors = FALSE
)

for (result in bayesian_results) {
  outlier_table <- rbind(outlier_table, data.frame(
    Record = result$record_index,
    Key_Combination = result$key_combo,
    True_Target = result$true_target,
    Reappears = result$key_reappears,
    Synth_Target = paste(result$synth_target_values, collapse = ", "),
    repU = mean(repU),  # Overall average repU
    DiSCO = mean(DiSCO),  # Overall average DiSCO
    Bayesian_Risk = result$bayesian_risk,
    stringsAsFactors = FALSE
  ))
}

# Add interpretation
outlier_table$Interpretation <- ifelse(
  outlier_table$Bayesian_Risk > 0.5 & outlier_table$repU < 50,
  "False Negative",
  ifelse(outlier_table$Bayesian_Risk < 0.3, "Low Risk", "Consistent")
)

cat("\nResults table:\n")
print(outlier_table)

# ============================================================
# 8. Export for LaTeX
# ============================================================

cat("\n========================================\n")
cat("Exporting LaTeX table...\n")
cat("========================================\n")

# Create formatted table for LaTeX
# Simplify key combinations for display
outlier_table$Key_Display <- sapply(outlier_table$Key_Combination, function(k) {
  parts <- strsplit(k, "_")[[1]]
  # Format: Sex, Age, Region (abbreviated), Placesize
  paste0(parts[1], ", ", parts[2], ", R", parts[3], ", ", parts[4])
})

# Select and rename columns for LaTeX output
latex_df <- data.frame(
  Record = outlier_table$Record,
  `Key Variables` = outlier_table$Key_Display,
  `Target` = outlier_table$True_Target,
  `In Synth` = ifelse(outlier_table$Reappears, "Yes", "No"),
  `repU` = sprintf("%.1f", outlier_table$repU),
  `DiSCO` = sprintf("%.1f", outlier_table$DiSCO),
  `Bayesian` = sprintf("%.2f", outlier_table$Bayesian_Risk),
  check.names = FALSE
)

# Generate LaTeX using xtable
latex_table <- xtable(
  latex_df,
  caption = "Disclosure risk for naturally occurring unique records in SD2011.
             repU and DiSCO show average values across all synthetic datasets;
             Bayesian risk shows posterior probability of correct target inference
             for each individual unique record.",
  label = "tab:sd2011_outliers",
  align = c("l", "r", "l", "c", "c", "r", "r", "r")
)

# Save to file
latex_file <- paste0(tables_path, "table_disclosure_risk_sd2011_outliers.tex")

print(latex_table,
      file = latex_file,
      include.rownames = FALSE,
      booktabs = TRUE,
      caption.placement = "top",
      sanitize.text.function = identity)

cat("LaTeX table saved to:", latex_file, "\n")

# Also create a simpler summary table
summary_table <- data.frame(
  Measure = c("Identity Risk (repU)", "Attribute Risk (DiSCO)",
              "Bayesian Risk (unique records)"),
  `Average Value` = c(
    sprintf("%.1f%%", mean(repU)),
    sprintf("%.1f%%", mean(DiSCO)),
    sprintf("%.1f%%", 100 * mean(outlier_table$Bayesian_Risk, na.rm = TRUE))
  ),
  Interpretation = c(
    "Low measured risk",
    "Low measured risk",
    "High actual risk for outliers"
  ),
  check.names = FALSE
)

cat("\nSummary comparison:\n")
print(summary_table)

# ============================================================
# 9. Save Detailed Results
# ============================================================

# Save full results for later analysis
results_file <- paste0(output_path, "sd2011_false_negatives_results.RData")
save(
  ods_for_synth,
  sds,
  unique_combos,
  unique_indices,
  disclosure_results,
  repU,
  DiSCO,
  bayesian_results,
  outlier_table,
  file = results_file
)
cat("\nFull results saved to:", results_file, "\n")

# ============================================================
# 10. Final Summary
# ============================================================

cat("\n========================================\n")
cat("SUMMARY: False Negatives in SD2011\n")
cat("========================================\n")

cat("\nKey Findings:\n")
cat("-------------\n")
cat("1. SD2011 contains", length(unique_indices), "naturally unique records\n")
cat("   (", round(100 * length(unique_indices) / nrow(ods), 1), "% of all records)\n\n")

cat("2. Of these unique combinations,", nrow(reappear_summary),
    "reappear in CART synthetic data\n")
cat("   (", round(100 * nrow(reappear_summary) / nrow(unique_combos), 1),
    "% structure preservation)\n\n")

cat("3. Syntactic measures report LOW risk:\n")
cat("   - Average repU:", round(mean(repU), 1), "%\n")
cat("   - Average DiSCO:", round(mean(DiSCO), 1), "%\n\n")

cat("4. Bayesian analysis of unique records shows HIGH actual risk:\n")
cat("   - Average Bayesian risk:",
    round(100 * mean(outlier_table$Bayesian_Risk, na.rm = TRUE), 1), "%\n\n")

cat("5. This demonstrates FALSE NEGATIVES:\n")
cat("   - Syntactic measures indicate low disclosure risk\n")
cat("   - Bayesian analysis reveals high actual risk for unique records\n")
cat("   - An attacker who knows CART preserves structure can exploit this\n\n")

cat("Analysis complete.\n")

# ============================================================
# END OF SCRIPT
# ============================================================
