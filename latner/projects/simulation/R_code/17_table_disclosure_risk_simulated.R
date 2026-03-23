# ============================================================
# 17_table_disclosure_risk.R
# Compute and tabulate disclosure risk for simulated data
#
# Purpose: Generate repU and DiSCO disclosure risk tables for
# both a single synthetic dataset and 10 synthetic datasets,
# using the simulated four-variable binary data with one
# rare combination (c_16 = 1111)
#
# Authors: Jonathan Latner, Marcel Neunhoeffer, Jorg Drechsler
# Date: 2025-01-21
# ============================================================

rm(list = ls())

# ============================================================
# 1. Setup
# ============================================================

library(synthpop)
library(tidyverse)
library(xtable)
library(readr)

# base_path <- "/Users/marcelneunhoeffer/Dropbox/Apps/Overleaf/JOS-privacy-measures/"
base_path <- "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
tables_path <- paste0(base_path, "tables/")

options(scipen = 999)
set.seed(1237)

# ============================================================
# 2. Create Simulated Data
# ============================================================

cat("Creating simulated data...\n")

n <- 1000

# Define the 16 possible combinations of four binary variables
combinations <- expand.grid(y1 = c(0, 1), y2 = c(0, 1), y3 = c(0, 1), y4 = c(0, 1))

# Define c_16 and C_minus_16
c_16 <- combinations[16, ]
C_minus_16 <- combinations[-16, ]

# Initialize the dataset
D <- data.frame(matrix(ncol = 4, nrow = n))
colnames(D) <- c("var1", "var2", "var3", "var4")

# Sample the first 999 observations from C_minus_16 with equal probability
for (i in 1:(n - 1)) {
  sampled_row <- sample(1:15, 1)
  D[i, ] <- C_minus_16[sampled_row, ]
}

# Set the 1000th observation to c_16
D[1000, ] <- c_16

df_ods <- as.data.frame(D)
cat("Original data created:", nrow(df_ods), "records\n\n")

# ============================================================
# 3. Generate Synthetic Data (10 Datasets)
# ============================================================

cat("Generating 10 synthetic datasets...\n")

current_seed <- 1237
df_sds_10 <- data.frame()

for (c in 1:10) {
  sds <- syn(df_ods, m = 1, seed = current_seed, print.flag = FALSE)
  sds <- sds$syn

  current_seed <- current_seed + 1

  sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
  sds <- sds %>% select(-matches("var"))

  df_sds_frequency <- as.data.frame(table(sds))
  df_sds_frequency$type <- "synthetic"
  df_sds_frequency$n <- c

  df_sds_10 <- rbind(df_sds_10, df_sds_frequency)
  cat("  Completed synthetic dataset", c, "of 10\n")
}

# Single synthetic dataset (seed 1237) for single-dataset table
sds_single <- syn(df_ods, m = 1, seed = 1237, print.flag = FALSE)
df_sds_1 <- sds_single$syn

cat("Synthesis complete.\n\n")

# ============================================================
# 4. Compute Frequency of Unique Record (1111)
# ============================================================

cat("Computing frequency of unique combination (1111)...\n")

# Original data
df_frequency_ods <- df_ods
df_frequency_ods$combine <- paste(
  df_frequency_ods$var1, df_frequency_ods$var2,
  df_frequency_ods$var3, df_frequency_ods$var4,
  sep = ""
)
df_frequency_ods <- df_frequency_ods %>% select(-matches("var"))
df_frequency_ods <- as.data.frame(table(df_frequency_ods))

df_frequency_ods_unique <- df_frequency_ods %>% filter(combine == "1111")
df_frequency_ods_unique <- df_frequency_ods_unique$Freq
cat("  Frequency of 1111 in original data:", df_frequency_ods_unique, "\n")

# Single synthetic dataset
df_frequency_sds_1 <- df_sds_1
df_frequency_sds_1$combine <- paste(
  df_frequency_sds_1$var1, df_frequency_sds_1$var2,
  df_frequency_sds_1$var3, df_frequency_sds_1$var4,
  sep = ""
)
df_frequency_sds_1 <- df_frequency_sds_1 %>% select(-matches("var"))
df_frequency_sds_1 <- as.data.frame(table(df_frequency_sds_1)) %>%
  filter(combine == "1111")
sds_1_unique <- df_frequency_sds_1$Freq
cat("  Frequency of 1111 in single synthetic dataset:", sds_1_unique, "\n")

# 10 synthetic datasets
df_frequency_ods$n <- 0
df_frequency_ods$type <- "original"

df_frequency_sds_10 <- rbind(df_frequency_ods, df_sds_10) %>%
  pivot_wider(names_from = "n", values_from = "Freq") %>%
  filter(combine == "1111") %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  pivot_longer(
    cols = `0`:`10`,
    names_to = "value",
    values_to = "count"
  ) %>%
  filter((type == "original" & value == 0) | (type == "synthetic" & value > 0))

df_frequency_sds_10_unique <- as.vector(df_frequency_sds_10$count)
cat("  Frequency of 1111 across 10 synthetic datasets:", paste(df_frequency_sds_10_unique, collapse = ", "), "\n\n")

# ============================================================
# 5. Compute Disclosure Risk (Single Synthetic Dataset)
# ============================================================

cat("========================================\n")
cat("DISCLOSURE RISK: SINGLE SYNTHETIC DATASET\n")
cat("========================================\n\n")

t1 <- multi.disclosure(sds_single, df_ods,
  print.flag = FALSE, plot = FALSE,
  keys = c("var1", "var2", "var3"), target = "var4"
)

cat("Identity risk (repU):\n")
print(print(t1, plot = FALSE, to.print = "ident"))
cat("\nAttribute risk (DiSCO):\n")
print(print(t1, plot = FALSE, to.print = "attrib"))

df_risk <- data.frame(
  data = c("Original", "Synthetic"),
  unique = c(df_frequency_ods_unique, sds_1_unique),
  identity = c(t1$ident.orig, t1$ident.syn),
  attribute = c(t1$attrib.table$attrib.orig, t1$attrib.table$attrib.syn),
  stringsAsFactors = FALSE
)

cat("\nSingle synthetic dataset risk summary:\n")
print(df_risk)

latex_table <- xtable(df_risk)
colnames(latex_table) <- c("Data", "Unique", "Identity Risk ($repU$)", "Attribute Risk ($DiSCO$)")

print.xtable(latex_table,
  include.rownames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  file = paste0(tables_path, "table_disclosure_risk_1.tex")
)
cat("LaTeX table saved to: table_disclosure_risk_1.tex\n\n")

# ============================================================
# 6. Compute Disclosure Risk (10 Synthetic Datasets)
# ============================================================

cat("========================================\n")
cat("DISCLOSURE RISK: 10 SYNTHETIC DATASETS\n")
cat("========================================\n\n")

cat("Assembling 10 synthetic datasets...\n")
df_sds <- syn(df_ods, m = 10, print.flag = FALSE)

current_seed <- 1237
for (c in 1:10) {
  sds <- syn(df_ods, m = 1, seed = current_seed, print.flag = FALSE)
  df_sds$syn[[c]] <- sds$syn
  current_seed <- current_seed + 1
}

t10 <- disclosure(df_sds, df_ods,
  keys = c("var1", "var2", "var3"),
  target = "var4",
  print.flag = FALSE
)

repU <- t10$ident$repU
average_row <- mean(repU)
repU <- c(0, repU, average_row)

DiSCO <- t10$attrib$DiSCO
average_row <- mean(DiSCO)
DiSCO <- c(0, DiSCO, average_row)

df_frequency_sds_10_unique <- c(df_frequency_sds_10_unique, NA)

df_risk <- data.frame(
  data = c(
    "Original", "Synthetic 1", "Synthetic 2", "Synthetic 3", "Synthetic 4",
    "Synthetic 5", "Synthetic 6", "Synthetic 7", "Synthetic 8", "Synthetic 9",
    "Synthetic 10", "Average"
  ),
  unique = df_frequency_sds_10_unique,
  identity = repU,
  attribute = DiSCO,
  stringsAsFactors = FALSE
)

cat("\n10-dataset risk summary:\n")
print(df_risk)

latex_table <- xtable(df_risk, align = "llrrr")
colnames(latex_table) <- c("Data", "Unique", "Identity Risk ($repU$)", "Attribute Risk ($DiSCO$)")

print.xtable(latex_table,
  include.rownames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  sanitize.text.function = function(x) {x},
  file = paste0(tables_path, "table_disclosure_risk_10.tex"),
  add.to.row = list(
    pos = list(nrow(latex_table) - 1),
    command = "\\midrule \n"
  )
)
cat("LaTeX table saved to: table_disclosure_risk_10.tex\n")

# ============================================================
# END OF SCRIPT
# ============================================================
