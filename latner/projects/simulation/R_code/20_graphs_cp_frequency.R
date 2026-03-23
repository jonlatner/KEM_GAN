# ============================================================
# 20_descriptives_frequency_cp.R
# Effect of cart.cp on joint frequency distributions
#
# Purpose: Show how varying the complexity parameter (cp)
# affects structure preservation, visualized across 10
# synthetic datasets per cp level
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
library(ggh4x)
library(readr)

# base_path <- "/Users/marcelneunhoeffer/Dropbox/Apps/Overleaf/JOS-privacy-measures/"
base_path <- "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
graphs_path <- paste0(base_path, "graphs/")

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
# 3. Generate Synthetic Data Across cp Values
# ============================================================

cp_values <- c("0.00000001", "0.0001", "0.01", "0.05")

cat("Testing cart.cp values:", paste(cp_values, collapse = ", "), "\n\n")

current_seed <- 1237
df_frequency <- data.frame()

for (cp in cp_values) {
  cat("Processing cart.cp =", cp, "...\n")

  for (c in 1:10) {
    current_seed <- current_seed + 1

    sds <- syn(df_ods, m = 1, seed = current_seed,
               cart.cp = cp, print.flag = FALSE)
    sds <- sds$syn

    sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
    sds <- sds %>% select(-matches("var"))

    df_sds_frequency <- as.data.frame(table(sds))
    df_sds_frequency$type <- "synthetic"
    df_sds_frequency$n <- c
    df_sds_frequency$cp <- cp

    df_frequency <- rbind(df_frequency, df_sds_frequency)
  }

  cat("  Completed 10 synthetic datasets for cart.cp =", cp, "\n")
}

cat("\nSynthesis complete.\n\n")

# ============================================================
# 4. Prepare Original Frequency Table
# ============================================================

df_test <- data.frame()
for (cp in cp_values) {
  df_ods_frequency <- df_ods
  df_ods_frequency$combine <- paste(
    df_ods_frequency$var1, df_ods_frequency$var2,
    df_ods_frequency$var3, df_ods_frequency$var4,
    sep = ""
  )
  df_ods_frequency <- df_ods_frequency %>% select(-matches("var"))
  df_ods_frequency <- as.data.frame(table(df_ods_frequency))
  df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods)) * 100
  df_ods_frequency$type <- "original"
  df_ods_frequency$cp <- cp
  df_test <- rbind(df_test, df_ods_frequency)
}
df_ods_frequency <- df_test

# ============================================================
# 5. Plot Frequency Distributions by cp
# ============================================================

cat("Generating cp comparison plots...\n")

df_graph_ods <- df_ods_frequency
df_graph_sds <- df_frequency

# Faceted plot across all cp values
df_graph <-
  ggplot() +
  geom_bar(
    data = df_graph_ods,
    aes(x = combine, y = Freq, fill = type),
    stat = "identity"
  ) +
  geom_boxplot(
    data = df_graph_sds,
    aes(x = combine, y = Freq, fill = type),
    alpha = .2
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  facet_wrap(~cp) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = .5),
    axis.line.x = element_line(color = "black", linewidth = .5)
  )

ggsave(df_graph,
  filename = paste0(graphs_path, "graph_cart_modified_cp_histogram_compare_10_v2.pdf"),
  height = 4, width = 6, units = "in"
)
cat("Saved: graph_cart_modified_cp_histogram_compare_10_v2.pdf\n")

# Single-panel plot for smallest cp (most complex trees)
df_graph <-
  ggplot() +
  geom_bar(
    data = subset(df_graph_ods, cp == "0.00000001"),
    aes(x = combine, y = Freq, fill = type),
    stat = "identity"
  ) +
  geom_boxplot(
    data = subset(df_graph_sds, cp == "0.00000001"),
    aes(x = combine, y = Freq, fill = type),
    alpha = .2
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    axis.title.x = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = .5),
    axis.line.x = element_line(color = "black", linewidth = .5)
  )

ggsave(df_graph,
  filename = paste0(graphs_path, "graph_cart_modified_cp_histogram_compare_10_v1.pdf"),
  height = 4, width = 6, units = "in"
)
cat("Saved: graph_cart_modified_cp_histogram_compare_10_v1.pdf\n")

# ============================================================
# END OF SCRIPT
# ============================================================
