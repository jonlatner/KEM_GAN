# ============================================================
# 15_descriptives_frequency.R
# Frequency table and graph for simulated CART data (10 runs)
#
# Purpose: Show distribution of joint combinations across 10
# independently drawn synthetic datasets to illustrate
# variability in CART synthesis
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
library(xtable)

# base_path <- "/Users/marcelneunhoeffer/Dropbox/Apps/Overleaf/JOS-privacy-measures/"
base_path <- "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
graphs_path <- paste0(base_path, "graphs/")
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
# 3. Generate 10 Synthetic Datasets
# ============================================================

cat("Generating 10 synthetic datasets...\n")

current_seed <- 1237
df_frequency <- data.frame()

for (c in 1:10) {
  current_seed <- current_seed + 1

  sds <- syn(df_ods, m = 1, seed = current_seed, print.flag = FALSE)
  sds <- sds$syn

  sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
  sds <- sds %>% select(-matches("var"))

  df_sds_frequency <- as.data.frame(table(sds))
  df_sds_frequency$type <- "synthetic"
  df_sds_frequency$n <- c

  df_frequency <- rbind(df_frequency, df_sds_frequency)
  cat("  Completed synthetic dataset", c, "of 10\n")
}

cat("Synthesis complete.\n\n")

# ============================================================
# 4. Prepare Original Frequency Table
# ============================================================

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

# ============================================================
# 5. Plot Frequency Comparison
# ============================================================

cat("Generating frequency comparison plots...\n")

df_graph_ods <- df_ods_frequency
df_graph_sds <- df_frequency

df_graph <-
  ggplot() +
  geom_bar(data = df_graph_ods, aes(x = combine, y = Freq, fill = type), stat = "identity") +
  geom_boxplot(data = df_graph_sds, aes(x = combine, y = Freq, fill = type), alpha = .2) +
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
  filename = paste0(graphs_path, "graph_cart_histogram_compare_10_v1.pdf"),
  height = 4, width = 10, units = "in"
)
ggsave(df_graph,
  filename = paste0(graphs_path, "graph_cart_histogram_compare_10_v2.pdf"),
  height = 6, width = 6, units = "in"
)
cat("Saved: graph_cart_histogram_compare_10_v1.pdf\n")
cat("Saved: graph_cart_histogram_compare_10_v2.pdf\n\n")

# ============================================================
# 6. Generate LaTeX Frequency Table
# ============================================================

cat("Generating LaTeX frequency table...\n")

df_freq_table <- df_ods_frequency
df_freq_table$pct <- NULL
df_freq_table$n <- 0

df_freq_table <- rbind(df_freq_table, df_frequency)
df_freq_table$type <- NULL

df_freq_table <- df_freq_table %>%
  pivot_wider(names_from = "n", values_from = "Freq") %>%
  mutate(across(everything(), ~ replace_na(., 0)))

latex_table <- xtable(df_freq_table, digits = 0)

add_to_row <- list(
  pos = list(0, 0),
  command = c(
    " & \\multicolumn{1}{l}{Original} & \\multicolumn{10}{c}{Synthetic Data} \\\\ \\cmidrule(lr){3-12}\n",
    "Combine & 0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 \\\\ \n"
  )
)

print.xtable(latex_table,
  include.rownames = FALSE,
  include.colnames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  file = paste0(tables_path, "table_frequency.tex"),
  add.to.row = add_to_row
)
cat("LaTeX table saved to: table_frequency.tex\n")

# ============================================================
# END OF SCRIPT
# ============================================================
