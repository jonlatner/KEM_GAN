# ============================================================
# 14_graph_cart.R
# Visualize CART synthetic data frequency distributions
#
# Purpose: Compare marginal and joint frequency distributions
# between original and CART-synthesized data for a simulated
# dataset with one rare combination (c_16 = 1111)
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

ods <- as.data.frame(D)
cat("Original data created:", nrow(ods), "records\n\n")

# ============================================================
# 3. Generate Synthetic Data
# ============================================================

cat("Generating CART synthetic data...\n")
sds <- syn(ods, m = 1, seed = 1237, print.flag = FALSE)
sds <- sds$syn
cat("Synthesis complete.\n\n")

# ============================================================
# 4. Compare Marginal Frequencies
# ============================================================

cat("Preparing marginal frequency comparison...\n")

df_sds <- sds
df_ods <- ods

df_sds$type <- "synthetic"
df_ods$type <- "original"

df_combine <- rbind(df_sds, df_ods)

# Reshape the data for plotting
df_combine_long <- df_combine %>%
  pivot_longer(!type, names_to = "variable", values_to = "value") %>%
  mutate(value = as.factor(value)) %>%
  group_by(type, variable, value) %>%
  tally() %>%
  group_by(type, variable) %>%
  mutate(
    total = sum(n),
    pct = n / total
  ) %>%
  ungroup()

# Plot: original only
p <- ggplot(subset(df_combine_long, type == "original"), aes(x = value, y = n)) +
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  geom_text(aes(label = n), vjust = -0.5, size = 4, position = position_dodge(.9)) +
  facet_wrap(~variable) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100)) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = .5),
    axis.line.x = element_line(color = "black", linewidth = .5)
  )

ggsave(p,
  filename = paste0(graphs_path, "graph_cart_frequency.pdf"),
  height = 4, width = 6, units = "in"
)
cat("Saved: graph_cart_frequency.pdf\n")

# Plot: original vs. synthetic
p <- ggplot(df_combine_long, aes(x = value, y = n, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  geom_text(aes(label = n), vjust = -0.5, size = 4, position = position_dodge(.9)) +
  facet_wrap(~variable) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 700), breaks = seq(0, 700, 100)) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = .5),
    axis.line.x = element_line(color = "black", linewidth = .5)
  )

ggsave(p,
  filename = paste0(graphs_path, "graph_cart_frequency_compare.pdf"),
  height = 4, width = 6, units = "in"
)
cat("Saved: graph_cart_frequency_compare.pdf\n\n")

# ============================================================
# 5. Compare Joint Frequencies (Histogram over Combinations)
# ============================================================

cat("Preparing joint frequency (combination histogram) comparison...\n")

df_ods_frequency <- ods
df_ods_frequency$combine <- paste(
  df_ods_frequency$var1, df_ods_frequency$var2,
  df_ods_frequency$var3, df_ods_frequency$var4,
  sep = ""
)
df_ods_frequency <- df_ods_frequency %>% select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(ods)) * 100
df_ods_frequency$type <- "original"

df_sds_frequency <- sds
df_sds_frequency$combine <- paste(
  df_sds_frequency$var1, df_sds_frequency$var2,
  df_sds_frequency$var3, df_sds_frequency$var4,
  sep = ""
)
df_sds_frequency <- df_sds_frequency %>% select(-matches("var"))
df_sds_frequency <- as.data.frame(table(df_sds_frequency))
df_sds_frequency$pct <- (df_sds_frequency$Freq / nrow(sds)) * 100
df_sds_frequency$type <- "synthetic"

df_combine <- rbind(df_ods_frequency, df_sds_frequency)

# Plot: original only
p <- ggplot(subset(df_combine, type == "original"), aes(x = combine, y = Freq)) +
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4, position = position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = .5),
    axis.line.x = element_line(color = "black", linewidth = .5)
  )

ggsave(p,
  filename = paste0(graphs_path, "graph_cart_histogram.pdf"),
  height = 4, width = 6, units = "in"
)
cat("Saved: graph_cart_histogram.pdf\n")

# Plot: original vs. synthetic
p <- ggplot(df_combine, aes(x = combine, y = Freq, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(.9)) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4, position = position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = .5),
    axis.line.x = element_line(color = "black", linewidth = .5)
  )

ggsave(p,
  filename = paste0(graphs_path, "graph_cart_histogram_compare.pdf"),
  height = 4, width = 6, units = "in"
)
cat("Saved: graph_cart_histogram_compare.pdf\n")

# ============================================================
# END OF SCRIPT
# ============================================================
