# ============================================================
# 16_attacker.R
# Simulate attacker scenario across all 16 possible last records
#
# Purpose: For each of the 16 possible combinations that could
# occupy the final record, generate synthetic data and compare
# the released synthetic distribution to the attacker's
# synthetic distribution across 10 repetitions
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
set.seed(1234)

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
# 3. Attacker Simulation Loop
# ============================================================

cat("========================================\n")
cat("ATTACKER SIMULATION\n")
cat("========================================\n\n")
cat("Testing all 16 possible last records across 10 repetitions...\n\n")

current_seed <- 1234
df_frequency <- data.frame()

for (c in 1:10) {
  for (r in 1:16) {
    current_seed <- current_seed + 1

    # Load original data and replace last record
    df_ods <- ods
    df_ods <- head(df_ods, -1)
    last_record <- combinations[r, ]
    df_ods[1000, ] <- last_record

    # Generate released synthetic data (based on true last record)
    sds <- syn(df_ods, m = 1, seed = current_seed, method = "cart", print.flag = FALSE)
    sds <- sds$syn
    df_sds <- sds

    # Frequency table for released synthetic data
    df_released <- df_sds
    df_released$combine <- paste(
      df_released$var1, df_released$var2,
      df_released$var3, df_released$var4,
      sep = ""
    )
    df_released <- df_released %>% select(-matches("var"))
    df_released_frequency <- as.data.frame(table(df_released)) %>%
      mutate(
        type = "released synthetic data",
        n = c,
        last_record = paste(last_record$y1, last_record$y2,
          last_record$y3, last_record$y4,
          sep = ""
        )
      )

    # Frequency table for attacker's synthetic data
    sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
    sds <- sds %>% select(-matches("var"))
    df_sds_frequency <- as.data.frame(table(sds))
    df_sds_frequency$type <- "synthetic data from attack"
    df_sds_frequency$n <- c
    df_sds_frequency$last_record <- paste(last_record$y1, last_record$y2,
      last_record$y3, last_record$y4,
      sep = ""
    )

    df_frequency <- rbind(df_frequency, df_sds_frequency, df_released_frequency)
  }
  cat("  Completed repetition", c, "of 10\n")
}

cat("\nSimulation complete.\n\n")

# ============================================================
# 4. Plot Attacker Results
# ============================================================

cat("Generating attacker comparison plots...\n")

df_graph_sds <- df_frequency %>% filter(type == "synthetic data from attack")
df_graph_ods <- df_frequency %>% filter(type == "released synthetic data")

df_graph <-
  ggplot() +
  geom_bar(
    data = df_graph_ods,
    aes(x = combine, y = Freq, fill = type),
    position = position_dodge(width = 0.9),
    stat = "identity"
  ) +
  geom_boxplot(
    data = df_graph_sds,
    aes(x = combine, y = Freq, fill = type),
    position = position_dodge(width = 0.9)
  ) +
  facet_wrap(~last_record, labeller = "label_both") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1, "cm"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = .5),
    axis.line.x = element_line(color = "black", linewidth = .5)
  )

ggsave(plot = df_graph,
  filename = paste0(graphs_path, "graph_attacker_default_v1.pdf"),
  height = 5, width = 10
)
ggsave(plot = df_graph,
  filename = paste0(graphs_path, "graph_attacker_default_v2.pdf"),
  height = 5, width = 5
)
cat("Saved: graph_attacker_default_v1.pdf\n")
cat("Saved: graph_attacker_default_v2.pdf\n")

# ============================================================
# END OF SCRIPT
# ============================================================
