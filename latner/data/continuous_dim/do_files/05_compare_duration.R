# Top commands ----
# https://alfurka.github.io/2023-01-30-creating-synthetic-values-with-synthepop-cart/
# Create empty R application (no figures, data frames, packages, etc.)
# Get a list of all loaded packages
packages <- search()[grepl("package:", search())]
# Unload each package
for (package in packages) {
  unloadNamespace(package)
}

rm(list=ls(all=TRUE))

# load library
library(tidyverse)
library(synthpop)
library(ggh4x)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
duration = "duration/"
graphs = "graphs/"

setwd(main_dir)

# Load data ----

df_synthpop <- read.csv(paste0(duration,"duration_synthpop_0.csv")) 
df_datasynthesizer <- read.csv(paste0(duration,"duration_datasynthesizer_0.csv")) 

# Load CTGAN duration data ----

duration_ctgan_rows_50000_cols_10 <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_10.csv"))
duration_ctgan_rows_50000_cols_15 <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_15.csv"))
duration_ctgan_rows_50000_cols_20 <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_20.csv"))

duration_ctgan_rows_100000_cols_10 <- read.csv(paste0(duration,"duration_ctgan_rows_100000_cols_10.csv"))
duration_ctgan_rows_100000_cols_15 <- read.csv(paste0(duration,"duration_ctgan_rows_100000_cols_15.csv"))
duration_ctgan_rows_100000_cols_20 <- read.csv(paste0(duration,"duration_ctgan_rows_100000_cols_20.csv"))

duration_ctgan_rows_200000_cols_10 <- read.csv(paste0(duration,"duration_ctgan_rows_200000_cols_10.csv"))
duration_ctgan_rows_200000_cols_15 <- read.csv(paste0(duration,"duration_ctgan_rows_200000_cols_15.csv"))
duration_ctgan_rows_200000_cols_20 <- read.csv(paste0(duration,"duration_ctgan_rows_200000_cols_20.csv"))

df_ctgan <- rbind(duration_ctgan_rows_50000_cols_10,duration_ctgan_rows_50000_cols_15,duration_ctgan_rows_50000_cols_20,
                     duration_ctgan_rows_100000_cols_10,duration_ctgan_rows_100000_cols_15,duration_ctgan_rows_100000_cols_20,
                     duration_ctgan_rows_200000_cols_10,duration_ctgan_rows_200000_cols_15,duration_ctgan_rows_200000_cols_20
) 

df_ctgan <- df_ctgan %>%
  filter(epochs == 50,
         batch_size == 1000) %>%
  select(type, rows, cols, n, duration)

# Merge ----

df_merge <- rbind(df_synthpop,df_datasynthesizer,df_ctgan)

df_merge$rows <- as.factor(df_merge$rows)
df_merge

# Graph data ----

df_graph <- ggplot(df_merge, aes(x = rows, y = duration, fill = type)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_wrap( ~ cols, scales = "free", labeller = labeller(.cols = label_both)) +
  xlab("Rows") +
  ylab("Duration (in seconds)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_duration.pdf"), height = 6, width = 10)

