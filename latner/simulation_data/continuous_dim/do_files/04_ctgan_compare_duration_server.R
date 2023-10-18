# Top commands ----

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
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/"
tables = "tables/ctgan/"
duration = "duration/"

setwd(main_dir)


# Load data ----

df_jpl <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_10.csv"))
df_jpl$type = "M2 macbook"
df_iab <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_10_server.csv"))
df_iab$type = "IAB server (N2040017)"

df_duration <- rbind(df_jpl, df_iab) 

# Graph data ----

df_duration$epochs <- as.factor(df_duration$epochs)
df_duration$rows <- as.factor(df_duration$rows)

df_graph <- ggplot(df_duration, aes(x = epochs, y = duration/60, fill = type)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_nested(cols ~ rows + batch_size, scales = "free", labeller = labeller(.cols = label_both, .rows = label_both)) +
  xlab("Epochs") +
  ylab("Duration (in minutes)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_ctgan_duration_server.pdf"), height = 4, width = 6)


# Graph data ----

df_duration$epochs <- as.factor(df_duration$epochs)
df_duration$rows <- as.factor(df_duration$rows)

df_total <- df_duration %>%
  group_by(type) %>%
  summarise(duration = sum(duration)/60)

df_graph <- ggplot(df_total, aes(x = type, y = duration)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  xlab("Machine") +
  ylab("Duration (in minutes)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_ctgan_duration_server_total.pdf"), height = 4, width = 6)
