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

duration_ctgan_rows_50000_cols_10 <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_10.csv"))
duration_ctgan_rows_50000_cols_15 <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_15.csv"))
duration_ctgan_rows_50000_cols_20 <- read.csv(paste0(duration,"duration_ctgan_rows_50000_cols_20.csv"))

duration_ctgan_rows_100000_cols_10 <- read.csv(paste0(duration,"duration_ctgan_rows_100000_cols_10.csv"))
duration_ctgan_rows_100000_cols_15 <- read.csv(paste0(duration,"duration_ctgan_rows_100000_cols_15.csv"))
duration_ctgan_rows_100000_cols_20 <- read.csv(paste0(duration,"duration_ctgan_rows_100000_cols_20.csv"))

duration_ctgan_rows_200000_cols_10 <- read.csv(paste0(duration,"duration_ctgan_rows_200000_cols_10.csv"))
duration_ctgan_rows_200000_cols_15 <- read.csv(paste0(duration,"duration_ctgan_rows_200000_cols_15.csv"))
duration_ctgan_rows_200000_cols_20 <- read.csv(paste0(duration,"duration_ctgan_rows_200000_cols_20.csv"))

df_duration <- rbind(duration_ctgan_rows_50000_cols_10,duration_ctgan_rows_50000_cols_15,duration_ctgan_rows_50000_cols_20,
                     duration_ctgan_rows_100000_cols_10,duration_ctgan_rows_100000_cols_15,duration_ctgan_rows_100000_cols_20,
                     duration_ctgan_rows_200000_cols_10,duration_ctgan_rows_200000_cols_15,duration_ctgan_rows_200000_cols_20
) 


# Graph data ----

df_duration$epochs <- as.factor(df_duration$epochs)
df_duration$rows <- as.factor(df_duration$rows)

group_by(df_duration,rows, cols) %>%
  summarise(sum=sum(duration))  %>%
  ungroup() %>%
  mutate(total=mean(sum))

sum(df_duration$duration)/3


df_graph <- ggplot(df_duration, aes(x = epochs, y = duration)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_nested(cols ~ rows + batch_size, scales = "free", labeller = labeller(.cols = label_both, .rows = label_both)) +
  xlab("Epochs") +
  ylab("Duration (in seconds)") +
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

ggsave(plot = df_graph, paste0(graphs,"graph_compare_ctgan_duration.pdf"), height = 6, width = 10)
