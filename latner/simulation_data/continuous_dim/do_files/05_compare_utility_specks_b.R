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
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)


# Load data ----

df_specks_synthpop <- read.csv(paste0(tables,"synthpop/utility_synthpop_specks.csv"))
df_specks_datasynthesizer <- read.csv(paste0(tables,"datasynthesizer/utility_datasynthesizer_specks.csv"))

# Load CTGAN data ----

df_specks_200000_10 <- read.csv(paste0(tables,"ctgan/utility_ctgan_specks_rows_200000_cols_10.csv"))
df_specks_200000_15 <- read.csv(paste0(tables,"ctgan/utility_ctgan_specks_rows_200000_cols_15.csv"))
df_specks_200000_20 <- read.csv(paste0(tables,"ctgan/utility_ctgan_specks_rows_200000_cols_20.csv"))
df_specks_100000 <- read.csv(paste0(tables,"ctgan/utility_ctgan_specks_rows_100000.csv"))
df_specks_50000 <- read.csv(paste0(tables,"ctgan/utility_ctgan_specks_rows_50000.csv"))
df_specks_ctgan <- rbind(df_specks_50000,df_specks_100000,df_specks_200000_10,df_specks_200000_15,df_specks_200000_20) %>% 
  arrange(batch, epochs) %>%
  mutate(data="CTGAN") %>%
  filter(batch == 1000, 
         epochs == 50) %>%
  select(data,rows,cols,specks)

# Merge ----
df_specks <- rbind(df_specks_synthpop,df_specks_datasynthesizer,df_specks_ctgan)
df_specks

# Graph data ----

df_specks$rows <- as.factor(df_specks$rows)

df_graph <- ggplot(df_specks, aes(x = rows, y = specks, fill = data)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_wrap( ~ cols, scales = "fixed", labeller = labeller(.cols = label_both)) +
  xlab("Synthetic data generator") +
  ylab("Kolmogorov-Smirnov") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_utility_ks.pdf"), height = 6, width = 10)
