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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
duration_folder = "duration/"
graphs = "graphs/"

setwd(main_dir)

# Load duration data from synthpop ----

df_duration <- data.frame()
maximum_factors <- c(10,17,18,30)
for (f in maximum_factors) {
  output <- read.csv(paste0(duration_folder,"duration_synthpop_data_SD2011_factors_",f,".csv"))
  df_duration <- rbind(df_duration,output)
}

output

# Graph data ----

df_graph <- ggplot(df_duration, aes(x = columns, y = duration)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_wrap( ~ max_factors, scales = "free", labeller = labeller(.cols = label_both), nrow = 1) +
  xlab("Synthesizer") +
  ylab("Duration (in seconds)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

# ggsave(plot = df_graph, paste0(graphs,"graph_compare_duration.pdf"), height = 4, width = 8)

