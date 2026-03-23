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
library(synthpop)
library(tidyverse)
library(readr)
library(ggh4x)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 

# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Graph frequency ----

df_ods_frequency <- df_ods
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods)) * 100
df_ods_frequency$type <- " original"

df_graph_ods <- df_ods_frequency

df_graph_sds <- read_csv(paste0(datasynthesizer_data,"datasynthesizer_frequency.csv"))
df_graph_sds$type <- "datasynthesizer"

df_graph <- 
  ggplot() +
  geom_bar(data = df_graph_ods, aes(x = combine, y = Freq, fill = type), stat = "identity") +
  geom_boxplot(data = df_graph_sds, aes(x = combine, y = Freq, fill = type), alpha = .2) +
  facet_wrap(~epsilon) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(df_graph, filename = paste0(graphs,"graph_dp_datasynthesizer_compare_histogram_10.pdf"), height = 4, width = 6, units = "in")
ggsave(df_graph, filename = paste0(graphs,"graph_dp_datasynthesizer_compare_histogram_10_v2.pdf"), height = 4, width = 10, units = "in")

