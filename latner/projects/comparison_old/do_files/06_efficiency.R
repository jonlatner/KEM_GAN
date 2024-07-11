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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
duration = "duration/"
graphs = "graphs/"

setwd(main_dir)

# Load duration data from CTGAN (based on optimized parameterization) ----

data <- c("sd2011","sd2011_clean","sd2011_clean_small","sd2011_clean_small_categorical")
data <- c("sd2011_clean_small")
type <- c("ctgan")

df_duration <- data.frame()
for (d in data) {
  for (t in type) {
    output <- read.csv(paste0(duration,"duration_",t,"_data_",d,".csv")) %>%
      filter(epochs == 600 & copies == 1 & j == 1) %>%
      select(type, data, duration)
    df_duration <- rbind(df_duration,output)
  }
}
df_duration

# Load duration data from datasynthesizer ----

type <- c("datasynthesizer")
for (d in data) {
  for (t in type) {
    correlated <- read.csv(paste0(duration,"duration_",t,"_correlated_data_",d,".csv")) 
    independent <- read.csv(paste0(duration,"duration_",t,"_independent_data_",d,".csv")) 
    output <- rbind(correlated, independent) %>%
      filter(parents == 2 & epsilon == 0 & copies == 1 & j == 1) %>%
      select(type, data, duration)
    df_duration <- rbind(df_duration,output)
  }
}

# Load duration data from synthpop ----

type <- c("synthpop")
for (d in data) {
  for (t in type) {
    output <- read.csv(paste0(duration,"duration_",t,"_data_",d,".csv"))
    df_duration <- rbind(df_duration,output)
  }
}

df_duration

# Graph data ----

df_duration

df_graph <- ggplot(df_duration, aes(x = type, y = duration)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  xlab("") +
  ylab("Duration (in seconds)") +
  theme_bw() +
  ylim(0,2000) +
  geom_text(aes(label = round(duration,0)), vjust = -1) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        # axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_efficiency.pdf"), height = 4, width = 8)

