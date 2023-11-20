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
library(xtable)
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# Load and optimize ctgan utility data ----

df_ctgan <- read.csv(paste0(tables,"ctgan/utility_output.csv"))
df_ctgan <- df_ctgan %>%
  group_by(data,copies) %>%
  mutate(best_fit = ifelse(specks==min(specks),yes=1,no=0)) %>%
  filter(best_fit == 1) %>%
  ungroup() %>%
  mutate(synthesizer = "ctgan") %>%
  select(-epochs,-best_fit)
df_ctgan
  
# Load utility data from other synthesizers ----

type <- c("synthpop", "datasynthesizer")
df_utility <- data.frame()
for (t in type) {
  output <- read.csv(paste0(tables,t,"/utility_output.csv"))
  output$synthesizer <- t
  df_utility <- rbind(df_utility,output)
}

df_utility <- rbind(df_utility,df_ctgan) %>% 
  arrange(data,synthesizer) %>%
  filter(copies == 5) %>%
  select(-copies)

# Graph ----

df_data <- df_utility %>% 
  pivot_longer(!c(data,synthesizer), names_to = "utility", values_to = "values")

df_graph <- ggplot(df_data, aes(x = synthesizer, y = values)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_grid(utility ~ data, labeller = labeller(.rows = label_both, .cols = label_both), scales = "free") +
  theme_bw() +
  geom_text(aes(label = round(values,2)), vjust = -0.3) +  # Adding labels
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 75, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_utility.pdf"), height = 4, width = 8)
