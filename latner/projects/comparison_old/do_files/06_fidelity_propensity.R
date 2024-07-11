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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# Load and optimize ctgan utility data ----

df_ctgan <- read.csv(paste0(tables,"ctgan/ctgan_fidelity_optimize_dataset.csv"))
df_ctgan <- df_ctgan %>%
  filter(epochs==600) %>%
  mutate(synthesizer = "ctgan") %>%
  select(data,copies, pmse, specks, synthesizer)
df_ctgan


# Load and optimize df_datasynthesizer utility data ----

df_datasynthesizer <- read.csv(paste0(tables,"datasynthesizer/datasynthesizer_fidelity_optimize_dataset.csv"))
df_datasynthesizer <- df_datasynthesizer %>%
  group_by(data,copies) %>%
  filter(privacy == 0 & parents == 2) %>%
  ungroup() %>%
  mutate(synthesizer = "datasynthesizer") %>%
  select(data,copies, pmse, specks, synthesizer)
df_datasynthesizer

# Load and optimize datasynthesizer utility data ----

df_synthpop <- read.csv(paste0(tables,"synthpop/synthpop_fidelity_optimize_dataset.csv"))
df_synthpop <- df_synthpop %>%
  group_by(data,copies) %>%
  mutate(best_fit = ifelse(specks==min(specks),yes=1,no=0)) %>%
  filter(best_fit == 1) %>%
  ungroup() %>%
  mutate(synthesizer = "synthpop") %>%
  select(data,copies, pmse, specks, synthesizer)
df_synthpop

# Load utility data from other synthesizers ----

df_fidelity <- rbind(df_ctgan,df_datasynthesizer,df_synthpop) %>% 
  arrange(data,synthesizer) %>%
  filter(copies == 5) %>%
  filter(data == "sd2011_clean_small") %>%
  select(-copies)

df_fidelity

# Graph ----

df_data <- df_fidelity %>% 
  pivot_longer(!c(data,synthesizer), names_to = "fidelity", values_to = "values")

df_graph <- ggplot(df_data, aes(x = synthesizer, y = values)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_grid( ~ fidelity, labeller = labeller(.cols = label_both)) +
  theme_bw() +
  xlab("") +
  ylim(0, 1) +
  geom_text(aes(label = round(values,2)), vjust = -0.3) +  # Adding labels
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 75, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_fidelity.pdf"), height = 4, width = 8)
