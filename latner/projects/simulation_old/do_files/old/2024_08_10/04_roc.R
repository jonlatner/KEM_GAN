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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"

setwd(main_dir)

#functions
options(scipen=999) 

# load data ----

df_synthpop <- read.csv(paste0(synthpop_data,"synthpop_roc_values.csv")) 

df_datasynthesizer <- read.csv(paste0(datasynthesizer_data,"datasynthesizer_roc_values.csv"))

# prepare ----

df_synthpop <- df_synthpop %>%
  rename(roc = value,
         variable = name)
df_synthpop$synthesizer <- "synthpop"

df_datasynthesizer <- df_datasynthesizer %>%
  rename(roc = ROC.Value,
         variable = Variable)
df_datasynthesizer$synthesizer <- paste0("DP_",df_datasynthesizer$epsilon)

# combine
df_compare <- bind_rows(df_synthpop,df_datasynthesizer) %>%
  group_by(synthesizer, number) %>%
  summarise(mean_roc = mean(roc)) %>%
  ungroup()

# graph ----

ggplot(df_compare, aes(x = synthesizer, y = roc)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  facet_wrap(~variable) +
  # geom_text(aes(label = label2), hjust = -0.5, size = 3, position = position_dodge(width=.9), angle = 90) +
  ylab("ROC value") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = .5),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )
