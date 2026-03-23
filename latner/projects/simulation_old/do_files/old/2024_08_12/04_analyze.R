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

setwd(main_dir)

#functions
options(scipen=999) 

# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

df_ods <- as.data.frame(table(df_ods))
df_ods$synthesizer <- "original"
df_ods$combine <- paste(df_ods$var1, df_ods$var2, df_ods$var3, df_ods$var4, sep = "")
df_ods <- df_ods %>%
  select(-matches("var"))
df_ods

# Load synthetic data from synthpop ----

df_synthpop <- read_csv(paste0(synthpop_data,"synthpop_frequency.csv"))
df_synthpop$synthesizer <- "synthpop"
df_synthpop

# Load synthetic data from datasynthesizer ----

df_datasynthesizer <- read_csv(paste0(datasynthesizer_data,"datasynthesizer_frequency.csv"))
df_datasynthesizer$synthesizer <- paste0("DP_",df_datasynthesizer$epsilon)

# Graph frequency ----

df_compare <- bind_rows(df_ods,df_synthpop,df_datasynthesizer)  %>%
  filter(combine == "1111")

ggplot(df_compare, aes(x = synthesizer, y = Freq)) +
  geom_boxplot(position = position_dodge(width=0.9)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

