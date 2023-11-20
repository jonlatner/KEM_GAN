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

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/categorical/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# Load utility baseline data ----

utility <- c("roc_univar", "roc_bivar", "cio")
synthetic <- c("ctgan")

df_ctgan <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(epochs == 300 & discriminator == 1 & frequency == "False" & batch == 500 & copies == 5) %>%
      select(-epochs, -discriminator, -frequency, -batch, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_ctgan <- rbind(df_ctgan,df)
  }
}

df_ctgan

synthetic <- c("datasynthesizer")
df_datasynthesizer <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(privacy == 0.1 & parents == 0 & copies == 5) %>%
      select(-privacy, -parents, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_datasynthesizer <- rbind(df_datasynthesizer,df)
  }
}

synthetic <- c("synthpop")
df_synthpop <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(cp == 0.00000001 & minbucket == 5 & copies == 5) %>%
      select(-cp, -minbucket, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_synthpop <- rbind(df_synthpop,df)
  }
}

df_utility_baseline <- rbind(df_ctgan, df_datasynthesizer, df_synthpop)
df_utility_baseline$data <- "Baseline"

# Load utility tuned data ----


utility <- c("roc_univar", "roc_bivar", "cio")
synthetic <- c("ctgan")

df_ctgan <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(epochs == 300 & discriminator == 10 & frequency == "False" & batch == 500 & copies == 10) %>%
      select(-epochs, -discriminator, -frequency, -batch, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_ctgan <- rbind(df_ctgan,df)
  }
}

synthetic <- c("datasynthesizer")
df_datasynthesizer <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(privacy == 0 & parents == 1 & copies == 5) %>%
      select(-privacy, -parents, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_datasynthesizer <- rbind(df_datasynthesizer,df)
    # assign(paste("df",u,s,sep = "_"), df)
  }
}

synthetic <- c("synthpop")
df_synthpop <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(cp == 0.00000001 & minbucket == 5 & copies == 5) %>%
      select(-cp, -minbucket, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_synthpop <- rbind(df_synthpop,df)
  }
}

df_utility_tuned <- rbind(df_ctgan, df_datasynthesizer, df_synthpop)
df_utility_tuned$data <- "Tuned"

# Clean table ----

df_utility <- rbind(df_utility_baseline,df_utility_tuned) 
df_utility$name <- factor(df_utility$name, 
                          levels = c("roc_univar", "roc_bivar", "std_diff", "ci_overlap"),
                          labels = c("ROE (univar)", "ROE (bivar)", "Std. Diff", "CI Overlap"))

df_graph <- ggplot(df_utility, aes(x = name, y = value, fill = type)) +
  facet_wrap(~ data) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("") +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_utility.pdf"), height = 4, width = 10)
