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
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/categorical_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_ods[df_ods == ""] <- NA
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)
copies <- c(1, 5)
for (c in copies) {
  df_synds <- syn(df_ods, m = c)
  saveRDS(df_synds, paste0(data_files,"synthetic/synds_",c,".rds"))
}


# Load original data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_ods[df_ods == ""] <- NA
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)


# Load synthetic data ----

df_synds_1 <- readRDS(paste0(data_files,"synthetic/synds_1.rds"))

df_sds <- compare(df_synds_1, df_ods)
df_sds <- data.frame(df_sds$tables) %>%
  rownames_to_column(var = "data") %>%
  pivot_longer(cols = starts_with(names(df_ods))) %>%
  rename(pct = value) %>%
  separate(name, into = c("variable", "value"), sep = "\\.", remove = FALSE) %>%
  select(-name)


# Prepare data for graph ----

df_observed <- df_sds %>%
  filter(data == "observed")

df_observed[df_observed == "NA"] <- NA
df_observed[df_observed == "miss"] <- NA
df_observed <- droplevels(df_observed)


# Graph ----

df_observed$value <- factor(as.character(df_observed$value), levels = str_sort(unique(df_observed$value), numeric = TRUE))

df_graph <- ggplot(df_observed, aes(x = value, y = pct)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_wrap( ~ variable, scales = "free_x", nrow = 1) +
  xlab("") +
  ylab("Percent") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 75, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_descriptives.pdf"), height = 4, width = 10)
