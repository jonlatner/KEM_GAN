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
library(janitor)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/"
tables = "tables/synthpop/"

setwd(main_dir)


# Load original data ----
# Dimensions
rows = c(50000) # Rows/observations
cols = c(10) # Columns/variables

df_sds <- data.frame()

for (r in rows) {
  for (c in cols) {
    df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
    df_ods$data <- "Original"
    df_synds <- readRDS(paste0(data_files,"synthetic/synds_rows_",r,"_cols_",c,"_m_1.rds"))
    sds <- df_synds$syn
    df_sds <- rbind(df_sds, sds)
    df_sds$data <- "Synthpop"
  }
}

# Combine data ----

df_combine <- rbind(df_ods,df_sds) %>%
  pivot_longer(cols = !data, names_to = "variables", values_to = "value") %>%
  arrange(data,variables,value)

df_combine$variables <- with(df_combine, reorder(variables, as.numeric(gsub("var_", "", as.character(variables)))))
df_combine$variables <- factor(df_combine$variables)

df_graph <- ggplot(df_combine, aes(x = value,color = data)) +
  # geom_density(linewidth=1)  +
  geom_density(data = subset(df_combine, data!="Original"), linewidth = 1)  +
  geom_histogram(data = subset(df_combine, data=="Original"), aes(y=after_stat(density)), color = "blue", alpha=0.2, position="identity")+
  facet_wrap( ~ variables, scales = "free", labeller = labeller(.cols = label_both), nrow = 2) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(colour = guide_legend(nrow = 1)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

