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
rows = c(50000, "100000", "200000") # Rows/observations
cols = c(10) # Columns/variables

df_combine <- data.frame()

for (r in rows) {
  for (c in cols) {
    df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
    df_ods$data <- "Original"
    
    df_synthpop <- read.csv(paste0(data_files,"synthetic/synthpop/sds_synthpop_rows_",r,"_cols_",c,"_m_1.csv"))
    df_synthpop$data <- "Synthpop"

    df_datasynthesizer <- read.csv(paste0(data_files,"synthetic/datasynthesizer/sds_datasynthesizer_rows_",r,"_cols_",c,"_n_1.csv"))
    df_datasynthesizer$data <- "Datasynthesizer"

    combine <- rbind(df_ods,df_synthpop,df_datasynthesizer)
    combine$rows = r
    df_combine <- rbind(df_combine, combine)
      
  }
}

df_ctgan_50000 <- read.csv(paste0(data_files,"synthetic/ctgan/sds_ctgan_rows_50000_cols_10_n_1_epochs_50_batch_1000.csv"))
df_ctgan_50000$rows = 50000
df_ctgan_100000 <- read.csv(paste0(data_files,"synthetic/ctgan/sds_ctgan_rows_100000_cols_10_n_1_epochs_50_batch_1000.csv"))
df_ctgan_100000$rows = "100000"
df_ctgan_200000 <- read.csv(paste0(data_files,"synthetic/ctgan/sds_ctgan_rows_200000_cols_10_n_1_epochs_50_batch_1000.csv"))
df_ctgan_200000$rows = "200000"

combine <- rbind(df_ctgan_50000,df_ctgan_100000,df_ctgan_200000)
combine$data <- "CTGAN"
df_combine <- rbind(df_combine, combine)


# Combine data ----

df_combine_long <- df_combine %>%
  pivot_longer(cols = !c(data,rows), names_to = "variables", values_to = "value") 

df_graph <- ggplot(df_combine_long, aes(x = value, color = data)) +
  geom_density(data = subset(df_combine_long, data!="Original"), linewidth = .75)  +
  geom_histogram(data = subset(df_combine_long, data=="Original"), aes(y=after_stat(density)), color = "blue", alpha=0.1, linewidth = .1, position="identity")+
  facet_grid(rows ~ variables, scales = "free", labeller = labeller(.cols = label_both, .rows = label_both)) +
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

ggsave(plot = df_graph, paste0(graphs,"graph_compare_frequency.pdf"), height = 6, width = 10)

