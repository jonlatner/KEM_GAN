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
library(xtable)

# https://arxiv.org/pdf/2109.12717
# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
tables = "tables/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 

# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# Load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Create synthetic data with numeric variables ----

sds_numeric <- syn(df_ods, m = 1, seed = my.seed)
sds_categorical <- syn(df_ods, m = 1, seed = my.seed, minnumlevels = 5)

# utility numeric ----

df_utility <- compare(sds_numeric, df_ods, utility.stats = c("SPECKS", "pMSE"), plot = FALSE)

df_table_numeric <- df_utility$tab.utility
# Assuming df_utility$tab.utility is your data frame
df_table_numeric <- data.frame(df_utility$tab.utility)
df_table_numeric

# Calculate the average for each column
average_row <- colMeans(df_table_numeric)
average_row

# Add the "average" row to the data frame
df_table_numeric["average", ] <- average_row
df_table_numeric$type <- "numeric"

df_table_numeric <- df_table_numeric %>% 
  rownames_to_column(var = "variable")
  
# utility categorical ----

df_utility <- compare(sds_categorical, df_ods, utility.stats = c("SPECKS", "pMSE"), plot = FALSE)

df_table_categorical <- df_utility$tab.utility
# Assuming df_utility$tab.utility is your data frame
df_table_categorical <- data.frame(df_utility$tab.utility)
df_table_categorical

# Calculate the average for each column
average_row <- colMeans(df_table_categorical)
average_row

# Add the "average" row to the data frame
df_table_categorical["average", ] <- average_row
df_table_categorical$type <- "categorical"

df_table_categorical <- df_table_categorical %>% 
  rownames_to_column(var = "variable") 
  
# combine ----

df_table <- rbind(df_table_numeric,df_table_categorical)
df_table

df_table <- df_table %>% 
  pivot_longer(!c("type","variable"))
df_table


df_graph <- ggplot(data = df_table, aes(x = variable, y = value, fill = type, label = round(value,2))) +
  facet_wrap(~ name) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  scale_y_continuous(limits = c(0,2)) +
  theme_bw() +
  geom_text(position = position_dodge(0.9), vjust = -.2) + 
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(df_graph, filename = paste0(graphs,"graph_compare_utility.pdf"), height = 4, width = 10, units = "in")
