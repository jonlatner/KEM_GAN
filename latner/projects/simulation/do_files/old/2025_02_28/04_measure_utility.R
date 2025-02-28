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

# create table ----

df_utility <- compare(sds_numeric, df_ods, plot = FALSE, utility.stats = "all")

df_table_numeric <- df_utility$tab.utility
# Assuming df_utility$tab.utility is your data frame
df_table_numeric <- data.frame(df_utility$tab.utility) 

# Calculate the average for each column
average_row <- colMeans(df_table_numeric)

# Add the "average" row to the data frame
df_table_numeric["average", ] <- average_row

# rotate
df_table_numeric <- df_table_numeric %>%
  rownames_to_column(var = "variable")  %>% 
  pivot_longer(!"variable") %>%
  pivot_wider(names_from = c("variable"))


# print table ----

# print
df_table_numeric <- df_table_numeric %>%
  filter(name!="U") %>%
  filter(name!="G") %>%
  filter(name!="PO50") %>%
  filter(name!="WMabsDD") %>%
  filter(name!="MabsDD") %>%
  filter(name!="df") 

df_table_numeric <- df_table_numeric[!grepl("S_", df_table_numeric$name), ]

df_table_numeric

df_table_numeric$name = c(
  "Voas Williamson utility measure", 
  "Freeman-Tukey utility measure", 
  "Jensen-Shannaon divergence", 
  "Kolmogorov-Smirnov statistic",
  "propensity score mean-squared error", 
  "Bhattacharyya distances"
)

# Print the data frame as a LaTeX table using xtable
latex_table <- xtable(df_table_numeric)
print.xtable(latex_table, 
             include.rownames = FALSE, 
             sanitize.text.function = identity,
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_utility.tex"))

