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

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulated_data/categorical/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# Load data ----

utility <- c("roc_univar", "cio")
synthetic <- c("ctgan")

df_ctgan <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(epochs == 300 & discriminator == 1 & frequency == "False" & batch == 500 & copies == 1) %>%
      select(-epochs, -discriminator, -frequency, -batch, -copies)
    df$type <- s
    df <- df %>%
      pivot_longer(cols=!c(type))
    df_ctgan <- rbind(df_ctgan,df)
    assign(paste("df",u,s,sep = "_"), df)
  }
}

df_ctgan

synthetic <- c("datasynthesizer")
df_datasynthesizer <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(privacy == 0.1 & parents == 0 & copies == 1) %>%
      select(-privacy, -parents, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_datasynthesizer <- rbind(df_datasynthesizer,df)
    # assign(paste("df",u,s,sep = "_"), df)
  }
}

df_datasynthesizer

synthetic <- c("synthpop")
df_synthpop <- data.frame()
for (u in utility) {
  for (s in synthetic) {
    df <- read.csv(paste0(tables,s,"/utility_",u,".csv"))
    df <- df %>%
      filter(cp == 0.00000001 & minbucket == 5 & copies == 1) %>%
      select(-cp, -minbucket, -copies)
    df$type <- s
    df <- df %>% 
      pivot_longer(cols=!c(type))
    df_synthpop <- rbind(df_synthpop,df)
    # assign(paste("df",u,s,sep = "_"), df)
  }
}

# Create table ----

df_ctgan
df_datasynthesizer

df_utility <- rbind(df_ctgan, df_datasynthesizer, df_synthpop) %>% 
  pivot_wider(names_from = "name", values_from = "value")
df_utility

# Clean table ----

# transpose
df_table <- t(df_utility)

# Convert first row to column names
colnames(df_table) <- df_table[1, ]
# Drop first row
df_table <- data.frame(df_table[-1, ])
# Convert row names to a column
df_table$Metric <- rownames(df_table)
# Reset row names to NULL
row.names(df_table) <- NULL

df_table

df_table$Metric <- factor(df_table$Metric,
                          levels = c("roc", "ci_overlap", "std_diff"),
                          labels = c("\\medskip ROE univariate", "CI Overlap", "Standardized difference"))
df_table

df_table <- df_table %>%
  rename("CTGAN" = ctgan,
         "DataSynthesizer" = datasynthesizer,
         "Synthpop" = synthpop) %>%
  mutate(CTGAN = as.numeric(CTGAN),
         DataSynthesizer = as.numeric(DataSynthesizer),
         Synthpop = as.numeric(Synthpop),
         Metric = as.factor(Metric)) %>%
  select(Metric, Synthpop, DataSynthesizer, CTGAN) %>%
  arrange(Metric)



# Prepare table ----

# Convert data frame to LaTeX table
latex_table <- xtable(df_table,
                      digits = 4,
                      align = c("l", "l", "r", "r", "r"))

# Save table ----

print(latex_table, 
      booktabs = TRUE, 
      include.rownames = FALSE,
      floating=FALSE,
      sanitize.text.function = identity,
      file = paste0(tables,"table_utility_baseline.tex"),
)

