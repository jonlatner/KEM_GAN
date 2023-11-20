# Top commands ----
# https://alfurka.github.io/2023-01-30-creating-synthetic-values-with-synthepop-cart/
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
library(ggplot2)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_data/simulation_cont/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

setwd(main_dir)

options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# synthetic data

cp <- c(0.00000001, 0.000001, 0.0001, 0.01)
minbucket <- c(5, 10, 25, 50)
copies <- c(1, 5, 10)

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_0 <- df_ods
df_0$minbucket <- "Original"
df_0$copies <- 1
test <- data.frame()
for (c in cp) {
  df <- df_0
  df$cp <- c
  test <- rbind(test,df)
}
df_0 <- test

df_combine_sds = data.frame()

for (a in cp) {
  for (b in minbucket) {
    for (c in copies) {
      df_sds = data.frame()
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_synthpop_tuning_cp_",a,"_b_",b,"_m_",c,"_n_",j,".csv"))
        df_sds <- rbind(df_sds,sds)
      }
      df_sds$cp = a
      df_sds$minbucket =  b
      df_sds$copies = c
      df_combine_sds <- rbind(df_combine_sds,df_sds)
    }
  }
}

df_compare <- rbind(df_0, df_combine_sds)

group_vars_1 <- c("cp", "minbucket", "copies")
group_vars_2 = c(group_vars_1, "variables", "value")
group_vars_3 = c(group_vars_1, "variables")

#Compare distribution of continuous variables ----
#select categorical variables
continuous_vars <- sapply(df_ods, function(x) is.numeric(x))
continuous_var_names <- names(df_ods[continuous_vars])
continuous_var_names <- c(continuous_var_names, group_vars_1)
df_continuous <- df_compare[, continuous_var_names]

head(df_continuous)

#reshape wide to long
df_long <- df_continuous %>%
  pivot_longer(cols = -c(group_vars_1), names_to = "variables", values_to = "value") 
head(df_long)

#sort minbucket
numeric_values <- as.numeric(df_long$minbucket[!is.na(as.numeric(df_long$minbucket))])
df_long$minbucket <- factor(as.character(df_long$minbucket), levels = c("Original", as.character(sort(unique(numeric_values)))))

numeric_values <- as.numeric(df_long$cp[!is.na(as.numeric(df_long$cp))])
df_long$cp <- factor(as.character(df_long$cp), levels = c("Original", as.character(sort(unique(numeric_values)))))

#graph - compare frequency
df_graph <- ggplot(df_long, aes(x = value, color = cp, linetype = minbucket)) +
  geom_density() +
  facet_wrap( ~ variables + copies, scales="free", ncol = 3, labeller = labeller(.cols = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_synthpop_continuous_distribution.pdf"), height = 8, width = 10)

