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
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

setwd(main_dir)

options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# synthetic data

parents = c(0, 1, 2)
privacy = c(0, 0.1, 1, 5, 10, 20, 30)
copies = c(1, 5, 10)

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_0 <- df_ods
df_0$parents <- "Original"
df_0$copies <- 1
test <- data.frame()
for (e in privacy) {
  df <- df_0
  df$privacy <- e
  test <- rbind(test,df)
}
df_0 <- test


df_combine_sds = data.frame()

for (k in parents) {
  for (e in privacy) {
    for (m in copies) {
      df_sds = data.frame()
      for (j in 1:m) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_tuning_e_",e,"_k_",k,"_m_",m,"_n_",j,".csv"))
        df_sds <- rbind(df_sds,sds)
      }
      df_sds$parents =  k
      df_sds$privacy = e
      df_sds$copies = m
      df_combine_sds <- rbind(df_combine_sds,df_sds)
    }
  }
}

df_compare <- rbind(df_0, df_combine_sds)

group_vars_1 <- c("parents", "privacy", "copies")
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

#sort parents
numeric_values <- as.numeric(df_long$parents[!is.na(as.numeric(df_long$parents))])
df_long$parents <- factor(as.character(df_long$parents), levels = c("Original", as.character(sort(unique(numeric_values)))))

numeric_values <- as.numeric(df_long$privacy[!is.na(as.numeric(df_long$privacy))])
df_long$privacy <- factor(as.character(df_long$privacy), levels = c("Original", as.character(sort(unique(numeric_values)))))

#graph - compare frequency
df_graph <- ggplot(df_long, aes(x = value, color = privacy, linetype = parents)) +
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

ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_continuous_distribution.pdf"), height = 8, width = 10)

