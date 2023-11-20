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
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulated_data/categorical/"

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


#Compare frequencies of values in categorical variables ----

#reshape wide to long
group_vars_1 = c("minbucket", "cp", "copies")
df_long <- df_combine_sds %>%
  pivot_longer(cols = -group_vars_1, names_to = "variables", values_to = "value") 
head(df_long)

#calculate frequency count of values in each variable by dataset
group_vars_2 = c(group_vars_1, "variables", "value")

df_count <- df_long %>%
  group_by(across(all_of(group_vars_2))) %>%
  tally() %>%
  ungroup() %>%
  mutate(n = ifelse(copies > 1, yes = n/copies, no = n)) %>%
  mutate(minbucket = as.numeric(minbucket),
         cp = as.numeric(cp),
         copies = as.numeric(copies),
         ) 
head(df_count)

df_long_ods_1 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = "1",
         minbucket = "5")




#graph - compare frequency

# Reorder the levels based on a summary statistic (e.g., mean)

df_count$copies <- factor(df_count$copies, levels = c(1, 5, 10))
df_long_ods_1$copies <- factor(df_long_ods_1$copies, levels = c(1, 5, 10))

df_count$minbucket <- factor(df_count$minbucket, levels = c(5, 10, 25, 50))
df_long_ods_1$minbucket <- factor(df_long_ods_1$minbucket, levels = c(5, 10, 25, 50))

df_count$cp <- factor(df_count$cp, levels = c(0.00000001, 0.000001, 0.0001, 0.01))

df_graph <- ggplot(df_count, aes(x = value, y = n, color = cp, shape = minbucket)) +
  geom_point(position = position_dodge(width = 0.75), stat = "identity") +
  facet_nested(variables ~ copies, scales = "free", labeller = labeller(.cols = label_both)) +
  geom_bar(data = df_long_ods_1, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(colour = guide_legend(nrow = 1)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_synthpop_frequency.pdf"), height = 8, width = 10)
