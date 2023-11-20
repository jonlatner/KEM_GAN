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
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/categorical_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

options(scipen=999) 

# Load data ----

# original data

rows = c(1000) # Rows/observations
cols = c(10) # Columns/variables
vals = c(5)  # Number of possible options for each character

for (r in rows) {
  for (c in cols) {
    for (v in vals) {
      
      print(paste(r, c, v,sep = ","))
      
      df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,"_vals_",v,".csv"))
      df_ods$data <- "Original"
      df_synthpop <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_rows_",r,"_cols_",c,"_vals_",v,"_n_1.csv"))
      df_synthpop$data <- "Synthpop"
      df_ctgan <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_rows_",r,"_cols_",c,"_vals_",v,"_n_1.csv"))
      df_ctgan$data <- "CTGAN"
      df_datasynthesizer <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_rows_",r,"_cols_",c,"_vals_",v,"_n_1.csv"))
      df_datasynthesizer$data <- "DataSynthesizer"
      
      df_combine <- rbind(df_ods,df_synthpop,df_ctgan,df_datasynthesizer)
      df_combine$rows <- r
      df_combine$cols <- c
      df_combine$vals <- v
    }
  }
}

#graph - compare frequency ----

df_combine_long <- df_combine %>%
  pivot_longer(cols = !c("data","rows","cols","vals"), names_to = "variables", values_to = "value") %>%
  group_by(data,rows,cols,vals,variables,value) %>%
  tally() %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total)

head(df_combine_long)

df_combine_long$data <- fct_relevel(df_combine_long$data, "Original")

table(df_combine_long$data)

df_combine_long$variables <- with(df_combine_long, reorder(variables, as.numeric(gsub("var_", "", as.character(variables)))))
df_combine_long$variables <- factor(df_combine_long$variables)

df_graph <- ggplot(df_combine_long, aes(x = value, y = pct, shape = data, color = data)) +
  geom_point(data = subset(df_combine_long, data!="Original"), size = 3, position = "jitter") +
  geom_bar(data = subset(df_combine_long, data=="Original"), stat = "identity", alpha = .2, show.legend = FALSE, fill = "blue") +
  scale_shape_manual(
    values = c(15, 16, 17),
    limits = c('CTGAN', 'DataSynthesizer', "Synthpop")
  ) +
  scale_color_manual(
    values = c("red", "blue", "green"),
    limits = c('CTGAN', 'DataSynthesizer', "Synthpop")
  ) +
  facet_wrap( ~ variables, labeller = labeller(.rows = label_both), nrow = 2) +
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

ggsave(plot = df_graph, paste0(graphs,"graph_compare_frequency.pdf"), height = 6, width = 10)
