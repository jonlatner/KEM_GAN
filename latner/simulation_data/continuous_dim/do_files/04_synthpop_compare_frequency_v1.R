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


# Create fake synthetic data ----

# rows = c(50000) # Rows/observations
# cols = c(10) # Columns/variables
# copies = c(5)
# for (r in rows) {
#   for (c in cols) {
#     for (m in copies) {
#       df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
#       df_synds <- syn(df_ods, m = m, seed = 1234)
#       saveRDS(df_synds, paste0(data_files,"synthetic/synds_",m,".rds"))
#     }
#   }
# }

# Load original data ----
# Dimensions
rows = c(50000) # Rows/observations
cols = c(10) # Columns/variables

for (r in rows) {
  for (c in cols) {
    df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
  }
}

# compare ----

df_synds <- readRDS(paste0(data_files,"synthetic/synds_5.rds"))

df_compare <- compare(df_synds, df_ods)

# Graph ----

df_compare_long <- data.frame(df_compare$tables) %>%
  rownames_to_column(var = "data") %>%
  pivot_longer(cols = starts_with(names(df_ods))) %>%
  rename(pct = value) %>%
  separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE) %>%
  mutate(contains_double_dot = if_else(str_detect(name, fixed("..")), 1, 0),
         value = as.numeric(value),
         value = if_else(contains_double_dot == 1, -1*value, value),
         ) %>%
  select(-name,-contains_double_dot)



df_compare_long$variables <- with(df_compare_long, reorder(variables, as.numeric(gsub("var_", "", as.character(variables)))))
df_compare_long$variables <- factor(df_compare_long$variables)

df_graph <- ggplot(df_compare_long, aes(x = value, y = pct, fill = data)) +
  geom_bar( stat = "identity", position=position_dodge2()) +
  # geom_density(data = subset(df_compare_long, data!="observed"), linewidth=1)  +
  # geom_histogram(data = subset(df_compare_long, data=="synthetic"), aes(y=after_stat(density)), color = "blue", alpha=0.2, position="identity")+
  facet_wrap( ~ variables, scales = "free", labeller = labeller(.rows = label_both), nrow = 2) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
