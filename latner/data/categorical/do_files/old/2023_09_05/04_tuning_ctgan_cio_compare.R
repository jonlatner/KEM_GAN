# Top commands ----
# The CIO and standardized difference are presented as the mean across 
# multiple regression models using each variable in the dataset as a target. 

# Create empty R application (no figures, data frames, packages, etc.)
# Get a list of all loaded packages
packages <- search()[grepl("package:", search())]
# Unload each package
for (package in packages) {
  unloadNamespace(package)
}

rm(list=ls(all=TRUE))

# load library
library(broom)
library(tidyverse)

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

#functions
source("/Users/jonathanlatner/Google Drive/My Drive/IAB/little_etal_2021/do_files/R/cio_function.R")
options(scipen=999) 

# Load data ----

df_sds_ctgan = read.csv(paste0(tables, "ctgan/utility_cio_estimates.csv"))

# original ----

df_ods_baseline <- df_sds_datasynthesizer %>%
  filter(parents == 0 & privacy == 0.1 & copies == 1 & data == "ods") %>%
  select(-parents, -privacy, -copies) %>%
  mutate(data = "Original",
         type = "Baseline")

df_ods_tuned <- df_sds_datasynthesizer %>%
  filter(parents == 0 & privacy == 0.1 & copies == 1 & data == "ods") %>%
  select(-parents, -privacy, -copies) %>%
  mutate(data = "Original",
         type = "Tuned")

df_ods <- rbind(df_ods_baseline,df_ods_tuned)

# baseline ----

sds_baseline_datasynthesizer <- df_sds_datasynthesizer %>%
  filter(parents == 0 & privacy == 0.1 & copies == 1 & data == "sds") %>%
  select(-parents, -privacy, -copies) %>%
  mutate(data = "Datasynthesizer")

sds_baseline_ctgan <- df_sds_ctgan %>%
  filter(epochs == 300 & discriminator == 1 & batch == 500 & frequency == "False" & copies == 1 & data == "sds") %>%
  select(-epochs, -discriminator, -batch, -frequency, -copies) %>%
  mutate(data = "CTGAN")

sds_baseline_synthpop <- df_sds_synthpop %>%
  filter(minbucket == 5 & cp == 0.00000001 & copies == 1 & data == "sds") %>%
  select(-minbucket, -cp, -copies)  %>%
  mutate(data = "Synthpop")

df_sds_baseline <- rbind(sds_baseline_synthpop,sds_baseline_ctgan,sds_baseline_datasynthesizer)
df_sds_baseline$type <- "Baseline"

rm(sds_baseline_synthpop,sds_baseline_ctgan,sds_baseline_datasynthesizer)

# tuned ----

sds_tuned_datasynthesizer <- df_sds_datasynthesizer %>%
  filter(parents == 1 & privacy == 20 & copies == 10 & data == "sds") %>%
  select(-parents, -privacy, -copies) %>%
  mutate(data = "Datasynthesizer")

sds_tuned_ctgan <- df_sds_ctgan %>%
  filter(epochs == 300 & discriminator == 10 & batch == 500 & frequency == "False" & copies == 10 & data == "sds") %>%
  select(-epochs, -discriminator, -batch, -frequency, -copies) %>%
  mutate(data = "CTGAN")

sds_tuned_synthpop <- df_sds_synthpop %>%
  filter(minbucket == 5 & cp == 0.00000001 & copies == 10 & data == "sds") %>%
  select(-minbucket, -cp, -copies) %>%
  mutate(data = "Synthpop")

df_sds_tuned <- rbind(sds_tuned_synthpop,sds_tuned_ctgan,sds_tuned_datasynthesizer)
df_sds_tuned$type <- "Tuned"
rm(sds_tuned_synthpop,sds_tuned_ctgan,sds_tuned_datasynthesizer)

df_sds <- rbind(df_sds_baseline, df_sds_tuned)

rm(df_sds_baseline, df_sds_tuned)

# Regression output graph ----

df_regression_data <- rbind(df_sds, df_ods) %>%
  filter(term != "(Intercept)")

df_regression_data$data <- fct_relevel(df_regression_data$data, "Original", after = Inf)

df_graph <- ggplot(df_regression_data, aes(x = estimate, y = term, color = data)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0, position = position_dodge(width = 0.9)) +
  facet_grid(dep_var ~ type, scales="free_x") +
  labs(x = "Estimated Coefficients", y = "Independent Variables") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_cio_regressions.pdf"), height = 8, width = 6)
