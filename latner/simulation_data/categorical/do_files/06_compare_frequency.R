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
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/categorical/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# synthetic data (baseline) ----

sds_baseline_synthpop <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_tuning_cp_0.00000001_b_5_m_1_n_1.csv"))
sds_baseline_synthpop$data <- "Synthpop"
sds_baseline_ctgan <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_tuning_e_300_d_1_b_500_f_False_m_1_n_1.csv"))
sds_baseline_ctgan$data <- "CTGAN"
sds_baseline_datasynthesizer <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_tuning_e_0.1_k_0_m_1_n_1.csv"))
sds_baseline_datasynthesizer$data <- "DataSynthesizer"

df_sds_baseline <- rbind(sds_baseline_synthpop,sds_baseline_ctgan,sds_baseline_datasynthesizer)
df_sds_baseline$type <- "Baseline"
rm(sds_baseline_synthpop,sds_baseline_ctgan,sds_baseline_datasynthesizer)

# synthetic data (tuned) ----

copies <- c(10)
sds_tuned_synthpop = data.frame()
for (j in 1:copies) {
  sds <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_tuning_cp_0.00000001_b_5_m_10_n_",j,".csv"))
  sds_tuned_synthpop <- rbind(sds_tuned_synthpop,sds)
}
sds_tuned_synthpop$data <- "Synthpop"

copies <- c(10)
sds_tuned_ctgan = data.frame()
for (j in 1:copies) {
  sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_tuning_e_300_d_10_b_500_f_False_m_10_n_",j,".csv"))
  sds_tuned_ctgan <- rbind(sds_tuned_ctgan,sds)
}
sds_tuned_ctgan$data <- "CTGAN"

copies <- c(10)
sds_tuned_datasynthesizer = data.frame()
for (j in 1:copies) {
  sds <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_tuning_e_20_k_1_m_10_n_",j,".csv"))
  sds_tuned_datasynthesizer <- rbind(sds_tuned_datasynthesizer,sds)
}
sds_tuned_datasynthesizer$data <- "DataSynthesizer"

df_sds_tuned <- rbind(sds_tuned_synthpop,sds_tuned_ctgan,sds_tuned_datasynthesizer)
df_sds_tuned$type <- "Tuned"
rm(sds_tuned_synthpop,sds_tuned_ctgan,sds_tuned_datasynthesizer)

df_sds <- rbind(df_sds_baseline, df_sds_tuned)

#reshape wide to long ----

group_vars_1 = c("data", "type")
group_vars_2 = c(group_vars_1, "variables", "value")
group_vars_3 = c(group_vars_1, "variables")

df_sds_long <- df_sds %>%
  pivot_longer(cols = -group_vars_1, names_to = "variables", values_to = "value") %>%
  group_by(across(all_of(group_vars_2))) %>%
  tally() %>%
  group_by(across(all_of(group_vars_3))) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total)

#add original data ----

df_ods_long_1 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  mutate(type = "Baseline",
         data = "Original") %>%
  group_by(across(all_of(group_vars_3))) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total)

df_ods_long_2 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  mutate(type = "Tuned",
         data = "Original") %>%
  group_by(across(all_of(group_vars_3))) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total)

df_ods_long <- rbind(df_ods_long_1, df_ods_long_2)

df_long_combined <- rbind(df_sds_long, df_ods_long)

#graph - compare frequency ----

df_long_combined$data <- fct_relevel(df_long_combined$data, "Original")

df_graph <- ggplot(df_long_combined, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(type ~ variables, scales = "free", labeller = labeller(.cols = label_both)) +
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

ggsave(plot = df_graph, paste0(graphs,"graph_compare_frequency.pdf"), height = 4, width = 10)
