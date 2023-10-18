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
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

options(scipen=999) 

# Load data ----

# original data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

continuous_vars <- sapply(df_ods, function(x) is.numeric(x))
continuous_var_names <- names(df_ods[continuous_vars])

# convert continuous variables to bins
df_ods_binned <- df_ods
for (col_name in continuous_var_names) {
  bins <- seq(min(df_ods_binned[[col_name]]), max(df_ods_binned[[col_name]]), length.out = 21) # 21 points to get 20 bins
  df_ods_binned[[col_name]] <- cut(df_ods_binned[[col_name]], breaks = bins, include.lowest = TRUE)
  # df_sds[[col_name]] <- cut(df_sds[[col_name]], breaks = bins, include.lowest = TRUE)
}

# synthetic data (baseline) ----

sds_baseline_synthpop <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_tuning_cp_0.00000001_b_5_m_1_n_1.csv"))
sds_baseline_synthpop$data <- "Synthpop"
sds_baseline_ctgan <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_tuning_e_300_d_1_b_500_f_False_m_1_n_1.csv"))
sds_baseline_ctgan$data <- "CTGAN"
sds_baseline_datasynthesizer <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_tuning_e_0.1_k_0_m_1_n_1.csv"))
sds_baseline_datasynthesizer$data <- "DataSynthesizer"

# convert continuous variables to bins
df_sds_baseline <- data.frame()
df_sds_baseline_list <- list(sds_baseline_synthpop,sds_baseline_ctgan,sds_baseline_datasynthesizer)
for (d in 1:length(df_sds_baseline_list)) {
  df_sds <- df_sds_baseline_list[[d]]
  for (col_name in continuous_var_names) {
  bins <- seq(min(df_ods[[col_name]]), max(df_ods[[col_name]]), length.out = 21) # 21 points to get 20 bins
  df_sds[[col_name]] <- cut(df_sds[[col_name]], breaks = bins, include.lowest = TRUE)
  }
  df_sds_baseline <- rbind(df_sds_baseline,df_sds)
}

df_sds_baseline$type <- "Baseline"
head(df_sds_baseline)

rm(sds_baseline_synthpop,sds_baseline_ctgan,sds_baseline_datasynthesizer,df_sds_baseline_list,df_sds)

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
  sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_tuning_e_900_d_10_b_500_f_False_m_10_n_",j,".csv"))
  sds_tuned_ctgan <- rbind(sds_tuned_ctgan,sds)
}
sds_tuned_ctgan$data <- "CTGAN"

copies <- c(10)
sds_tuned_datasynthesizer = data.frame()
for (j in 1:copies) {
  sds <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_tuning_e_30_k_1_m_10_n_",j,".csv"))
  sds_tuned_datasynthesizer <- rbind(sds_tuned_datasynthesizer,sds)
}
sds_tuned_datasynthesizer$data <- "DataSynthesizer"


# convert continuous variables to bins
df_sds_tuned <- data.frame()
df_sds_tuned_list <- list(sds_tuned_synthpop,sds_tuned_ctgan,sds_tuned_datasynthesizer)
for (d in 1:length(df_sds_tuned_list)) {
  df_sds <- df_sds_tuned_list[[d]]
  for (col_name in continuous_var_names) {
    bins <- seq(min(df_ods[[col_name]]), max(df_ods[[col_name]]), length.out = 21) # 21 points to get 20 bins
    df_sds[[col_name]] <- cut(df_sds[[col_name]], breaks = bins, include.lowest = TRUE)
  }
  df_sds_tuned <- rbind(df_sds_tuned,df_sds)
}

df_sds_tuned$type <- "Tuned"
rm(sds_tuned_synthpop,sds_tuned_ctgan,sds_tuned_datasynthesizer,df_sds,df_sds_tuned_list)

df_sds <- rbind(df_sds_baseline, df_sds_tuned)
rm(df_sds_baseline, df_sds_tuned)

head(df_sds)

#reshape wide to long ----

group_vars_1 = c("data", "type")
group_vars_2 = c(group_vars_1, "variables", "value")
group_vars_3 = c(group_vars_1, "variables")

df_sds_long <- df_sds %>%
  pivot_longer(cols = -c(group_vars_1), names_to = "variables", values_to = "value") %>%
  group_by(across(all_of(group_vars_2))) %>%
  tally() %>%
  group_by(across(all_of(group_vars_3))) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total)


#add original data ----

df_ods_long_1 <- df_ods_binned %>%
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

df_ods_long_2 <- df_ods_binned %>%
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
  facet_nested_wrap(variables ~ type, scales = "free", labeller = labeller(.cols = label_both), ncol = 2) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(colour = guide_legend(nrow = 1)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_compare_frequency.pdf"), height = 8, width = 6)
