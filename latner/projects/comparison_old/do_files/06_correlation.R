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
library(ggcorrplot)
library(fastDummies)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
tables = "tables/"
graphs = "graphs/"

setwd(main_dir)

#functions
options(scipen=999) 

# Create fake synthetic data ----

ods <- SD2011
ods[ods == ""] <- NA
ods[ods < 0] <- NA

df_synthpop <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_sd2011_clean_small_m_1_n_1.csv"))
df_datasynthesizer <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_sd2011_clean_small_k_2_e_0_m_1_n_1.csv"))
df_ctgan <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_data_sd2011_clean_small_epochs_600_m_1_n_1.csv"))

# Correlation plots ----

df_corr <- ods %>%
  filter(smoke!="") %>%
  filter(edu!="") %>%
  select(age,height,weight,sex,edu,income,smoke) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE)
df_corr <- dummy_cols(df_corr, select_columns = c("smoke"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE) %>%
  rename("SMOKE"="YES")
df_corr <- dummy_cols(df_corr, select_columns = c("edu"), remove_selected_columns = TRUE,omit_colname_prefix = TRUE)
corr_ods <- cor(df_corr,use = "pairwise.complete.obs")
write.csv(corr_ods, paste0(graphs,"correlation_sd2011.csv"), row.names=FALSE)

df_corr <- df_synthpop %>%
  filter(smoke!="") %>%
  filter(edu!="") %>%
  select(age,height,weight,sex,edu,income,smoke) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE)
df_corr <- dummy_cols(df_corr, select_columns = c("smoke"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE) %>%
  rename("SMOKE"="YES")
df_corr <- dummy_cols(df_corr, select_columns = c("edu"), remove_selected_columns = TRUE,omit_colname_prefix = TRUE)
corr_synthpop <- cor(df_corr,use = "pairwise.complete.obs")
write.csv(corr_synthpop, paste0(graphs,"correlation_synthpop.csv"), row.names=FALSE)

df_corr <- df_datasynthesizer %>%
  filter(smoke!="") %>%
  filter(edu!="") %>%
  select(age,height,weight,sex,edu,income,smoke) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE)
df_corr <- dummy_cols(df_corr, select_columns = c("smoke"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE) %>%
  rename("SMOKE"="YES")
df_corr <- dummy_cols(df_corr, select_columns = c("edu"), remove_selected_columns = TRUE,omit_colname_prefix = TRUE)
corr_datasynthesizer <- cor(df_corr,use = "pairwise.complete.obs")

write.csv(corr_datasynthesizer, paste0(graphs,"correlation_datasynthesizer.csv"), row.names=FALSE)

df_corr <- df_ctgan %>%
  filter(smoke!="") %>%
  filter(edu!="") %>%
  select(age,height,weight,sex,edu,income,smoke) 
df_corr <- dummy_cols(df_corr, select_columns = c("sex"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE)
df_corr <- dummy_cols(df_corr, select_columns = c("smoke"), remove_selected_columns = TRUE, remove_most_frequent_dummy = TRUE,omit_colname_prefix = TRUE) %>%
  rename("SMOKE"="YES")
df_corr <- dummy_cols(df_corr, select_columns = c("edu"), remove_selected_columns = TRUE,omit_colname_prefix = TRUE)
corr_ctgan <- cor(df_corr,use = "pairwise.complete.obs")
write.csv(corr_ctgan, paste0(graphs,"correlation_ctgan.csv"), row.names=FALSE)

# Graph (ggcorrplot) ----

# Assuming your datasets are stored in CSV files
data <- c("correlation_sd2011", "correlation_synthpop", "correlation_datasynthesizer", "correlation_ctgan")

for (d in data) {
  # Load the dataset
  df <- read.csv(paste0(graphs, d, ".csv"))
  rownames(df) <- colnames(df)
  
  # Create correlation plot
  df_graph <- ggcorrplot(df,type = "lower")
  
  # Save the plot
  ggsave(plot = df_graph, file = paste0(graphs, d, ".pdf"), height = 4, width = 6)
}

df_graph

# Graph (geom_tile) ----

# Assuming your datasets are stored in CSV files

df_correlation <- data.frame()
data <- c("sd2011", "synthpop", "datasynthesizer", "ctgan")
for (d in data) {
  df <- read.csv(paste0(graphs,"correlation_", d, ".csv"))
  colnames(df) <- tolower(colnames(df))
  df$X1 <- colnames(df)
  df$type <- d
  df_correlation <- rbind(df_correlation,df)
}

df <- pivot_longer(df_correlation,!c(type,X1)) %>%
  mutate(type = ifelse(type == "sd2011", yes = " original (SD2011)", no = type))

df_graph <- ggplot(df, aes(X1, name,fill = value)) +
  xlab("") +
  ylab("") +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000", midpoint = 0, limits = c(-1, 1)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "right",
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )+
  geom_tile(color = "black") +
  facet_wrap(~type) +
  geom_text(aes(label = round(value,2)), size = 1.5)

df_graph

ggsave(plot = df_graph, file = paste0(graphs, "graph_correlation.pdf"), height = 4, width = 6)


