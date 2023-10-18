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
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/continuous/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

setwd(main_dir)

options(scipen=999) 

# Create fake synthetic data ----

# df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
# df_ods[df_ods == ""] <- NA
# df_ods <- df_ods %>%
#   mutate_if(is.character, as.factor)
# copies <- c(1, 5)
# for (c in copies) {
#   df_synds <- syn(df_ods, m = c)
#   saveRDS(df_synds, paste0(data_files,"synthetic/synds_",c,".rds"))
# }


# Load original data ----
df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_ods[df_ods == ""] <- NA
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))


# Load synthetic data ----

df_synds_1 <- readRDS(paste0(data_files,"synthetic/synds_1.rds"))
df_synds_5 <- readRDS(paste0(data_files,"synthetic/synds_5.rds"))

df_combine_sds = data.frame()

cp <- c(0.00000001, 0.01)
minbucket <- c(5, 50)
copies <- c(5)

for (c in copies) {
  sds_list <- get(paste0("df_synds_",c))
  for (a in cp) {
    for (b in minbucket) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_synthpop_tuning_cp_",a,"_b_",b,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn[[c]] <- sds
      }
      df_sds <- compare(sds_list, df_ods)
      df_sds <- data.frame(df_sds$tables) %>%
        rownames_to_column(var = "data") %>%
        pivot_longer(cols = starts_with(names(df_ods))) %>%
        rename(pct = value) %>%
        separate(name, into = c("variable", "value"), sep = "\\.", remove = FALSE) %>%
        select(-name)
      df_sds$cp = a
      df_sds$minbucket =  b
      df_sds$copies = c
      df_combine_sds <- rbind(df_combine_sds,df_sds)
    }
  }
}

# copies <- c(1)
# cp <- c(0.00000001)
# minbucket <- c(5)
# for (c in copies) {
#   sds_list <- get(paste0("df_synds_",c))
#   for (a in cp) {
#     for (b in minbucket) {
#       sds <- read.csv(paste0(synthetic_data,"sds_synthpop_tuning_cp_",a,"_b_",b,"_m_",c,"_n_1.csv"))
#       sds <- sds %>%
#         mutate_if(is.character, as.factor)
#       sds_list$syn <- sds
#     df_sds <- compare(sds_list, df_ods)
#     df_sds <- data.frame(df_sds$tables) %>%
#       rownames_to_column(var = "data") %>%
#       pivot_longer(cols = starts_with(names(df_ods))) %>%
#       rename(pct = value) %>%
#       separate(name, into = c("variable", "value"), sep = "\\.", remove = FALSE) %>%
#       select(-name)
#     df_sds$cp = a
#     df_sds$minbucket =  b
#     df_sds$copies = c
#     df_combine_sds <- rbind(df_combine_sds,df_sds)
#     }
#   }
# }


# Prepare data for graph ----

df_synthetic <- df_combine_sds %>%
  filter(data == "synthetic")

for (b in minbucket) {
  df_test <- df_combine_sds %>%
    filter(data == "observed")%>%
    mutate(minbucket = b,
           cp = "Original",
           copies = "Original") %>%
    unique()
  df_synthetic <- rbind(df_synthetic, df_test)
}

df_synthetic[df_synthetic == "NA"] <- NA
df_synthetic[df_synthetic == "miss"] <- NA
df_synthetic <- droplevels(df_synthetic)


# Graph ----

df_synthetic$cp <- factor(as.character(df_synthetic$cp), levels = c("Original", "0.00000001", "0.01"))
df_synthetic$minbucket <- factor(as.character(df_synthetic$minbucket), levels = c(as.character(sort(unique(df_synthetic$minbucket)))))
df_synthetic$value <- factor(as.character(df_synthetic$value), levels = str_sort(unique(df_synthetic$value), numeric = TRUE))

df_graph <- ggplot(df_synthetic, aes(x = value, y = pct, fill = cp)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_grid(minbucket ~ variable, scales = "free", labeller = labeller(.rows = label_both)) +
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

# ggsave(plot = df_graph, paste0(graphs,"graph_synthpop_continuous_distribution.pdf"), height = 8, width = 10)

