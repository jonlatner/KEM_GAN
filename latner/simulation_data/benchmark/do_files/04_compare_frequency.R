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
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/simulation_data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"

setwd(main_dir)

# Load synthetic data from CTGAN (based on optimized paramterization) ----

data <- c("adult","grid","gridr","sd2011_small")
data <- c("sd2011_small")

copies = c(1)
epochs = c(50)

df_ctgan <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (e in epochs) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      }
      
      df_temp <- compare(sds_list, df_ods) 
      df_compare <- data.frame(df_temp$tables) %>%
        rownames_to_column(var = "data") %>%
        pivot_longer(cols = starts_with(names(df_ods))) %>%
        rename(pct = value) %>%
        separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE) %>%
        mutate(contains_double_dot = if_else(str_detect(name, fixed("..")), 1, 0),
               value_new = as.numeric(value),
               value_new = as.character(if_else(contains_double_dot == 1, -1*value_new, value_new)),
               value_new = if_else(is.na(value_new), value, value_new),
               value_new = if_else(value_new == "miss", NA, value_new),
        ) %>%
        select(-name,-contains_double_dot,-value) %>%
        rename(value=value_new)

      df_ctgan <- rbind(df_ctgan,df_compare)
    }
  }
}

df_ctgan$type <- "CTGAN"

# Load synthetic data from Synthpop ----

data <- c("adult","grid","gridr","sd2011_small")
data <- c("sd2011_small")

copies = c(1)

df_synthpop <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      }
      
      df_temp <- compare(sds_list, df_ods) 
      df_compare <- data.frame(df_temp$tables) %>%
        rownames_to_column(var = "data") %>%
        pivot_longer(cols = starts_with(names(df_ods))) %>%
        rename(pct = value) %>%
        separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE) %>%
        mutate(contains_double_dot = if_else(str_detect(name, fixed("..")), 1, 0),
               value_new = as.numeric(value),
               value_new = as.character(if_else(contains_double_dot == 1, -1*value_new, value_new)),
               value_new = if_else(is.na(value_new), value, value_new),
               value_new = if_else(value_new == "miss", NA, value_new),
        ) %>%
        select(-name,-contains_double_dot,-value) %>%
        rename(value=value_new)

      df_synthpop <- rbind(df_synthpop,df_compare)
  }
}

df_synthpop$type <- "Synthpop"

# Graph ----

df_comparison <- rbind(df_ctgan, df_synthpop)
df_comparison_ods <- filter(df_comparison,data == "observed") %>%
  select(-type)
df_comparison_ods <- unique(df_comparison_ods)
df_comparison_ods$type = "observed"

df_comparison_sds <- filter(df_comparison,data != "observed")
df_comparison <- rbind(df_comparison_ods, df_comparison_sds)

df_comparison$value <- factor(as.character(df_comparison$value), levels = str_sort(unique(df_comparison$value), numeric = TRUE))
df_comparison$type <- factor(df_comparison$type, levels = c("observed", "CTGAN", "Synthpop"))

table(df_comparison$type)

df_graph <- ggplot(df_comparison, aes(x = value, y = pct, fill = type, color = type, group = type)) +
  geom_bar(data = subset(df_comparison, data=="observed"), position = position_dodge(width = .9), stat = "identity", alpha = .2) +
  geom_line(data = subset(df_comparison, data!="observed")) +
  facet_grid( ~ variables, scales = "free", labeller = labeller(.rows = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

