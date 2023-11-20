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

# Load synthetic data from CTGAN (based on optimized parameterization) ----

data <- c("adult","grid","gridr","sd2011_small")
data <- c("adult")

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
      
      df_temp_2 <- compare(sds_list, df_ods) 
      df_compare <- data.frame(df_temp$plots$data)

      # Splitting the string
      split_string <- strsplit(as.character(df_compare$Variable), ": S_pMSE")
      df_compare$Variable <- sapply(split_string, `[`, 1)

      df_ctgan <- rbind(df_ctgan,df_compare)
    }
  }
}

df_ctgan$type <- "CTGAN"

# Load synthetic data from other data synthesizers ----

type <- c("datasynthesizer","synthpop")

copies = c(1)

df_synthesizer <- data.frame()

for (t in type) {
  for (c in copies) {
    for (d in data) {
      df_ods <- read.csv(paste0(original_data,d,".csv"))
      sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,t,"/sds_",t,"_",d,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      }
      
      df_temp <- compare(sds_list, df_ods) 
      df_compare <- data.frame(df_temp$plots$data)

      # Splitting the string
      split_string <- strsplit(as.character(df_compare$Variable), ": S_pMSE")
      df_compare$Variable <- sapply(split_string, `[`, 1)
      
      df_compare$type <- t
      df_synthesizer <- rbind(df_synthesizer,df_compare)
    }
  }
}

# Graph ----

df_comparison <- rbind(df_ctgan, df_synthesizer)
names(df_comparison) <- tolower(names(df_comparison))

df_comparison_ods <- filter(df_comparison,data == "observed") %>%
  select(-type)

df_comparison_ods <- unique(df_comparison_ods)
df_comparison_ods$type = "observed"

df_comparison_sds <- filter(df_comparison,data != "observed")
df_comparison <- rbind(df_comparison_ods, df_comparison_sds)

df_comparison$type <- relevel(as.factor(df_comparison$type), ref = "observed")

df_graph <- ggplot(df_comparison, aes(x = value, y = percent, shape = type, color = type, group = type)) +
  geom_point(data = subset(df_comparison, data!="observed"), position = position_dodge(width = .9), size = 2) +
  geom_bar(data = subset(df_comparison, data=="observed"), stat = "identity", alpha = .2) +
  facet_wrap( ~ variable, scales = "free") +
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

