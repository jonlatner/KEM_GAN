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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/ctgan/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

setwd(main_dir)

# Load original data ----

data <- c("sd2011_clean_small")
copies = c(1)
df_fidelity <- data.frame()

epochs = c(150)
batch_size = c(250)
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (e in epochs) {
      for (mb in batch_size) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,"_batch_size_",mb,"_epochs_",e,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
      output <- data.frame(data = d,
                           copies = c,
                           epochs = as.character(e),
                           batch = as.character(mb),
                           pmse = as.numeric(mean(utility_measure$pMSE)))
      df_fidelity <- rbind(df_fidelity,output)
      }
    }
  }
}

epochs = c(300)
batch_size = c(500)
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (e in epochs) {
      for (mb in batch_size) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,"_batch_size_",mb,"_epochs_",e,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
        
        utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
        output <- data.frame(data = d,
                             copies = c,
                             epochs = as.character(e),
                             batch = as.character(mb),
                             pmse = as.numeric(mean(utility_measure$pMSE)))
        df_fidelity <- rbind(df_fidelity,output)
      }
    }
  }
}

epochs = c(600)
batch_size = c(1000)
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (e in epochs) {
      for (mb in batch_size) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,"_batch_size_",mb,"_epochs_",e,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
        
        utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
        output <- data.frame(data = d,
                             copies = c,
                             epochs = as.character(e),
                             batch = as.character(mb),
                             pmse = as.numeric(mean(utility_measure$pMSE)))
        df_fidelity <- rbind(df_fidelity,output)
      }
    }
  }
}


write.csv(df_fidelity, paste0(tables,"ctgan_fidelity_batch_size.csv"), row.names=FALSE)

# Graph ----


df_fidelity <- read.csv(paste0(tables,"ctgan_fidelity_batch_size.csv"))
df_fidelity$epochs <- factor(as.character(df_fidelity$epochs), levels = str_sort(unique(df_fidelity$epochs), numeric = TRUE))
df_fidelity$batch <- factor(as.character(df_fidelity$batch), levels = str_sort(unique(df_fidelity$batch), numeric = TRUE))

df_fidelity_long <- df_fidelity %>%
  pivot_longer(!c(data,copies,epochs,batch), names_to = "utility", values_to = "values")

df_graph <- ggplot(df_fidelity_long, aes(x = batch, y = values)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  ylab("pMSE") +
  theme_bw() +
  ylim(0,1.25) +
  geom_text(aes(label = round(values,2)), vjust = -.5, size = 5) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text = element_text(size=14),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"ctgan_fidelity_optimize_batch_size.pdf"), height = 4, width = 10)
