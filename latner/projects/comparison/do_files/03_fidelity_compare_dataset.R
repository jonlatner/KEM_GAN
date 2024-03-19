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
library(ggh4x)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 

# dataset ----

data <- c("sd2011_clean_small","sd2011_clean_small_categorical")
data <- c("sd2011_clean_small","sd2011_clean_small_numeric","sd2011_clean_small_categorical")
data <- c("sd2011_clean_small")
df_compare <- data.frame()
df_fidelity <- data.frame()
c=5 # multiple copies
parents = c(2)
privacy = c(0)
epochs = c(600)

# Load synthetic data from datasynthesizer ----


for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in privacy) {
    for (k in parents) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn[[j]] <- sds  # use when m>1
        # sds_list$syn <- sds # use when m==1
      }
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
      output <- data.frame(data = d,
                           sdg = "DataSynthesizer",
                           pmse = mean(as.numeric(utility_measure$pMSE)))
      df_compare <- rbind(df_compare,output)
      
      utility <- utility.tables(sds_list, df_ods, tables = "twoway")
      utility_plot <- data.frame(utility$utility.plot$data)
      utility_plot$data <- d
      utility_plot$sdg <- "DataSynthesizer"
      df_fidelity <- rbind(df_fidelity,utility_plot)
      
    }
  }
  
  
  
  # Load synthetic data from ctgan ----
  
  
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in epochs) {
    for (j in 1:c) {
      sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
      sds[sds == ""] <- NA
      sds <- sds %>%
        mutate_if(is.character, as.factor)
      sds_list$syn[[j]] <- sds  # use when m>1
      # sds_list$syn <- sds # use when m==1
    }
    
    utility <- utility.tables(sds_list, df_ods, tables = "twoway")
    utility_plot <- data.frame(utility$utility.plot$data)
    utility_plot$data <- d
    utility_plot$sdg <- "CTGAN"
    df_fidelity <- rbind(df_fidelity,utility_plot)
    
    utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
    output <- data.frame(data = d,
                         sdg = "CTGAN",
                         pmse = as.numeric(mean(utility_measure$pMSE)))
    df_compare <- rbind(df_compare,output)
  }
  
  
  
  # Load synthetic data from synthpop ----
  
  
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  
  utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
  output <- data.frame(data = d,
                       sdg = "Synthpop",
                       pmse = mean(as.numeric(utility_measure$pMSE)))
  df_compare <- rbind(df_compare,output)
  
  utility <- utility.tables(sds_list, df_ods, tables = "twoway")
  utility_plot <- data.frame(utility$utility.plot$data)
  utility_plot$data <- d
  utility_plot$sdg <- "Synthpop"
  df_fidelity <- rbind(df_fidelity,utility_plot)
}

# Graph ----

df_compare_graph <- df_compare %>%
  filter(data == "sd2011_clean_small") %>%
  pivot_longer(!c(data,sdg), names_to = "utility", values_to = "values")

# df_compare_graph$data <- factor(df_compare_graph$data, 
#                                 levels = c("sd2011_clean_small", "sd2011_clean_small_numeric", "sd2011_clean_small_categorical"),
#                                 labels = c("SD2011","SD2011 (numeric)","SD2011 (categorical)"))

df_graph <- ggplot(df_compare_graph, aes(x = sdg, y = values)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  theme_bw() +
  xlab("") +
  ylab("sPMSE") +
  ylim(0,1.25)+
  geom_text(aes(label = round(values,2)), vjust = -.5, position =  position_dodge2(.9)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_fidelity_compare_dataset.pdf"), height = 4, width = 8)

# Graph (two-way) ----

df_fidelity_graph <- df_fidelity %>%
  # filter(sdg!="CTGAN") %>%
  filter(data == "sd2011_clean_small")
  
# df_fidelity_graph$data <- factor(df_fidelity_graph$data, 
#                                 levels = c("sd2011_clean_small", "sd2011_clean_small_numeric"),
#                                 labels = c("SD2011","SD2011 (numeric)"))

df_graph <- ggplot(df_fidelity_graph, aes(x = X2, y = X1, fill = val)) + 
  geom_tile() +
  facet_wrap( ~ sdg,nrow = 1) +
  scale_fill_gradient(low = "gray95", high = "red") +
  xlab("") +
  ylab("") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust = 0.2), 
        axis.text.y = element_text(margin = margin(r = 0)),
        title = element_text(size = 11),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_fidelity_twoway_compare.pdf"), height = 4, width = 10)
