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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

setwd(main_dir)

# Load original data ----

epochs = c(25, 50, 75, 100)
data <- c("adult","grid","gridr","sd2011_small","sd2011")
data <- c("sd2011_duration_wo_missing","sd2011_duration_w_missing")

# 1 copy
copies = c(1)
df_comparison_single <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (e in epochs) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      }
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
      output <- data.frame(data = d,
                           copies = c,
                           epochs = as.character(e),
                           pmse = as.numeric(utility_measure$pMSE),
                           spmse = as.numeric(utility_measure$S_pMSE),
                           specks = as.numeric(utility_measure$SPECKS))
      df_comparison_single <- rbind(df_comparison_single,output)
    }
  }
}

# multiple copies
copies = c(5)
# epochs = c(50) # the idea is that you already optimized epochs
df_comparison_multiple <- data.frame()
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (e in epochs) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn[[j]] <- sds  # use when m>1
        # sds_list$syn <- sds # use when m==1
      }
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
      output <- data.frame(data = d,
                           copies = c,
                           epochs = as.character(e),
                           pmse = as.numeric(mean(utility_measure$pMSE)),
                           spmse = as.numeric(mean(utility_measure$S_pMSE)),
                           specks = as.numeric(mean(utility_measure$SPECKS)))
      df_comparison_multiple <- rbind(df_comparison_multiple,output)
    }
  }
}

df_comparison <- rbind(df_comparison_single,df_comparison_multiple)%>% 
  arrange(data,copies,epochs)

write.csv(df_comparison, paste0(tables,"utility_output.csv"), row.names=FALSE)

utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
utility_measure

# Graph ----

df_comparison <- read.csv(paste0(tables,"utility_output.csv"))
df_comparison$epochs <- factor(as.character(df_comparison$epochs), levels = str_sort(unique(df_comparison$epochs), numeric = TRUE))

df_graph <- ggplot(df_comparison, aes(x = epochs, y = specks)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_grid(copies ~ data, labeller = labeller(.rows = label_both)) +
  ylab("Kolmogorov-Smirnov (lower is better)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"ctgan_optimize_utility.pdf"), height = 4, width = 6)

