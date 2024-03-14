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
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

#functions
options(scipen=999) 

# Load utility from datasynthesizer data ----

data <- c("sd2011","sd2011_clean","sd2011_clean_small")
data <- c("sd2011")
c=1 # number of copies

df_fidelity_plot <- data.frame()
for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_default.csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1

      utility <- utility.tables(sds_list, df_ods, tables = "twoway",plot.stat = "pMSE")
      utility_plot <- data.frame(utility$utility.plot$data)
      utility_plot$data <- d
      df_fidelity_plot <- rbind(df_fidelity_plot,utility_plot)
}

# Presentation graph (two-way) ----

df_plot <- df_fidelity_plot

for (d in data) {
  df_plot_graph <- df_plot
  
  df_graph <- ggplot(df_plot_graph, aes(x = X2, y = X1, fill = val)) + 
    geom_tile() +
    scale_fill_gradient(low = "gray95", high = "red") +
    xlab("") +
    ylab("") +
    theme_minimal() + 
    theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 0.9, vjust = 0.2), 
          axis.text.y = element_text(size = 8, margin = margin(r = 0)),
          title = element_text(size = 8),
          legend.title = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  # ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_fidelity_twoway_",d,"_default_presentation.pdf"), height = 4, width = 6)
}

df_graph
