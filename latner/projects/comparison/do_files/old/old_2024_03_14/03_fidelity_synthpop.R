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
library(xtable)
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

#functions
options(scipen=999) 

# Load utility from synthpop data ----

copies <- c(5)
data <- c("sd2011","sd2011_clean","sd2011_clean_small","sd2011_clean_small_categorical")

df_comparison <- data.frame()
df_fidelity_plot <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))

    utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
    output <- data.frame(data = d,
                         copies = c,
                         pmse = mean(as.numeric(utility_measure$pMSE)),
                         specks = mean(as.numeric(utility_measure$SPECKS)))
    df_comparison <- rbind(df_comparison,output)
    
    utility <- utility.tables(sds_list, df_ods, tables = "twoway")
    utility_plot <- data.frame(utility$utility.plot$data)
    utility_plot$copies <- c
    utility_plot$data <- d
    df_fidelity_plot <- rbind(df_fidelity_plot,utility_plot)
  }
}

write.csv(df_comparison, paste0(tables,"synthpop_fidelity_optimize_dataset.csv"), row.names=FALSE)
write.csv(df_fidelity_plot, paste0(tables,"synthpop_fidelity_twoway_dataset.csv"), row.names=FALSE)

# Graph ----

df_plot <- read.csv(paste0(tables,"synthpop_fidelity_twoway_dataset.csv"))

data <- c("sd2011","sd2011_clean","sd2011_clean_small","sd2011_clean_small_categorical")
for (d in data) {
  df_plot_graph <- df_plot %>%
    filter(data == d) %>%
    filter(copies == 5) 
  
  df_graph <- ggplot(df_plot_graph, aes(x = X2, y = X1, fill = val)) + 
    geom_tile() +
    scale_fill_gradient(low = "gray95", high = "red") +
    xlab("") +
    ylab("") +
    theme_minimal() + 
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = 0.9, vjust = 0.2), 
          axis.text.y = element_text(size = 10, margin = margin(r = 0)),
          title = element_text(size = 11),
          legend.title = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  ggsave(plot = df_graph, paste0(graphs,"synthpop_fidelity_twoway_",d,".pdf"), height = 4, width = 6)
}

df_graph
