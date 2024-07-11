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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

#functions
options(scipen=999) 

# Load utility from synthpop data ----

copies <- c(5)
data <- c("sd2011","sd2011_clean","sd2011_clean_small")

df_comparison <- data.frame()
df_fidelity_plot <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    for (j in 1:c) {
      sds <- read.csv(paste0(synthetic_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
      sds[sds == ""] <- NA
      sds <- sds %>%
        mutate_if(is.character, as.factor)
      sds_list$syn[[j]] <- sds  # use when m>1
      # sds_list$syn <- sds # use when m==1
    }
    
    utility <- utility.tables(sds_list, df_ods, tables = "twoway",plot.stat = "pMSE")
    utility_plot <- data.frame(utility$utility.plot$data)
    utility_plot$copies <- c
    utility_plot$data <- d
    df_fidelity_plot <- rbind(df_fidelity_plot,utility_plot)
    
    utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
    output <- data.frame(data = d,
                         copies = c,
                         pmse = as.numeric(mean(utility_measure$pMSE)))
    df_comparison <- rbind(df_comparison,output)
  }
}

write.csv(df_comparison, paste0(tables,"synthpop_fidelity_optimize_dataset.csv"), row.names=FALSE)
write.csv(df_fidelity_plot, paste0(tables,"synthpop_fidelity_twoway_dataset.csv"), row.names=FALSE)


# Graph ----

df_comparison <- read.csv(paste0(tables,"synthpop_fidelity_optimize_dataset.csv"))

df_comparison <- df_comparison %>%
  filter(copies == 5) %>%
  filter(data == "sd2011_clean_small") %>%
  pivot_longer(!c(data,copies), names_to = "utility", values_to = "values")

df_graph <- ggplot(df_comparison, aes(x = data, y = values)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  ylab("pMSE") +
  theme_bw() +
  ylim(0,1.25) +
  geom_text(aes(label = round(values,2)), vjust = -.5) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"synthpop_fidelity_optimize_dataset.pdf"), height = 4, width = 6)

# Graph ----

df_plot <- read.csv(paste0(tables,"synthpop_fidelity_twoway_dataset.csv"))

data <- c("sd2011","sd2011_clean","sd2011_clean_small")
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
