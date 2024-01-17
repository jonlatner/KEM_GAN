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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"

#functions
options(scipen=999) 

# Load utility from synthpop data ----

copies <- c(1,5)
data <- c("adult","grid","gridr","sd2011_small","sd2011")
data <- c("sd2011_duration_wo_missing","sd2011_duration_w_missing")

df_comparison <- data.frame()

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))

    utility_measure <- utility.gen(sds_list, df_ods, print.stats = "all", nperms = 3)
    output <- data.frame(data = d,
                         copies = c,
                         pmse = as.numeric(mean(utility_measure$pMSE)),
                         spmse = as.numeric(mean(utility_measure$S_pMSE)),
                         specks = as.numeric(mean(utility_measure$SPECKS)))
    df_comparison <- rbind(df_comparison,output)
  }
}

write.csv(df_comparison, paste0(tables,"utility_output.csv"), row.names=FALSE)

# Graph ----

df_comparison <- read.csv(paste0(tables,"utility_output.csv"))

df_comparison$copies <- factor(as.character(df_comparison$copies))

df_graph <- ggplot(df_comparison, aes(x = copies, y = specks)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_grid( ~ data, labeller = labeller(.rows = label_both)) +
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

ggsave(plot = df_graph, paste0(graphs,"synthpop_optimize_utility.pdf"), height = 4, width = 6)
