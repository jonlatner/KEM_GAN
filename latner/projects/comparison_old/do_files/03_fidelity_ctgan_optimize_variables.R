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
library(ggh4x)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"
graphs = "graphs/ctgan/"

#functions
options(scipen=999) 

# Load utility from ctgan data ----

epochs = c(100, 300, 600, 900)
data <- c("sd2011","sd2011_clean","sd2011_clean_small","sd2011_clean_small_categorical")

# 1 copy
# multiple copies
c=5

df_comparison <- data.frame()

for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in epochs) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn[[j]] <- sds  # use when m>1
        # sds_list$syn <- sds # use when m==1
        
      }
      
      df_compare <- compare(sds_list,df_ods,utility.stats = "all")
      
      specks <- df_compare$tab.utility[,4]
      output <- data.frame(specks)
      output$variables <- rownames(output)
      rownames(output) <- NULL
      output$data = d
      output$copies = c
      output$epochs = e
      
      df_comparison <- rbind(df_comparison,output)

  }
}

write.csv(df_comparison, paste0(tables,"ctgan_fidelity_optimize_variables.csv"), row.names=FALSE)


# Graph ----

df_comparison <- read.csv(paste0(tables,"ctgan_fidelity_optimize_variables.csv"))
df_comparison$variables <- factor(as.character(df_comparison$variables))
df_comparison$epochs <- factor(as.character(df_comparison$epochs), levels = c("25", "50", "75", "100", "300", "600", "900"))


df_graph <- ggplot(df_comparison, aes(x = variables, y = specks, fill = epochs)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_wrap( ~ data, labeller = labeller(.rows = label_both), scales = "free_y", nrow = 4) +
  # ylab("Kolmogorov-Smirnov (lower is better)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        # legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"ctgan_fidelity_optimize_variables.pdf"), height = 4, width = 6)
