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
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",d,"_learning_rate.csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
      output <- data.frame(data = d,
                           copies = c,
                           pmse = as.numeric(mean(utility_measure$pMSE)),
                           specks = as.numeric(mean(utility_measure$SPECKS)))
      df_fidelity <- rbind(df_fidelity,output)
  }
}

write.csv(df_fidelity, paste0(tables,"ctgan_fidelity_learning_rate.csv"), row.names=FALSE)

# Graph ----

df_fidelity <- read.csv(paste0(tables,"ctgan_fidelity_learning_rate.csv"))

df_fidelity_long <- df_fidelity %>%
  pivot_longer(!c(data,copies), names_to = "utility", values_to = "values")

df_graph <- ggplot(df_fidelity_long, aes(x = utility, y = values)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  # ylab("Kolmogorov-Smirnov (lower is better)") +
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

ggsave(plot = df_graph, paste0(graphs,"ctgan_fidelity_optimize_learning_rate.pdf"), height = 4, width = 6)

