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
graphs = "graphs/"
tables = "tables/"

#functions
options(scipen=999) 


# Load utility from datasynthesizer data ----

parents = c(2)
privacy = c(0)
data <- c("sd2011","sd2011_clean","sd2011_clean_small")
c=5 # multiple copies

df_comparison <- data.frame()
for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in privacy) {
    for (k in parents) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn[[j]] <- sds  # use when m>1
      }
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
      output <- data.frame(data = d,
                           copies = c,
                           privacy = as.character(e),
                           parents = as.character(k),
                           pmse = mean(as.numeric(utility_measure$pMSE)))
      df_comparison <- rbind(df_comparison,output)
      
    }
  }
}

df_comparison <- df_comparison %>% 
  arrange(data,copies,privacy)

write.csv(df_comparison, paste0(tables,"datasynthesizer_fidelity_optimize_dataset.csv"), row.names=FALSE)

# Graph (sd2011) ----

df_comparison <- read.csv(paste0(tables,"datasynthesizer_fidelity_optimize_dataset.csv"))

df_comparison_long <- df_comparison %>%
  filter(copies == "5" & parents == 2) %>%
  pivot_longer(!c(data,copies,privacy,parents), names_to = "utility", values_to = "values") %>%
  mutate(data = ifelse(data == "sd2011", yes = "sd2011(a)",
                       ifelse(data == "sd2011_clean", yes = "sd2011(b)",
                              ifelse(data == "sd2011_clean_small", yes = "sd2011(c)", no = data))))

df_graph <- ggplot(df_comparison_long, aes(x = data, y = values)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  theme_bw() +
  ylab("pMSE") +
  ylim(0,.3)+
  geom_text(aes(label = round(values,2)), vjust = -.5) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_fidelity_optimize_dataset_compare.pdf"), height = 4, width = 6)

