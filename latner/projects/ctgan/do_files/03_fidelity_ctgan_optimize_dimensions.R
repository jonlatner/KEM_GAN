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

embedding = c(16,64,128)

diminsions_1_1 = c("[128]")
diminsions_1_2 = c("[128, 128]")
diminsions_1_3 = c("[128, 128, 128]")

diminsions_2_1 = c("[256]")
diminsions_2_2 = c("[256, 256]")
diminsions_2_3 = c("[256, 256, 256]")

diminsions_3_1 = c("[512]")
diminsions_3_2 = c("[512, 512]")
diminsions_3_3 = c("[512, 512, 512]")

dimensions = c(diminsions_1_1,diminsions_1_2,diminsions_1_3,
              diminsions_2_1,diminsions_2_2,diminsions_2_3,
              diminsions_3_1,diminsions_3_2,diminsions_3_3
)

for (c in copies) {
  for (df in data) {
    df_ods <- read.csv(paste0(original_data,df,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",df,"_m_",c,".rds"))
    for (d in dimensions) {
      for (e in embedding) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_data_",df,"_embedding_",e,"_dimensions_",d,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
        
        utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 0)
        output <- data.frame(data = df,
                             copies = c,
                             embeddings = as.character(e),
                             dimensions = as.character(d),
                             pmse = as.numeric(mean(utility_measure$pMSE)))
        df_fidelity <- rbind(df_fidelity,output)
      }
    }
  }
}

write.csv(df_fidelity, paste0(tables,"ctgan_fidelity_dimensions.csv"), row.names=FALSE)

# Graph ----

df_fidelity <- read.csv(paste0(tables,"ctgan_fidelity_dimensions.csv"))
df_fidelity$embeddings <- factor(as.character(df_fidelity$embeddings), levels = str_sort(unique(df_fidelity$embeddings), numeric = TRUE))


df_fidelity_long <- df_fidelity %>%
  pivot_longer(!c(data,copies,dimensions,embeddings), names_to = "utility", values_to = "values")

df_fidelity_long$dimensions = factor(as.factor(df_fidelity_long$dimensions),
                                levels = c("[128]","[256]","[512]",
                                           "[128, 128]","[256, 256]","[512, 512]",
                                           "[128, 128, 128]","[256, 256, 256]","[512, 512, 512]")
)


df_graph <- ggplot(df_fidelity_long, aes(x = values, y = dimensions)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_wrap( ~ embeddings, labeller = label_both) +
  theme_bw() +
  scale_y_discrete(limits = rev(levels(df_fidelity_long$dimensions))) +
  xlim(0,1) +
  xlab("pMSE") +
  geom_text(aes(label = round(values,2)), hjust = -.5, size = 2.5) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"ctgan_fidelity_optimize_dimensions.pdf"), height = 4, width = 6)
