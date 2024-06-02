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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

setwd(main_dir)

# Graph (frequency - wkabdur) ----

data <- c("sd2011")
parents = c(2)
privacy = c(0)
copies = c(5)

df_sds <- data.frame()
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv")) # load original data
    for (e in privacy) {
      for (k in parents) {
        for (j in 1:c) {
          sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
          sds[sds == ""] <- NA
          sds <- sds %>%
            mutate_if(is.character, as.factor)
          df_sds <- rbind(df_sds,sds)
        }
      }
    }
  }
}

# Graph (frequency - agegroup) 

ods <- data.frame(with(df_ods,table(agegr,age, useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds <- data.frame(with(df_sds,table(agegr,age, useNA = "ifany")))
names(sds)[1:2] <- c("value", "freq")
sds$data <- "synthetic"

sds %>%   filter(Freq>0) 

df_compare <- rbind(sds,ods) %>% 
  # filter(is.na(value), Freq>0) %>%
  group_by(data,value) %>%
  mutate(total = sum(Freq),
         pct = Freq/total) %>%
  ungroup() %>%
  filter(Freq>0) 
df_compare

df_graph <- ggplot(df_compare, aes(x = freq, y = Freq, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  theme_bw() +
  xlab("Age") +
  ylab("Number of missing values for agegr") +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_agegr.pdf"), height = 4, width = 10)
