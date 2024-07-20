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
library(xtable)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison_old/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)


# load data ----


data <- c("sd2011_clean_small")
parents = c(2)
privacy = c(0)
copies = c(5)

df_sds <- data.frame()
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv")) # load original data
    df_ods$bmi <- with(df_ods,weight/(height^2)*10000)
    for (e in privacy) {
      for (k in parents) {
        for (j in 1:c) {
          sds <- read.csv(paste0(synthetic_data,"datasynthesizer/sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
          sds$bmi <- with(sds,weight/(height^2)*10000)
          sds[sds == ""] <- NA
          sds <- sds %>%
            mutate_if(is.character, as.factor)
          df_sds <- rbind(df_sds,sds)
        }
      }
    }
  }
}

df_datasynthesizer <- df_sds

# Graph (bmi) ----


ods <- data.frame(with(df_ods,table(round(bmi,0),useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds_datasynthesizer <- data.frame(with(df_datasynthesizer,table(round(bmi,0),useNA = "ifany")))
names(sds_datasynthesizer)[1:2] <- c("value", "freq")
sds_datasynthesizer$data <- "datasynthesizer"

df_compare_1_ds <- rbind(sds_datasynthesizer,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data))

df_compare <- rbind(df_compare_1_ds)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  theme_bw() +
  scale_x_discrete(breaks = c("13","20","25","30","40","50","76",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         
        text = element_text(size=14),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
