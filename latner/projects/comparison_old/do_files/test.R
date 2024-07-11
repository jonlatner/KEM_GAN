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
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

data <- c("sd2011_clean_small")

# Graph (nofriend) ----

df_ods <- read.csv(paste0(original_data,data,".csv"))


data <- c("sd2011_clean_small")
epochs = c(600)
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

df_sds <- data.frame()
for (c in copies) {
  for (d in data) {
    for (e in epochs) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"ctgan/sds_ctgan_data_",d,"_epochs_",e,"_m_",c,"_n_",j,".csv"))
        sds$bmi <- with(sds,weight/(height^2)*10000)
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        df_sds <- rbind(df_sds,sds)
      }
    }
  }
}

df_ctgan <- df_sds

df_sds <- data.frame()
for (c in copies) {
  for (d in data) {
    for (j in 1:c) {
      sds <- read.csv(paste0(synthetic_data,"synthpop/sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
      sds$bmi <- with(sds,weight/(height^2)*10000)
      sds[sds == ""] <- NA
      sds <- sds %>%
        mutate_if(is.character, as.factor)
      df_sds <- rbind(df_sds,sds)
    }
  }
}

df_synthpop <- df_sds

ods <- data.frame(with(df_ods,table(nofriend,useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds_ctgan <- data.frame(with(df_ctgan,table(nofriend,useNA = "ifany")))
names(sds_ctgan)[1:2] <- c("value", "freq")
sds_ctgan$data <- "ctgan"

sds_datasynthesizer <- data.frame(with(df_datasynthesizer,table(nofriend,useNA = "ifany")))
names(sds_datasynthesizer)[1:2] <- c("value", "freq")
sds_datasynthesizer$data <- "datasynthesizer"

sds_synthpop <- data.frame(with(df_synthpop,table(nofriend,useNA = "ifany")))
names(sds_synthpop)[1:2] <- c("value", "freq")
sds_synthpop$data <- "synthpop"

df_compare_1_ds <- rbind(sds_datasynthesizer,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<=30 | is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend<=30")

df_compare_2_ds <- rbind(sds_datasynthesizer,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>30) %>%
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend>30")

df_compare_1_ctgan <- rbind(sds_ctgan,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<=30 | is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend<=30")

df_compare_2_ctgan <- rbind(sds_ctgan,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>30) %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend>30")

df_compare_1_synthpop <- rbind(sds_synthpop,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<=30 | is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "synthpop",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend<=30")

df_compare_2_synthpop <- rbind(sds_synthpop,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>30) %>%
  mutate(value = as.factor(value),
         sdg = "synthpop",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend>30")

df_compare <- rbind(df_compare_1_synthpop, df_compare_2_synthpop, df_compare_1_ds, df_compare_2_ds,df_compare_1_ctgan,df_compare_2_ctgan)

df_compare <- rbind(df_compare_1_ds, df_compare_2_ds)
# df_compare_2 <- rbind(df_compare_1_synthpop, df_compare_2_synthpop, df_compare_1_ds, df_compare_2_ds)
# View(filter(df_compare_2,sdg=="datasynthesizer"))

df_graph <- ggplot(df_compare, aes(x = as.numeric(value), y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~type+sdg, scales = "free") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         
        # axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

