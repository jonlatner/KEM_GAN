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
library(car)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/"

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


ods <- data.frame(with(df_ods,table(wkabdur, useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds <- data.frame(with(df_sds,table(wkabdur, useNA = "ifany")))
names(sds)[1:2] <- c("value", "freq")
sds$data <- "synthetic"
sds

df_compare_1 <- rbind(sds,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<=0 | is.na(value)) %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "wkabdur<=0")

df_compare_2 <- rbind(sds,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>0) %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "wkabdur>0")

df_compare <- rbind(df_compare_1, df_compare_2) %>%
  mutate(value = as.numeric(value))

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  theme_bw() +
  facet_wrap(~type,scales = "free") + 
  xlab("value") + 
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        text = element_text(size=14),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_wkabdur_1.pdf"), height = 4, width = 10)


# Graph (frequency - wkabdur) - sd2011_clean ----

data <- c("sd2011_clean")
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
ods <- data.frame(with(df_ods,table(wkabdur, useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds <- data.frame(with(df_sds,table(wkabdur, useNA = "ifany")))
names(sds)[1:2] <- c("value", "freq")
sds$data <- "synthetic"
sds

df_compare_1 <- rbind(sds,ods) %>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(is.na(value)) %>%
  mutate(type = "wkabdur==NA")

df_compare_2 <- rbind(sds,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>0) %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "wkabdur>=0")

df_compare <- rbind(df_compare_1, df_compare_2) 

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  theme_bw() +
  facet_wrap(~type,scales = "free") + 
  xlab("value") + 
  scale_x_discrete(breaks = c("NA","1","10","20","30","40","50","60")) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        text = element_text(size=14),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_wkabdur_clean.pdf"), height = 4, width = 10)

