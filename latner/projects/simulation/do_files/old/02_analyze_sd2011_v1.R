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
library(synthpop)
library(tidyverse)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"

setwd(main_dir)

#functions
options(scipen=999) 

# Original ----

df_ods <- SD2011
df_ods[df_ods < 0] <- NA
df_ods[df_ods == ""] <- NA


df_ods <- df_ods %>%
  select(-agegr,-bmi) %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)

# Append synthetic data with m copies ----

c=5
d="sd2011"
df_sds <- data.frame()
for (j in 1:c) {
  sds <- read.csv(paste0(synthetic_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
  sds[sds == ""] <- NA
  sds <- sds %>%
    mutate_if(is.character, as.factor)
  df_sds <- rbind(df_sds,sds)
}

# Graph (nofriend) ----

df_synthpop <- df_sds

ods <- data.frame(with(df_ods,table(nofriend,useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds_synthpop <- data.frame(with(df_synthpop,table(nofriend,useNA = "ifany")))
names(sds_synthpop)[1:2] <- c("value", "freq")
sds_synthpop$data <- "synthpop"

df_compare_1 <- rbind(sds_synthpop,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<=30 | is.na(value)) %>%
  mutate(value = as.factor(value),
         type = "nofriend<=30")

df_compare_2 <- rbind(sds_synthpop,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>30) %>%
  mutate(value = as.factor(value),
         type = "nofriend>30")

df_compare <- rbind(df_compare_1, df_compare_2)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~type, scales = "free") +
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","31","40","50","60","70","80","90","100","110",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         
        # axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph