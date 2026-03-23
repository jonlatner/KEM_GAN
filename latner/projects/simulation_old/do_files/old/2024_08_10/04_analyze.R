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

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthpop_data = "data_files/synthetic/synthpop/"
datasynthesizer_data = "data_files/synthetic/datasynthesizer/"

setwd(main_dir)

#functions
options(scipen=999) 

# load original data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

copies <- c(1)
data <- c("simulated")

# Load synthetic data from synthpop ----

for (c in copies) {
  for (d in data) {
    for (j in 1:c) {
      sds <- read.csv(paste0(synthpop_data,"sds_synthpop_",d,"_m_",c,"_n_",j,".csv"))
    }
  }
}

df_sds_synthpop <- sds

# Load synthetic data from datasynthesizer ----

sds_datasynthesizer <- c()
privacy = c(0,0.1,0.25,0.5,0.75,1,10,25,50)
for (d in data) {
  for (e in privacy) {
    for (j in 1:c) {
      sds <- read.csv(paste0(datasynthesizer_data,"sds_datasynthesizer_",d,"_e_",e,"_m_",c,"_n_",j,".csv"))
    }
    sds_datasynthesizer <- c(sds_datasynthesizer,paste0("df_sds_datasynthesizer_",e))
    assign(paste0("df_sds_datasynthesizer_",e),sds)
  }
}

# Frequency ----

# original
df_frequency <- as.data.frame(table(df_ods))
colnames(df_frequency)[colnames(df_frequency) == "Freq"] <- "original"

# add synthetic data
sds = c("df_sds_synthpop",sds_datasynthesizer)
for (s in sds) {
  df_sds <- get(s)
  frequency_sds <- as.data.frame(table(df_sds))
  df_frequency$test <- frequency_sds$Freq
  colnames(df_frequency)[colnames(df_frequency) == "test"] <- paste0(s)
}

df_frequency$combine <- paste(df_frequency$var1, df_frequency$var2, df_frequency$var3, df_frequency$var4, sep = "")

df_frequency <- df_frequency %>%
  select(-matches("var")) %>%
  pivot_longer(!combine)

df_frequency$name <- sub("^df_sds_", "", df_frequency$name)

df_frequency <- df_frequency %>%
  separate(name, into = c("synthesizer", "privacy"), sep = "_")

df_frequency

# Percent ----

# original
df_pct <- as.data.frame(table(df_ods))
df_pct$original <- (df_pct$Freq / nrow(df_ods)) * 100
df_pct$Freq <- NULL

# add synthetic data
sds = c("df_sds_synthpop",sds_datasynthesizer)
for (s in sds) {
  df_sds <- get(s)
  frequency_sds <- as.data.frame(table(df_sds))
  frequency_sds$Percent <- (frequency_sds$Freq / nrow(df_sds)) * 100
  df_pct$test <- frequency_sds$Percent
  colnames(df_pct)[colnames(df_pct) == "test"] <- paste0(s)
}

# combine and prepare for graph

df_pct$combine <- paste(df_pct$var1, df_pct$var2, df_pct$var3, df_pct$var4, sep = "")

df_pct <- df_pct %>%
  select(-matches("var")) %>%
  pivot_longer(!combine)

df_pct$name <- sub("^df_sds_", "", df_pct$name)

df_pct <- df_pct %>%
  separate(name, into = c("synthesizer", "privacy"), sep = "_")

df_pct


# Graph percent ----

df_compare <- data.frame()
privacy = c(0,0.1,0.25,0.5,0.75,1,10,25,50)
for (e in privacy) {
  df_test <- df_pct %>%
    filter(synthesizer == "original" | synthesizer == "synthpop" | (synthesizer == "datasynthesizer" & privacy == e)) %>%
    mutate(type = paste0("DP = ",e))
  df_compare <- rbind(df_compare,df_test)
}
    
df_compare <- df_compare %>%
  mutate(label2 = ifelse(combine == 1111, yes = round(value,1), no = NA))

ggplot(df_compare, aes(x = combine, y = value, fill = synthesizer)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~type) +
  geom_text(aes(label = label2), hjust = -0.5, size = 3, position = position_dodge(width=.9), angle = 90) +
  ylab("Frequency") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = .5),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

# Graph frequency ----

df_compare <- data.frame()
privacy = c(0,0.1,0.25,0.5,0.75,1,10,25,50)
for (e in privacy) {
  df_test <- df_frequency %>%
    filter(synthesizer == "original" | synthesizer == "synthpop" | (synthesizer == "datasynthesizer" & privacy == e)) %>%
    mutate(type = paste0("DP = ",e))
  df_compare <- rbind(df_compare,df_test)
}

df_compare

df_compare <- df_compare %>%
  mutate(label2 = ifelse(combine == 1111, yes = round(value,1), no = NA))

ggplot(df_compare, aes(x = combine, y = value, fill = synthesizer)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~type) +
  geom_text(aes(label = label2), hjust = -0.5, size = 3, position = position_dodge(width=.9), angle = 90) +
  ylab("Frequency") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = .5),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )
