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

df_compare <- rbind(sds,ods) %>%
  group_by(data) %>%
  mutate(total = sum(freq),
         pct = freq/total) %>%
  ungroup()

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  theme_bw() +
  scale_x_discrete(breaks = c(NA,"-8","-4",
                              "0","10","20","30","40","50","60")) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        text = element_text(size=14),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_wkabdur.pdf"), height = 4, width = 10)

# Graph (frequency - agegroup)  ----

ods <- data.frame(with(df_ods,table(agegr,age, useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds <- data.frame(with(df_sds,table(agegr,age, useNA = "ifany")))
names(sds)[1:2] <- c("value", "freq")
sds$data <- "synthetic"

df_combine <- rbind(ods,sds) %>%
  filter(Freq>0) %>%
  rename(age=freq,
         agegr=value)

df_combine$agegr_2 <- recode(df_combine$age, 
                             "16:24='16-24';
                      25:34='25-34';
                      35:44='35-44';
                      45:59='45-59';
                      60:64='60-64';
                      65:hi='65+'")

df_combine_2 <- df_combine %>%
  group_by(data,agegr,agegr_2) %>%
  summarise(n = sum(Freq)) %>%
  group_by(data) %>%
  mutate(total = sum(n),
         pct = round(n/total*100,3)) %>%
  ungroup() %>%
  filter(agegr!=agegr_2)
df_combine_2 %>% summarise(sum=sum(pct))

df_graph <- ggplot(df_combine_2, aes(x = agegr, y = n, fill = agegr_2)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  ylab("Frequency") +
  xlab("Age group (synthetic group)") +
  theme_bw() +
  labs(fill = "Age group (synthetic values)") +
  scale_y_continuous(breaks = seq(0,600,100), limits = c(0,600)) +
  geom_text(aes(label = n), position = position_dodge(width = .8), vjust=-0.5) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_frequency_agegr_errors.pdf"), height = 4, width = 10)

# Graph (frequency - bmi) ----

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


ods <- data.frame(with(df_ods,table(bmi, useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds <- data.frame(with(df_sds,table(bmi, useNA = "ifany")))
names(sds)[1:2] <- c("value", "freq")
sds$data <- "synthetic"

df_compare_1 <- rbind(sds,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<100 | is.na(value)) %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "bmi<100")

df_compare_2 <- rbind(sds,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>=100) %>%
  mutate(value = as.character(value)) %>%
  mutate(type = "bmi>=100")

df_compare <- rbind(df_compare_1, df_compare_2)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_wrap(~type,scales = "free") + 
  scale_x_discrete(breaks = c("13","20","40","70",NA, "141", "351","450")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        text = element_text(size=14),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_bmi.pdf"), height = 4, width = 10)


# Graph (frequency - nofriend) ----

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
          sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
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



ods <- data.frame(with(df_ods,table(nofriend, useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"
ods

sds <- data.frame(with(df_sds,table(nofriend, useNA = "ifany")))
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
  filter(value<=30 | is.na(value)) %>%
  mutate(value = as.factor(value),
         type = "nofriend<=30",
         sdg = "DataSynthesizer")

df_compare_2 <- rbind(sds,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>30) %>%
  mutate(value = as.factor(value),
         type = "nofriend>30",
         sdg = "DataSynthesizer")

df_compare <- rbind(df_compare_1, df_compare_2)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_wrap(~type,scales = "free") + 
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","31","40","50","74","99",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        text = element_text(size=14),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_nofriend.pdf"), height = 4, width = 10)

# bmi (cleaned) ----

# main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison_old/"
# setwd(main_dir)


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
          sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
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

ods <- data.frame(with(df_ods,table(round(bmi,0),useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds_datasynthesizer <- data.frame(with(df_datasynthesizer,table(round(bmi,0), useNA = "ifany")))
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

df_graph <- ggplot(df_compare_1_ds, aes(x = value, y = pct, fill = data)) +
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

ggsave(plot = df_graph, paste0(graphs,"compare_ds_bmi.pdf"), height = 4, width = 10)
