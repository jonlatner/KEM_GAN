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
synthetic_data = "data_files/synthetic/"
graphs = "graphs/"

setwd(main_dir)


# this function rounds .5 up to the nearest whole number
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

# https://github.com/clairelittle/psd2022-comparing-utility-risk/blob/main/code/ROC_Ratio_of_Counts_Estimates.R
# INPUTS:
#     original:   the original dataset
#     synthetic:  the synthetic dataset
#     var_num:    index of the variable (e.g. for column 1, use 1)
# OUTPUTS:
#     results:    the ratio of estimates/counts for the particular variable


roc_univariate <- function(original, synthetic, var_num) {
  # create frequency tables for the original and synthetic data, on the variable
  orig_table <- as.data.frame(ftable(original[,var_num]))
  syn_table <- as.data.frame(ftable(synthetic[,var_num]))
  # calculate the proportions by dividing by the number of records in each dataset
  orig_table$prop <- orig_table$Freq/nrow(original)
  syn_table$prop <- syn_table$Freq/nrow(synthetic)
  # merge the two tables, by the variable
  combined<- merge(orig_table, syn_table, by= c('Var1'), all = TRUE) 
  # merging will induce NAs where there is a category mismatch - i.e. the category exists in one dataset but not the other
  # to deal with this set the NA values to zero:
  combined[is.na(combined)] <- 0
  # get the maximum proportion for each category level:
  combined$max <- pmax(combined$prop.x, combined$prop.y)
  # get the minimum proportion for each category level:
  combined$min <- pmin(combined$prop.x, combined$prop.y)
  # roc is min divided by max (a zero value for min results in a zero for ROC, as expected)
  combined$roc <- combined$min/combined$max 
  combined$roc[is.na(combined$roc)] <- 1
  return(mean(combined$roc))
}

# Load synthetic data ----

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

# ROE (nofriend) ---- 
ods <- data.frame(with(df_ods,table(nofriend,useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

var_num=21
original = df_ods
synthetic = df_ctgan
# create frequency tables for the original and synthetic data, on the variable
orig_table <- as.data.frame(ftable(original[,var_num]))
syn_table <- as.data.frame(ftable(synthetic[,var_num]))
# calculate the proportions by dividing by the number of records in each dataset
orig_table$prop <- orig_table$Freq/nrow(original)
syn_table$prop <- syn_table$Freq/nrow(synthetic)
# merge the two tables, by the variable
combined<- merge(orig_table, syn_table, by= c('Var1'), all = TRUE) 
# merging will induce NAs where there is a category mismatch - i.e. the category exists in one dataset but not the other
# to deal with this set the NA values to zero:
combined[is.na(combined)] <- 0
# get the maximum proportion for each category level:
combined$max <- pmax(combined$prop.x, combined$prop.y)
# get the minimum proportion for each category level:
combined$min <- pmin(combined$prop.x, combined$prop.y)
# roc is min divided by max (a zero value for min results in a zero for ROC, as expected)
combined$roc <- combined$min/combined$max 
combined$roc[is.na(combined$roc)] <- 1
mean(combined$roc)


sds_ctgan <- data.frame(with(df_ctgan,table(nofriend,useNA = "ifany")))
names(sds_ctgan)[1:2] <- c("value", "freq")
sds_ctgan$data <- "ctgan"

sds_datasynthesizer <- data.frame(with(df_datasynthesizer,table(nofriend,useNA = "ifany")))
names(sds_datasynthesizer)[1:2] <- c("value", "freq")
sds_datasynthesizer$data <- "datasynthesizer"

sds_synthpop <- data.frame(with(df_synthpop,table(nofriend,useNA = "ifany")))
names(sds_synthpop)[1:2] <- c("value", "freq")
sds_synthpop$data <- "synthpop"

df_compare_1 <- rbind(sds_ctgan,sds_datasynthesizer,ods)%>%
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

df_compare_2 <- rbind(sds_ctgan,sds_datasynthesizer,ods)%>%
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
  # facet_wrap(~type,scales = "free") +
  facet_nested_wrap(~type, scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","31","40","50","75","99",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"compare_ds_ctgan_nofriend.pdf"), height = 4, width = 10)

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

df_compare <- rbind(df_compare_1_ds, df_compare_2_ds,df_compare_1_ctgan,df_compare_2_ctgan)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~type+sdg, scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","31","40","50","74","99",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"compare_ds_ctgan_nofriend_1.pdf"), height = 4, width = 10)

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

df_compare <- rbind(df_compare_1_synthpop, df_compare_2_synthpop,df_compare_1_ds, df_compare_2_ds,df_compare_1_ctgan,df_compare_2_ctgan)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~type+sdg, scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","31","40","50","74","99",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"compare_nofriend_1.pdf"), height = 4, width = 10)

# Graph (bmi) ----


ods <- data.frame(with(df_ods,table(bmi,useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds_ctgan <- data.frame(with(df_ctgan,table(bmi,useNA = "ifany")))
names(sds_ctgan)[1:2] <- c("value", "freq")
sds_ctgan$data <- "ctgan"

sds_datasynthesizer <- data.frame(with(df_datasynthesizer,table(bmi,useNA = "ifany")))
names(sds_datasynthesizer)[1:2] <- c("value", "freq")
sds_datasynthesizer$data <- "datasynthesizer"

sds_synthpop <- data.frame(with(df_synthpop,table(bmi,useNA = "ifany")))
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
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data))

df_compare_1_ctgan <- rbind(sds_ctgan,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data))

df_compare_1_synthpop <- rbind(sds_synthpop,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  mutate(value = as.factor(value),
         sdg = "synthpop",
         data = ifelse(data!="observed",yes = "synthetic", no = data))

df_compare <- rbind(df_compare_1_ds, df_compare_1_ctgan)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~sdg) +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("13","20","25","30","40","50","75",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"compare_ds_ctgan_bmi_1.pdf"), height = 4, width = 10)


df_compare <- rbind(df_compare_1_ds, df_compare_1_ctgan, df_compare_1_synthpop)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~sdg) +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("13","20","25","30","40","50","75",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"compare_bmi_1.pdf"), height = 4, width = 10)


# Graph (wkabdur) ----


ods <- data.frame(with(df_ods,table(wkabdur,useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds_ctgan <- data.frame(with(df_ctgan,table(wkabdur,useNA = "ifany")))
names(sds_ctgan)[1:2] <- c("value", "freq")
sds_ctgan$data <- "ctgan"

sds_datasynthesizer <- data.frame(with(df_datasynthesizer,table(wkabdur,useNA = "ifany")))
names(sds_datasynthesizer)[1:2] <- c("value", "freq")
sds_datasynthesizer$data <- "datasynthesizer"

sds_synthpop <- data.frame(with(df_synthpop,table(wkabdur,useNA = "ifany")))
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
  filter(!is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nonmissing")

df_compare_2_ds <- rbind(sds_datasynthesizer,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "missing")

df_compare_1_ctgan <- rbind(sds_ctgan,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nonmissing")

df_compare_2_ctgan <- rbind(sds_ctgan,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "missing")

df_compare_1_synthpop <- rbind(sds_synthpop,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "synthpop",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nonmissing")

df_compare_2_synthpop <- rbind(sds_synthpop,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "synthpop",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "missing")


df_compare <- rbind(df_compare_1_ds, df_compare_1_ctgan, df_compare_2_ds, df_compare_2_ctgan)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~type+sdg,scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","40","50","60", NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"compare_ds_ctgan_wkabdur_1.pdf"), height = 4, width = 10)


df_compare <- rbind(df_compare_1_ds, df_compare_2_ds, df_compare_1_ctgan, df_compare_2_ctgan, df_compare_1_synthpop, df_compare_2_synthpop)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~type+sdg,scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","40","50","60", NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"compare_wkabdur_1.pdf"), height = 4, width = 10)

# ROE ----

df_nofriend_1 <- data.frame(Measure = "ROE",
                            ctgan = roc_univariate(df_ods,df_ctgan,21),
                            datasynthesizer = roc_univariate(df_ods,df_datasynthesizer,21))

df_nofriend_1

#nofriend
roc_univariate(df_ods,df_ctgan,21)
roc_univariate(df_ods,df_synthpop,21)
roc_univariate(df_ods,df_datasynthesizer,21)

#bmi
roc_univariate(df_ods,df_ctgan,34)
roc_univariate(df_ods,df_synthpop,34)
roc_univariate(df_ods,df_datasynthesizer,34)

#wkabdur
roc_univariate(df_ods,df_ctgan,27)
roc_univariate(df_ods,df_synthpop,27)
roc_univariate(df_ods,df_datasynthesizer,27)

