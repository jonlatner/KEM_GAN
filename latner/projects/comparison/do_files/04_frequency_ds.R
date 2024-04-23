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

data <- c("sd2011_clean_small")
parents = c(1,2,3)
privacy = c(0)
copies = c(5)

df_comparison <- data.frame()
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
        
        df_temp <- compare(df_sds, df_ods) 
        df_compare <- data.frame(df_temp$tables) %>%
          rownames_to_column(var = "data") %>%
          pivot_longer(cols = starts_with(names(df_ods))) %>%
          rename(pct = value) %>%
          # separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE)
          separate(name, into = c("variables", "value"), sep = "\\.\\.|\\.", remove = FALSE, extra = "merge") %>%
          mutate(contains_double_dot = if_else(str_detect(name, fixed("..")), 1, 0),
                 value_new = as.numeric(value),
                 value_new = as.character(if_else(contains_double_dot == 1, -1*value_new, value_new)),
                 value_new = if_else(is.na(value_new), value, value_new),
                 value_new = if_else(value_new == "miss", NA, value_new),
          ) %>%
          select(-name,-contains_double_dot,-value) %>%
          rename(value=value_new) %>%
          mutate(value = ifelse(value=="miss.NA", yes = "NA", no = value))
        df_compare$parents = k
        df_compare$privacy = e
        df_compare$dataset = d
        
        df_comparison <- rbind(df_comparison,df_compare)
        
      }
    }
  }
}

# Graph (select variables) ----

df_graph_data <- df_comparison %>%
  filter(dataset == "sd2011_clean_small") %>%
  filter(variables %in% c("edu","income")) %>%
  mutate(parents = ifelse(data == "observed", yes = "observed", no = parents)) 

df_graph_data$value <- factor(as.character(df_graph_data$value), levels = str_sort(unique(df_graph_data$value), numeric = TRUE))
df_graph_data$parents <- factor(as.character(df_graph_data$parents), levels = str_sort(unique(df_graph_data$parents), numeric = TRUE))
df_graph_data$value <- fct_relevel(df_graph_data$value, "NA", after = Inf)

df_graph <- ggplot(df_graph_data, aes(x = value, y = pct, fill = parents)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested( ~ variables, scales = "free", labeller = labeller(.rows = label_both)) +
  xlab("") +
  ylab("Percent frequency (x 100)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        # text = element_text(size = 6),
        legend.margin = margin(t = -50),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_frequency_optimize_variables_parents.pdf"), height = 4, width = 10)


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
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_wkabdur.pdf"), height = 4, width = 10)

df_compare_1 <- rbind(sds,ods) %>%
  group_by(data) %>%
  mutate(total = sum(freq),
         pct = freq/total) %>%
  ungroup() %>%
  filter(as.numeric(as.character(value)) < 0) %>%
  mutate(type = "< 0")

df_compare_2 <- rbind(sds,ods) %>%
  group_by(data) %>%
  mutate(total = sum(freq),
         pct = freq/total) %>%
  ungroup() %>%
  filter(as.numeric(as.character(value)) >= 0) %>%
  mutate(type = ">= 0")

df_compare <- rbind(df_compare_1,df_compare_2)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_wrap(~type, scales = "free") +
  theme_bw() +
  scale_x_discrete(breaks = c(NA,"-8","-6","-1",
                              "0","10","20","30","40","50","60")) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_wkabdur_1.pdf"), height = 4, width = 10)

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

df_compare <- rbind(sds,ods) %>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data) %>%
  mutate(total = sum(freq),
         pct = freq/total) %>%
  ungroup() 

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data, color = data, group = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  theme_bw() +
  # scale_x_discrete(breaks = c(10,20,50,100,455)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

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
  scale_x_discrete(breaks = c("13","20","40","70",NA, "141", "351","446")) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
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
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_nofriend.pdf"), height = 4, width = 10)


df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~sdg + type,scales = "free") + 
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","31","40","50","74","99",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "none",
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_nofriend_1.pdf"), height = 2, width = 10)

