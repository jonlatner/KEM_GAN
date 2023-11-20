# Top commands ----
# https://alfurka.github.io/2023-01-30-creating-synthetic-values-with-synthepop-cart/
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
library(ggplot2)
library(ggh4x) # facet_nested

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "N:/Ablagen/D01700-KEM/Latner/simulation/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulated_data/categorical/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

setwd(main_dir)

options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# synthetic data

parents = c(0, 1, 2)
privacy = c(0, 0.1, 1, 5, 10, 20, 30)
copies = c(1, 5, 10)

df_combine_sds = data.frame()

for (k in parents) {
  for (e in privacy) {
    for (m in copies) {
      df_sds = data.frame()
      for (j in 1:m) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_tuning_e_",e,"_k_",k,"_m_",m,"_n_",j,".csv"))
        df_sds <- rbind(df_sds,sds)
      }
      df_sds$parents =  k
      df_sds$privacy = e
      df_sds$copies = m
      df_combine_sds <- rbind(df_combine_sds,df_sds)
    }
  }
}

#Compare frequencies of values in categorical variables ----

#reshape wide to long
group_vars_1 = c("parents", "privacy", "copies")
df_long <- df_combine_sds %>%
  pivot_longer(cols = -group_vars_1, names_to = "variables", values_to = "value") 
head(df_long)

#calculate frequency count of values in each variable by dataset
group_vars_2 = c(group_vars_1, "variables", "value")

df_count <- df_long %>%
  group_by(across(all_of(group_vars_2))) %>%
  tally() %>%
  ungroup() %>%
  mutate(n = ifelse(copies > 1, yes = n/copies, no = n)) %>%
  mutate(parents = as.numeric(parents),
         privacy = as.numeric(privacy),
         copies = as.numeric(copies),
         ) 
head(df_count)


df_long_ods_1 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = 1,
         parents = 0)

df_long_ods_2 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = 5,
         parents = 0)

df_long_ods_3 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = 10,
         parents = 0)

#graph - compare frequency

# Reorder the levels based on a summary statistic (e.g., mean)

df_count$copies <- factor(df_count$copies, levels = c(1, 5, 10))
df_long_ods_1$copies <- factor(df_long_ods_1$copies, levels = c(1, 5, 10))
df_long_ods_2$copies <- factor(df_long_ods_2$copies, levels = c(1, 5, 10))
df_long_ods_3$copies <- factor(df_long_ods_3$copies, levels = c(1, 5, 10))

df_count$parents <- factor(df_count$parents, levels = c(0, 1, 2))
df_long_ods_1$parents <- factor(df_long_ods_1$parents, levels = c(0, 1, 2))
df_long_ods_2$parents <- factor(df_long_ods_2$parents, levels = c(0, 1, 2))
df_long_ods_3$parents <- factor(df_long_ods_3$parents, levels = c(0, 1, 2))

df_count$privacy <- factor(df_count$privacy, levels = c(0, 0.1, 1, 5, 10, 20, 30))

df_graph <- ggplot(df_count, aes(x = value, y = n, color = privacy)) +
  geom_point(position = position_dodge(width = 0.75), stat = "identity") +
  facet_nested(variables ~ copies + parents, scales = "free", labeller = labeller(.cols = label_both)) +
  geom_bar(data = df_long_ods_1, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  geom_bar(data = df_long_ods_2, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  geom_bar(data = df_long_ods_3, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_color_grey(start = .3, end = .7) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_datasynthesizer_frequency.pdf"), height = 8, width = 10)
