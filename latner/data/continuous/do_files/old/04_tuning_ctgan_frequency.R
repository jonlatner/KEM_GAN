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
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_cat/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

setwd(main_dir)

options(scipen=999) 

# Load data ----

# original data

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))

# synthetic data

epochs = c(300, 600, 900)
discriminator = c(1, 5, 10)
batch = c(500, 1000)
frequency = c("True", "False")
copies = c(1, 5, 10)

df_sds <- data.frame()
df_combine_sds = data.frame()

for (e in epochs) {
  for (d in discriminator) {
    for (b in batch) {
      for (f in frequency) {
        for (m in copies) {
          df_sds = data.frame()
          for (j in 1:m) {
            sds <- read.csv(paste0(synthetic_data,"sds_ctgan_tuning_e_",e,"_d_",d,"_b_",b,"_f_",f,"_m_",m,"_n_",j,".csv"))
            df_sds <- rbind(df_sds,sds)
          }
          df_sds$epochs =  e
          df_sds$discriminator = d
          df_sds$batch = b
          df_sds$frequency = f
          df_sds$copies = m
          df_combine_sds <- rbind(df_combine_sds,df_sds)
        }
      }
    }
  }
}

rm(b, batch, m, copies, d, discriminator, e, epochs, f, frequency, j, sds, df_sds)    

#Compare frequencies of values in categorical variables ----

#reshape wide to long
group_vars_1 = c("epochs", "discriminator", "batch", "frequency", "copies")
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
  mutate(epochs = as.numeric(epochs),
         discriminator = as.numeric(discriminator),
         batch = as.numeric(batch),
         frequency = as.factor(frequency),
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
  mutate(copies = "1",
         frequency = "False",
         epochs = "300",
         discriminator = "1",
         batch = "500")

df_long_ods_2 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = "1",
         frequency = "True",
         epochs = "300",
         discriminator = "1",
         batch = "500")

df_long_ods_3 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = "5",
         frequency = "False",
         epochs = "300",
         discriminator = "1",
         batch = "500")

df_long_ods_4 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = "5",
         frequency = "True",
         epochs = "300",
         discriminator = "1",
         batch = "500")

df_long_ods_5 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = "10",
         frequency = "False",
         epochs = "300",
         discriminator = "1",
         batch = "500")

df_long_ods_6 <- df_ods %>%
  mutate(index = row_number()) %>%
  pivot_longer(cols = -index, names_to = "variables", values_to = "value")%>%
  select(-index) %>%
  group_by(variables, value) %>%
  tally() %>%
  ungroup() %>%
  mutate(copies = "10",
         frequency = "True",
         epochs = "300",
         discriminator = "1",
         batch = "500")

#graph - compare frequency

# Reorder the levels based on a summary statistic (e.g., mean)

df_count$copies <- factor(df_count$copies, levels = c(1, 5, 10))
df_long_ods_1$copies <- factor(df_long_ods_1$copies, levels = c(1, 5, 10))
df_long_ods_2$copies <- factor(df_long_ods_2$copies, levels = c(1, 5, 10))
df_long_ods_3$copies <- factor(df_long_ods_3$copies, levels = c(1, 5, 10))
df_long_ods_4$copies <- factor(df_long_ods_4$copies, levels = c(1, 5, 10))
df_long_ods_5$copies <- factor(df_long_ods_5$copies, levels = c(1, 5, 10))
df_long_ods_6$copies <- factor(df_long_ods_6$copies, levels = c(1, 5, 10))

df_count$batch <- factor(df_count$batch, levels = c(500, 1000))
df_long_ods_1$batch <- factor(df_long_ods_1$batch, levels = c(500, 1000))
df_long_ods_2$batch <- factor(df_long_ods_2$batch, levels = c(500, 1000))
df_long_ods_3$batch <- factor(df_long_ods_3$batch, levels = c(500, 1000))
df_long_ods_4$batch <- factor(df_long_ods_4$batch, levels = c(500, 1000))
df_long_ods_5$batch <- factor(df_long_ods_5$batch, levels = c(500, 1000))
df_long_ods_6$batch <- factor(df_long_ods_6$batch, levels = c(500, 1000))


df_count$epochs <- factor(df_count$epochs, levels = c(300, 600, 900))
df_count$discriminator <- factor(df_count$discriminator, levels = c(1, 5, 10))

df_graph <- ggplot(df_count, aes(x = value, y = n, shape = epochs, color = discriminator)) +
  geom_point(position = position_dodge(width = 0.75), stat = "identity") +
  facet_nested(variables ~ copies + frequency + batch, scales = "free", labeller = labeller(.cols = label_both)) +
  geom_bar(data = df_long_ods_1, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  geom_bar(data = df_long_ods_2, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  geom_bar(data = df_long_ods_3, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  geom_bar(data = df_long_ods_4, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  geom_bar(data = df_long_ods_5, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  geom_bar(data = df_long_ods_6, aes(x = value, y = n, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(size=guide_legend(override.aes=list(fill=NA))) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_ctgan_frequency.pdf"), height = 8, width = 10)
