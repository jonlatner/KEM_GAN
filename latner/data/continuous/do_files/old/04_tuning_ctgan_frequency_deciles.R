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
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/simulation_data/simulation_cont/"

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
continuous_vars <- sapply(df_ods, function(x) is.numeric(x))
continuous_var_names <- names(df_ods[continuous_vars])

# synthetic data

df_combine_sds = data.frame()

epochs = c(300, 600, 900)
discriminator = c(1, 5, 10)
batch = c(500, 1000)
frequency = c("True", "False")
copies = c(1, 5, 10)

# epochs = c(900)
# discriminator = c(10)
# batch = c(1000)
# frequency = c("False")
# copies = c(5)

models = 0
count = 0
for (e in epochs) {
  for (d in discriminator) {
    for (b in batch) {
      for (f in frequency) {
        for (m in copies) {
          models = models + 1
          df_ods_binned <- df_ods
          df_sds = data.frame()
          for (j in 1:m) {
            count = count + 1
            sds <- read.csv(paste0(synthetic_data,"sds_ctgan_tuning_e_",e,"_d_",d,"_b_",b,"_f_",f,"_m_",m,"_n_",j,".csv"))
            df_sds <- rbind(df_sds,sds)
          }
          for (col_name in continuous_var_names) {
            bins <- seq(min(df_ods_binned[[col_name]]), max(df_ods_binned[[col_name]]), length.out = 21) # 21 points to get 20 bins
            df_ods_binned[[col_name]] <- cut(df_ods_binned[[col_name]], breaks = bins, include.lowest = TRUE)
            df_sds[[col_name]] <- cut(df_sds[[col_name]], breaks = bins, include.lowest = TRUE)
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

print(models)
print(count)

# 
# df_0 <- df_ods_binned
# df_0$epochs <- "Original"
# df_0$copies <- 1
# test <- data.frame()
# for (c in discriminator) {
#   df <- df_0
#   df$discriminator <- c
#   test <- rbind(test,df)
# }
# df_0 <- test


#Compare frequencies of values in continuous variables by deciles ----
group_vars_1 <- c("discriminator", "epochs", "frequency", "batch", "copies")
group_vars_2 = c(group_vars_1, "variables", "value")
group_vars_3 = c(group_vars_1, "variables")

head(df_combine_sds)

df_long <- df_combine_sds %>%
  pivot_longer(cols = -c(group_vars_1), names_to = "variables", values_to = "value") 
head(df_long)

#calculate frequency count of values in each variable by group
df_count <- df_long %>%
  group_by(across(all_of(group_vars_2))) %>%
  tally() %>%
  group_by(across(all_of(group_vars_3))) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total)
head(df_count)


#graph - compare frequency
#sort variables

# df_count$epochs <- factor(as.character(df_count$epochs), levels = c("Original", as.character(sort(unique(df_count$epochs)))))
# df_count$discriminator <- factor(as.character(df_count$discriminator), levels = c("Original", as.character(sort(unique(df_count$discriminator)))))
# df_count$copies <- factor(as.character(df_count$copies), levels = c("Original", as.character(sort(unique(df_count$copies)))))
 
# df_long_ods_1$copies <- factor(as.character(df_long_ods_1$copies), levels = c("Original", as.character(sort(unique(df_long_ods_1$copies)))))
# df_long_ods_2$copies <- factor(as.character(df_long_ods_2$copies), levels = c("Original", as.character(sort(unique(df_long_ods_2$copies)))))
# df_long_ods_3$copies <- factor(as.character(df_long_ods_3$copies), levels = c("Original", as.character(sort(unique(df_long_ods_3$copies)))))

df_count$copies <- factor(df_count$copies, levels = c(1, 5, 10))
df_count$epochs <- factor(df_count$epochs, levels = c(300, 600, 900))
df_count$batch <- factor(df_count$batch, levels = c(500, 1000))
df_count$discriminator <- factor(df_count$discriminator, levels = c(1, 5, 10))

df_graph <- ggplot(df_count, aes(x = pct, y = value, shape = epochs, color = discriminator, size = batch)) +
  geom_point(position = position_dodge(width = 0.75), stat = "identity") +
  facet_grid(variables ~ copies + frequency, scales = "free", labeller = labeller(.cols = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  # geom_bar(data = df_long_ods_1, aes(x = value, y = pct, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  # geom_bar(data = df_long_ods_2, aes(x = value, y = pct, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  # geom_bar(data = df_long_ods_3, aes(x = value, y = pct, fill = "Original"), colour = NA, position = position_dodge(width = 0.75), stat = "identity", alpha=0.5) +
  # scale_fill_brewer(palette = "Dark") +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"graph_ctgan_continuous_deciles.pdf"), height = 8, width = 10)
