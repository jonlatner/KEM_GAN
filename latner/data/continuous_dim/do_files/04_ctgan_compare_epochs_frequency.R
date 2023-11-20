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
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Documents/GitHub/IAB/simulation_data/continuous_dim/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/"
tables = "tables/ctgan/"

setwd(main_dir)


# Create fake synthetic data ----

# rows = c(50000) # Rows/observations
# cols = c(10) # Columns/variables
# copies = c(5)
# for (r in rows) {
#   for (c in cols) {
#     for (m in copies) {
#       df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
#       df_synds <- syn(df_ods, m = m, seed = 1234)
#       saveRDS(df_synds, paste0(data_files,"synthetic/synds_",m,".rds"))
#     }
#   }
# }

# Load original data ----
# Dimensions
rows = c(50000) # Rows/observations
cols = c(20) # Columns/variables

for (r in rows) {
  for (c in cols) {
    df_ods <- read.csv(paste0(original_data,"ods_rows_",r,"_cols_",c,".csv"))
  }
}

# Load synthetic data ----

df_sds <- data.frame()
epochs = c(50)
batch = c(1000)

df_sds <- data.frame()

for (r in rows) {
  for (c in cols) {
    for (e in epochs) {
      for (b in batch) {
        sds <- read.csv(paste0(synthetic_data,"sds_ctgan_rows_",r,"_cols_",c,"_n_1_epochs_",e,"_batch_",b,".csv"))
        sds$epochs = e
        sds$batch = b
        df_sds <- rbind(df_sds, sds)
      }
    }
  }
}

# convert continuous variables to bins
continuous_vars <- sapply(df_sds, function(x) is.numeric(x))
continuous_var_names <- names(df_sds[continuous_vars])
continuous_var_names <- setdiff(continuous_var_names, c("epochs", "batch"))
df_sds_binned <- df_sds
for (col_name in continuous_var_names) {
  bins <- seq(min(df_sds_binned[[col_name]]), max(df_sds_binned[[col_name]]), length.out = 21) # 21 points to get 20 bins
  labels <- round(bins[-length(bins)],0)  # Remove the last element of breaks
  df_sds_binned[[col_name]] <- cut(df_sds_binned[[col_name]], breaks = bins, labels = labels)
}

head(df_sds_binned)

df_sds_long <- df_sds_binned %>%
  pivot_longer(cols = !c(epochs,batch), names_to = "variables", values_to = "value") %>%
  arrange(epochs, batch, variables, value) %>%
  group_by(epochs, batch, variables, value) %>%
  tally() %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/total,
         epochs = as.factor(epochs)) 

# reorder variable/value names
df_sds_long$value <- factor(df_sds_long$value, levels=sort(as.numeric(as.character(unique(df_sds_long$value)))))
df_sds_long$variables <- reorder(df_sds_long$variables, as.numeric(gsub("var_", "", as.character(df_sds_long$variables))))

# Graph
df_graph <- ggplot(df_sds_long, aes(x = value, y = pct, color = epochs, group = epochs)) +
  geom_line() +
  facet_grid(batch ~ variables, scales = "free", labeller = labeller(.cols = label_both)) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(colour = guide_legend(nrow = 1)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

# Combine data ----

df_combine <- df_ods
df_combine$epochs <- "Original"
df_combine$batch <- "Original"

df_combine <- rbind(df_combine,df_sds) %>%
  pivot_longer(cols = !c(epochs,batch), names_to = "variables", values_to = "value") 

df_combine$variables <- with(df_combine, reorder(variables, as.numeric(gsub("var_", "", as.character(variables)))))
df_combine$variables <- factor(df_combine$variables)

# Convert to character, then to factor with ordered levels
df_combine$epochs <- as.character(df_combine$epochs)
df_combine$epochs <- factor(df_combine$epochs, levels = c("Original", sort(setdiff(as.numeric(df_combine$epochs), NA), decreasing = FALSE)))

df_graph <- ggplot(df_combine, aes(x = value, color = epochs, group = epochs)) +
  geom_density(data = subset(df_combine, epochs!="Original"), linewidth=.5)  +
  geom_histogram(data = subset(df_combine, epochs=="Original"), aes(y=after_stat(density)), color = "blue", alpha=0.1, position="identity", linewidth = .25)+
  facet_wrap( ~ variables, scales = "free", labeller = labeller(.cols = label_both), ncol = 5) +
  xlab("") +
  ylab("") +
  theme_bw() +
  guides(colour = guide_legend(nrow = 1)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

