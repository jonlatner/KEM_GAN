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
library(readr)
library(xtable)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/synthpop/"
graphs = "graphs/"
tables = "tables/"

setwd(main_dir)

#functions
options(scipen=999) 


# Set seed for reproducibility
my.seed = 1237
set.seed(my.seed)

# Load data ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))

# Loop ----

df_frequency <- data.frame()
for (c in 1:10) {

    # Create fake synthetic data
    sds <- syn(df_ods, m = 1, seed = my.seed)
    sds <- sds$syn

    # create seed
    my.seed = my.seed + 1
    
    # Create a frequency table for synthetic data
    
    sds$combine <- paste(sds$var1, sds$var2, sds$var3, sds$var4, sep = "")
    sds <- sds %>%
      select(-matches("var"))
    df_sds_frequency <- as.data.frame(table(sds))
    df_sds_frequency$type <- "synthetic"
    df_sds_frequency$n <- c

    # Combine
    df_frequency <- rbind(df_frequency,df_sds_frequency)
}

df_frequency

# Save data ----

write.csv(df_frequency, paste0(synthetic_data,"synthetic_cart_10.csv"), row.names = FALSE)

# Compare histogram ----

df_ods <- read.csv(paste0(original_data,"simulated.csv"))
df_frequency <- read_csv(paste0(synthetic_data,"synthetic_cart_10.csv"))

df_graph_sds <- df_frequency 

df_ods_frequency <- df_ods
df_ods_frequency$combine <- paste(df_ods_frequency$var1, df_ods_frequency$var2, df_ods_frequency$var3, df_ods_frequency$var4, sep = "")
df_ods_frequency <- df_ods_frequency %>%
  select(-matches("var"))
df_ods_frequency <- as.data.frame(table(df_ods_frequency))
df_ods_frequency$pct <- (df_ods_frequency$Freq / nrow(df_ods)) * 100
df_ods_frequency$type <- "original"

df_graph_ods <- df_ods_frequency

df_graph <- 
  ggplot() +
  geom_bar(data = df_graph_ods, aes(x = combine, y = Freq, fill = type), stat = "identity") +
  geom_boxplot(data = df_graph_sds, aes(x = combine, y = Freq, fill = type), alpha = .2) +
  scale_y_continuous(limits = c(0,100), breaks = seq(0,100,10)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(df_graph, filename = paste0(graphs,"graph_cart_histogram_compare_10.pdf"), height = 4, width = 10, units = "in")

ggsave(df_graph, filename = paste0(graphs,"graph_cart_histogram_compare_10_v2.pdf"), height = 6, width = 6, units = "in")

# Create table ----

df_frequency <- df_graph_ods
df_frequency$pct <- NULL
df_frequency$n <- 0

df_frequency <- rbind(df_frequency,df_graph_sds)

df_frequency$type <- NULL
df_frequency <- df_frequency %>%
  pivot_wider(names_from = "n", values_from = "Freq") 

# Replace all NA values with 0
df_frequency <- df_frequency %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Create the xtable object
latex_table <- xtable(df_frequency,digits = 0)

add_to_row <- list(
  pos = list(0,0), # Add after \toprule
  command = c(" & \\multicolumn{1}{l}{Original} & \\multicolumn{10}{c}{Synthetic Data} \\\\ \\cmidrule(lr){3-12}\n",
              "Combine & 0 & 1 & 2 & 3 & 4 & 5 & 6 & 7 & 8 & 9 & 10 \\\\ \n")
)

print.xtable(latex_table, 
             include.rownames = FALSE, 
             include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_frequency.tex"),
             add.to.row = add_to_row)
