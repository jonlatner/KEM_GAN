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
library(xtable)
library(synthpop)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/data/benchmark/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthpop/"
graphs = "graphs/synthpop/"
tables = "tables/synthpop/"
graphs = "graphs/synthpop/"

#functions
options(scipen=999) 

# Load utility from synthpop data ----

copies <- c(1)
data <- c("sd2011_bmi_large")

for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv"))
    sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
    sds <- sds_list$syn # use when m==1
  }
}

df_utility <- utility.tables(sds_list, df_ods, tables = "twoway")
df_utility$utility.plot
ggsave(plot = last_plot(), paste0(graphs,"graph_synthpop_sd2011_bmi_two-way_utility.pdf"), height = 4, width = 8)

df_compare <- compare(sds_list,df_ods,utility.stats = "all")
df_compare$tab.utility[,4]

utility_measure <- utility.gen(sds_list, df_ods, print.stats = "all", nperms = 3)
utility_measure$SPECKS


f1 <- lm.synds(bmi ~ sex + age + edu + height + weight, data = sds_list)
compare(f1, df_ods)
compare(f1, df_ods, print.coef = TRUE, plot = "coef")

f2 <- lm.synds(log(income) ~ sex + age + edu, data = sds_list)
compare(f2, df_ods)
compare(f2, df_ods, print.coef = TRUE, plot = "coef")
ggsave(plot = last_plot(), paste0(graphs,"graph_synthpop_sd2011_bmi_cio.pdf"), height = 4, width = 8)

