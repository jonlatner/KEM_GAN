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
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"
graphs = "graphs/datasynthesizer/"

#functions
options(scipen=999) 

# Load utility from datasynthesizer data ----

parents = c(1)
privacy = c(0)
data <- c("sd2011_bmi_large")

# 1 copy
# multiple copies
c=1
for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in privacy) {
    for (k in parents) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds[sds < 0] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      }
    }
  }
}

with(sds,table(age,agegr))
df_utility <- utility.tables(sds_list, df_ods, tables = "twoway")
df_utility$utility.plot
ggsave(plot = last_plot(), paste0(graphs,"graph_datasynthesizer_sd2011_bmi_two-way_utility.pdf"), height = 4, width = 8)

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
ggsave(plot = last_plot(), paste0(graphs,"graph_datasynthesizer_sd2011_bmi_cio.pdf"), height = 4, width = 8)
