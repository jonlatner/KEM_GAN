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

#functions
options(scipen=999) 

# Load utility from datasynthesizer data ----

parents = c(0,1,2)
privacy = c(0,.1,1)
data <- c("adult","grid","gridr","sd2011_small","sd2011")
data <- c("sd2011","sd2011_duration_wo_missing","sd2011_duration_w_missing")

data <- c("sd2011")
parents = c(1)
privacy = c(0)

# 1 copy
c=1
df_comparison_single <- data.frame()

for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in privacy) {
    for (k in parents) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        # sds_list$syn[[j]] <- sds  # use when m>1
        sds_list$syn <- sds # use when m==1
      }
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
      output <- data.frame(data = d,
                           copies = c,
                           privacy = as.character(e),
                           parents = as.character(k),
                           pmse = as.numeric(utility_measure$pMSE),
                           spmse = as.numeric(utility_measure$S_pMSE),
                           specks = as.numeric(utility_measure$SPECKS))
      df_comparison_single <- rbind(df_comparison_single,output)
    }
  }
}

test <- compare(sds_list, df_ods, utility.stats = "SPECKS")
test$tab.utility[1]

df_comparison_single

# multiple copies
c=5

df_comparison_multiple <- data.frame()
for (d in data) {
  df_ods <- read.csv(paste0(original_data,d,".csv"))
  sds_list <- readRDS(paste0(data_files,"synthetic/synds_",d,"_m_",c,".rds"))
  for (e in privacy) {
    for (k in parents) {
      for (j in 1:c) {
        sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
        sds[sds == ""] <- NA
        sds <- sds %>%
          mutate_if(is.character, as.factor)
        sds_list$syn[[j]] <- sds  # use when m>1
        # sds_list$syn <- sds # use when m==1
      }
      
      utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
      output <- data.frame(data = d,
                           copies = c,
                           privacy = as.character(e),
                           parents = as.character(k),
                           pmse = mean(as.numeric(utility_measure$pMSE)),
                           spmse = mean(as.numeric(utility_measure$S_pMSE)),
                           specks = mean(as.numeric(utility_measure$SPECKS)))
      df_comparison_multiple <- rbind(df_comparison_multiple,output)
    }
  }
}

# test <- compare(object = sds_list,data = df_ods,utility.stats = "SPECKS",utility.for.plot = "SPECKS")
# test_2 <- data.frame(test$tab.utility)
# test_2

df_comparison <- rbind(df_comparison_single,df_comparison_multiple)%>% 
  arrange(data,copies,privacy)

write.csv(df_comparison, paste0(tables,"utility_output.csv"), row.names=FALSE)

utility_measure <- utility.gen(sds_list$syn, df_ods, print.stats = "all", nperms = 3)
utility_measure

# Graph ----

df_comparison <- read.csv(paste0(tables,"utility_output.csv"))

df_comparison$copies <- factor(as.character(df_comparison$copies))
df_comparison$parents <- factor(as.character(df_comparison$parents))
df_comparison$privacy <- factor(as.character(df_comparison$privacy))

df_comparison <- df_comparison %>% 
  filter(copies == 5) %>%
  pivot_longer(!c(data,copies,privacy,parents), names_to = "utility", values_to = "values")

df_comparison

df_graph <- ggplot(df_comparison, aes(x = privacy, y = values, fill = parents)) +
  geom_bar(stat="identity",position = position_dodge2()) +
  facet_wrap( ~ utility, labeller = labeller(.rows = label_both)) +
  # ylab("Kolmogorov-Smirnov (lower is better)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        # legend.title = element_blank(), 
        legend.key.width=unit(1, "cm"),
        # axis.text.x = element_text(angle = 90, hjust = 1),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_optimize_utility.pdf"), height = 4, width = 6)
