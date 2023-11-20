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
library(broom)
library(tidyverse)

# FOLDERS - ADAPT THIS PATHWAY
# main_dir = "N:/Ablagen/D01700-KEM/Latner/little_etal_2021/"
main_dir = "/Users/jonathanlatner/Google Drive/My Drive/IAB/drechsler_latner_2023/simulation_data/categorical/"

setwd(main_dir)

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/ctgan/"
graphs = "graphs/ctgan/"
tables = "tables/ctgan/"

#functions
options(scipen=999) 

roc_bivariate <- function(original, synthetic, var1, var2){
  # create frequency tables for the original and synthetic data, on the two variable cross-tabulation
  orig_table <- as.data.frame(ftable(original[,var1], original[,var2]))   
  syn_table <- as.data.frame(ftable(synthetic[,var1], synthetic[,var2]))
  # calculate the proportions by dividing by the number of records in each dataset
  orig_table$prop <- orig_table$Freq/nrow(original)                       
  syn_table$prop <- syn_table$Freq/nrow(synthetic)
  # merge the two tables, by the variables
  combined <- merge(orig_table, syn_table, by= c('Var1', 'Var2'), all=TRUE)
  # merging will induce NAs where there is a category mismatch - i.e. the category exists in one dataset but not the other
  # to deal with this set the NA values to zero:
  combined[is.na(combined)] <- 0
  # get the maximum proportion for each category level:
  combined$max<- pmax(combined$prop.x, combined$prop.y)
  # get the minimum proportion for each category level:
  combined$min <- pmin(combined$prop.x, combined$prop.y)
  # roc is min divided by max (a zero value for min results in a zero for ROC, as expected)
  combined$roc <- combined$min/combined$max 
  combined$roc[is.na(combined$roc)]<-1
  return(mean(combined$roc))
}

# Load original data ----

df_ods <- read.csv(paste0(original_data,"ods_0.csv"))
df_ods <- df_ods %>%
  mutate_if(is.character, as.factor)
col_names <- colnames(df_ods)

# Load synthetic data ----

df_bivar = data.frame()

epochs = c(300, 600, 900)
discriminator = c(1, 5, 10)
batch = c(500, 1000)
frequency = c("True", "False")
copies = c(1, 5, 10)

for (c in copies) {
  for (e in epochs) {
    for (d in discriminator) {
      for (b in batch) {
        df_sds <- data.frame()
        for (f in frequency) {
          for (j in 1:c) {
            sds <- read.csv(paste0(synthetic_data,"sds_ctgan_tuning_e_",e,"_d_",d,"_b_",b,"_f_",f,"_m_",c,"_n_",j,".csv"))
            df_sds <- rbind(df_sds,sds)
          }
          df_sds <- df_sds %>%
            mutate_if(is.character, as.factor)
          # Loop through each combination of two variables
          for (col1 in 1:(ncol(df_ods) - 1)) {
            for (col2 in (col1 + 1):ncol(df_ods)) {
              var1 <- col_names[col1]
              var2 <- col_names[col2]
              roc_bivar <- roc_bivariate(df_ods, df_sds, var1, var2)
              df <- data.frame(v1 = var1,
                               v2 = var2,
                               bivar = roc_bivar)
              df$epochs = e
              df$discriminator =  d
              df$frequency =  f
              df$batch =  b
              df$copies = c
              df_bivar <- rbind(df_bivar, df)
            }
          }
        }
      }
    }
  }
}

## Summary table 

df_bivar_2 <- df_bivar %>%
  group_by(v1, v2, epochs, discriminator, frequency, batch, copies) %>%
  summarise(bivar = mean(bivar)) %>%
  group_by(epochs, discriminator, frequency, batch, copies) %>%
  summarise(roc_bivar = mean(bivar)) %>%
  ungroup()

df_bivar_2

write.csv(df_bivar_2, paste0(tables,"utility_roc_bivar.csv"), row.names=FALSE)
