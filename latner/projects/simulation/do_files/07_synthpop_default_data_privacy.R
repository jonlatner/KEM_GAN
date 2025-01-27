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

# Load data ----

ods <- SD2011[, c("sex", "age", "region","placesize","depress")]

# original ----
s5 <- syn(ods, seed = 8564, m = 5, print.flag = FALSE)
t5 <- disclosure( s5, ods, keys = c("sex", "age", "region", "placesize"), target = "depress", print.flag = FALSE)

repU <- t5$ident$repU
average_row <- mean(repU) # calculate average row across 10 synthetic data sets
repU <- c(0, repU, average_row)

DiSCO <- t5$attrib$DiSCO
average_row <- mean(DiSCO) # calculate average row across 10 synthetic data sets
DiSCO <- c(0, DiSCO, average_row)

# create table
df_risk_1 <- data.frame(
  data = c("Original", "Synthetic 1", "Synthetic 2", "Synthetic 3", "Synthetic 4", "Synthetic 5", "Average"),
  identity = c(repU),
  attribute = c(DiSCO)
)

# Create the xtable object
latex_table <- xtable(df_risk_1,align = "llrr")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_disclosure_risk_sd2011_original.tex"))



# modified ----

s6 <- s5
for (c in 1:5) {
  sds <- data.frame(s5$syn[c])
  sds$depress <- 0
  s6$syn[[c]] <- sds
}

t6 <- disclosure( s6, ods, keys = c("sex", "age", "region", "placesize"), target = "depress", print.flag = FALSE)

repU <- t6$ident$repU
average_row <- mean(repU) # calculate average row across 10 synthetic data sets
repU <- c(0, repU, average_row)

DiSCO <- t6$attrib$DiSCO
average_row <- mean(DiSCO) # calculate average row across 10 synthetic data sets
DiSCO <- c(0, DiSCO, average_row)

# create table
df_risk_2 <- data.frame(
  data = c("Original", "Synthetic 1", "Synthetic 2", "Synthetic 3", "Synthetic 4", "Synthetic 5", "Average"),
  identity = c(repU),
  attribute = c(DiSCO)
)

# Create the xtable object
latex_table <- xtable(df_risk_2,align = "llrr")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             # include.colnames = FALSE, 
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_disclosure_risk_sd2011_modified.tex"))

# combine ----

df_risk_1$type <- "Original"
df_risk_2$type <- "Modified"

df_risk <- rbind(df_risk_1,df_risk_2) %>%
  pivot_longer(!c(data,type)) %>%
  pivot_wider(names_from = c(name, type), values_from = c(value)) %>%
  select(data, identity_Original, identity_Modified, attribute_Original, attribute_Modified)
df_risk

columns_header_top <- c("
\\toprule & 
\\multicolumn{2}{l}{Identity risk} &
\\multicolumn{2}{l}{Attribute risk}
\\\\  \n 
\\cmidrule(lr){2-3}
\\cmidrule(lr){4-5}
")


columns_header_mid <- c("
Data & Original & Modified & Original & Modified
\\\\ \n
\\midrule
")

notes <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{5}{p{2.5in}}{Note: Modified indicates that values of \\texttt{depress}=0  in synthetic data} \n")


# Create the xtable object
latex_table <- xtable(df_risk,align = "llrrrr")

print.xtable(latex_table, 
             include.rownames = FALSE, 
             include.colnames = FALSE,
             floating = FALSE,
             booktabs = TRUE, 
             hline.after = NULL,
             add.to.row = list(
               pos = list(0,0,7),
               command = c(columns_header_top,
                           columns_header_mid,
                           notes)),
             file = paste0(tables,"table_disclosure_risk_sd2011.tex"))
