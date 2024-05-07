
roc_univariate <- function(original, synthetic, var_num) {
  # create frequency tables for the original and synthetic data, on the variable
  orig_table <- as.data.frame(ftable(original[,var_num]))
  syn_table <- as.data.frame(ftable(synthetic[,var_num]))
  # calculate the proportions by dividing by the number of records in each dataset
  orig_table$prop <- orig_table$Freq/nrow(original)
  syn_table$prop <- syn_table$Freq/nrow(synthetic)
  # merge the two tables, by the variable
  combined<- merge(orig_table, syn_table, by= c('Var1'), all = TRUE) 
  # merging will induce NAs where there is a category mismatch - i.e. the category exists in one dataset but not the other
  # to deal with this set the NA values to zero:
  combined[is.na(combined)] <- 0
  # get the maximum proportion for each category level:
  combined$max <- pmax(combined$prop.x, combined$prop.y)
  # get the minimum proportion for each category level:
  combined$min <- pmin(combined$prop.x, combined$prop.y)
  # roc is min divided by max (a zero value for min results in a zero for ROC, as expected)
  combined$roc <- combined$min/combined$max 
  combined$roc[is.na(combined$roc)] <- 1
  return(mean(combined$roc))
}


#bmi
roc_univariate(df_ods,df_ctgan,34)
roc_univariate(df_ods,df_synthpop,34)
roc_univariate(df_ods,df_datasynthesizer,34)


df_bmi_1 <- data.frame(Measure = "ROE",
                       ctgan = roc_univariate(df_ods,df_ctgan,34),
                       datasynthesizer = roc_univariate(df_ods,df_datasynthesizer,34))

df_bmi_2 <- data.frame(Measure = "ROE",
                       ctgan = roc_univariate(df_ods,df_ctgan,34),
                       datasynthesizer = roc_univariate(df_ods,df_datasynthesizer,34),
                       synthpop = roc_univariate(df_ods,df_synthpop,34))

# Print the data frame as a LaTeX table using xtable
latex_table <- xtable(df_bmi_1)
print.xtable(latex_table, 
             include.rownames = FALSE, 
             sanitize.text.function = identity,
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_compare_sd_ctgan_bmi.tex"))

# Print the data frame as a LaTeX table using xtable
latex_table <- xtable(df_bmi_2)
print.xtable(latex_table, 
             include.rownames = FALSE, 
             sanitize.text.function = identity,
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_compare_bmi.tex"))
