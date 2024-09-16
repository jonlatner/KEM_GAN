
# Summary table (small) ----

df_data_structure_small <- df_data_structure

# Replace rows 29 to 32
df_data_structure_small <- df_data_structure_small[-c(30:32), ]
df_data_structure_small[29, seq(1, 4, 1)] <- NA       # Set the first and second columns to NA
df_data_structure_small[29, 5] <- "\\dots"           # Set the third column to "..."
df_data_structure_small[29, seq(6, 10, 1)] <- NA        # Set the last two columns to NA


# Replace rows 25 to 26
df_data_structure_small <- df_data_structure_small[-c(26:27), ]
df_data_structure_small[25, seq(1, 4, 1)] <- NA       # Set the first and second columns to NA
df_data_structure_small[25, 5] <- "\\dots"           # Set the third column to "..."
df_data_structure_small[25, seq(6, 10, 1)] <- NA        # Set the last two columns to NA


# Replace rows 16 to 21
df_data_structure_small <- df_data_structure_small[-c(17:21), ]
df_data_structure_small[16, seq(1, 4, 1)] <- NA       # Set the first and second columns to NA
df_data_structure_small[16, 5] <- "\\dots"           # Set the third column to "..."
df_data_structure_small[16, seq(6, 10, 1)] <- NA        # Set the last two columns to NA

# Replace rows 8 to 10
df_data_structure_small <- df_data_structure_small[-c(9, 13), ]
df_data_structure_small[8, seq(1, 4, 1)] <- NA       # Set the first and second columns to NA
df_data_structure_small[8, 5] <- "\\dots"           # Set the third column to "..."
df_data_structure_small[8, seq(6, 10, 1)] <- NA        # Set the last two columns to NA


# Print the data frame as a LaTeX table using xtable
latex_table <- xtable(df_data_structure_small)
print.xtable(latex_table, 
             include.rownames = FALSE, 
             sanitize.text.function = identity,
             floating = FALSE,
             booktabs = TRUE, 
             file = paste0(tables,"table_sd2011_data_structure_small.tex"))
