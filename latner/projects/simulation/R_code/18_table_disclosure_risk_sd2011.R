# ============================================================
# 18_table_disclosure_risk_SD2011.R
# Disclosure risk tables for SD2011: original vs. modified
#
# Purpose: Compare repU and DiSCO for standard CART synthesis
# versus a modified version where all synthetic depress values
# are set to zero, demonstrating false negative behaviour
#
# Authors: Jonathan Latner, Marcel Neunhoeffer, Jorg Drechsler
# Date: 2025-01-21
# ============================================================

rm(list = ls())

# ============================================================
# 1. Setup
# ============================================================

library(synthpop)
library(tidyverse)
library(xtable)

# base_path <- "/Users/marcelneunhoeffer/Dropbox/Apps/Overleaf/JOS-privacy-measures/"
base_path <- "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/simulation/"
graphs_path <- paste0(base_path, "graphs/")
tables_path <- paste0(base_path, "tables/")

options(scipen = 999)

# ============================================================
# 2. Load and Prepare Data
# ============================================================

cat("Loading SD2011 data...\n")
data(SD2011)
ods <- SD2011[, c("sex", "age", "region", "placesize", "depress")]
cat("SD2011 data loaded:", nrow(ods), "records\n\n")

# ============================================================
# 3. Original Synthesis: 5 Synthetic Datasets
# ============================================================

cat("========================================\n")
cat("ORIGINAL SYNTHESIS (m = 5)\n")
cat("========================================\n\n")

s5 <- syn(ods, seed = 8564, m = 5, print.flag = FALSE)
t5 <- disclosure(s5, ods,
  keys = c("sex", "age", "region", "placesize"),
  target = "depress",
  print.flag = FALSE
)

cat("Identity and attribute risk (original synthesis):\n")
print(print(t5, to.print = "allCAPs"))

repU <- t5$ident$repU
average_row <- mean(repU)
repU <- c(t5$ident$UiO[1], repU, average_row)

DiSCO <- t5$attrib$DiSCO
average_row <- mean(DiSCO)
DiSCO <- c(t5$attrib$Dorig[1], DiSCO, average_row)

df_risk_1 <- data.frame(
  data = c("Original data", "Synthetic 1", "Synthetic 2", "Synthetic 3",
           "Synthetic 4", "Synthetic 5", "Average"),
  identity = repU,
  attribute = DiSCO,
  stringsAsFactors = FALSE
)

cat("\nOriginal synthesis risk table:\n")
print(df_risk_1)

latex_table <- xtable(df_risk_1, align = "llrr")
print.xtable(latex_table,
  include.rownames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  file = paste0(tables_path, "table_disclosure_risk_sd2011_original.tex")
)
cat("LaTeX table saved to: table_disclosure_risk_sd2011_original.tex\n\n")

# ============================================================
# 4. Modified Synthesis: depress = 0 for All Records
# ============================================================

cat("========================================\n")
cat("MODIFIED SYNTHESIS (depress = 0)\n")
cat("========================================\n\n")

s6 <- s5
for (c in 1:5) {
  sds <- data.frame(s5$syn[c])
  sds$depress <- 0
  s6$syn[[c]] <- sds
}

t6 <- disclosure(s6, ods,
  keys = c("sex", "age", "region", "placesize"),
  target = "depress",
  print.flag = FALSE
)

cat("Identity and attribute risk (modified synthesis):\n")
print(print(t6, to.print = "allCAPs"))

repU <- t6$ident$repU
average_row <- mean(repU)
repU <- c(t6$ident$UiO[1], repU, average_row)

DiSCO <- t6$attrib$DiSCO
average_row <- mean(DiSCO)
DiSCO <- c(t6$attrib$Dorig[1], DiSCO, average_row)

df_risk_2 <- data.frame(
  data = c("Original data", "Synthetic 1", "Synthetic 2", "Synthetic 3",
           "Synthetic 4", "Synthetic 5", "Average"),
  identity = repU,
  attribute = DiSCO,
  stringsAsFactors = FALSE
)

cat("\nModified synthesis risk table:\n")
print(df_risk_2)

latex_table <- xtable(df_risk_2, align = "llrr")
print.xtable(latex_table,
  include.rownames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  sanitize.text.function = identity,
  file = paste0(tables_path, "table_disclosure_risk_sd2011_modified.tex")
)
cat("LaTeX table saved to: table_disclosure_risk_sd2011_modified.tex\n\n")

# ============================================================
# 5. Combined Table: Original vs. Modified (repU + DiSCO)
# ============================================================

cat("========================================\n")
cat("COMBINED TABLE: repU AND DiSCO\n")
cat("========================================\n\n")

df_risk_1$type <- "Original"
df_risk_2$type <- "Modified"

df_risk <- rbind(df_risk_1, df_risk_2) %>%
  pivot_longer(!c(data, type)) %>%
  pivot_wider(names_from = c(name, type), values_from = c(value)) %>%
  select(data, identity_Original, identity_Modified, attribute_Original, attribute_Modified)

cat("Combined risk table:\n")
print(df_risk)

columns_header_top <- c("
\\toprule & 
\\multicolumn{2}{l}{Identity risk ($repU$)} &
\\multicolumn{2}{l}{Attribute risk ($DiSCO$)}
\\\\  \n 
\\cmidrule(lr){2-3}
\\cmidrule(lr){4-5}
")

columns_header_mid <- c("
Data & Raab et al., 2024 & Modified & Raab et al., 2024 & Modified
\\\\ \n
\\midrule
")

notes <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{5}{p{5in}}{Note: Modified indicates that values of \\texttt{depress}=0 for all records in the synthetic data} \n")
notes_v2 <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{5}{p{4in}}{Note: Modified indicates that values of \\texttt{depress}=0 for all records in the synthetic data} \n")

latex_table <- xtable(df_risk, align = "llrrrr")

print.xtable(latex_table,
  include.rownames = FALSE,
  include.colnames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  hline.after = NULL,
  sanitize.text.function = identity,
  add.to.row = list(
    pos = list(0, 0, 1, 6, 7),
    command = c(columns_header_top, columns_header_mid,
                "\\midrule\n", "\\midrule\n", notes)
  ),
  file = paste0(tables_path, "table_disclosure_risk_sd2011.tex")
)

print.xtable(latex_table,
  include.rownames = FALSE,
  include.colnames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  hline.after = NULL,
  sanitize.text.function = identity,
  add.to.row = list(
    pos = list(0, 0, 1, 6, 7),
    command = c(columns_header_top, columns_header_mid,
                "\\midrule\n", "\\midrule\n", notes_v2)
  ),
  file = paste0(tables_path, "table_disclosure_risk_sd2011_v2.tex")
)
cat("LaTeX tables saved to: table_disclosure_risk_sd2011.tex and _v2.tex\n\n")

# ============================================================
# 6. DiSCO-Only Table
# ============================================================

cat("========================================\n")
cat("DISCO-ONLY TABLE\n")
cat("========================================\n\n")

df_risk_disco <- rbind(df_risk_1, df_risk_2) %>%
  pivot_longer(!c(data, type)) %>%
  pivot_wider(names_from = c(name, type), values_from = c(value)) %>%
  select(data, attribute_Original, attribute_Modified)

columns_header_top_disco <- c("
\\toprule & 
\\multicolumn{2}{l}{Attribute risk ($DiSCO$)}
\\\\  \n 
\\cmidrule(lr){2-3}
")

columns_header_mid_disco <- c("
Data & Raab et al., 2024 & Modified
\\\\ \n
\\midrule
")

notes_disco <- c("\\bottomrule \\\\[-1.8ex] \\multicolumn{3}{p{2in}}{Note: Modified indicates that values of \\texttt{depress}=0 for all records in the synthetic data} \n")

latex_table <- xtable(df_risk_disco, align = "llrr")

print.xtable(latex_table,
  include.rownames = FALSE,
  include.colnames = FALSE,
  floating = FALSE,
  booktabs = TRUE,
  hline.after = NULL,
  sanitize.text.function = identity,
  add.to.row = list(
    pos = list(0, 0, 1, 6, 7),
    command = c(columns_header_top_disco, columns_header_mid_disco,
                "\\midrule\n", "\\midrule\n", notes_disco)
  ),
  file = paste0(tables_path, "table_disclosure_risk_sd2011_disco.tex")
)
cat("LaTeX table saved to: table_disclosure_risk_sd2011_disco.tex\n")

# ============================================================
# END OF SCRIPT
# ============================================================
