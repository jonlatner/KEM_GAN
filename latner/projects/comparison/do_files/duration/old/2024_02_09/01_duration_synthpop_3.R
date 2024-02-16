# load library ----
library(synthpop)
library(tidyverse)

# analysis ----

df_ods <- SD2011

df_ods <- df_ods %>%
  select(-eduspec, eduspec) %>%
  arrange(desc(eduspec))

df_ods$wkabdur <- as.numeric(df_ods$wkabdur)
# df_ods[df_ods < 0] <- NA
# df_ods[df_ods == ""] <- NA

time_start <- proc.time()
df_synds <- syn(df_ods, m = 1) 
time_end <- proc.time()
time_duration <- as.numeric(time_end[1] - time_start[1])
time_duration
