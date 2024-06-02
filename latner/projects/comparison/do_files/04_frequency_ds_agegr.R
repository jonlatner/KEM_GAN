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
library(synthpop)
library(ggh4x) # facet_nested
library(car)

# FOLDERS - ADAPT THIS PATHWAY
main_dir = "/Users/jonathanlatner/Documents/GitHub/KEM_GAN/latner/projects/comparison/"

data_files = "data_files/"
original_data = "data_files/original/"
synthetic_data = "data_files/synthetic/datasynthesizer/"
graphs = "graphs/datasynthesizer/"
tables = "tables/datasynthesizer/"

setwd(main_dir)

# Graph (frequency - wkabdur) ----

data <- c("sd2011")
parents = c(2)
privacy = c(0)
copies = c(5)

df_sds <- data.frame()
for (c in copies) {
  for (d in data) {
    df_ods <- read.csv(paste0(original_data,d,".csv")) # load original data
    for (e in privacy) {
      for (k in parents) {
        for (j in 1:c) {
          sds <- read.csv(paste0(synthetic_data,"sds_datasynthesizer_",d,"_k_",k,"_e_",e,"_m_",c,"_n_",j,".csv"))
          sds[sds == ""] <- NA
          sds <- sds %>%
            mutate_if(is.character, as.factor)
          df_sds <- rbind(df_sds,sds)
        }
      }
    }
  }
}

# Graph (frequency - agegroup) 

ods <- data.frame(with(df_ods,table(agegr,age, useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds <- data.frame(with(df_sds,table(agegr,age, useNA = "ifany")))
names(sds)[1:2] <- c("value", "freq")
sds$data <- "synthetic"

df_combine <- rbind(ods,sds) %>%
  filter(Freq>0) %>%
  rename(age=freq,
         agegr=value)


df_combine$agegr_2 <- recode(df_combine$age, 
                      "16:24='16-24';
                      25:34='25-34';
                      35:44='35-44';
                      45:59='45-59';
                      60:64='60-64';
                      65:hi='65+'")

with(subset(df_combine,data=="observed"),table(agegr,agegr_2,useNA = "ifany"))
with(subset(df_combine,data=="synthetic"),table(agegr,agegr_2,useNA = "ifany"))


df_graph <- ggplot(df_combine, aes(x = agegr, y = Freq, fill = agegr_2)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested( ~ data, scales = "free", labeller = labeller(.rows = label_both)) +
  # xlab("") +
  # ylab("Percent frequency (x 100)") +
  theme_bw() +
  geom_text(aes(label = Freq), position = position_dodge(width = .9)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        # text = element_text(size = 6),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph


df_combine_2 <- df_combine %>%
  group_by(data,agegr,agegr_2) %>%
  summarise(n = sum(Freq)) %>%
  group_by(data) %>%
  mutate(total = sum(n),
         pct = round(n/total*100,3)) %>%
  ungroup() %>%
  filter(agegr!=agegr_2)
df_combine_2 %>% summarise(sum=sum(pct))

df_graph <- ggplot(df_combine_2, aes(x = agegr, y = n, fill = agegr_2)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  ylab("Frequency") +
  xlab("Age group (synthetic group)") +
  theme_bw() +
  labs(fill = "Age group (synthetic values)") +
  geom_text(aes(label = n), position = position_dodge(width = .8), vjust=-0.5) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(2, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

ggsave(plot = df_graph, paste0(graphs,"datasynthesizer_frequency_agegr_errors.pdf"), height = 4, width = 10)
