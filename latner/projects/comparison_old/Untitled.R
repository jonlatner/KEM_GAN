ods <- data.frame(with(df_ods,table(edu,useNA = "ifany")))
names(ods)[1:2] <- c("value", "freq")
ods$data <- "observed"

sds_ctgan <- data.frame(with(df_ctgan,table(edu,useNA = "ifany")))
names(sds_ctgan)[1:2] <- c("value", "freq")
sds_ctgan$data <- "ctgan"

sds_datasynthesizer <- data.frame(with(df_datasynthesizer,table(edu,useNA = "ifany")))
names(sds_datasynthesizer)[1:2] <- c("value", "freq")
sds_datasynthesizer$data <- "datasynthesizer"

sds_synthpop <- data.frame(with(df_synthpop,table(edu,useNA = "ifany")))
names(sds_synthpop)[1:2] <- c("value", "freq")
sds_synthpop$data <- "synthpop"

df_compare_ds <- rbind(sds_datasynthesizer,ods)%>%
  group_by(data) %>%
  mutate(total_1 = sum(freq)) %>%
  ungroup() %>%
  mutate(pct = freq/total_1) %>%
  mutate(sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data)
         )


df_compare_ctgan <- rbind(sds_ctgan,ods)%>%
  group_by(data) %>%
  mutate(total_1 = sum(freq)) %>%
  ungroup() %>%
  mutate(pct = freq/total_1) %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data)
         )

df_compare_synthpop <- rbind(sds_synthpop,ods)%>%
  group_by(data) %>%
  mutate(total_1 = sum(freq)) %>%
  ungroup() %>%
  mutate(pct = freq/total_1) %>%
  mutate(value = as.factor(value),
         sdg = "synthpop",
         data = ifelse(data!="observed",yes = "synthetic", no = data)
         )


df_compare <- rbind(df_compare_ds, df_compare_ctgan, df_compare_synthpop)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_wrap(~sdg,scales = "free") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",         
        # axis.title.x=element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 45, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
