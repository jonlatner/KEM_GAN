df_compare_1_ds <- rbind(sds_datasynthesizer,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<=30 | is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend<=30")

df_compare_2_ds <- rbind(sds_datasynthesizer,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>30) %>%
  mutate(value = as.factor(value),
         sdg = "datasynthesizer",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend>30")

df_compare_1_ctgan <- rbind(sds_ctgan,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value<=30 | is.na(value)) %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend<=30")

df_compare_2_ctgan <- rbind(sds_ctgan,ods)%>%
  mutate(value = round(as.numeric(as.character(value))),0) %>%
  group_by(data,value) %>%
  summarise(total_1 = sum(freq)) %>%
  group_by(data) %>%
  mutate(total_2 = sum(total_1),
         pct = total_1/total_2) %>%
  ungroup() %>%
  filter(value>30) %>%
  mutate(value = as.factor(value),
         sdg = "ctgan",
         data = ifelse(data!="observed",yes = "synthetic", no = data),
         type = "nofriend>30")

df_compare <- rbind(df_compare_1_ds, df_compare_2_ds,df_compare_1_ctgan,df_compare_2_ctgan)

df_graph <- ggplot(df_compare, aes(x = value, y = pct, fill = data)) +
  geom_bar(position = position_dodge(width = .9), stat = "identity") +
  facet_nested_wrap(~type+sdg, scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_x_discrete(breaks = c("0","10","20","30","31","40","50","74","99",NA)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.key.width=unit(1, "cm"),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph
