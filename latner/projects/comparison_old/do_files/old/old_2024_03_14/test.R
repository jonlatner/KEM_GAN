library(ggpubr)


df_regression_data_2 <- df_regression_data %>%
  select(term, estimate, std.error, type, model, dv, sdg) %>%
  mutate(type = ifelse(type == "synthetic", yes = sdg, no = type))
df_regression_data_2 <- unique(df_regression_data_2)
df_regression_data_2

table(df_regression_data_2$type)

df_regression_data_2$type <- factor(df_regression_data_2$type, 
                                    levels = c("synthpop", "datasynthesizer", "ctgan", "observed"))


df_graph <- ggplot(df_regression_data_2, aes(x = estimate, y = term, color = type)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error), height = 0, position = position_dodge(width = 0.9)) +
  labs(x = "Estimated Coefficients", y = "Independent Variables") +
  facet_wrap(~ dv) +
  scale_y_discrete(limits = rev(unique(df_regression_data$term))) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "solid", color = "red") +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width=unit(1, "cm"),
        axis.line.y = element_line(color="black", linewidth=.5),
        axis.line.x = element_line(color="black", linewidth=.5)
  )

df_graph

# Graph cio ----

df_regression_data_cio_2 <- df_regression_data_cio %>%
  filter(type == "little")%>%
  mutate(columns = "Dependent variable") %>%
  select(sdg,cio, dv) %>%
  pivot_wider(names_from = dv, values_from = cio) %>%
  select(sdg, "LN income", smoke)

df_text <- ggtexttable(df_regression_data_cio_2, rows = NULL)
# Arrange the plots on the same page
ggarrange(df_graph, df_text, 
          ncol = 1, nrow = 3,
          heights = c(1, 0.5, 0.3))

# ggsave(plot = df_graph, paste0(graphs,"graph_utility_regression.pdf"), height = 4, width = 8)



