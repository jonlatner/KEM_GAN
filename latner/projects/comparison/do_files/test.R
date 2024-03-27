

# df_compare_2 <- rbind(df_compare_2_ds, df_compare_2_ctgan, df_compare_2_synthpop)
# 
# 
# df_graph <- ggplot(df_compare_2, aes(x = value, y = pct, fill = data)) +
#   geom_bar(position = position_dodge(width = .9), stat = "identity") +
#   facet_nested_wrap(~type+sdg,scales = "free") +
#   xlab("") +
#   ylab("") +
#   theme_bw() +
#   scale_x_discrete(breaks = c("0","10","20","30","40","60", NA)) +
#   theme(panel.grid.minor = element_blank(), 
#         legend.position = "none",         
#         axis.title.x=element_blank(),
#         legend.key.width=unit(1, "cm"),
#         # axis.text.x = element_text(angle = 90, vjust = .5),
#         axis.line.y = element_line(color="black", linewidth=.5),
#         axis.line.x = element_line(color="black", linewidth=.5)
#   )
# 
# df_graph
# 
# ggsave(plot = df_graph, paste0(graphs,"compare_wkabdur_2.pdf"), height = 2, width = 10)
# 
# df_compare_1 <- rbind(df_compare_1_ds, df_compare_1_ctgan, df_compare_1_synthpop)
# 
# df_graph <- ggplot(df_compare_1, aes(x = value, y = pct, fill = data)) +
#   geom_bar(position = position_dodge(width = .9), stat = "identity") +
#   facet_nested_wrap(~type+sdg) +
#   xlab("") +
#   ylab("") +
#   theme_bw() +
#   scale_x_discrete(breaks = c("0","10","20","30","40","60", NA)) +
#   theme(panel.grid.minor = element_blank(), 
#         legend.position = "bottom",         axis.title.x=element_blank(),
#         legend.key.width=unit(1, "cm"),
#         # axis.text.x = element_text(angle = 90, vjust = .5),
#         axis.line.y = element_line(color="black", linewidth=.5),
#         axis.line.x = element_line(color="black", linewidth=.5)
#   )
# 
# df_graph
# 
# ggsave(plot = df_graph, paste0(graphs,"compare_wkabdur_1.pdf"), height = 2, width = 10)
