model_res = S_rsv_1_288_res
model_run = models_list
names(model_run) = seq(1:length(model_run))

# After getting the results
# orders the results by decreasing AUC value
models = rownames(model_res)
new_mod = cbind(models, model_res)
ord_mod = new_mod[order(new_mod$AUC, decreasing = T), ]

# orders the models by decreasing AUC value
order_models = lapply(ord_mod$models, function(x) model_run[[x]])
names(order_models) = ord_mod$models

# extracts the features used in the final models
feat_data = sapply(order_models, function(x) colnames(x[[1]][[2]]))
# converts the vector to a dataframe, with features and feature frequency across all models
feat_freq = as.data.frame(sort(table(feat_data), decreasing = T))

# Groups features by their percentage cover across all models
get_feature_frequency <- function(freq_tbl, N) {
  across_100 = freq_tbl[freq_tbl$Freq == N, ]
  across_95 = freq_tbl[freq_tbl$Freq >= round(N*0.95) & freq_tbl$Freq < N, ]
  across_75 = freq_tbl[freq_tbl$Freq >= round(N*0.75) & freq_tbl$Freq <  round(N*0.95), ]
  across_50 = freq_tbl[freq_tbl$Freq >= round(N*0.50) & freq_tbl$Freq < round(N*0.75), ]
  across_25 = freq_tbl[freq_tbl$Freq >= round(N*0.25) & freq_tbl$Freq < round(N*0.50), ]
  across_0 = freq_tbl[freq_tbl$Freq < round(N*0.25), ]
  return(list("100%" = across_100, "95%-99%" = across_95, "75-94%" = across_75, "50-74%" = across_50, "25-75%" = across_25, "<25%" = across_0))
}
freq_out <- get_feature_frequency(feat_freq, nrow(model_res))

# returns the names of each group and the number of features in the group
group_freq <- data.frame(Object = names(freq_out), Count = sapply(freq_out, nrow))
group_freq$Object = factor(group_freq$Object, levels = unique(group_freq$Object))

# Plots each group and the number of features in each group
plot1 <- ggplot(group_freq, aes(x = Object, y = Count, fill = Object)) +  # Different colors for each bar
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3.5) +  # Add labels above bars
  labs(title = "Feature Coverage Across Models", x = "Percentage Coverage", y = "Number of Features", fill = "Percentage Coverage") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.border = element_rect(colour = "black", fill = NA),  # Adding axis borders
    axis.line = element_line(colour = "black"),  # Adding axis lines
    legend.position = "right",  # Adjust legend position
    legend.title = element_text(size = 10), # Remove legend title for simplicity
  ) +
  scale_fill_viridis_d(option = "D", direction = 1) + # Apply colorblind-friendly palette
  scale_y_continuous(breaks = seq(0, max(group_freq$Count) + 10, by = 10), expand = expansion(mult = c(0, 0.2)))  # Control y-axis breaks

#
#
# The below function will get feature importance for all models. Consider subsetting it to the top 50
#
#
set.seed(0)
options(future.globals.maxSize = 1e9)
# Gathers feature importance for each model
feat_imp = lapply(order_models, function(x) {
  feature_importance_main(
    feature_importance_method = "permutation",
    trained_model = x[["final_model"]][["trained_model"]],
    test_data = x[["final_model"]][["test_data"]],
    test_metadata = x[["final_model"]][["test_metadata"]],
    outcome_colname = "ParticipantType",
    perf_metric_function = get_perf_metric_fn("binary"),
    perf_metric_name = "AUC",
    class_probs = T,
    method = x[["final_model"]][["trained_model"]][["method"]],
    seed = 0,
    nperms = 100,
    threads = 1,
    performance_table = x[["final_model"]][["performance"]])
})

# Selects only features with a value of < 0.05 across all models
sig_models <- lapply(feat_imp, function(x) x[[1]][x[[1]][["pvalue"]] <= 0.05, ])
sig_features <- do.call(rbind, lapply(feat_imp, function(x) x[[1]][x[[1]][["pvalue"]] <= 0.05, ]))

# gets the names of the models with the most significant features, sorts by decreasing
num_sig = sapply(sig_models, function(x) nrow(x))
num_sig = sort(num_sig, decreasing = T)
ord_sig_mod <- names(num_sig)

# produces a data frame of each significant feature and how many times it was found to be significant
count_sig <- as.data.frame(sort(table(sig_features$feat), decreasing = T))
count_sig <- count_sig[count_sig$Freq != 0, ]

# Gets the frequency of occurrence of all the significant feats
all_sig_feat <- unique(sig_features$feat)
sig_data <- feat_freq[feat_freq$feat_data %in% all_sig_feat, ]

# plots the significant features and they're occurrence across all models
plot2 <- ggplot(sig_data, aes(x = feat_data, y = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability with many features
  labs(title = "Frequency of Signifcant Features Across Models", x = "Features", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.border = element_rect(colour = "black", fill = NA),  # Adding axis borders
    axis.line = element_line(colour = "black"),  # Adding axis lines
    legend.position = "right",  # Adjust legend position
    legend.title = element_text(size = 10),
  ) +
  scale_y_continuous(limits = c(0, nrow(model_res)), expand = expansion(mult = c(0, 0)))  # Control y-axis breaks

# plots the significant features and how frequently they were significant across all models
plot3 <- ggplot(count_sig, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better readability with many features
  labs(title = "Frequency of Significance For Significant Features Across Models", x = "Features", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    panel.border = element_rect(colour = "black", fill = NA),  # Adding axis borders
    axis.line = element_line(colour = "black"),  # Adding axis lines
    legend.position = "right",  # Adjust legend position
    legend.title = element_text(size = 10),
  ) +
  scale_y_continuous(limits = c(0, nrow(model_res)), expand = expansion(mult = c(0, 0)))  # Control y-axis breaks

# returns the parameters of the best models (by AUC and by most significant features)
best_auc = new_comb[ord_mod$models, ]
best_sig = new_comb[ord_sig_mod, ]

# save lists/tables/plots
save(freq_out, sig_features, sig_data, count_sig, best_auc, best_sig, file = "r_results.RData")

ggsave("r_plot1.png", plot = plot1)
ggsave("r_plot2.png", plot = plot2)
ggsave("r_plot3.png", plot = plot3)
