######### Change the next two lines ##########
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
  return(list("100%" = across_100, "95%-99%" = across_95, "75-95%" = across_75, "50-75%" = across_50, "25-50%" = across_25, "<25%" = across_0))
}
freq_out <- get_feature_frequency(feat_freq, nrow(model_res))
g_freq_5 <- rbind(freq_out[[2]], freq_out[[3]][1:4, ])  ############### CHECK HERE, DO NOT AUTO RUN ####################

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
    plot.title.position = "plot",
  ) +
  scale_fill_viridis_d(option = "D", direction = 1) + # Apply colorblind-friendly palette
  scale_y_continuous(breaks = seq(0, max(group_freq$Count) + 10, by = 10), expand = expansion(mult = c(0, 0.2))) + # Control y-axis breaks
  scale_x_discrete(limits = c("<25%", "25-75%", "50-74%", "75-94%", "95%-99%", "100%"))  # Specify the order of the x-axis

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

# Gets the frequency of occurrence of all the significant feats
all_sig_feat <- unique(sig_features$feat)
sig_data <- feat_freq[feat_freq$feat_data %in% all_sig_feat, ]

# assigns the groups for each column
get_colour <- function(freq_tbl, N) {
  apply(freq_tbl, 1, function(x) {
    if (as.numeric(x[["Freq"]]) == N) {
      return("100%")
    }
    if (as.numeric(x[["Freq"]]) >= round(N*0.80) & x[["Freq"]] < N) {
      return("80-100%")
    }
    if (as.numeric(x[["Freq"]]) >= round(N*0.60) & x[["Freq"]] <  round(N*0.80)) {
      return("60-80%")
    }
    if (as.numeric(x[["Freq"]]) >= round(N*0.40) & x[["Freq"]] <  round(N*0.60)) {
      return("40-60%")
    }
    if (as.numeric(x[["Freq"]]) >= round(N*0.20) & x[["Freq"]] <  round(N*0.40)) {
      return("20-40%")
    }
    if (as.numeric(x[["Freq"]]) <  round(N*0.20)) {
      return("<20%")
    }
  })
}
sig_data_colour <- cbind(sig_data, get_colour(sig_data, 288))
colnames(sig_data_colour)[[3]] = "Percentage_Cover"

# plots the significant features and they're occurrence across all models
plot2 <- ggplot(sig_data_colour, aes(x = feat_data, y = Freq, fill = Percentage_Cover)) +
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
    plot.title.position = "plot",
  ) +
  scale_fill_viridis_d(option = "D", direction = 1) +
  scale_y_continuous(limits = c(0, 288), expand = expansion(mult = c(0, 0)))  # Control y-axis breaks

# produces a data frame of each significant feature and how many times it was found to be significant
count_sig <- as.data.frame(sort(table(sig_features$feat), decreasing = T))
count_sig <- count_sig[count_sig$Freq != 0, ]
count_sig_colour <- cbind(count_sig, Percentage_Cover = get_colour(count_sig, 288))
count_sig_colour$Percentage_Cover <- factor(count_sig_colour$Percentage_Cover, levels = c("<5%","5-15%", "15-25%", "25-40%", "40-55%"))
g_sig_5 <- count_sig_colour[1:5, ]

# plots the significant features and how frequently they were significant across all models
plot3 <- ggplot(count_sig_colour, aes(x = Var1, y = Freq, fill = Percentage_Cover)) +
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
    plot.title.position = "plot",
  ) +
  scale_fill_viridis_d(option = "D", direction = 1) +
  scale_y_continuous(limits = c(0, 288), expand = expansion(mult = c(0, 0)))  # Control y-axis breaks

# returns the parameters of the best models (by AUC and by most significant features)
best_auc = new_comb[ord_mod$models, ]
best_auc = cbind(best_auc, model_res[rownames(best_auc), ]$AUC)
most_sig = new_comb[ord_sig_mod, ]
most_sig = cbind(best_auc, model_res[rownames(best_auc), ]$AUC)

scl_param = c("clr", "tss", "rclr")
flt_param = c("mrmr_filter", "glm_filter", "boruta_filter")
mdl_param = c("rf", "glmnet", "kknn", "svmRadial")
scl_list = set_names(lapply(scl_param, function(x) best_auc[best_auc$scale_method == x, ]$AUC), scl_param)
flt_list = set_names(lapply(flt_param, function(x) best_auc[best_auc$fs_method == x, ]$AUC), flt_param)
mdl_list = set_names(lapply(mdl_param, function(x) best_auc[best_auc$method == x, ]$AUC), mdl_param)
AUC_list = c(scl_list, flt_list, mdl_list)
sum_list = lapply(AUC_list, summary)






# extracts the most significant features, their respective count data and groups them according to Control or Athlete
library(reshape2)
                                  ############# Change the second and third line below #################

# gets the x most significant features and their count data
best_sig = as.character(head(count_sig_colour, 12)$Var1)
S_genus = data.frame(t(apply(S_genus, 1, function(x) x/sum(x))))
best_sig = sapply(best_sig, function (x) gsub(" ", ".", x)) ########## comment out these 3 lines for meta ###########
best_sig = sapply(best_sig, function (x) gsub("-", ".", x))
best_genus = S_genus[, best_sig]

# adds a group column (Control/Athlete)
group_best = cbind(best_species, Group = S_meta$ParticipantType)
group_best[group_best$Group != "control", 13] = "athlete"

# performs a ttest between control and athlete for each of te 12 significant features
g_ra_res = apply(group_best[, 1:12], 2, function(x) t.test(x ~ group_best$Group)$p.value)

# joins all colnames into one column with their corresponding count data as another column
melt_best <- melt(group_best, id.vars = 'Group')

# boxplot of feature count data between groups. With outliers
inc_out_plot <- ggplot(melt_best,
  aes(x = Group, y = value, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Group), width = 0.2, alpha = 0.5) +
  labs(x = NULL, y = 'Count Data', title = 'Comparison of Control vs. Athlete Count Data for Most Siginifcant Species') +
  theme_minimal() +
  theme(legend.position = 'top') +
  facet_wrap(~variable, scales = "free_y")

# removes outliers for each respective feature
rem_outliers <- function(sig_features, melted_data, outlier_range = 0.9) {
  out_gone <- lapply(sig_features, function(x) {
    specific_feat = melted_data[melted_data$variable == x, ]
    lessthan90 = specific_feat[specific_feat$value <= quantile(specific_feat$value, 0.9), ]
    return(lessthan90)
  })
  names(out_gone) = sig_features
  return(do.call(rbind, out_gone))
}
exc_out <- rem_outliers(best_sig, melt_best)

# boxplot of feature count data between groups. Without outliers
exc_out_plot = ggplot(exc_out,
  aes(x = Group, y = value, fill = Group)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Group), width = 0.2, alpha = 0.5) +
  labs(x = NULL, y = 'Count Data', title = 'Comparison of Control vs. Athlete Count Data for Most Siginifcant Species (Outlier Excluded)') +
  theme_minimal() +
  theme(legend.position = 'top') +
  facet_wrap(~variable, scales = "free_y")

# save lists/tables/plots                     ###### Change the save locations ######
save(freq_out, sig_features, sig_data, count_sig, best_auc, best_sig, group_best, g_freq_5, g_sig_5, sum_list, file = "g_results.RData")

ggsave("g_plot1.png", plot = plot1, width = 10, height = 7, units = "in")
ggsave("g_plot2.png", plot = plot2, width = 10, height = 7, units = "in")
ggsave("g_plot3.png", plot = plot3, width = 10, height = 7, units = "in")
ggsave("g_plot4.png", plot = inc_out_plot, width = 12, height = 7, units = "in")
ggsave("g_plot5.png", plot = exc_out_plot, width = 12, height = 7, units = "in")
