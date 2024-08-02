### Function-1 ###
#' @noRd
#' Main function for producing plots related to the model
plot_main <- function(trained_model, test_data, test_metadata, outcome_colname, method, outcome_type) {
 roc_plot <- roc_plot_main(trained_model, test_data, test_metadata, outcome_colname, method, outcome_type)
 con_mat <- confusion_matrix(test_metadata, outcome_colname, trained_model, test_data)
 return(list(roc_plot, con_mat))
}


### Function-2 ###
#' @noRd
#' Main function for Prediction and ROC curve plotting:
roc_plot_main <- function(trained_model, test_data, test_metadata, outcome_colname, method, outcome_type) {
  # get prediction from best train model across all folds
  mod_pred <- trained_model$pred %>%
    dplyr::right_join(trained_model$bestTune, by = names(trained_model$bestTune)) # selects predictions with the best hyperparameter
  uniq_obs <- levels(mod_pred[["obs"]]) ### changed $ to [[]]
  obs <- factor(mod_pred[["obs"]], levels = uniq_obs) ### changed pull to [[]]
  if (outcome_type == "binary") {
    roc.obj <- pROC::roc(obs, (mod_pred %>% dplyr::pull(uniq_obs[2])), ci=TRUE, direction = "<") # builds an roc curve for plotting
    train_model <- list("train" = roc.obj) # creates a list with roc.obj in it
  } else {
    train.obj <- multiclass.roc(obs, mod_pred[, uniq_obs], direction = "<")
  }

  # get prediction from hold-out test dataset
  mod_pred <- stats::predict(trained_model, test_data, type = "prob")
  uniq_obs <- levels(trained_model[["pred"]][["obs"]])
  obs <- factor(test_metadata[[outcome_colname]], levels = uniq_obs)
  if (outcome_type == "binary") {
    roc.obj <- pROC::roc(obs, (mod_pred %>% dplyr::pull(uniq_obs[2])), ci=TRUE, direction = "<")
    # uses uniq_obs[2] because thats whats used in get_performance_metric
    # 1-AUC for uniq_obs[2] = AUC for uniq_obs[1])
    test_model <- list("test" = roc.obj)
  } else {
    test.obj <- multiclass.roc(obs, mod_pred[, uniq_obs], direction = "<")
  }

  if (outcome_type == "binary") {
    roc_list <- c(train_model, test_model)
    roc_plot <- roc_plot_function(roc_list, method, p.ci = FALSE, shuffle = TRUE, outcome_type)
  } else {
    roc_list <- extract_roc(train.obj, test.obj)
    roc_plot <- list()
    for (i in 1:length(roc_list)) {
      roc_plot[[i]] = roc_plot_function(roc_list[[i]], names(train.obj$rocs)[i], p.ci = FALSE, shuffle = TRUE, outcome_type)
    }
  }
  return(list("roc_dat"=roc_list, "roc_plot"=roc_plot))
}


### Function-3 ###
#' @noRd
#' helper function to plot ROC curve
roc_plot_function <- function(roc.list, title.in, p.ci = FALSE, shuffle = TRUE, outcome_type) {
  # generates palette
  if (shuffle == TRUE){
    lengthroc <- length(roc.list)/2
    pal <- rep(viridis::viridis(lengthroc), 2)
    aes_ggroc <- c("linetype", "colour")
    line_types <- c(rep("solid", lengthroc), rep("twodash", lengthroc))
  }
  else {
    pal <- viridis::viridis(length(roc.list)) # Plot colour is changed here
    aes_ggroc <- "colour"
    line_types <- "solid"
  }

   # extracts auc
  if (outcome_type == "binary") {
    roc.list %>%
     purrr::map(~dplyr::tibble(AUC = .x$ci)) %>% # extracts ci from the test and train roc and converts them to tibble
     data.table::rbindlist(idcol = "name") %>% # binds the two lists as rows in a dataframe and adds a new column with test or train
     dplyr::mutate(AUC = round(AUC, 2)) %>% # rounds the AUC column
     dplyr::group_by(name) %>% # groups train together and test together
     dplyr::mutate(type=c("lower", "mean", "upper")) %>% # adds type column to signify what type of interval it is
     tidyr::pivot_wider(id_cols = name, names_from = type, values_from = AUC) %>% # turns each type into a column and removes AUC column. Values of AUC are put into their respective type columns
     dplyr::mutate(label_long = paste0(name, ", AUC = ", mean, " (", lower, "-", upper, ")")) -> data.labels # adds another column called label_long which contains group (train/test), mean, and interval (lower-upper)

    names(roc.list) <- data.labels$label_long # changes the list names from train/test to label_long
  }

  p <- pROC::ggroc(c(roc.list), size = 1, legacy.axes = TRUE, aes = aes_ggroc) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::labs(x = "1-Specificity", y = "Sensitivity", title = title.in) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::scale_linetype_manual(values = line_types) +
    ggplot2::geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color = "grey", linetype = "dashed", alpha = 0.5) +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.2), limits = c(-0.01, 1.01)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.2), limits = c(-0.01, 1.01)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12, family = "sans", face = "plain"),
             axis.text = ggplot2::element_text(size = 12, family = "sans", face = "plain"),
             legend.title = ggplot2::element_blank(),
             legend.text = ggplot2::element_text(size = 12, family = "sans", face = "plain"),
             plot.title = ggplot2::element_text(size = 12, vjust = 0.5, hjust = 0.5, family = "sans", face = "plain"),
             legend.position.inside = c(0.6, 0.1),
             legend.background = ggplot2::element_blank(),
             legend.box.background = ggplot2::element_rect(colour = "black")) +
    ggplot2::coord_equal()

  if (p.ci == TRUE) {
    ## plots + confidence intervals
    ci.list <- lapply(roc.list, pROC::ci.se, specificities = seq(0, 1, l = 25))

    dat.ci.list <- lapply(ci.list, function(ciobj)
      data.frame(x = as.numeric(rownames(ciobj)),
                 lower = 1 - ciobj[, 3],
                 upper =  1 - ciobj[, 1]))

    for(i in c(1)) {
      p <- p + ggplot2::geom_ribbon(
        data = 1 - dat.ci.list[[i]],
        aes(x = x, ymin = lower, ymax = upper),
        fill = pal[i], col = pal[i],
        alpha = 0.2, size = 0.3,
        inherit.aes = FALSE)
    }}
  return(p)
}


### Function-4 ###
#' @noRd
#' helper function to extract roc.obj's from multiclass.roc.obj
extract_roc <- function(train_obj, test_obj) {
  train_rocs = train_obj$rocs
  test_rocs = test_obj$rocs
  roc_list = list()
  for (i in 1:length(train_rocs)) {
    roc_list[[i]] = list(train = train_rocs[[i]][[1]], test = test_rocs[[i]][[1]])
  }
  return(roc_list)
}


### Function-5 ###
#' @noRd
#' Main function for plotting a confusion matrix
confusion_matrix <- function(test_metadata, outcome_colname, trained_model, test_data) {
  obs = factor(test_metadata[[outcome_colname]]) # sets actual class values as factor
  preds = stats::predict(trained_model, test_data, type = "raw") # produces predicted class values as factor
  cm = confusionMatrix(preds, obs) # creates a confusion matrix
  cm_data = as.data.frame(cm$table) # extracts the confusion matrix table from the confusion matrix list
  cm_data$Color = ifelse(cm_data$Reference == cm_data$Prediction, "True Positive", "Mismatch") # Adds a column to cm_data to specify the color based on condition

  # Plots the confusion matrix with distinguishing True Positives from Mismatches
  cm_plot <- ggplot(cm_data, aes(x = Reference, y = Prediction, fill = Color)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +
    scale_fill_manual(values = c("True Positive" = "yellow", "Mismatch" = "blue")) +
    theme_minimal() +
    labs(title = "Confusion Matrix",
         x = "Actual",
         y = "Predicted",
         fill = "Type")
  return(list(cm, cm_plot))
}
