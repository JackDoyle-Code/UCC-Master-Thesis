### Function-1 ###
#' @noRd
#' Main function for Prediction and ROC curve plotting:
roc_plot_main <- function(trained_model, test_data, test_metadata, outcome_colname, method) {
  # get prediction from best train model across all folds
  mod_pred <- trained_model$pred %>%
    dplyr::right_join(trained_model$bestTune, by = names(trained_model$bestTune))
  uniq_obs <- levels(mod_pred$obs)
  obs <- factor(mod_pred %>% dplyr::pull(obs), levels = uniq_obs)
  roc.obj <- pROC::roc(obs, (mod_pred %>% dplyr::pull(uniq_obs[2])), ci=TRUE, direction = "<")
  train_model <- list("train" = roc.obj)

  # get prediction from hold-out test dataset
  mod_pred <- stats::predict(trained_model, test_data, type = "prob")
  uniq_obs <- levels(trained_model$pred$obs)
  obs <- factor(test_metadata %>% dplyr::pull(outcome_colname), levels = uniq_obs)
  roc.obj <- pROC::roc(obs, (mod_pred %>% dplyr::pull(uniq_obs[2])), ci=TRUE, direction = "<")
  test_model <- list("test" = roc.obj)

  # combine both
  roc_list <- c(train_model, test_model)
  roc_plot <- roc_plot_function(roc_list, method, p.ci = FALSE, shuffle = TRUE)
  return(list("roc_dat"=roc_list, "roc_plot"=roc_plot))
}


### Function-2 ###
#' @noRd
#' helper function to plot ROC curve
roc_plot_function <- function(roc.list, title.in, p.ci = FALSE, shuffle = TRUE) {
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
  roc.list %>%
    purrr::map(~dplyr::tibble(AUC = .x$ci)) %>%
    data.table::rbindlist(idcol = "name") %>%
    dplyr::mutate(AUC = round(AUC, 2)) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(type=c("lower", "mean", "upper")) %>%
    tidyr::pivot_wider(id_cols = name, names_from = type, values_from = AUC) %>%
    dplyr::mutate(label_long = paste0(name, ", AUC = ", mean, " (", lower, "-", upper, ")")) -> data.labels

  names(roc.list) <- data.labels$label_long

  p <- pROC::ggroc(c(roc.list), size = 1, legacy.axes = TRUE, aes = aes_ggroc) +
    ggplot2::geom_line(size = 1) +
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
             legend.position = c(0.6, 0.1),
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
