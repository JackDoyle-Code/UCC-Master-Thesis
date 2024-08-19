### Function-1 ###
#' Get feature importance using the selected method
#' @noRd
feature_importance_main <- function(feature_importance_method, trained_model, test_data, test_metadata,
                                    outcome_colname, perf_metric_function,
                                    perf_metric_name, performance_table, class_probs, method,
                                    grouped_features_list = NULL, seed = NULL,
                                    nperms = 100, threads = 1) {

  feature_importance = list()
  if ("permutation" %in% feature_importance_method) {
    message("Performing feature importance analysis using permutation")
    # Feature importance analysis and plot
    ### @feature_importance.R -> Function-1
    feature_importance[["Permutation"]] <- with_progress(feature_importance_permuted(
      trained_model, test_data, test_metadata, outcome_colname, perf_metric_function, perf_metric_name, performance_table,
      class_probs, method, grouped_features_list, seed, nperms, threads))
  }
  if ("embedded" %in% feature_importance_method) {
    message("Performing feature importance analysis using embedded methods")
    feature_importance[["Embedded"]] <- feature_importance_embedded(trained_model, feature_importnace_method, test_data)
  }
  return(feature_importance)
}


### Function-2 ###
#' Get feature importance using the permutation method
#' @noRd
feature_importance_permuted <- function(trained_model, test_data, test_metadata,
                                        outcome_colname, perf_metric_function,
                                        perf_metric_name, performance_table, class_probs, method,
                                        grouped_features_list = NULL, seed = NULL,
                                        nperms = 100, threads = 1) { ### removed grouped_features_list argument

  grouped_features_list <- colnames(test_data) ### removed if grouped_features_list is null
  test_perf_value <- performance_table[[perf_metric_name]][[2]] # this is the same as calc_perf_metric
  nsteps <- length(grouped_features_list) ### changed nsteps so it doesn't include nperms
  #progbar <- NULL # uncomment if you do not want a progress bar
  progbar <- progressr::progressor(
    steps = nsteps,
    message = "Feature importance"
  )

  future::plan(future::multicore, workers = threads)
  imps <- future.apply::future_lapply(grouped_features_list, function(feat) { # future lapply allows for multicore usage, lappy doesn't
    result <- find_permuted_perf_metric(
      test_data,
      test_metadata,
      trained_model,
      outcome_colname,
      perf_metric_function,
      perf_metric_name,
      class_probs,
      feat,
      test_perf_value,
      nperms = nperms,
      alpha = 0.05)
    pbtick(progbar) ### switched the position of progbar so it updates on every feature rather than permutation
    return(result)
  }, future.seed = seed) %>%
    dplyr::bind_rows()
  return(as.data.frame(imps) %>%
           dplyr::mutate(
             feat = factor(grouped_features_list),
             method = method,
             perf_metric_name = perf_metric_name,
             seed = seed
           ) %>%
           dplyr::select(feat, dplyr::everything()) %>%
           dplyr::arrange(pvalue)
  )
}


### Function-3 ###
#' Get feature importance using the embedded method
#' @noRd
feature_importance_embedded <- function(trained_model, feature_importnace_method, test_data) {
  final_model = trained_model$finalModel
  if (method == "xgbTree") {
    feat_imp = xgb.importance(model = final_model)
    not_imp = test_data[, !colnames(test_data) %in% feat_imp$Feature]
    feat_imp = list(xgb_imp, not_imp)
  } else {
    imp = varImp(final_model, scale = F) # rpart2 and rf, glmnet}
    imp$Variable = rownames(imp)
    feat_imp = imp[order(imp$Overall, decreasing = T), ]
  }
  return(feat_imp)
}

### Function-4 ###
#' Get permuted performance metric difference for a single feature
#' @noRd
find_permuted_perf_metric <- function(test_data, test_metadata, trained_model, outcome_colname,
                                      perf_metric_function, perf_metric_name,
                                      class_probs, feat,
                                      test_perf_value,
                                      nperms = 100,
                                      alpha = 0.05) {
  test_data <- as.data.frame(test_data)

  # permute grouped features together
  #fs <- strsplit(feat, "\\|")[[1]] # splits string at |
  # only include ones in the test data split
  #fs <- fs[fs %in% colnames(test_data)]
  # get the new performance metric and performance metric differences
  n_rows <- nrow(test_data)
  perm_perfs <- sapply(seq(1, nperms), function(x) {
    permuted_test_data <- test_data
    # this strategy works for any number of features
    rows_shuffled <- sample(n_rows) # random sample of 1-nrows
    permuted_test_data[, feat] <- permuted_test_data[rows_shuffled, feat]
    return(
      calc_perf_metrics(
        permuted_test_data,
        test_metadata,
        trained_model,
        outcome_colname,
        perf_metric_function,
        class_probs
      )[[perf_metric_name]]
    )
  })
  mean_perm_perf <- mean(perm_perfs)
  return(c(
    perf_metric = mean_perm_perf,
    perf_metric_diff = test_perf_value - mean_perm_perf,
    pvalue = calc_pvalue(perm_perfs, test_perf_value),
    lower = lower_bound(perm_perfs, alpha),
    upper = upper_bound(perm_perfs, alpha)
  ))
}



### Function-5 ###
#' @noRd
#' Get the lower bound for an empirical confidence interval
lower_bound <- function(x, alpha) {
  x <- sort(x)
  return(x[length(x) * alpha / 2]) # 0.025 of the bell curve
}



### Function-6 ###
#' @noRd
#' Get the upper bound for an empirical confidence interval
upper_bound <- function(x, alpha) {
  x <- sort(x)
  return(x[length(x) - length(x) * alpha / 2]) # 0.975 of the bell curve
}



### Function-7 ###
#' @noRd
#' Calculate the p-value for a permutation test
calc_pvalue <- function(vctr, test_stat) {
  return((sum(vctr >= test_stat) + 1) / (length(vctr) + 1)) # gets the number of permutations that had a higher or equal
  # performance metric to the test performance. This is divided by the number of permuations
}



### Function-8 ###
#' @noRd
#' Update progress if the progress bar is not `NULL`.
pbtick <- function(pb, message = NULL) {
  if (!is.null(pb)) {
    pb()
  }
}



