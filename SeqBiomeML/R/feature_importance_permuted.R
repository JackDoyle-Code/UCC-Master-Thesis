### Function-1 ###
#' Get feature importance using the permutation method
#' @noRd
feature_importance_permuted <- function(trained_model, test_data, test_metadata,
                                        outcome_colname, perf_metric_function,
                                        perf_metric_name, class_probs, method,
                                        grouped_features_list = NULL, seed = NA,
                                        nperms = 100, threads = 1) {

  # get outcome and features
  outcome <- test_metadata %>% dplyr::pull(outcome_colname)
  features <- test_data

  if (is.null(grouped_features_list)) {
    grouped_features_list <- colnames(features)
  }

  test_perf_value <- calc_perf_metrics(
    test_data,
    test_metadata,
    trained_model,
    outcome_colname,
    perf_metric_function,
    class_probs
  )[[perf_metric_name]]

  nsteps <- nperms * length(grouped_features_list)
  progbar <- NULL
  progbar <- progressr::progressor(
    steps = nsteps,
    message = "Feature importance"
  )

  future::plan(future::multicore, workers = threads)
  imps <- future.apply::future_lapply(grouped_features_list, function(feat) {
    return(
      find_permuted_perf_metric(
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
        alpha = 0.05,
        progbar = progbar
      )
    )
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



### Function-2 ###
#' Get permuted performance metric difference for a single feature
#' @noRd
find_permuted_perf_metric <- function(test_data, test_metadata, trained_model, outcome_colname,
                                      perf_metric_function, perf_metric_name,
                                      class_probs, feat,
                                      test_perf_value,
                                      nperms = 100,
                                      alpha = 0.05,
                                      progbar = NULL) {
  test_data <- as.data.frame(test_data)

  # permute grouped features together
  fs <- strsplit(feat, "\\|")[[1]]
  # only include ones in the test data split
  fs <- fs[fs %in% colnames(test_data)]
  # get the new performance metric and performance metric differences
  n_rows <- nrow(test_data)
  perm_perfs <- sapply(seq(1, nperms), function(x) {
    permuted_test_data <- test_data
    # this strategy works for any number of features
    rows_shuffled <- sample(n_rows)
    permuted_test_data[, fs] <- permuted_test_data[rows_shuffled, fs]
    pbtick(progbar)
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



### Function-3 ###
#' @noRd
#' Get the lower bound for an empirical confidence interval
lower_bound <- function(x, alpha) {
  x <- sort(x)
  return(x[length(x) * alpha / 2])
}



### Function-4 ###
#' @noRd
#' Get the upper bound for an empirical confidence interval
upper_bound <- function(x, alpha) {
  x <- sort(x)
  return(x[length(x) - length(x) * alpha / 2])
}



### Function-5 ###
#' @noRd
#' Calculate the p-value for a permutation test
calc_pvalue <- function(vctr, test_stat) {
  return((sum(vctr >= test_stat) + 1) / (length(vctr) + 1))
}



### Function-6 ###
#' @noRd
#' Update progress if the progress bar is not `NULL`.
pbtick <- function(pb, message = NULL) {
  if (!is.null(pb)) {
    if (!is.null(message)) {
      pb(message)
    } else {
      pb()
    }
  }
  invisible()
}
