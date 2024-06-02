### Function-1 ###
#' @noRd
#' Get outcome type.
#' If the outcome is numeric, the type is continuous.
#' Otherwise, the outcome type is binary if there are only two outcomes or
#' multiclass if there are more than two outcomes.
get_outcome_type <- function(outcomes_vec) {
  num_outcomes <- length(unique(outcomes_vec))
  if (num_outcomes < 2) {
    stop(
      paste0(
        "A continuous, binary, or multi-class outcome variable is required, but this dataset has ",
        num_outcomes,
        " outcome(s)."
      )
    )
  }
  if (is.numeric(outcomes_vec)) {
    # regression
    otype <- "continuous"
  } else if (num_outcomes == 2) {
    # binary classification
    otype <- "binary"
  } else {
    # multi-class classification
    otype <- "multiclass"
  }
  return(otype)
}


### Function-2 ###
#' @noRd
#' Get default performance metric function
get_perf_metric_fn <- function(outcome_type) {
  if (outcome_type == "continuous") { # regression
    perf_metric_fn <- caret::defaultSummary
  } else if (outcome_type %in% c("binary", "multiclass")) { # binary or multiclass
    perf_metric_fn <- caret::multiClassSummary
  } else {
    stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
  }
  return(perf_metric_fn)
}


### Function-3 ###
#' @noRd
#' Get default performance metric name
#' Get default performance metric name for cross-validation.
get_perf_metric_name <- function(outcome_type) {
  if (outcome_type == "continuous") { # regression
    perf_metric_name <- "RMSE"
  } else {
    if (outcome_type == "binary") { # binary classification
      perf_metric_name <- "AUC"
    } else if (outcome_type == "multiclass") { # multi-class classification
      perf_metric_name <- "logLoss"
    } else {
      stop(paste0('Outcome type of outcome must be one of: `"continuous"`,`"binary"`,`"multiclass`), but you provided: ', outcome_type))
    }
  }
  return(perf_metric_name)
}


### Function-4 ###
#' @noRd
#' Get model performance metrics as a one-row tibble
get_performance_tbl <- function(trained_model,
                                test_data,
                                test_metadata,
                                outcome_colname,
                                perf_metric_function,
                                perf_metric_name,
                                class_probs,
                                method,
                                seed = NA) {
  # test data
  test_perf_metrics <- calc_perf_metrics(
    test_data,
    test_metadata,
    trained_model,
    outcome_colname,
    perf_metric_function,
    class_probs
  )
  test_perf_metrics <- dplyr::bind_rows(test_perf_metrics) %>%
    dplyr::mutate("method" = dplyr::if_else(grepl("custom", method), "custom", method)) %>%
    dplyr::mutate(data = "test")

  # train data
  train_perf_metrics <- caret::getTrainPerf(trained_model)
  names(train_perf_metrics) <- gsub("Train", "", names(train_perf_metrics))
  train_perf_metrics <- dplyr::mutate(train_perf_metrics, data = "train")

  # return
  return(
    dplyr::bind_rows(train_perf_metrics, test_perf_metrics) %>%
      dplyr::select(data, dplyr::everything()) %>%
      dplyr::mutate(seed = seed)
    )
}


### Function-5 ###
#' @noRd
#' Get performance metrics for test data
calc_perf_metrics <- function(test_data, test_metadata, trained_model, outcome_colname, perf_metric_function, class_probs) {
  pred_type <- "raw"
  if (class_probs) pred_type <- "prob"
  preds <- stats::predict(trained_model, test_data, type = pred_type)
  if (class_probs) {
    uniq_obs <- unique(c(test_metadata %>% dplyr::pull(outcome_colname), as.character(trained_model$pred$obs)))
    obs <- factor(test_metadata %>% dplyr::pull(outcome_colname), levels = uniq_obs)
    pred_class <- factor(names(preds)[apply(preds, 1, which.max)], levels = uniq_obs)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = pred_class, preds), lev = uniq_obs)
  } else {
    obs <- test_metadata %>% dplyr::pull(outcome_colname)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = preds))
  }
  return(perf_met)
}


### Function-6 ###
#' @noRd
#' Get best tune parameters from inner folds
finaliseTune <- function(x) {
  fintune <- lapply(colnames(x), function(i) {
    if (is.numeric(x[, i])) return(stats::median(x[, i]))
    tab <- table(x[, i])
    names(tab)[which.max(tab)]  # majority vote for factors
  })
  names(fintune) <- colnames(x)
  data.frame(fintune, check.names = FALSE)
}
