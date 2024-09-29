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
  # adds two new columns to the tibble, method and test (two new columns should contain rf and test)
  test_perf_metrics <- dplyr::bind_rows(test_perf_metrics) %>% # converts to tibble (similar to dataframe)
    dplyr::mutate("method" = dplyr::if_else(grepl("custom", method), "custom", method)) %>% # if grepl is true "custom" else method
    dplyr::mutate(data = "test")


  # train data
  rowIndex <- dplyr::inner_join(trained_model$pred, trained_model$bestTune)[, "rowIndex"] # selects rows indices from pred that have the bestTune hyperparameter
  train_metadata <- trained_model$trainingData %>% dplyr::select('.outcome') %>% # .outcome is a column at the end of trainingData that contains the label for that sample
    dplyr::rename_with(~outcome_colname, ".outcome") %>% # renmaes .outcome with outcome_colname
    dplyr::mutate(row_num = dplyr::row_number()) %>% # new column with 1:nrow
    dplyr::arrange(match(row_num, rowIndex)) # rearranges the new row so the values match the indices of rowIndex
### removed two redundant lines
### pulled uniq_obs, pred_class and obs out of the if. changed preds and created preds_probs. Removed obs from else. tidied up code.
  preds <- dplyr::inner_join(trained_model$pred, trained_model$bestTune)
  uniq_obs <- unique(as.character(train_metadata[[outcome_colname]])) # creates the levels (should be equal to number of unique groups in the outcome_colname)
  pred_class <- preds[["pred"]] %>% factor(levels = uniq_obs) # gets the predicted groups for the best hyperparameter
  obs <- preds[["obs"]] %>% factor(levels = uniq_obs) # gets the observed groups
  if (class_probs) {
    preds_probs <- dplyr::select(preds, as.character(train_metadata[[outcome_colname]])) # selects columns from the predictions that have names in the outcome column
    train_perf_metrics <- perf_metric_function(data.frame(obs = obs, pred = pred_class, preds_probs), lev = uniq_obs)
  }
  else {
    train_perf_metrics <- perf_metric_function(data.frame(obs = obs, pred = pred_class), lev = uniq_obs)
  }
  train_perf_metrics <- train_perf_metrics %>% dplyr::bind_rows() %>%
    dplyr::mutate(data = "train") # adds a nee column with train in it

  # return
  return(
    dplyr::bind_rows(train_perf_metrics, test_perf_metrics) %>%
      dplyr::select(data, dplyr::everything()) %>%
      dplyr::mutate(seed = seed) # binds the train and test performance metrics together and adds a seed column
    )
}


### Function-5 ###
#' @noRd
#' Get performance metrics for test data
#' ### changed some of the code. Pulled obbs and uniq_obs out of the if statement, removed obs from else and tidied up some code
calc_perf_metrics <- function(test_data, test_metadata, trained_model, outcome_colname, perf_metric_function, class_probs) {
  pred_type <- "raw"
  if (class_probs) pred_type <- "prob"
  preds <- stats::predict(trained_model, test_data, type = pred_type)
  uniq_obs <- unique(test_metadata[[outcome_colname]]) ### replaced pull and removed as.character(trained_model$pred$obs), all factors should be in outcome_colname
  obs <- factor(test_metadata[[outcome_colname]], levels = uniq_obs)
  if (class_probs) {
    pred_class <- factor(names(preds)[apply(preds, 1, which.max)], levels = uniq_obs)
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = pred_class, preds), lev = uniq_obs)
  }
  else {
    perf_met <- perf_metric_function(data.frame(obs = obs, pred = preds), lev = uniq_obs)
  }
  return(perf_met)
}


### Function-6 ###
#' @noRd
#' Get best model from outer folds
median_model <- function(outer_fold_results, perf_metric_name) {
  # produces a data frame indicating the selected performance metric and the associated fold
  all_perf = lapply(outer_fold_results, function(x) x$performance[2, perf_metric_name]) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate("Fold" = paste0("Fold", row.names(.)))
  # selects the fold with the median performance value
  median_row = all_perf[all_perf[[perf_metric_name]] == median(all_perf[[perf_metric_name]]), ]
  median_fold = outer_fold_results[[median_row$Fold]]
  return(median_fold)
}
