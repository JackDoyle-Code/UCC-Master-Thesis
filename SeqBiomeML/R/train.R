### Function-1 ###
#' @noRd
#' Train model using [caret::train()].
train_model <- function(train_data,
  train_metadata,
  outcome_colname,
  method,
  cv,
  perf_metric_name,
  tune_grid,
  ...) {
    withCallingHandlers(
      if (!grepl("custom", method)) {
        if (startsWith(method, "svm")) {
          # some mutate
          train_data_tmp <- train_data %>%
            dplyr::mutate(!!outcome_colname := (train_metadata %>% dplyr::pull(outcome_colname)))

          # https://github.com/topepo/caret/issues/809#issuecomment-875038420
          model_formula <- stats::as.formula(paste(outcome_colname, "~ ."))
          trained_model_caret <- caret::train(
            form = model_formula,
            data = train_data_tmp,
            method = method,
            metric = perf_metric_name,
            trControl = cv,
            tuneGrid = tune_grid,
            ...
          )
        } else {
          features_train <- train_data
          outcomes_train <- train_metadata %>% dplyr::pull(outcome_colname)
          if (is.character(outcomes_train)) {
            outcomes_train <- outcomes_train %>% as.factor()
          }
          trained_model_caret <- caret::train(
            x = features_train,
            y = outcomes_train,
            method = method,
            metric = perf_metric_name,
            trControl = cv,
            tuneGrid = tune_grid,
            ...
          )
        }
      } else if (grepl("custom", method)) {
        features_train <- train_data
        outcomes_train <- train_metadata %>% dplyr::pull(outcome_colname)
        if (is.character(outcomes_train)) {
          outcomes_train <- outcomes_train %>% as.factor()
        }
        trained_model_caret <- caret::train(
          x = features_train,
          y = outcomes_train,
          method = custom_method_grid(method),
          metric = perf_metric_name,
          trControl = cv,
          tuneGrid = tune_grid,
          ...
        )
      },
      warning = function(w) {
        if (conditionMessage(w) == "There were missing values in resampled performance measures.") {
          warning(
            "`caret::train()` issued the following warning:\n \n",
            w,
            "\n",
            "This warning usually means that the model didn't converge in some cross-validation folds ",
            "because it is predicting something close to a constant. ",
            "As a result, certain performance metrics can't be calculated. ",
            "This suggests that some of the hyperparameters chosen are doing very poorly."
          )
          invokeRestart("muffleWarning")
        }
      }
    )
    return(trained_model_caret)
  }
