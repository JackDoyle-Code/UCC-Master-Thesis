#### Function-1 ###
#' @noRd
#' Main function for nested cross validation
nestedcvCore <- function(
  dataset,
  metdata,
  groups,
  method,
  outcomes_vctr,
  outcome_colname,

  out_fold,
  outer_folds,

  hyperparameters,
  resamp_method,
  perf_metric_name,
  n_inner_folds,
  cv_times,

  filter_features,
  filterFunList,
  grouped_feat,

  threads,
  seed
) {

  tic = Sys.time()
  message_parallel("Starting Fold", out_fold, " ...")

  # Get the data, metadata and groups for inner fold
  tmp_data <- dataset[outer_folds[[out_fold]], ]
  tmp_metadata <- metadata[outer_folds[[out_fold]], ]
  tmp_groups <- groups[outer_folds[[out_fold]]]


  # Set hyper-parameter and tun-grid
  ### @hyperparameters.R -> Function-1 to Function-8
  if (is.null(hyperparameters) & !grepl("custom", method)) {
    hyperparameters <- get_hyperparams_list(tmp_data, method)
    tune_grid <- get_tuning_grid(hyperparameters, method)
  } else if (!is.null(hyperparameters) & !grepl("custom", method)) {
    tune_grid <- get_tuning_grid(hyperparameters, method)
  } else if (grepl("custom", method)) {
    tune_grid <- get_tuning_grid(hyperparameters, method)
    customrf_param = custom_method_grid(method = method)
  }


  # Get type of predicting variable
  ### @performance.R -> Function-1
  outcome_type <- get_outcome_type(outcomes_vctr)
  class_probs <- outcome_type != "continuous"

  # Get performance metric function
  ### @performance.R -> Function-2 and Function-3
  if (is.null(perf_metric_function)) {
    perf_metric_function <- get_perf_metric_fn(outcome_type)
  }

  # Get performance metric name for cross-validation.
  if (is.null(perf_metric_name)) {
    perf_metric_name <- get_perf_metric_name(outcome_type)
  }


  # At this stage implement feature filtering
  ### @filter_features.R -> Function-1 to function-11
  if (filter_features) {
    message("Performing feature filtering.")
    filt_dataset <- preprocess_filter_featuress(
      train_data = tmp_data,
      train_metadata = tmp_metadata,
      outcome_colname = outcome_colname,
      filterFunList = filterFunList,
      grouped_feat = grouped_feat)
  } else {
    filt_dataset <- list("filt_data"=tmp_data,
                         "filt_metadata"=tmp_metadata,
                         "final_feat"=colnames(tmp_data),
                         "grouped_feat"=grouped_feat)
  }


  # Define cross-validation scheme and training parameters
  ### @cross_val.R -> Function-1 to Function-4
  cross_val <- define_cv_nested(
    resamp_method = resamp_method,
    train_data = filt_dataset[[1]],
    train_metadata = filt_dataset[[2]],
    outcome_colname = outcome_colname,
    hyperparams_list = hyperparameters,
    perf_metric_function = perf_metric_function,
    class_probs = class_probs,
    inner_fold = n_inner_folds,
    cv_times = cv_times,
    groups = tmp_groups,
    seed = seed
  )


  # Train model now
  ### @train.R -> Function-1
  doFuture::registerDoFuture()
  future::plan(future::multicore, workers = threads)
  trained_model_caret <- train_model(
    train_data = filt_dataset[[1]],
    train_metadata = filt_dataset[[2]],
    outcome_colname = outcome_colname,
    method = method,
    cv = cross_val,
    perf_metric_name = perf_metric_name,
    tune_grid = tune_grid
  )


  # Calculate performance of the model on validation data
  ### @performance.R -> Function-4 and Function-5
  ### @utils.R -> Function-4
  performance_tbl <- get_performance_tbl(
    trained_model = trained_model_caret,
    test_data = dataset[-outer_folds[[out_fold]], filt_dataset[[3]]],
    test_metadata = metadata[-outer_folds[[out_fold]], ],
    outcome_colname = outcome_colname,
    perf_metric_function = perf_metric_function,
    perf_metric_name = perf_metric_name,
    class_probs = class_probs,
    method = method,
    seed = seed
  ) %>% dplyr::mutate("outer_fold" = out_fold) %>% dplyr::select(outer_fold, everything())


  message("Plotting ROC curve.")
  # Plot ROC curve
  ### @plot.R -> Function-1 and Function-2
  roc_out = roc_plot_main(
    trained_model = trained_model_caret,
    test_data = dataset[-outer_folds[[out_fold]], filt_dataset[[3]]],
    test_metadata = metadata[-outer_folds[[out_fold]], ],
    outcome_colname = outcome_colname,
    method = method)

    toc = Sys.time()
    message_parallel("Finished Fold", out_fold, " (", format(toc - tic, digits = 3), ")")


    # return
    return(
      list(
        "dataset" = filt_dataset,
        "test_data" = dataset[-outer_folds[[out_fold]], filt_dataset[[3]]],
        "test_metadata" = metadata[-outer_folds[[out_fold]], ],
        "cross_val" = cross_val,
        "trained_model" = trained_model_caret,
        "performance" = performance_tbl,
        "roc_plot" = roc_out
      )
    )
  }



  #### Function-2 ###
  #' @noRd
  # Prints a message using shell echo from inside mclapply when run in Rstudio
  message_parallel <- function(...) {
    if (Sys.getenv("RSTUDIO") != "1") return()
    system(sprintf('echo "%s"', paste0(..., collapse = "")))
  }
