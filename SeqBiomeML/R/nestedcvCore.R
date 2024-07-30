#### Function-1 ###
#' @noRd
#' Main function for nested cross validation
nestedcvCore <- function(
  dataset,
  metadata,
  groups,
  method,
  preprocess_methods,
  corrFunList,
  outcome_type,
  class_probs,
  outcome_colname,

  out_fold,
  outer_folds,

  hyperparameters,
  resamp_method,
  perf_metric_name,
  perf_metric_function,
  n_inner_folds,
  cv_times,

  filter_features,
  filterFunList,
  grouped_feat,

  threads,
  seed
) {

  out_fold <- paste0("Fold", out_fold)
  tic = Sys.time()
  message_parallel("Starting ", out_fold, " ...")

  # Get the data, metadata and groups for inner fold
  tmp_train_data <- dataset[outer_folds[[out_fold]], ]
  tmp_test_data <- dataset[-outer_folds[[out_fold]], ]
  tmp_train_metadata <- metadata[outer_folds[[out_fold]], ]
  tmp_test_metadata <- metadata[-outer_folds[[out_fold]], ]
  tmp_groups <- groups[outer_folds[[out_fold]]]

  ### added this line
  # At this stage implement feature preprocessing
  ### @preprocess_features.R -> Function-1 to function-9
  message("Performing preprocessing of dataset.")
  filt_dataset <- preprocess_features(train_data = tmp_train_data,
                                      test_data = tmp_test_data,
                                      outcome_colname = outcome_colname,
                                      preprocess_methods = preprocess_methods,
                                      corrFunList = corrFunList)
  tmp_train_data  = filt_dataset[[1]] ### removed metadata = filt_dataset[[2]] (redundant)
  tmp_test_data = filt_dataset[[2]]
  nzv_feat = filt_dataset[[3]] # removed by preprocessing
  grouped_feat = filt_dataset[[4]] # correlated features


  # At this stage implement feature selection
  ### @filter_features.R -> Function-1 to function-6
  if (filter_features) {
    message("Performing feature selection.")
    filt_dataset <- filter_features_main(train_data = tmp_train_data,
                                         train_metadata = tmp_train_metadata,
                                         test_data = tmp_test_data,
                                         outcome_colname = outcome_colname,
                                         filterFunList = filterFunList,
                                         outcome_type = outcome_type)
    tmp_train_data = filt_dataset[[1]]
    tmp_test_data = filt_dataset[[2]]
    rem_feat = filt_dataset[[3]]
    filt_dataset[[3]] = c(filt_dataset[[3]], nzv_feat)
    filt_dataset[["Grouped_Features"]] = grouped_feat
  }


  # Set hyper-parameter and tun-grid
  ### @hyperparameters.R -> Function-1 to Function-8
  if (is.null(hyperparameters) & !grepl("custom", method)) {
    hyperparameters <- get_hyperparams_list(tmp_train_data, method)
    tune_grid <- get_tuning_grid(hyperparameters, method)
  } else if (!is.null(hyperparameters) & !grepl("custom", method)) {
    tune_grid <- get_tuning_grid(hyperparameters, method)
  } else if (grepl("custom", method)) {
    tune_grid <- get_tuning_grid(hyperparameters, method)
    customrf_param = custom_method_grid(method = method)
  }


  # Get performance metric function
  ### @performance.R -> Function-2 and Function-3
  if (is.null(perf_metric_function)) {
    perf_metric_function <- get_perf_metric_fn(outcome_type)
  }

  # Get performance metric name for cross-validation.
  if (is.null(perf_metric_name)) {
    perf_metric_name <- get_perf_metric_name(outcome_type)
  }


  # Define cross-validation scheme and training parameters
  ### @cross_val.R -> Function-1 to Function-4
  cross_val <- define_cv_nested(
    resamp_method = resamp_method,
    train_data = tmp_train_data,
    train_metadata = tmp_train_metadata,
    outcome_colname = outcome_colname,
    hyperparams_list = hyperparameters,
    perf_metric_function = perf_metric_function,
    class_probs = class_probs,
    inner_fold = n_inner_folds,
    cv_times = cv_times,
    groups = tmp_groups,
    seed = seed
  )

  message("Training the model...")
  # Train model now
  ### @train.R -> Function-1
  doFuture::registerDoFuture()
  future::plan(future::multicore, workers = threads)
  trained_model_caret <- train_model(
    train_data = tmp_train_data,
    train_metadata = tmp_train_metadata,
    outcome_colname = outcome_colname,
    method = method,
    cv = cross_val,
    perf_metric_name = perf_metric_name,
    tune_grid = tune_grid
  )

  message("Calculating performance of test dataset.")
  # Calculate performance of the model on validation data
  ### @performance.R -> Function-4 and Function-5
  ### @utils.R -> Function-4
  performance_tbl <- get_performance_tbl(
    trained_model = trained_model_caret,
    test_data = tmp_test_data,
    test_metadata = tmp_test_metadata,
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
  plot_out = plot_main(
    trained_model = trained_model_caret,
    test_data = tmp_test_data,
    test_metadata = tmp_test_metadata,
    outcome_colname = outcome_colname,
    method = method,
    outcome_type = outcome_type)

    toc = Sys.time()
    message_parallel("Finished ", out_fold, " (", format(toc - tic, digits = 3), ")")


    # return
    return(
      list(
        "dataset" = filt_dataset,
        "test_data" = tmp_test_data,
        "test_metadata" = tmp_test_metadata,
        "cross_val" = cross_val,
        "trained_model" = trained_model_caret,
        "performance" = performance_tbl,
        "roc_plot" = plot_out
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
