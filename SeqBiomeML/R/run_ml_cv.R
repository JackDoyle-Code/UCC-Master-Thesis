 #' Run the machine learning pipeline
#'
#' This function splits the data set into a train & test set,
#' trains machine learning (ML) models using k-fold cross-validation or repeated cross-validation or,
#' leave one out cross validation and it evaluates the best model on the held-out test set,
#' and it optionally calculates feature importance using the framework
#'
#' Required inputs are a count table, the metadata file and the ML method.
#'
#' @param dataset Data frame with a columns as features.
#' @param metadata Data frame with a columns as a outcome column names
#' @param method ML method.
#'   Options: `c("glmnet", "rf", "rpart2", "svmRadial", "xgbTree")`.
#'   - glmnet: linear, logistic, or multiclass regression
#'   - rf: random forest
#'   - customrf: custom random forest
#'   - rpart2: decision tree
#'   - svmRadial: support vector machine
#'   - xgbTree: xgboost
#' @param outcome_colname Column name as a string of the outcome variable
#'   (default `NULL`; the first column will be chosen automatically).
#' @param group_colname Vector of groups to keep together when splitting the data into
#'   train and test sets. If the number of groups in the training set is larger
#'   than `kfold`, the groups will also be kept together for cross-validation.
#'   Length matches the number of rows in the dataset (default: `NULL`).
#'
#' @param preprocess_methods Run feature preprocessing and/ or scaling of data
#'   Options: `c("zv", "nzv", "center", "scale")` (default: `c("zv", "nzv")`).
#' @param corrFunList list of function to pass to correlation arguments while preprocessing.
#'   (default: `list(corr_method = "spearman", corr_thresh = 1, group_neg_corr = TRUE)`).
#' @param filter_features perform features selection or not (default: `FALSE`).
#' @param filterFunList list of function to pass to filter features arguments while filtering.
#'   (default: `list(test = "wilcoxon_filter", p_cutoff=0.05)`).
#'
#' @param resamp_method Resampling methods
#'   Options: `c("cv", "repeatedcv", "LOOCV")`.
#'   - cv: cross validation
#'   - repeatedcv: repeated cross validation
#'   - LOOCV: leave one out cross validation (default: `cv`)
#' @param cv_times Number of cross-validation partitions to create (default: `100`).
#' @param training_frac Fraction of data for training set. Rows
#'   from the dataset will be randomly selected for the training set, and all
#'   remaining rows will be used in the testing set. Alternatively, if you
#'   provide a vector of integers, these will be used as the row indices for the
#'   training set. All remaining rows will be used in the testing set (default: `0.8`).
#' @param hyperparameters Dataframe of hyperparameters
#'   (default `NULL`; sensible defaults will be chosen automatically).
#' @param cross_val a custom cross-validation scheme from `caret::trainControl()`
#'   (uses `kfold` cross validation repeated `cv_times`).
#'   `kfold` and `cv_times` are ignored if the user provides a custom cross-validation scheme.
#'   See the `caret::trainControl()` docs for information on how to use it (default: `NULL`).
#' @param perf_metric_function Function to calculate the performance metric to
#'   be used for cross-validation and test performance. Some functions are
#'   provided by caret (see [caret::defaultSummary()]).
#'   Defaults: binary classification = `twoClassSummary`,
#'             multi-class classification = `multiClassSummary`,
#'             regression = `defaultSummary`.
#' @param perf_metric_name The column name from the output of the function
#'   provided to perf_metric_function that is to be used as the performance metric.
#'   Defaults: binary classification = `"ROC"`,
#'             multi-class classification = `"logLoss"`,
#'             regression = `"RMSE"`.
#' @param group_partitions Specify how to assign `groups` to the training and
#'   testing partitions. If `groups` specifies that some
#'   samples belong to group `"A"` and some belong to group `"B"`, then setting
#'   `group_partitions = list(train = c("A", "B"), test = c("B"))` will result
#'   in all samples from group `"A"` being placed in the training set, some
#'   samples from `"B"` also in the training set, and the remaining samples from
#'   `"B"` in the testing set. The partition sizes will be as close to
#'   `training_frac` as possible. If the number of groups in the training set is
#'   larger than `kfold`, the groups will also be kept together for
#'   cross-validation (default: `NULL`).
#' @param class_weight Class weight for imbalance datasets (default: `FALSE`).

#' @param find_feature_importance Run feature importance.
#'   `TRUE` is recommended if you would like to identify features important for
#'   predicting your outcome, but it is resource-intensive (default: `FALSE`).
#' @param feature_importance_method method for feature_importance
#'   Options: `c("endoR", "permutation")`. (default: `permutation`).

#' @param jobs Number of paralllel jobs to run for nested cross validation model (default: `1`).
#' @param threads Number of paralllel threads to run for training model (default: `1`).
#' @param seed Random seed.
#'  Your results will only be reproducible if you set a seed (default: `NA`).
#' @param ... All additional arguments are passed on to `caret::train()`, such as
#'   case weights via the `weights` argument or `ntree` for `rf` models.
#'   See the `caret::train()` docs for more details.
#'
#'
#' @return Named list with results:
#' - `trained_model`: Output of [caret::train()], including the best model.
#' - `test_data`: Part of the data that was used for testing.
#' - `performance`: Data frame of performance metrics.
#' - 'roc_plot': ROC plot for best trained model and test data.
#' - `feature_importance`: list with output and plot for feature importance analysis.
#' @export
#'
#'
run_ml_cv <-
  function(
          dataset,
          metadata,
          method = "rf",
          outcome_colname = NULL,
          group_colname = NULL,

          ### added scale method
          scale_method = list("clr", "pseudo", 1),
          preprocess_methods = c("zv", "nzv"), # can be c("zv", "nzv", "center", "scale")
          corrFunList = list(corr_method = "spearman", corr_thresh = 1, group_neg_corr = TRUE),
          filter_features = FALSE,
          filterFunList = list(test = "wilcoxon_filter", p_cutoff=0.05),

          resamp_method = "cv", # can be c(cv, repeatedcv, LOOCV)
          kfold = 5,
          cv_times = 100,
          training_frac = 0.80,
          hyperparameters = NULL,
          cross_val = NULL,
          perf_metric_function = NULL,
          perf_metric_name = NULL,
          group_partitions = NULL,
          class_weight = FALSE,

          find_feature_importance = FALSE,
          feature_importance_method = "permutation", # can be endoR or permutation

          jobs = 1,
          threads = 1,
          seed = NULL, ### changed seed from NA to NULL
          ...) {


    message("Performing various checks on datasets, metadata and supplied params.")
    # Various checks
    ### @checks.R -> Function-1 and Function-12
    check_out <- check_all(
                  dataset,
                  metadata,
                  outcome_colname,
                  method,
                  scale_method,
                  preprocess_methods,
                  kfold,
                  perf_metric_function,
                  perf_metric_name,
                  group_colname,
                  group_partitions,
                  seed,
                  hyperparameters,
                  feature_importance_method
                )

    # extracts check_all outputs
    outcome_colname <- check_out[[1]]
    na_vals <- check_out[[2]]

    # set seed if provided
    set.seed(seed) ### removed if is.na as seed is now null by default


    # Check for any space in the outcome_colname of Metadata
    ### @utils.R -> Function-3
    metadata[[outcome_colname]] <- replace_spaces(metadata[[outcome_colname]])
    if (!is.null(group_colname)) {
      metadata[[group_colname]] <- replace_spaces(metadata[[group_colname]])
    }


    # shuffle and randomized outcome_colname
    ### @utils.R -> Function-1
    dataset <- randomize_feature_order(dataset) %>% as.data.frame()


    # pull outcomes vector and groups vector
    outcomes_vctr <- metadata[[outcome_colname]] ### replaced dplyr pull
    if (is.null(group_colname)) {
      groups <- NULL
    } else {
      groups <- metadata[[group_colname]] ### replaced dplyr pull
    }


    # Sample preprocessing is implemented prior to data partition
    ### @preprocess_samples.R -> Function-1 to function-2
    dataset <- preprocess_samples(dataset = dataset,
                                  scale_method = scale_method)

    message("Creating data partition.")
    # Get indices to split dataset into train
    ### @partition.R -> Function-1 and Function-2
    if (length(training_frac) == 1) {
      training_inds <- get_partition_indices(outcomes_vctr,
                                             training_frac = training_frac,
                                             groups = groups,
                                             group_partitions = group_partitions)
      if (class_weight == TRUE) {
        case_weights_vctr <- metadata[training_inds] %>% ### removed filter(row.names()) and replaced with subsetting
          dplyr::count(!!rlang::sym(outcome_colname)) %>% # sym unquotes the colname, counts the number of rows for each group (e.g. control)
          dplyr::mutate(weight = n / sum(n)) %>% # adds a column called weight calculated as such
          dplyr::select(outcome_colname, weight) %>% # select just the outcome_colname and weight
          dplyr::right_join(metadata, by = outcome_colname) %>% # adds weight to the metadata using outcome_colname as joining point
          dplyr::pull(weight)
      }
    } else {
      training_inds <- training_frac
      training_frac <- length(training_inds) / nrow(dataset)
      message(paste0(
          "Using the custom training set indices provided by `training_frac`.
          The fraction of data in the training set will be ", round(training_frac, 2)))
    }


    # Check that training fractions and training indices falls within range of dataset
    ### @checks.R -> Function-13 and Function-14
    ### @utils.R -> Function-2
    check_training_frac(training_frac)
    check_training_indices(training_inds, dataset)

    # Get train and test data
    train_data <- dataset[training_inds, ]
    test_data <- dataset[-training_inds, ]

    # train_groups & test_groups will be NULL if groups is NULL
    train_groups <- groups[training_inds]
    test_groups <- groups[-training_inds]

    # subset metadata for train and test
    train_metadata <- metadata[training_inds, ]
    test_metadata <- metadata[-training_inds, ]


    # At this stage implement feature preprocessing
    ### @preprocess_features.R -> Function-1 to function-9
    message("Performing preprocessing of dataset.")
    filt_dataset <- preprocess_features(train_data = train_data, ### removed metadata argument
                                        test_data = test_data,
                                        outcome_colname = outcome_colname,
                                        preprocess_methods = preprocess_methods,
                                        corrFunList = corrFunList)
    train_data = filt_dataset[[1]]
    ### removed metadata = filt_dataset[[2]] (redundant)
    test_data = filt_dataset[[2]] ### added this line
    nzv_feat = filt_dataset[[3]] # removed by preprocessing (e.g. nzv) ### added this line
    grouped_feat = filt_dataset[[4]] # correlated features


    # Get type of predicting variable
    ### @performance.R -> Function-1
    outcome_type <- get_outcome_type(outcomes_vctr) # checks if outcome type is continous/binary/multiclass
    class_probs <- outcome_type != "continuous"


    # At this stage implement feature selection
    ### @filter_features.R -> Function-1 to function-6
    if (filter_features) {
      message("Performing feature selection.")
      filt_dataset <- filter_features_main(train_data = train_data,
                                           train_metadata = train_metadata,
                                           test_data = test_data,
                                           outcome_colname = outcome_colname,
                                           filterFunList = filterFunList,
                                           outcome_type = outcome_type)
      train_data = filt_dataset[[1]]
      test_data = filt_dataset[[2]]
      rem_feat = filt_dataset[[3]]
      filt_dataset[[3]] = c(filt_dataset[[3]], nzv_feat)
      filt_dataset[["Grouped_Features"]] = grouped_feat
    }


    message("Creating tune grid.")
    # Set hyper-parameter and tun-grid
    ### @hyperparameters.R -> Function-1 to Function-8
    if (is.null(hyperparameters) & !grepl("custom", method)) {
      hyperparameters <- get_hyperparams_list(train_data, method)
      tune_grid <- get_tuning_grid(hyperparameters, method) # produces a matrix of all possible combinations of parameters
    }
    else if (!is.null(hyperparameters) & !grepl("custom", method)) {
      tune_grid <- get_tuning_grid(hyperparameters, method)
    }
    else if (grepl("custom", method)) {
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


    message("Defining training params...")
    # Define cross-validation scheme and training parameters
    ### @cross_val_unnested.R -> Function-1 to Function-4
    if (is.null(cross_val)) {
      cross_val <- define_cv_unnested(
        resamp_method = resamp_method,
        train_data = train_data,
        train_metadata = train_metadata,
        outcome_colname = outcome_colname,
        hyperparams_list = hyperparameters,
        perf_metric_function = perf_metric_function,
        class_probs = class_probs,
        kfold = kfold,
        cv_times = cv_times,
        groups = train_groups,
        group_partitions = group_partitions,
        seed = seed
      )
    } else {
      cross_val = cross_val
    }


    message("Training the model...")
    # Train model now
    ### @train.R -> Function-1
    tic = Sys.time()
    doFuture::registerDoFuture()
    future::plan(future::multicore, workers = threads)
    trained_model_caret <- train_model(
      train_data = train_data,
      train_metadata = train_metadata,
      outcome_colname = outcome_colname,
      method = method,
      cv = cross_val,
      perf_metric_name = perf_metric_name,
      tune_grid = tune_grid,
      weights = if (class_weight) case_weights_vctr else NULL
    )
    message("Training completed.")
    toc = Sys.time()
    cat("Total time:", toc-tic, "\n")


    message("Calculating performance of test dataset.")
    # Calculate performance of the model on test data
    ### @performance.R -> Function-4 and Function-5
    performance_tbl <- get_performance_tbl(
      trained_model = trained_model_caret,
      test_data = test_data,
      test_metadata = test_metadata,
      outcome_colname = outcome_colname,
      perf_metric_function = perf_metric_function,
      perf_metric_name = perf_metric_name,
      class_probs = class_probs,
      method = method,
      seed = seed
    )


    message("Plotting ROC curve.")
    # Plot ROC curve
    ### @plot.R -> Function-1 and Function-2
    plot_out = plot_main(
      trained_model = trained_model_caret,
      test_data = test_data,
      test_metadata = test_metadata,
      outcome_colname = outcome_colname,
      method = method,
      outcome_type = outcome_type)


    # Feature importance analysis
    ### @feature_importance_endoR.R -> Function-1
    ### @feature_importance_permuted.R -> Function-1 to Function-6
    if (find_feature_importance) {
      if (feature_importance_method == "endoR") {
        message("Performing feature importance analysis using endoR")
        # Feature importance analysis and plot
        ### @feature_importance.R -> Function-1
        if (method == "rf" | method == "customrf") {
          model_type = "random forest"
        } else if (method == "xgbTree") {
          model_type = "xgboost"
        } else {
          model_type = NULL
        }
        if (!is.null(model_type)) {
          feat_imp = feature_importance_endoR(
            trained_model = trained_model_caret,
            method = model_type,
            train_data = train_data,
            train_metadata = train_metadata,
            outcome_colname = outcome_colname,
            jobs = threads
          )
        } else {
          message("Skipping feature importance analysis as ML method is not supported by endoR.")
          feat_imp = list("feature_importance_out" = "Skipped feature importance analysis",
                          "feature_importance_plot" = "Skipped feature importance plot")
        }
      }

      if (feature_importance_method == "permutation") {
        message("Performing feature importance analysis using permutation")
        # Feature importance analysis and plot
        ### @feature_importance.R -> Function-1
          feat_imp = with_progress(feature_importance_permuted(
            trained_model = trained_model_caret,
            test_data = test_data,
            test_metadata = test_metadata,
            outcome_colname = outcome_colname,
            perf_metric_function = perf_metric_function,
            perf_metric_name = perf_metric_name,
            performance_table = performance_tbl,
            class_probs = class_probs,
            method = method,
            grouped_features_list = NULL,
            seed = seed,
            nperms = 100,
            threads = threads))
      }
    } else {
      message("Skipping feature importance analysis.")
      feat_imp = list("feature_importance_out" = "Skipped feature importance analysis",
                      "feature_importance_plot" = "Skipped feature importance plot")
    }

    # return
    return(
        list(
          "NA_values" = na_vals,
          "dataset" = filt_dataset,
          "test_data" = test_data,
          "test_metadata" = test_metadata,
          "cross_val" = cross_val,
          "trained_model" = trained_model_caret,
          "performance" = performance_tbl,
          "roc_plot" = plot_out,
          "feature_importance" = feat_imp
        )
    )
  }
