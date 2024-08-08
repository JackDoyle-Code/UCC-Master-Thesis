#' Run the machine learning pipeline
#'
#' This function splits the data set into a train & test set,
#' trains machine learning (ML) models using k-fold cross-validation or repeated cross-validation or,
#' leave one out cross valication and it evaluates the best model on the held-out test set,
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

#' @param outer_folds Index number for creating outer folds (default: `NULL`).
#' @param resamp_method Resampling methods
#'   Options: `c("cv", "repeatedcv", "LOOCV")`.
#'   - cv: cross validation
#'   - repeatedcv: repeated cross validation
#'   - LOOCV: leave one out cross validation (default: `cv`)
#' @param filter_feature Pre-process features or not (default: `TRUE`).
#' @param n_outer_folds Fold number outer fold for nested cross validation (default: `5`).
#' @param n_inner_folds Fold number inner fold for nested cross validation (default: `5`).
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
#'
#' - `trained_model`: Output of [caret::train()], including the best model.
#' - `test_data`: Part of the data that was used for testing.
#' - `performance`: Data frame of performance metrics.
#' - 'roc_plot': ROC plot for best trained model and test data.
#' - `feature_importance`: list with output and plot for feature importance analysis.
#' @export
#'
#'
run_ml_nestedcv <-
  function(
    dataset,
    metadata,
    method = "rf",
    outcome_colname = NULL,
    group_colname = NULL,

    scale_method = list("clr", "pseudo", 1), ### added line
    preprocess_methods = c("zv", "nzv"), # can be c("zv", "nzv", "center", "scale")
    corrFunList = list(corr_method = "spearman", corr_thresh = 1, group_neg_corr = TRUE),
    filter_features = FALSE,
    filterFunList = list(test = "wilcoxon_filter", p_cutoff=0.05),

    outer_folds = NULL,
    resamp_method = "cv", # can be c(cv, repeatedcv, LOOCV)
    n_outer_folds = 5,
    n_inner_folds = 5,
    cv_times = 100,
    training_frac = 0.80,
    ### removed group_partitions
    hyperparameters = NULL,
    cross_val = NULL,
    perf_metric_function = NULL,
    perf_metric_name = NULL,
    class_weight = FALSE,

    find_feature_importance = FALSE,
    feature_importance_method = "permutation", # can be endoR or permutation

    jobs = 1,
    threads = 1,
    seed = NULL, ### changed default to NULL
    ...) {


    message("Performing various checks on datasets, metadata and supplied params")
    # Various checks
    ### @checks.R -> Function-1 and Function-12
    check_out <- check_all(
                  dataset = dataset,
                  metadata = metadata,
                  outcome_colname = outcome_colname,
                  method = method,
                  scale_method = scale_method, ### added this
                  preprocess_methods = preprocess_methods,
                  kfold = list(n_outer_folds, n_inner_folds), ### now checks the folds for both inner and outer
                  perf_metric_function = perf_metric_function,
                  perf_metric_name = perf_metric_name,
                  group_colname = group_colname,
                  ### removed group_partitions
                  seed = seed,
                  hyperparameters = hyperparameters,
                  feature_importance_method = feature_importance_method ### added this
                )

    ### added below
    # extracts check_all outputs
    outcome_colname <- check_out[[1]]
    na_vals <- check_out[[2]]

    # set seed if provided #
    set.seed(seed) ### removed if statement because NULL still works with set seed


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


    # Get type of predicting variable
    ### @performance.R -> Function-1
    outcome_type <- get_outcome_type(outcomes_vctr)
    class_probs <- outcome_type != "continuous"


    # Get indices to split dataset into train
    ### @define_folds.R -> Function-1 to Function-3
    if (is.null(outer_folds)) {
      message("Creating outer data folds.")
      outer_folds <- define_outer_folds(
        metadata = metadata,
        outcome_colname = outcome_colname,
        outer_fold = n_outer_folds,
        groups = groups,
        seed = seed)
    } else {
      message("Validating provided outer data folds")
      # check names of provided outer folds
      if (!all(names(outer_fold) %in% paste0("Fold", seq(1, n_outer_folds, 1)))) {
        stop("provide names of outer_folds with Fold{N}")
      }
      # check consistency between groups and outer folds provided
      if (!is.null(groups)) {
        if (any(validate_outer_folds_with_groups(groups, outer_folds))) {
          stop("groups and provided outer_folds are not consistent")
        }
      }
      message("Number of samples in training dataset in outer-loop range from ", paste0(round(range(sapply(outer_folds, length)/nrow(metadata)), 2), collapse = '-'))
      if (n_outer_folds != length(outer_folds)) message("Setting n_outer_folds")
      n_outer_folds <- length(outer_folds)
    }


    # Run Inner folds now
    ### @nestedcvCore.R -> Function-1 to Function-2
    ### @cross_val_unnested.R -> Function-1 to Function-3
    outer_res <- parallel::mclapply(seq_along(outer_folds), function(out_fold)
    tryCatch(
      {
      nestedcvCore(
        dataset = dataset,
        metadata = metadata,
        groups = groups,
        method = method,
        preprocess_methods = preprocess_methods,
        corrFunList = corrFunList,
        outcome_type = outcome_type,
        class_probs = class_probs,
        class_weight = class_weight,
        outcome_colname = outcome_colname,

        out_fold = out_fold,
        outer_folds = outer_folds,

        hyperparameters = hyperparameters,
        resamp_method = resamp_method,
        perf_metric_name = perf_metric_name,
        perf_metric_function = perf_metric_function,
        n_inner_folds = n_inner_folds,
        cv_times = cv_times,

        filter_features = filter_features,
        filterFunList = filterFunList, ### removed grouped_feat (redundant)

        threads = threads,
        seed = seed)
      }, error = function(e) print(e)
    ), mc.cores = jobs, mc.allow.recursive = FALSE)
    names(outer_res) <- paste0("Fold", seq(1, length(outer_res)))


    message("Fitting final single model using best tuned parameters")
    # Finalise bestTune params and Fit final model
    ### @performance.R -> Function-6
    bestTunes <- lapply(outer_res, function(i) i$trained_model$bestTune) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate("Fold" = paste0("Fold", row.names(.))) %>%
      tibble::column_to_rownames("Fold")
    finalTune <- finaliseTune(bestTunes) # selects the optimal hyperparameters
    message("Final tuned parameter(s) are...")
    print(finalTune, digits = 2L, print.gap = 2L, row.names = FALSE)


    # Fit final single model using optimized parameters
    fitControl <- caret::trainControl(
      method = "none", number = NA, repeats = NA,
      index = NULL, classProbs = class_probs,
      savePredictions = TRUE, seeds = seed
    )

    final_fit <- caret::train(
      x = dataset,
      y = (metadata %>% dplyr::pull(outcome_colname)),
      method = if(!grepl("custom", method)) method else custom_method_grid(method),
      trControl = fitControl,
      tuneGrid = finalTune
    )

    # Feature importance analysis
    ### @feature_importance_endoR.R -> Function-1
    ### @feature_importance_permuted.R -> Function-1 to Function-6
    if (find_feature_importance) {
      # if (feature_importance_method == "endoR") {
      #   message("Performing feature importance analysis using endoR")
      #   # Feature importance analysis and plot
      #   ### @feature_importance_endoR.R -> Function-1
      #   if (method == "rf" | method == "customrf") {
      #     model_type = "random forest"
      #   } else if (method == "xgbTree") {
      #     model_type = "xgboost"
      #   } else {
      #     model_type = NULL
      #   }
      #   if (!is.null(model_type)) {
      #     feat_imp = feature_importance_endoR(
      #       trained_model = final_fit,
      #       method = model_type,
      #       train_data = dataset,
      #       train_metadata = metadata,
      #       outcome_colname = outcome_colname,
      #       jobs = threads
      #     )
      #   } else {
      #     message("Skipping feature importance analysis as ML method is not supported by endoR.")
      #     feat_imp = list("feature_importance_out" = "Skipped feature importance analysis",
      #                     "feature_importance_plot" = "Skipped feature importance plot")
      #   }
      # }

      message("Performing feature importance analysis using permutation")
      # Feature importance analysis and plot
      ### @feature_importance_permuted.R -> Function-1 to Function-6
        feat_imp = feature_importance_main(
          trained_model = final_fit,
          test_data = dataset,
          test_metadata = metadata,
          outcome_colname = outcome_colname,
          perf_metric_function = perf_metric_function,
          perf_metric_name = perf_metric_name,
          class_probs = class_probs,
          method = method,
          grouped_features_list = NULL,
          seed = seed,
          nperms = 100,
          threads = threads)
    } else {
      message("Skipping feature importance analysis.")
      feat_imp = list("feature_importance_out" = "Skipped feature importance analysis",
      "feature_importance_plot" = "Skipped feature importance plot")
    }


    # return
    return(
      list(
        "outer_result" = outer_res,
        "outer_folds" = outer_folds,
        "finalTune" = finalTune,
        "final_model" = final_fit,
        "feature_importance" = feat_imp
      )
    )
  }
