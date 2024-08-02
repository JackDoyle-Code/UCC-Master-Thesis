### Function-1 ###
#' @keywords internal
#' @noRd
#' Check all params that don't return a value
check_all <- function(dataset, metadata, outcome_colname, method, scale_method, preprocess_methods,
                      kfold, perf_metric_function, perf_metric_name, group_colname,
                      group_partitions, seed, hyperparameters, feature_importance_method) {

  check_seed(seed)
  check_method(method, hyperparameters)
  check_scale_method(scale_method)
  check_preprocess_methods(preprocess_methods)
  check_dataset(dataset)
  outcome_colname <- check_outcome_column(metadata, outcome_colname)
  na_outcome_vals <- check_outcome_value(metadata, outcome_colname)
  #check_cat_feats(dataset)
  check_kfold(kfold, dataset)
  na_group_vals <- check_groups(metadata, group_colname)
  check_group_partitions(metadata, group_colname, group_partitions)
  check_perf_metric_function(perf_metric_function)
  check_perf_metric_name(perf_metric_name)
  check_feature_importance(feature_importance_method, method)
  na_vals <- rbind(na_outcome_vals, na_group_vals)
  return(list(outcome_colname, na_vals))
}


### Function-2 ###
#' @inheritParams run_ml
#' check that the seed is either NA or a number
check_seed <- function(seed) {
  if (!is.null(seed) & !is.numeric(seed)) { ### changed seed to is.null instead of is.na (null still works with seed but doesn't set a seed)
    stop(paste0(
      "`seed` must be `NULL` or numeric.\n",
      "    You provided: ", seed
    ))
  }
}


### Function-3 ###
#' @noRd
#' Check if the method is supported. If not, throws error.
check_method <- function(method, hyperparameters) {
  methods <- c("glmnet", "svmRadial", "rpart2", "rf", "customrf", "xgbTree", "kknn") ### added kknn
  if (!(method %in% methods) & is.null(hyperparameters)) {
    stop(paste0("Method ", method, " is not supported!!! "
    ))
  }
}


### Function-4 ###
#' @noRd
#' Check that the dataset is not empty and has more than 1 column.
check_dataset <- function(dataset) {
  if (!any(class(dataset) == "data.frame")) {
    stop(paste("The dataset must be a `data.frame` or `tibble`, but you supplied:", class(dataset)))
  }
  if (nrow(dataset) == 0) {
    stop("No rows detected in dataset.")
  }
  if (ncol(dataset) <= 1) {
    stop(
      "1 or fewer columns detected in dataset. There should be an outcome column and at least one feature column."
    )
  }
}


### Function-5 ###
#' @noRd
#' Check that outcome column exists. ### Removed untrue line here
check_outcome_column <- function(metadata, outcome_colname) { ### Removed check_values and show_message (redundant)
  ### added an extra condition to the below if statment
  if (!is.null(outcome_colname) && !outcome_colname %in% colnames(metadata)) {
    stop(paste0("Outcome '", outcome_colname, "' not in column names of metadata."))
  }
  ### added the below if statement
  if (is.null(outcome_colname)){
    outcome_colname <- colnames(metadata)[1]
  }
  ### Removed redundant if statements
  ### removed check_outcome_value (redundant since it's already called in check_all)
  message(paste0("Using '", outcome_colname, "' as the outcome column."))
  return(outcome_colname)
}


### Function-6 ###
#' @noRd
#' Check that the outcome variable is valid. Pick outcome value if necessary.
check_outcome_value <- function(metadata, outcome_colname) {
  # check no NA's
  outcomes_vec <- metadata[[outcome_colname]] ### Replaced dplyr::pull with [[]]
  num_missing <- sum(is.na(outcomes_vec))
  na_ind <- NA
  na_rows <- NA
  if (num_missing != 0) {
    na_ind <- which(is.na(outcomes_vec))
    na_rows <- metadata[na_ind, ]
    message("There are a total of ", num_missing, " row(s), which have been removed from the metadata")
  } ### replaced stop message with the three lines above instead

  ### added the below line
  if (!is.na(na_ind)){
    outcomes_vec <- outcomes_vec[-na_ind] # removes NA values which interrupt further checks
  }
  # check for empty strings
  num_empty <- sum(outcomes_vec == "")
  if (num_empty != 0) {
    warning(paste0("Possible missing data in the output variable: ", num_empty, " empty value(s)."))
  }
  # check if continuous outcome
  isnum <- is.numeric(outcomes_vec)
  if (isnum) {
    # check if it might actually be categorical
    if (all(floor(outcomes_vec) == outcomes_vec)) {
      warning("Data is being considered numeric, but all outcome values are integers. If you meant to code your values as categorical, please use character values.")
    }
  }

  # check binary and multiclass outcome
  outcomes <- unique(outcomes_vec) ### removed unnecessary piping
  num_outcomes <- length(outcomes)
  if (num_outcomes < 2) {
    stop(paste0("A binary or multi-class outcome variable is required, but this dataset has ",
        num_outcomes, " outcome(s): ", paste(outcomes, collapse = ", ")))
  }
  return(na_rows) ### added this line
  # it returns the rows of the metadata that contained NAs
}


### Function-7 ###
#' @noRd
#' Check if any features are categorical
#check_cat_feats <- function(dataset) {
#  if (any(sapply(dataset, class) %in% c("factor", "character"))) {
#    stop("You have categorical features in your dataset. Please check the data and apply get_caret_dummyvars_df().")
#  }
#}


### changed the below function so it works for nestedcv
### Function-8 ###
#' @noRd
#' Check that kfold is an integer of reasonable size (for both n_outer_folds and n_inner_folds)
check_kfold <- function(kfold, dataset) {
  if (length(kfold) > 1) {
    sapply(kfold, function(x) help_kfold(x, dataset))
  }
  else {
    help_kfold(kfold, dataset)
  }
}


### Function-9 ###
#' @noRd
#' Check grouping vector
check_groups <- function(metadata, group_colname) {
  # check that groups is a vector or NULL
  if (is.null(group_colname)) {
    groups = NULL
  } else {
    groups = metadata[[group_colname]] ### replaced dplyr pull
  }
  isvec <- is.vector(groups)
  isnull <- is.null(groups)
  if (!(isvec | isnull)) {
    stop(paste0("group should be either a vector or NULL, but group is class ", class(groups), "."))
  }
  # if group is a vector, check that it's the correct length
  if (isvec) {
    ndat <- nrow(metadata)
    ngrp <- length(groups)
    if (ndat != ngrp) {
      stop(paste0("group should be a vector that is the same length as the number of rows in the dataset (", ndat, "), but it is of length ", ngrp, "."))
    }
    # check that there are no NAs in group
    nas <- is.na(groups)
    nnas <- sum(nas)
    na_ind <- NA
    na_rows <- NA
    ### adapted the below if statement similarly to how check_outcome_values was changed
    if (any(nas)) {
      na_ind <- which(nas)
      na_rows <- metadata[na_ind, ]
      message("There are a total of ", nnas, " row(s), which have been removed from the metadata")
    }
    ### added the below line
    if (!is.na(na_ind)) {
      groups <- groups[-na_ind]
    }
    # check that there is more than 1 group
    ngrp <- length(unique(groups))
    if (ngrp < 2) {
      stop(paste0("The total number of groups should be greater than 1. If all samples are from the same group, use `group=NULL`"))
    return(na_rows)
    }
  }
}


### Function-10 ###
#' @noRd
#' Check the validity of the group_partitions list
check_group_partitions <- function(metadata, group_colname, group_partitions) {
  if (is.null(group_partitions)) {
    return()
  }
  groups = metadata[[group_colname]] ### replaced dplyr pull

  names_unrecognized <- names(group_partitions[!names(group_partitions) %in% c("train", "test")])
  if (length(names_unrecognized) > 0) {
    stop(paste("Unrecognized name(s) in `group_partitions`:",
      paste(names_unrecognized, collapse = " ")))
  }
  groups_unrecognized <- setdiff(unlist(group_partitions), unique(groups)) %>% sort() # checks the difference between all groups in test/train and unique(groups)
  if (length(groups_unrecognized) > 0) {
    stop(paste("`group_partitions` contains group names not in groups vector:",
      paste(groups_unrecognized, collapse = " ")))
  }
}


### Function-11 ###
#' @noRd
#' Check perf_metric_function is NULL or a function
check_perf_metric_function <- function(perf_metric_function) {
  if (!is.function(perf_metric_function) & !is.null(perf_metric_function)) {
    stop(paste0("`perf_metric_function` must be `NULL` or a function.\n    You provided: ", class(perf_metric_function)))
  }
}


### Function-12 ###
#' @noRd
#' Check perf_metric_name is NULL or a function
check_perf_metric_name <- function(perf_metric_name) {
  if (!is.character(perf_metric_name) & !is.null(perf_metric_name)) {
    stop(paste0("`perf_metric_name` must be `NULL` or a character\n    You provided: ", perf_metric_name))
  }
}




### Function-13 ###
#' @noRd
#' Check that the training fraction is between 0 and 1
check_training_frac <- function(frac) {
  if (!is.numeric(frac) | (frac <= 0 | frac >= 1)) {
    stop(paste0(
      "`training_frac` must be a numeric between 0 and 1.\n",
      "    You provided: ", frac
    ))
  } else if (frac < 0.5) {
    warning("`training_frac` is less than 0.5. The training set will be smaller than the testing set.")
  }
}


### Function-14 ###
#' @noRd
#' Check the validity of the training indices
check_training_indices <- function(training_inds, dataset) {
  if (!all(is_whole_number(training_inds))) {
    warning(
      "The training indices vector contains non-integer numbers."
    )
  }
  stop_msg <- character(0)
  if (min(training_inds) < 1) {
    stop_msg <- paste0(stop_msg,
      "The training indices vector contains a value less than 1.",
      sep = "\n"
    )
  }
  if (max(training_inds) > nrow(dataset)) {
    stop_msg <- paste0(stop_msg,
      "The training indices vector contains a value that is too large for the number of rows in the dataset.",
      sep = "\n"
    )
  }
  if (length(training_inds) >= nrow(dataset)) {
    stop_msg <- paste0(stop_msg,
      "The training indices vector contains too many values for the size of the dataset.",
      sep = "\n"
    )
  }
  if (length(stop_msg) > 0) {
    stop(stop_msg)
  }
}


### Function-15 ###
#' @noRd
#' Check preprocess methods
check_preprocess_methods <- function(preprocess_methods) {
  if (!is.null(preprocess_methods)) {
    if (!all(preprocess_methods %in% c("zv", "nzv", "center", "scale"))) {
      stop(paste0("`preprocess_methods` must be one of: NULL, 'zv', 'nzv', 'center', 'scale'. You provided ", paste0(preprocess_methods, collapse = "  ")))
    }
  }
}

### Function-16 ###
#' @noRd
#' Check scale methods
check_scale_method <- function(scale_method) {
  if (!is.null(scale_method)) {
    if (!all(scale_method[[1]] %in% c("clr", "rclr", "tss", NA))) {
      stop(paste0("invalid scale method, must be one of: NULL, 'clr', 'rclr', 'tss'. You provided ", paste0(scale_method[[1]], collapse = "  ")))
    }
    if (!all(scale_method[[2]] %in% c("pseudo", "GBM", "BL", "SQ", "CZM", NA))) {
      stop(paste0("invalid imputation method, must be one of: NULL, 'pseudo', 'GBM', 'BL', 'SQ', 'CZM'. You provided ", paste0(scale_method[[2]], collapse = "  ")))
    }
    if (!scale_method[[3]] == "min" && !as.numeric(scale_method[[3]]) | length(scale_method[[3]]) != 1) {
      stop(paste0("imputation value must be 'min' or numeric and of length 1. You provided ", paste0(scale_method[[3]], collapse = "  ")))
    }
  }
}


### Function-17 ###
#' @noRd
#' Check feature importance methods
check_feature_importance <- function(feature_importance_method, method) {
  if (!all(feature_importance_method %in% c("permutation", "embedded"))) {
    stop(paste0(
      "Feature Importance method must be either permutation and/or embedded",
      "    You provided: ", feature_importance_method
    ))
  }
  if ("embedded" %in% feature_importance_method) {
    if (!method %in% c("glmnet", "rpart2", "xgbTree", "rf")) {
      stop(paste0(
        "Embedded feature importance is only compatible with the following models: glmnet, rpart2, xgbTree, rf",
        "    You provided: ", method
      ))
    }
  }
}


### added the below function so that check_kfold works with nestedcv
### Function-18 ###
#' @noRd
#' Helper function for checking valid kfold
help_kfold <- function(kfold, dataset) {
  not_a_number <- !is.integer(kfold) & !is.numeric(kfold)
  not_an_int <- kfold != as.integer(kfold)
  nfeats <- ncol(dataset)
  if (not_a_number | not_an_int | kfold <= 1) {
    stop(paste0("`kfold` must be an integer between 1 and the number of features in the data.\n",
                "  You provided ", kfold, " folds and your dataset has ", nfeats, " features."))
  }
  if (nrow(dataset) < 1000) {
    warning("Sample size is < 1000 in length. Using k-fold cv may produce biased results. Consider using alternative method")
  }
}
