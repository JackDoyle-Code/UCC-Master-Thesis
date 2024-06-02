### Function-1 ###
#' @keywords internal
#' @noRd
#' Check all params that don't return a value
check_all <- function(dataset, metadata, outcome_colname, method, preprocess_methods,
                      kfold, perf_metric_function, perf_metric_name, group_colname,
                      group_partitions, seed, hyperparameters) {

  check_seed(seed)
  check_method(method, hyperparameters)
  check_preprocess_methods(preprocess_methods)
  check_dataset(dataset)
  check_outcome_column(metadata, outcome_colname)
  check_outcome_value(metadata, outcome_colname)
  check_cat_feats(dataset)
  check_kfold(kfold, dataset)
  check_groups(metadata, group_colname)
  check_group_partitions(metadata, group_colname, group_partitions)
  check_perf_metric_function(perf_metric_function)
  check_perf_metric_name(perf_metric_name)
}


### Function-2 ###
#' @inheritParams run_ml
#' check that the seed is either NA or a number
check_seed <- function(seed) {
  if (!is.na(seed) & !is.numeric(seed)) {
    stop(paste0(
      "`seed` must be `NA` or numeric.\n",
      "    You provided: ", seed
    ))
  }
}


### Function-3 ###
#' @noRd
#' Check if the method is supported. If not, throws error.
check_method <- function(method, hyperparameters) {
  methods <- c("glmnet", "svmRadial", "rpart2", "rf", "customrf", "xgbTree")
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
#' Check that outcome column exists. Pick outcome column if not specified.
check_outcome_column <- function(metadata, outcome_colname, check_values = TRUE, show_message = TRUE) {
  # If no outcome colname specified stop
  if (!outcome_colname %in% colnames(metadata)) {
    stop(paste0("Outcome '", outcome_colname, "' not in column names of metadata."))
  }

  if (check_values) check_outcome_value(metadata, outcome_colname)

  if (show_message) {
    message(
      paste0(
        "Using '",
        outcome_colname,
        "' as the outcome column."
      )
    )
  }

  return(outcome_colname)
}


### Function-6 ###
#' @noRd
#' Check that the outcome variable is valid. Pick outcome value if necessary.
check_outcome_value <- function(metadata, outcome_colname) {
  # check no NA's
  outcomes_vec <- metadata %>% dplyr::pull(outcome_colname)
  num_missing <- sum(is.na(outcomes_vec))
  if (num_missing != 0) {
    stop(paste0("Missing data in the output variable is not allowed, but the outcome variable has ", num_missing, " missing value(s) (NA)."))
  }

  # check for empty strings
  num_empty <- sum(outcomes_vec == "")
  if (num_empty != 0) {
    warning(paste0("Possible missing data in the output variable: ", num_empty, " empty value(s)."))
  }

  outcomes_all <- metadata %>% dplyr::pull(outcome_colname)

  # check if continuous outcome
  isnum <- is.numeric(outcomes_all)
  if (isnum) {
    # check if it might actually be categorical
    if (all(floor(outcomes_all) == outcomes_all)) {
      warning("Data is being considered numeric, but all outcome values are integers. If you meant to code your values as categorical, please use character values.")
    }
  }

  # check binary and multiclass outcome
  outcomes <- outcomes_all %>% unique()
  num_outcomes <- length(outcomes)
  if (num_outcomes < 2) {
    stop(
      paste0(
        "A binary or multi-class outcome variable is required, but this dataset has ",
        num_outcomes, " outcome(s): ", paste(outcomes, collapse = ", ")
      )
    )
  }
}


### Function-7 ###
#' @noRd
#' Check if any features are categorical
check_cat_feats <- function(dataset) {
  if (any(sapply(dataset, class) %in% c("factor", "character"))) {
    stop("You have categorical features in your dataset. Please check the data and apply get_caret_dummyvars_df().")
  }
}


### Function-8 ###
#' @noRd
#' Check that kfold is an integer of reasonable size
check_kfold <- function(kfold, dataset) {
  not_a_number <- !is.integer(kfold) & !is.numeric(kfold)
  not_an_int <- kfold != as.integer(kfold)
  nfeats <- ncol(dataset)
  out_of_range <- (kfold <= 1) | (kfold > nfeats)
  if (not_a_number | not_an_int | out_of_range) {
    stop(paste0(
      "`kfold` must be an integer between 1 and the number of features in the data.\n",
      "  You provided ", kfold, " folds and your dataset has ", nfeats, " features."
    ))
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
    groups = metadata %>% dplyr::pull(group_colname)
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
    if (any(nas)) {
      stop(paste0("No NA values are allowed in group, but ", nnas, " NA(s) are present."))
    }
    # check that there is more than 1 group
    ngrp <- length(unique(groups))
    if (ngrp < 2) {
      stop(paste0("The total number of groups should be greater than 1. If all samples are from the same group, use `group=NULL`"))
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
  groups = metadata %>% dplyr::pull(group_colname)

  names_unrecognized <-
    names(group_partitions[!names(group_partitions) %in% c("train", "test")])
  if (length(names_unrecognized) > 0) {
    stop(paste(
      "Unrecognized name(s) in `group_partitions`:",
      paste(names_unrecognized, collapse = " ")
    ))
  }
  groups_unrecognized <- setdiff(
    group_partitions %>% unlist(),
    groups %>% unique()
  ) %>% sort()
  if (length(groups_unrecognized) > 0) {
    stop(paste(
      "`group_partitions` contains group names not in groups vector:",
      paste(groups_unrecognized, collapse = " ")
    ))
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
    if (!all((preprocess_methods %in% c("zv", "nzv", "center", "scale")))) {
      stop(paste0("`preprocess_methods` must be one of: NULL, 'zv', 'nzv', 'center', 'scale'. You provided ", paste0(preprocess_methods, collapse = "  ")))
    }
  }
}
