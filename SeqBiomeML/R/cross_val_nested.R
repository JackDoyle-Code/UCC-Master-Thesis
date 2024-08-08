# TODO:- Define N Outer_fold and then continue from here
# see define_folds.R for outer folds partition

### Function-1 ###
#' @noRd
#' Define cross-validation scheme and training parameters for nested analysis
define_cv_nested <- function(resamp_method, train_data, train_metadata, outcome_colname, hyperparams_list, perf_metric_function, class_probs, inner_fold, cv_times, groups, seed) {
  # set seed
  set.seed(seed)
  # for cross-validation
  if(resamp_method == "cv") {
    if (!is.null(groups)) {
      cvIndex <- create_grouped_k_innerfolds(
        resamp_method,
        groups,
        inner_fold = inner_fold
      )
      message("Groups will be kept together in CV partitions")
    } else {
      cvIndex <- caret::createFolds(
        factor(train_metadata %>% dplyr::pull(outcome_colname)),
        k = inner_fold,
        returnTrain = TRUE
      )
      message("Groups not provided for inner fold partitions")
    }
    message("Number of samples in training dataset in inner-loop range from ", paste0(round(range(sapply(cvIndex, length)/nrow(train_metadata)), 2), collapse = '-'))

    seeds <- get_seeds_trainControl_nested(hyperparams_list=hyperparams_list, inner_fold=inner_fold, cv_times=1, ncol_train=ncol(train_data), nrow_train=1)
    cv <- caret::trainControl(
      method = resamp_method,
      number = inner_fold,
      repeats = NA,
      index = cvIndex,
      returnResamp = "final",
      classProbs = class_probs,
      summaryFunction = perf_metric_function,
      indexFinal = NULL,
      savePredictions = TRUE,
      seeds = seeds
    )
  }

  # for repeated cross-validation
  if(resamp_method == "repeatedcv") {
    if (!is.null(groups)) {
      cvIndex <- create_grouped_k_innerfolds(
        resamp_method,
        groups,
        inner_fold = inner_fold,
        cv_times = cv_times
      )
      message("Groups will be kept together in CV partitions")
    } else {
      cvIndex <- caret::createFolds(
        factor(train_metadata %>% dplyr::pull(outcome_colname)),
        k = inner_fold,
        returnTrain = TRUE
      )
      message("Groups not provided for inner fold partitions")
    }
    message("Number of samples in training dataset in inner-loop range from ", paste0(round(range(sapply(cvIndex, length)/nrow(train_metadata)), 2), collapse = '-'))

    seeds <- get_seeds_trainControl_nested(hyperparams_list=hyperparams_list, inner_fold=inner_fold, cv_times=cv_times, ncol_train=ncol(train_data), nrow_train=1)
    cv <- caret::trainControl(
      method = resamp_method,
      number = inner_fold,
      repeats = cv_times,
      index = cvIndex,
      returnResamp = "final",
      classProbs = class_probs,
      summaryFunction = perf_metric_function,
      indexFinal = NULL,
      savePredictions = TRUE,
      seeds = seeds
    )
  }

  # for loocv
  if(resamp_method == "LOOCV") {
    if (!is.null(groups)) {
      ### Get index in and index out
      index_test <- vector(mode = "list", length(unique(groups)))
      index_train <- vector(mode = "list", length(unique(groups)))
      j=0
      for (i in unique(groups)) {
        j = (j + 1)
        index_test[[j]] <- which(groups %in% i)
        index_train[[j]] <- which(!groups %in% i)
      }
      message("Groups will be kept together in fold partitions")
      seeds <- get_seeds_trainControl_nested(hyperparams_list=hyperparams_list, inner_fold=1, cv_times=1, ncol_train=ncol(train_data), nrow_train=length(unique(groups)))
    } else {
      ### Get index in and index out
      index_test <- vector(mode = "list", length(row.names(train_data)))
      index_train <- vector(mode = "list", length(row.names(train_data)))
      j=0
      for (i in row.names(train_data)) {
        j = (j + 1)
        index_test[[j]] <- which(row.names(train_data) %in% i)
        index_train[[j]] <- which(!row.names(train_data) %in% i)
      }
      message("Groups not provided for inner fold partitions")
      seeds <- get_seeds_trainControl_nested(hyperparams_list=hyperparams_list, inner_fold=1, cv_times=1, ncol_train=ncol(train_data), nrow_train=nrow(train_data))
    }

    cv <- caret::trainControl(
      method = resamp_method,
      number = NA,
      repeats = NA,
      index = index_train,
      indexOut = index_test,
      returnResamp = "final",
      classProbs = class_probs,
      summaryFunction = perf_metric_function,
      indexFinal = NULL,
      savePredictions = TRUE,
      seeds = seeds
    )
  }
  return(cv)
}

### replaced the previous innerfolds function with this
### Function-2 ###
#' @noRd
#' Splitting into folds for cross-validation when using groups
create_grouped_k_innerfolds <- function(resamp_method, groups, inner_fold = 10, cv_times = 0) {
  # for cross-validation
  if(resamp_method == "cv") {
    out <- caret::groupKFold(groups, k = inner_fold)
    if (any(sapply(out, length)) == 0) { # checks if any of the lengths of the folds are == 0
      stop("Could not split the data into muti-folds. This could mean you do not have enough samples or groups to perform an ML analysis using the groupsing functionality. Alternatively, you can try another seed, or decrease inner_fold.")
    }
  }

  # for repeated cross-validation
  if(resamp_method == "repeatedcv") {
    prettyNums <- paste("Rep", gsub(" ", "0", format(1:cv_times)),
                        sep = "")
    for (i in 1:cv_times) {
      tmp <- caret::groupKFold(groups, k = inner_fold)
      names(tmp) <- paste("Fold", gsub(" ", "0", format(seq(along = tmp))),
                          ".", prettyNums[i],
                          sep = "")
      out <- if (i == 1) {
        tmp
      } else {
        c(out, tmp)
      }
    }

    if (any(sapply(out, length)) == 0) {
      stop("Could not split the data into train and validate folds. This could mean you do not have enough samples or groups to perform an ML analysis using the groupsing functionality. Alternatively, you can try another seed, or decrease kfold or cv_times.")
    }
  }
  return(out)
}


### Function-3 ###
#' @noRd
#' Get seeds for `caret::trainControl()`
get_seeds_trainControl_nested <- function(hyperparams_list, inner_fold, cv_times, ncol_train, nrow_train) {
  seeds <- vector(mode = "list", length = inner_fold * cv_times * nrow_train + 1)
  sample_from <- ncol_train * 1000
  n_tuning_combos <- sapply(hyperparams_list, FUN = length) %>% prod() ### Shortened the line by removing one piping
  for (i in 1:(inner_fold * cv_times * nrow_train)) {
    seeds[[i]] <- sample.int(n = sample_from, size = n_tuning_combos)
  }
  ## For the last model:
  seeds[[inner_fold * cv_times * nrow_train + 1]] <- sample.int(n = sample_from, size = 1)
  return(seeds)
}
