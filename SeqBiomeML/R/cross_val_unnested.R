#### Function-1 ###
#' @noRd
#' Define cross-validation scheme and training parameters
define_cv_unnested <- function(resamp_method, train_data, train_metadata, outcome_colname, hyperparams_list, perf_metric_function, class_probs, kfold, cv_times, groups, group_partitions, seed) {
  # set seed
  set.seed(seed)
  # for cross-validation
  if(resamp_method == "cv") {
    if (keep_groups_in_cv_partitions(groups, group_partitions, kfold)) {
      cvIndex <- create_grouped_k_multifolds(
        resamp_method,
        groups,
        kfold = kfold,
        cv_times = cv_times
      )
      message("Groups will be kept together in CV partitions")
    } else {
      cvIndex <- caret::createFolds(
        factor(train_metadata %>% dplyr::pull(outcome_colname)),
        k = kfold,
        returnTrain = TRUE
      )
      message("Groups not provided for CV partitions")
    }
    message("Number of samples in training dataset range from ", paste0(round(range(sapply(cvIndex, length)/nrow(train_metadata)), 2), collapse = '-'))

    seeds <- get_seeds_trainControl_unnested(hyperparams_list=hyperparams_list, kfold=kfold, cv_times=1, ncol_train=ncol(train_data), nrow_train=1)
    cv <- caret::trainControl(
      method = resamp_method,
      number = kfold,
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
    if (keep_groups_in_cv_partitions(groups, group_partitions, kfold)) {
      cvIndex <- create_grouped_k_multifolds(
        resamp_method,
        groups,
        kfold = kfold,
        cv_times = cv_times
      )
      message("Groups will be kept together in CV partitions")
    } else {
      cvIndex <- caret::createMultiFolds(
        factor(train_metadata %>% dplyr::pull(outcome_colname)),
        kfold,
        times = cv_times
      )
      message("Groups not provided for repeatedcv partitions")
    }
    message("Number of samples in training dataset range from ", paste0(round(range(sapply(cvIndex, length)/nrow(train_metadata)), 2), collapse = '-'))

    seeds <- get_seeds_trainControl_unnested(hyperparams_list=hyperparams_list, kfold=kfold, cv_times=cv_times, ncol_train=ncol(train_data), nrow_train=1)
    cv <- caret::trainControl(
      method = resamp_method,
      number = kfold,
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
      seeds <- get_seeds_trainControl_unnested(hyperparams_list=hyperparams_list, kfold=1, cv_times=1, ncol_train=ncol(train_data), nrow_train=length(unique(groups)))
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
      seeds <- get_seeds_trainControl_unnested(hyperparams_list=hyperparams_list, kfold=1, cv_times=1, ncol_train=ncol(train_data), nrow_train=nrow(train_data))
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



### Function-2 ###
#' @noRd
#' Whether groups can be kept together in partitions during cross-validation
keep_groups_in_cv_partitions <- function(groups, group_partitions, kfold) {
  return(!is.null(groups) & ((is.null(group_partitions) & length(unique(groups)) >= kfold) | (length(group_partitions[["train"]]) >= kfold)))
}



### Function-3 ###
#' @noRd
#' Splitting into folds for cross-validation when using groups
create_grouped_k_multifolds <- function(resamp_method, groups, kfold = 10, cv_times = 5) {
  # for cross-validation
  if(resamp_method == "cv") {
    out <- caret::groupKFold(groups, k = kfold)
    if (any(sapply(out, length)) == 0) {
      stop("Could not split the data into muti-folds. This could mean you do not have enough samples or groups to perform an ML analysis using the groupsing functionality. Alternatively, you can try another seed, or decrease inner_fold.")
    }
  }

  # for repeated cross-validation
  if(resamp_method == "repeatedcv") {
    prettyNums <- paste("Rep", gsub(" ", "0", format(1:cv_times)),
                        sep = ""
    )
    for (i in 1:cv_times) {
      tmp <- caret::groupKFold(groups, k = kfold)
      names(tmp) <- paste("Fold", gsub(" ", "0", format(seq(along = tmp))),
                          ".", prettyNums[i],
                          sep = ""
      )
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



### Function-4 ###
#' @noRd
#' Get seeds for `caret::trainControl()`
get_seeds_trainControl_unnested <- function(hyperparams_list, kfold, cv_times, ncol_train, nrow_train) {
  seeds <- vector(mode = "list", length = kfold * cv_times * nrow_train + 1)
  sample_from <- ncol_train * 1000
  n_tuning_combos <- hyperparams_list %>%
    sapply(FUN = length) %>%
    prod()
  for (i in 1:(kfold * cv_times * nrow_train)) {
    seeds[[i]] <- sample.int(n = sample_from, size = n_tuning_combos)
  }
  ## For the last model:
  seeds[[kfold * cv_times * nrow_train + 1]] <- sample.int(n = sample_from, size = 1)
  return(seeds)
}
