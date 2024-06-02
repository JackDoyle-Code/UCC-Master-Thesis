### Function-1 ###
#' @noRd
#' Define cross-validation outer loop (for nested cross-validation)
define_outer_folds <- function(metadata, outcome_colname, outer_fold, groups, seed) {
  # set seed
  set.seed(seed)
  # define outer folds
  if (!is.null(groups)) {
    cvIndex <- create_grouped_k_outerfolds(groups,
      outer_fold = outer_fold
    )
    message("Groups will be kept together in Outer fold partitions")
    message("Number of samples in training dataset in outer-loop range from ", paste0(round(range(sapply(cvIndex, length)/nrow(metadata)), 2), collapse = '-'))
  } else {
    cvIndex <- caret::createFolds(
      factor(metadata %>% dplyr::pull(outcome_colname)),
      k = outer_fold,
      returnTrain = TRUE
    )
      message("Groups not provided for Outer fold partitions")
      message("Number of samples in training dataset in outer-loop range from ", paste0(round(range(sapply(cvIndex, length)/nrow(metadata)), 2), collapse = '-'))
  }
  return(cvIndex)
}


### Function-2 ###
#' @noRd
#' Splitting into folds
create_grouped_k_outerfolds <- function(groups, outer_fold = 10) {
  out <- caret::groupKFold(groups, k = outer_fold)
  if (any(sapply(out, length)) == 0) {
    stop("Could not split the data into muti-folds. This could mean you do not have enough samples or groups to perform an ML analysis using the groupsing functionality. Alternatively, you can try another seed, or decrease outer_fold.")
  }
  return(out)
}

swapFoldIndex <- function(folds, len = max(unlist(folds))) {
  lapply(folds, function(i) setdiff(seq_len(len), i))
}


### Function-3 ###
#' @noRd
#' Make sure groups and outer folds are consistent
validate_outer_folds_with_groups <- function(groups, outer_folds) {
  return(sapply(1:length(outer_folds), function(i) any(unique(groups[outer_folds[[i]]]) %in% unique(groups[setdiff(seq(1, length(groups)), outer_folds[[i]])]))))
}
