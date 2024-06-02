### Function-1 ###
#' @noRd
#' Select indices to partition the data into training & testing sets.
get_partition_indices <- function(outcomes,
                                  training_frac = 0.8,
                                  groups = NULL,
                                  group_partitions = NULL) {
  if (is.null(groups)) {
    training_inds <- caret::createDataPartition(outcomes,
      p = training_frac,
      list = FALSE
    ) %>% .[, 1]
  } else {
    training_inds <-
      create_grouped_data_partition(groups,
        group_partitions = group_partitions,
        training_frac = training_frac
      )
  }
  return(training_inds)
}


### Function-2 ###
#' @noRd
#' split by groups
#' Split into train and test set while splitting by groups.
#' When `group_partitions` is `NULL`, all samples from each group will go into
#' either the training set or the testing set.
#' Otherwise, the groups will be split according to `group_partitions`
create_grouped_data_partition <- function(groups, group_partitions = NULL, training_frac = 0.8) {
  # get indices
  indices <- seq(along = groups)
  # get unique groups
  unique_groups <- unlist(unique(groups))
  if (is.null(group_partitions)) {
    # initialize train groups & set
    train_grps <- unique_groups
    train_set <- indices
    train_set_grp <- groups
    # initialize fraction of data in train set
    frac_in_train <- length(train_set) / length(indices)
    # keep removing data from train set until fraction in train set <= p (e.g. 0.8)
    while (frac_in_train > training_frac) {
      # randomly choose a groups
      grp <- sample(train_grps, size = 1)
      # remove groups from train groups & set
      train_grps <- train_grps[train_grps != grp]
      train_set <- train_set[train_set_grp != grp]
      train_set_grp <- train_set_grp[train_set_grp != grp]
      # calculate fraction of data in train set
      frac_in_train <- length(train_set) / length(indices)
    }
  } else {
    in_train_only <- setdiff(group_partitions$train, group_partitions$test)
    in_test_only <- setdiff(group_partitions$test, group_partitions$train)
    in_both <- intersect(group_partitions$test, group_partitions$train)
    in_neither <- setdiff(unique_groups, union(group_partitions$test, group_partitions$train))

    # initialize train & test sets with samples that must be in one or the other
    train_set <- indices[groups %in% in_train_only]
    test_set <- indices[groups %in% in_test_only]

    # sample from remaining samples to reach target training fraction
    remaining <- indices[-c(train_set, test_set)]
    if (length(remaining) > 0) {
      num_needed <- round(training_frac * length(indices) - length(train_set))
      if (num_needed > length(remaining)) {
        num_needed <- length(remaining)
      }
      if (num_needed > 0) {
        more_train_samples <- indices[sample(remaining, size = num_needed)]
        train_set <- c(train_set, more_train_samples)
      }
    }
  }

  frac_in_train <- length(train_set) / length(indices)
  message(paste(
    "Fraction of data in the training set:",
    round(frac_in_train, 3),
    "\n\tGroups in the training set:",
    paste(groups[train_set] %>% sort() %>% unique(), collapse = " "),
    "\n\tGroups in the testing set:",
    paste(groups[-train_set] %>% sort() %>% unique(), collapse = " ")
  ))

  return(train_set)
}
