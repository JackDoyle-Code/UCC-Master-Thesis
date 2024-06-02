#' Function-1
#' @noRd
#' Preprocess data prior to running machine learning
#' @export
#'
#'
preprocess_features <- function(
  train_data, train_metadata, outcome_colname,
  preprocess_methods = c("zv", "nzv", "center", "scale"),
  corrFunList = corrFunList) {

  # remove zero or near zero variance features and center/ scale data
  preprocess_data = train_data %>%
    change_to_num() %>%
    get_caret_processed_df(method = preprocess_methods)

  # remove nearly or perfectly correlated features
  train_data = collapse_correlated_features(
    features = preprocess_data[[1]],
    corr_thresh = corrFunList$corr_thresh,
    corr_method = corrFunList$corr_method,
    group_neg_corr = corrFunList$group_neg_corr)

    if (!is.null(train_data[[2]])) {
      grouped_feat = utils::stack(train_data[[2]]) %>%
      dplyr::select(ind, values) %>%
      dplyr::rename("feat_group"=ind, "feat_names"=values)
      message("A total of N=",
        length(strsplit(paste0(grouped_feat$feat_names[grepl("\\|", grouped_feat$feat_names)], collapse = "\\|"), "\\|")[[1]]),
        " predictors could be grouped into N=", length(grouped_feat$feat_names[grepl("\\|", grouped_feat$feat_names)]), " groups")
    } else {
      grouped_feat = NULL
      message("None of the predictors could be grouped")
    }

  # return now
  return(list(
    filt_data = train_data[[1]],
    filt_metadata = train_metadata,
    final_feat = colnames(train_data[[1]]),
    grouped_feat = grouped_feat
  ))
}


### Function-2 ###
#' @noRd
#' Collapse correlated features
collapse_correlated_features <- function(features,
                                         corr_thresh,
                                         corr_method,
                                         group_neg_corr) {
  feats_nocorr <- features
  grp_feats <- NULL
  sapply_fn = utils::getFromNamespace("future_sapply", "future.apply")
  if (ncol(features) != 1) {
    corr_feats <- group_correlated_features(
      features = features,
      corr_thresh = corr_thresh,
      corr_method = corr_method,
      group_neg_corr = group_neg_corr
    )
    corr_cols <- gsub("\\|.*", "", corr_feats)
    feats_nocorr <- features %>% dplyr::select(dplyr::all_of(corr_cols))
    names_grps <- sapply_fn(names(feats_nocorr), function(n) {
      not_corr <- n %in% corr_feats
      if (not_corr) {
        name <- n
      } else {
        name <-
        corr_feats[grep(
          paste0("^", n, "\\||\\|", n, "\\||\\|", n, "$"),
          corr_feats
        )]
      }
    })
    grp_cols <- grep("\\|", names_grps)
    num_grps <- length(grp_cols)
    if (num_grps == 0) {
      grp_feats <- NULL
    } else {
      names(names_grps)[grp_cols] <- paste0("grp", 1:num_grps)
      names(feats_nocorr) <- names(names_grps)
      grp_feats <-
      sapply_fn(names_grps, function(x) {
        x
      })
    }
  }
  return(list(features = feats_nocorr, grp_feats = grp_feats))
}


### Function-3 ###
#' @noRd
#' group_correlated_features(features)
group_correlated_features <- function(features,
                                      corr_thresh,
                                      corr_method,
                                      group_neg_corr) {
  bin_corr_mat <- get_binary_corr_mat(
    features,
    corr_thresh = corr_thresh,
    group_neg_corr = group_neg_corr,
    corr_method = corr_method
  )
  # get single linkage clusters at height zero
  cluster_ids <- cluster_corr_mat(bin_corr_mat)
  return(get_groups_from_clusters(cluster_ids))
}


### Function-4 ###
#' @noRd
#' Identify correlated features as a binary matrix
get_binary_corr_mat <- function(features,
                                corr_thresh,
                                group_neg_corr,
                                corr_method) {
  corr_mat <- features %>%
    stats::cor(method = corr_method)
  corr_mat[is.na(corr_mat)] <- 0 # switch NAs to zero
  if (group_neg_corr) {
    in_thresh <- corr_mat >= corr_thresh | corr_mat <= -corr_thresh
  } else {
    in_thresh <- corr_mat >= corr_thresh
  }
  bin_mat <- corr_mat
  bin_mat[in_thresh] <- 1
  bin_mat[!in_thresh] <- 0
  return(bin_mat)
}


### Function-5 ###
#' @noRd
#' Cluster a matrix of correlated features
cluster_corr_mat <- function(bin_corr_mat,
                             hclust_method = "single",
                             cut_height = 0) {
  dist_mat <- 1 - bin_corr_mat %>% stats::as.dist()
  if (identical(dist_mat, numeric(0))) {
    stop("The correlation matrix contains nothing. Hint: is the features data frame empty?")
  }
  return(stats::cutree(
    stats::hclust(dist_mat,
                  method = hclust_method
    ),
    h = cut_height
  ))
}


### Function-6 ###
#' @noRd
#' Assign features to groups
get_groups_from_clusters <- function(cluster_ids) {
  # Call `sort()` with `method = 'radix'`
  radix_sort <- function(...) {
    return(sort(..., method = "radix"))
  }

  feat_groups <- character(length = max(cluster_ids))
  for (feat in radix_sort(names(cluster_ids))) { # assign each feature to its group/cluster
    cluster_id <- cluster_ids[[feat]]
    current_cluster <- feat_groups[cluster_id]
    if (nchar(current_cluster) > 0) {
      new_cluster <- paste(c(current_cluster, feat), collapse = "|")
    } else { # no need for paste if the current cluster has nothing in it
      new_cluster <- feat
    }
    feat_groups[cluster_id] <- new_cluster
  }
  return(radix_sort(feat_groups))
}


### Function-7 ###
#' @noRd
#' Get preprocessed dataframe for continuous variables
get_caret_processed_df <- function(features, method) {
  check_preprocess_methods(method)
  processed <- features
  removed <- NULL
  if (!is.null(method)) {
    preproc_values <- caret::preProcess(features, method = method)
    processed <- stats::predict(preproc_values, features)
    removed <- names(features)[!names(features) %in% names(processed)]
  }
  return(list(processed = processed, removed = removed))
}


### Function-8 ###
#' @noRd
#' Get dummyvars dataframe (i.e. design matrix)
get_caret_dummyvars_df <- function(features, full_rank = FALSE) {
  check_cat_feats(features)
  feature_design <- caret::dummyVars(" ~ .", data = features, fullRank = full_rank)
  feature_design_mat <- stats::predict(feature_design, features)
  return(as.data.frame(feature_design_mat))
}


### Function-9 ###
#' @noRd
#' Change columns to numeric if possible
change_to_num <- function(features) {
  lapply_fn = utils::getFromNamespace("future_lapply", "future.apply")
  features[] <- lapply_fn(features, function(col) {
    if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
      as.numeric(as.character(col))
    } else {
      col
    }
  })
  return(features)
}
