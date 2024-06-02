#' Function-1
#' @noRd
#' Preprocess data prior to running machine learning
#' @export
#'
#'
filter_features_main <- function(
  train_data, train_metadata, outcome_colname,
  filterFunList = list(test = "wilcoxon_filter", p_cutoff=0.05),
  grouped_feat) {

  # remove features based on significance test
  args <- list(y = train_metadata %>% dplyr::pull(outcome_colname), x = train_data)
  args <- append(args, filterFunList[!names(filterFunList) %in% "test"])
  fset <- do.call(filterFunList[["test"]], args)
  filt_xtrain <- train_data[, fset]
  final_feat <- colnames(train_data[, fset])

  message("Out of a total of N=", ncol(train_data), " predictors, N=", ncol(filt_xtrain), " could be retained after ", filterFunList[["test"]], " filtering")
  # return now
  return(list(
    filt_data = filt_xtrain,
    filt_metadata = train_metadata,
    final_feat = final_feat,
    grouped_feat = grouped_feat
  ))
}


#' Function-2
#' @noRd
#' Filter end function to order predictors
filter_end <- function(pval, x, force_vars, nfilter, p_cutoff,
                       type, keep_factors, factor_ind, ...) {
  if (keep_factors) {
    force_vars <- unique(c(force_vars, colnames(x)[factor_ind]))
  }
  check_vars <- which(!colnames(x) %in% force_vars)
  outp <- pval[check_vars]
  outorder <- order(outp)
  out <- check_vars[outorder]
  outp <- outp[outorder]
  if (!is.null(p_cutoff)) out <- out[outp < p_cutoff]
  if (!keep_factors && !is.null(factor_ind)) out <- out[!out %in% factor_ind]
  if (!is.null(nfilter) && length(out) > nfilter) out <- out[1:nfilter]
  if (length(out) == 0) stop("No predictors left after filtering!!! Run without feature filtering...")
  out <- c(out, which(colnames(x) %in% force_vars))
  out <- out[!is.na(out)]
  switch(type, index = out, names = colnames(x)[out])
}


#' Function-3
#' @noRd
#' t-test filter
ttest_filter <- function(y,
                         x,
                         force_vars = NULL,
                         nfilter = NULL,
                         p_cutoff = 0.05,
                         type = c("index", "names", "full"),
                         keep_factors = TRUE,
                         ...) {
  type <- match.arg(type)
  y <- factor(y)
  indx1 <- as.numeric(y) == 1
  indx2 <- as.numeric(y) == 2
  factor_ind <- which_factor(x)
  if (is.data.frame(x)) x <- data.matrix(x)
  if (is.null(colnames(x))) colnames(x) <- seq_len(ncol(x))
  res <- Rfast::ttests(x[indx1, ], x[indx2, ])
  rownames(res) <- colnames(x)
  if (type == "full") {
    if (length(factor_ind) == 0) {
      return(res)
    } else return(res[-factor_ind, ])
  }
  filter_end(res[, "pvalue"],
             x, force_vars, nfilter, p_cutoff, type, keep_factors,
             factor_ind, ...)
}


#' Function-4
#' @noRd
#' wilcoxon-test filter
wilcoxon_filter <- function(y,
                            x,
                            force_vars = NULL,
                            nfilter = NULL,
                            p_cutoff = 0.05,
                            type = c("index", "names", "full"),
                            exact = FALSE,
                            keep_factors = TRUE,
                            ...) {
  type <- match.arg(type)
  y <- factor(y)
  indx1 <- as.numeric(y) == 1
  indx2 <- as.numeric(y) == 2
  factor_ind <- which_factor(x)
  if (is.data.frame(x)) x <- data.matrix(x)
  res <- suppressWarnings(
    matrixTests::row_wilcoxon_twosample(t(x[indx1, ]), t(x[indx2, ]),
                                        exact = exact, ...)
  )
  if (type == "full") {
    if (length(factor_ind) == 0) {
      return(res)
    } else return(res[-factor_ind, ])
  }
  filter_end(res[, "pvalue"],
             x, force_vars, nfilter, p_cutoff, type,
             keep_factors, factor_ind)
}


#' Function-5
#' @noRd
#' function to support t-test filter
which_factor <- function(x) {
  if (is.matrix(x)) return(NULL)
  which(index_factor(x, convert_bin = TRUE))
}


#' Function-6
#' @noRd
#' function to support t-test filter
index_factor <- function(x, convert_bin = FALSE) {
  if (is.matrix(x)) return(NULL)
  if (convert_bin) {
    num_ind <- unlist(lapply(x, function(i) {
      is.numeric(i) || nlevels(factor(i)) <= 2 || is.ordered(i)
    }))
  } else {
    num_ind <- unlist(lapply(x, is.numeric))
  }
  !num_ind
}
