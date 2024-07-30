#' Function-1
#' @noRd
#' Preprocess data prior to running machine learning
#' @export
#'
#'
filter_features_main <- function(
  train_data, train_metadata, test_data, outcome_colname,
  filterFunList = list(test = "wilcoxon_filter", p_cutoff=0.05), outcome_type) {

  # remove features using specific feature selection methods
  args <- list(y = train_metadata[[outcome_colname]], x = train_data) ### replaced pull
  if (filterFunList[["test"]] == "glm_filter") {
    args <- append(args, outcome_type)
  }
  args <- append(args, filterFunList[!names(filterFunList) %in% "test"]) # using filterFunList returns 0.05 and not $p_cutoff 0.05
  fset <- do.call(filterFunList[["test"]], args) # calls the function (either wilcoxon/ttest)
  filt_xtrain <- train_data[, fset]
  filt_xtest <- test_data[, fset]
  rem_feat <- colnames(train_data[, -fset])

  message("Out of a total of N=", ncol(train_data), " predictors, N=", ncol(filt_xtrain), " could be retained after ", filterFunList[["test"]], " filtering")
  # return now
  return(list(
    filt_traindata = filt_xtrain,
    filt_testdata = filt_xtest,
    rem_feat = rem_feat
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


#' Function-7
#' @noRd
#' feature selection using LASSO
glm_filter <- function(y, x, n = 100, cv_method = list(method = "cv", times = 10 , repeats = NA), outcome_type = "binary") {
  family = switch(outcome_type,
                   "continuous" = "gaussian",
                   "binary" = "binomial",
                   "multiclass" = "multinomial",
                   "unknown")
  model = train(x, y,
                method = "glmnet",
                family = family,
                trControl = trainControl(method = cv_method[[1]],
                                         number = cv_method[[2]],
                                         repeats = cv_method[[3]]),
                tuneGrid = expand.grid(alpha = c(1),
                                       lambda = seq(0.1, 1, by = 0.01)))
  best_lambda = model$bestTune$lambda
  best_alpha = model$bestTune$alpha
  best_model = glmnet(x, y, family = family, alpha = best_alpha, lambda = best_lambda)
  co_ef = coef(best_model)
  ord_coef = as.matrix(co_ef[order(abs(co_ef), decreasing = T),])
  ord_feat = rownames(ord_coef)
  final_feats = ord_feat[ord_feat != "(Intercept)"]
  N_feats = head(final_feats, n)
  feat_ind = which(colnames(x) %in% N_feats)
  return(feat_ind)
}


#' Function-8
#' @noRd
#' feature selection using Boruta
boruta_filter <- function(y, x, n = 100, maxRuns = 100) { # getImp can be normalised permutation, raw permutation or gini impurity. Higher value for MaxRuns will likely reduce tentative features
  stats = Boruta(x, as.factor(y), maxRuns = maxRuns)
  if (!is.null(n)) {
    imp = attStats(stats)
    ord_imp = imp[order(imp$meanImp, decreasing = T),]
    feats = head(rownames(ord_imp), n)
  }
  else {
    non_rej = stats$finalDecision[stats$finalDecision != "Rejected"]
    feats = rownames(as.matrix(non_rej))
  }
  feat_ind = which(colnames(x) %in% feats)
  return(feat_ind)
}


#' Function-9
#' @noRd
#' feature selection using RFE-SVM
rfe_filter <- function(y, x, cv_method = list(method = "cv", times = 10 , repeats = NA)) {
  mx = ncol(x)
  ctrl = rfeControl(functions = caretFuncs,
                    method = cv_method[[1]],
                    number = cv_method[[2]],
                    repeats = cv_method[[3]])
  svm_feats = rfe(x = x,
                  y = as.factor(y),
                  rfeControl = ctrl,
                  sizes = c(0.1*mx, 0.25*mx, 0.5*mx, 0.75*mx),
                  method = "svmRadial")
  final_features = svm_feats$optVariables
  feat_ind = which(colnames(x) %in% final_features)
  return(feat_ind)
}


#' Function-10
#' @noRd
#' feature selection using mRMR
mrmr_filter <- function(y, x, n = 100) {
  fact = as.numeric(as.factor(y)) - 1
  data = mRMR.data(cbind(x, fact))
  mrmr = mRMR.classic(data = data, target_indices = (ncol(x) + 1), feature_count = n)
  feats = mrmr@filters[[1]]
  return(as.vector(feats))
}
