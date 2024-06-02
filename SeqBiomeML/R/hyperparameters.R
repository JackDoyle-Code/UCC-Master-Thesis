### Function-1 ###
#' @noRd
#' Set hyperparameters based on ML method and dataset characteristics
get_hyperparams_list <- function(dataset, method) {
  n_features <- ncol(dataset)
  n_samples <- nrow(dataset)
  hparams_functions <- list(
    glmnet = rlang::quo(set_hparams_glmnet()),
    rf = rlang::quo(set_hparams_rf(n_features)),
    parRF = rlang::quo(set_hparams_rf(n_features)),
    rpart2 = rlang::quo(set_hparams_rpart2(n_samples)),
    svmRadial = rlang::quo(set_hparams_svmRadial()),
    xgbTree = rlang::quo(set_hparams_xgbTree(n_samples))
  )
  if (!(method %in% names(hparams_functions))) {
    stop(paste0("method '", method, "' is not supported."))
  }
  return(rlang::eval_tidy(hparams_functions[[method]]))
}


### Function-2 ###
#' @noRd
#' Set hyperparameters for regression models for use with glmnet
set_hparams_glmnet <- function() {
  return(list(
    lambda = 10^seq(-4, 1, 1),
    alpha = 0 # this makes it ridge (i.e. L2). 1 would make it lasso (i.e. L1).
  ))
}


### Function-3 ###
#' @noRd
#' Set hyparameters for random forest models
set_hparams_rf <- function(n_features) {
  sqrt_features <- round(sqrt(n_features))
  if (n_features == 1) {
    mtry <- c(1)
  } else {
    mtry <- c(
      sqrt_features / 2,
      sqrt_features,
      sqrt_features * 2
    ) %>%
      round() %>%
      .[. >= 1 & . < n_features]
  }
  return(list(mtry = mtry))
}


### Function-4 ###
#' @noRd
#' Set hyperparameters for decision tree models
set_hparams_rpart2 <- function(n_samples) {
  return(list(maxdepth = c(1, 2, 4, 8, 16, 30) %>% .[. < n_samples]))
}


### Function-5 ###
#' @noRd
#' Set hyperparameters for SVM with radial kernel
set_hparams_svmRadial <- function() {
  return(list(
    C = 10^seq(-3, 2, 1),
    sigma = 10^seq(-6, -1, 1)
  ))
}


### Function-6 ###
#' @noRd
#' Set hyperparameters for SVM with radial kernel
set_hparams_xgbTree <- function(n_samples) {
  return(list(
    nrounds = c(100),
    gamma = c(0),
    eta = c(0.01, 0.05, 0.1),
    max_depth = set_hparams_rpart2(n_samples)[["maxdepth"]],
    colsample_bytree = c(0.6, 0.8, 1.0),
    min_child_weight = c(1),
    subsample = seq(4, 7, 1) / 10
  ))
}


### Function-7 ###
#' @noRd
#' Generate the tuning grid for tuning hyperparameters
get_tuning_grid <- function(hyperparams_list, method) {
  return(hyperparams_list %>%
    expand.grid() %>%
    dplyr::mutate_all(utils::type.convert, as.is = TRUE))
}


### Function-8 ###
#' @noRd
#' Custom functions to set a tuning with multiple param
custom_method_grid <- function(method) {
  # custom random forest
  if (method == "customrf") {
    modelInfo <- list(label = "Random Forest",
    library = "randomForest",
    loop = NULL,
    type = c("Classification", "Regression"),
    parameters = data.frame(parameter = c("mtry", "ntree"),
    class = c("numeric", "numeric"),
    label = c("#Randomly Selected Predictors", "#number of trees used in aggregation")),
    grid = function(x, y, len = NULL, search = "grid") {
      if(search == "grid") {
        out <- data.frame(mtry = caret::var_seq(p = ncol(x),
        classification = is.factor(y),
        len = len))
      } else {
        out <- data.frame(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)))
      }
    },
    fit = function(x, y, wts, param, lev, last, classProbs, ...)
    randomForest::randomForest(x, y, mtry = param$mtry, ntree = param$ntree, ...),
    predict = function(modelFit, newdata, submodels = NULL)
    if(!is.null(newdata)) stats::predict(modelFit, newdata) else stats::predict(modelFit),
    prob = function(modelFit, newdata, submodels = NULL)
    if(!is.null(newdata)) stats::predict(modelFit, newdata, type = "prob") else stats::predict(modelFit, type = "prob"),
    predictors = function(x, ...) {
      ## After doing some testing, it looks like randomForest
      ## will only try to split on plain main effects (instead
      ## of interactions or terms like I(x^2).
      varIndex <- as.numeric(names(table(x$forest$bestvar)))
      varIndex <- varIndex[varIndex > 0]
      varsUsed <- names(x$forest$ncat)[varIndex]
      varsUsed
    },
    varImp = function(object, ...){
      varImp <- randomForest::importance(object, ...)
      if(object$type == "regression") {
        if("%IncMSE" %in% colnames(varImp)) {
          varImp <- data.frame(Overall = varImp[,"%IncMSE"])
        } else {
          varImp <- data.frame(Overall = varImp[,1])
        }
      }
      else {
        retainNames <- levels(object$y)
        if(all(retainNames %in% colnames(varImp))) {
          varImp <- varImp[, retainNames]
        } else {
          varImp <- data.frame(Overall = varImp[,1])
        }
      }

      out <- as.data.frame(varImp, stringsAsFactors = TRUE)
      if(dim(out)[2] == 2) {
        tmp <- apply(out, 1, mean)
        out[,1] <- out[,2] <- tmp
      }
      out
    },
    levels = function(x) x$classes,
    tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
    sort = function(x) x[order(x[,1]),],
    oob = function(x) {
      out <- switch(x$type,
        regression = c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
        classification = c(1 - x$err.rate[x$ntree, "OOB"],
        e1071::classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]]))
        names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
        out
      })
    }
    return(modelInfo)
  }
