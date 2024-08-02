### Function-1 ###
#' @noRd
#' feature Importance analysis
feature_importance_endoR <- function(trained_model,
  method,
  train_data,
  train_metadata,
  outcome_colname,
  jobs,
  ...) {
    # feature importance analysis using endoR
    feature_importance_out = endoR::model2DE(
      model = trained_model[["finalModel"]],
      model_type = method,
      data = train_data,
      target = train_metadata[[outcome_colname]],
      classPos = train_metadata[[outcome_colname]][1],
      n_cores = jobs, in_parallel = TRUE,
      filter = FALSE, discretize = FALSE, prune = FALSE
    )

    # Plot the most important features
    feature_importance_plot = endoR::plotFeatures(
      decision_ensemble = feature_importance_out
    )

    # return
    return(
      list("feature_importance_out"=feature_importance_out, "feature_importance_plot"=feature_importance_plot)
    )
  }
