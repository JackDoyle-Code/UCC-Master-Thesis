# UCC Master Thesis
This is a machine learning pipeline for the classification/multiclassification/regression of microbiome data. 

It consists of two main functions run_ml_cv and run_ml_nestedcv that requires a dataset (e.g. OTU table from NGS) and metadata (e.g. count abundance). 

run_ml_cv consists of 9 main functions:
  1. check_all - ensure that all the inputs are suitable options for the pipeline
  2. preproces_samples - performs NGS specific preprocessing by sample prior to data splitting.
  3. get_partition_indices - which splits the data using caret::createDataPartition. It allows allows for group partitioning.
  4. preprocess_features - performs preprocessing of the data by removing feature with high multicolinearity, zero and near zero variance.
  5. filter_features_main - performs feature selection using 6 possible methods (recommended glm_filter or boruta_filter).
  6. get_hyperparams_list and get_tuning_grid - used to determine the range of hyperparameters that are going to used for the hyperparameter optimisation during cross-validation.
  7. cross_val - performs cross-validation during training using either k-fold cv, repeated cv or leave-one-out cv.
  8. train_model - trains the model using the optimal hyperparameters.
  9. calc_perf_metrics - returns performance values and can take into account class weights.
  10. roc_plot_main - plots ROC curves. 
  11. feature_importance_mains - determines which features are important for classification/regression using either permutation importance, embedded importance or endR (recommended permutation for accuracy and embedded for speed).

run_ml_nestedcv uses the same functions but also included some extra ones:
  1. Define_fold - splits the data into k-folds (this is called after preprocess_samples)
  2. nestedcvCore - performs most of the function from run_ml_cv but for each individual fold (from 3-9)
  3. median_model - extracts the median model from the k-trained folds and uses this as the final model (feature importance is only called on this model)

For more information on any of the function please check the code as they have been commented with headings and many of the lines are also commented as well.

The application of these functions can be found in the thesis paper found in the thesis paper folder. This also discusses why many of the parameter such as feature selection methods, models, etc, were selected for used.
