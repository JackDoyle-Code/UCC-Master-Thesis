# SeqBiomeML
This is a ML pipeline for microbiome analysis. 

It consists of two main functions run_ml_cv and run_ml_nestedcv that requires a dataset (e.g. OTU table from NGS) and metadata (e.g. count abundance). 

run_ml_cv consists of 9 main functions:
  1. preprocess_features - performs preprocessing of the data by removing feature with high multicolinearity, zero and near zero variance.
  2. filter_features_main - performs feature based on either a wilcoxon or t-test with a dafault p-value cutoff of 0.05
  3. get_partition_indices - which splits the data using caret::createDataPartition. It allows allows for group partitioning
  4. get_hyperparams_list and get_tuning_grid - used to determine what hyperparameters are going to used for the training and sets up the tuning grid
  5. cross_val - performs cross-validation during training using either k-fold cv, repeated cv or leave-one-out cv
  6. train_model - trains the model
  7. calc_perf_metrics - returns performance values and can take into account class weights
  8. roc_plot_main - plots ROC curves
  9. feature_importance_endoR/feature_importance_permuted - determines which features are important for classification/regression using either endoR or permutation

run_ml_nestedcv uses the same functions but also included some extra ones:
  1. 

