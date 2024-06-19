### Function-1 ###
#' @noRd
#' Randomize feature order to eliminate any position-dependent effects
randomize_feature_order <- function(dataset) {
  features_reordered <- colnames(dataset) %>%
    sample()
  dataset <- dplyr::select(
    dataset, all_of(features_reordered)) ### removing all_of function provides error saying it has depreciated
  return(dataset)
}


### Function-2 ###
#' @noRd
#' Check whether a numeric vector contains whole numbers.
is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}


### Function-3 ###
#' @noRd
#' Replace spaces in all elements of a character vector with underscores
replace_spaces <- function(x, new_char = "_") {
  if (is.character(x)) {
    x <- gsub(" |-", new_char, x)
  }
  return(x)
}
