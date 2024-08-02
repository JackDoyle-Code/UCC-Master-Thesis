#' Function-1
#' @noRd
#' Preprocess data prior to data partition
#' @export
#'
#'
preprocess_samples <- function(
    dataset = dataset,
    scale_method = list(method = c("clr", "tss", "rclr"), impute_method = "pseudo", val = 1)) {

  # imputes zeros if the impute_method is not NA
  if (!is.na(scale_method[[2]])){
    dataset <- impute_zeros(dataset, scale_method)
  }

  # performs centred log ratio transformation if selected
  if (scale_method[[1]] == "clr") {
    dataset <- t(apply(dataset, 1, function(x) # applies clr formula to every column
      log(x / exp(mean(log(x)))) # clr formula
    ))
  }
  # performs robust centred log ratio transformation if selected
  else if (scale_method[[1]] == "rclr") {
    temp_data <- apply(dataset, 1, function(row) {
      log_nz <- log(row[row > 0]) # logs the non-zero values
      new_vals <- log_nz - mean(log_nz)
      row[row > 0] <- new_vals # changes the non_zero values in each row so it is log transformed
      return(row)
    })
    dataset <- as.data.frame(t(temp_data))
  }
  # performs total sum scaling if selected
  else if (scale_method[[1]] == "tss") {
    dataset <- t(apply(dataset, 1, function(x) x/sum(x)))
  }
  return(as.data.frame(dataset))
}



### Function-2 ###
#' @noRd
#' imputes zero values within the count data for CLR transformation
impute_zeros <- function(dataset, scale_method) {
  if (scale_method[[2]] == "pseudo") {
    if (scale_method[[3]] == "min") {
      val = min(dataset[dataset != 0])*0.65 # applies 0.65 * detection limit (i.e. smallest observed value)
    }
    else {
      val = as.numeric(scale_method[[3]])
    }
    dataset + val
  }
  else {
    zCompositions::cmultRepl(dataset, method = scale_method[[2]], output = "p-counts", z.delete = F) # uses Bayesian multiplicative replacement
  }
}

