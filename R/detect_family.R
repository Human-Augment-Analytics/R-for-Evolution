
# purpose: to automatically determine the appropriate fitness type 
# binary -> binomial GLM
# continuous -> Gaussian GLM 

detect_family <- function(y) {
  # remove NA 
  y_clean <- y[!is.na(y)]
  
  if (length(y_clean) == 0) {
    stop("No non-NA values in fitness vector")
  }
  
  # unique fitness values
  unique_vals <- sort(unique(y_clean))
  
  # binary check: either 0 or 1 and min sample size is required 
  is_binary <- length(unique_vals) <= 2 &&
    all(unique_vals %in% c(0, 1)) &&
    length(y_clean) >= 10  
  
  if (is_binary) {
    # warn if complete separation which can cause convergence issue in logistic regression
    if (all(y_clean == 0) || all(y_clean == 1)) {
      warning("Complete separation detected: all fitness values are ",
              ifelse(all(y_clean == 0), "0", "1"),
              ". Consider using Firth regression or more data.")
    }
    
    return(list(
      type   = "binary",
      family = binomial("logit")
    ))
  } else {
    return(list(
      type   = "continuous",
      family = gaussian()
    ))
  }
}