
selection_differential <- function(data, fitness_col, trait_col,
                                   assume_standardized = TRUE,
                                   use_relative = TRUE) {

  z <- data[[trait_col]]
  w <- data[[fitness_col]]
  
  # drop rows with NA in either
  keep <- stats::complete.cases(z, w)
  z <- z[keep]; w <- w[keep]
  
  # standardize trait 
  if (!assume_standardized) {
    sd_z <- stats::sd(z)
    if (!is.finite(sd_z) || sd_z == 0) {
      stop("Trait column has zero variance; cannot standardize.")
    }
    z <- as.numeric(scale(z))
  }
  
  # relative fitness
  if (use_relative) {
    mu <- mean(w)
    if (is.finite(mu) && mu != 0) {
      w <- w / mu
    } else {
      stop("Cannot compute relative fitness: mean fitness is zero or non-finite.")
    }
  }
  
  # Cov(w_rel, z) = mean(w_rel * z) because mean(z) = 0 after standardization
  mean(z * w)
}
