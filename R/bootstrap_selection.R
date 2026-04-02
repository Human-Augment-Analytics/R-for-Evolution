bootstrap_selection <- function(data, fitness_col, trait_cols,
                                fitness_type = c("binary","continuous"),
                                B = 1000, conf = 0.95, seed = NULL) {
  fitness_type <- match.arg(fitness_type)
  
  # check bootstrap replicates (resamples)
  if (B < 100) {
    warning("Bootstrap replicates (B) < 100 may give unstable results")
  }
  
  if (B > 10000) {
    warning("Large number of bootstrap replicates (B > 10000) may be computationally intensive")
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # 1) Clean + standardize once, up front
  cols_needed <- c(fitness_col, trait_cols)
  ok <- stats::complete.cases(data[, cols_needed, drop = FALSE])
  df0 <- data[ok, , drop = FALSE]
  
  if (nrow(df0) == 0L) stop("No complete cases for fitness and trait_cols.")
  
  # check if the sample size is enough for bootstrap
  if (nrow(df0) < 10) {
    stop("Insufficient complete cases (n < 10) for bootstrap")
  }
  
  # z-score traits
  for (t in trait_cols) {
    df0[[t]] <- as.numeric(scale(df0[[t]]))
    if (any(is.infinite(df0[[t]]) | any(is.na(df0[[t]])))) {
      stop("Standardization failed for trait: ", t)
    }
  }
  
  n <- nrow(df0)
  alpha <- (1 - conf) / 2
  
  # 2) define coefficient extraction function (including error handling)
  get_coeffs <- function(df) {
    tryCatch({
      lin   <- analyze_linear_selection(df, fitness_col, trait_cols, fitness_type)
      nonlin<- analyze_nonlinear_selection(df, fitness_col, trait_cols, fitness_type)
      
      lin_tab  <- extract_linear_coefficients(trait_cols, lin)
      quad_tab <- extract_quadratic_coefficients(trait_cols, nonlin)
      int_tab  <- extract_interaction_coefficients(trait_cols, nonlin)
      
      out <- rbind(lin_tab, quad_tab, int_tab)
      if (is.null(out) || nrow(out) == 0) {
        return(data.frame(Term=character(), Type=character(),
                          Beta_Coefficient=numeric(), Standard_Error=numeric(),
                          P_Value=numeric(), check.names = FALSE))
      }
      out
    }, error = function(e) {
      warning("Bootstrap iteration failed: ", e$message)
      data.frame(Term=character(), Type=character(),
                 Beta_Coefficient=numeric(), Standard_Error=numeric(),
                 P_Value=numeric(), check.names = FALSE)
    })
  }
  
  # 3) Bootstrap with progress tracking
  draws_list <- vector("list", B)
  success_count <- 0
  
  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    res <- get_coeffs(df0[idx, , drop = FALSE])
    
    if (nrow(res) > 0) {
      res$replicate <- b
      success_count <- success_count + 1
    }
    draws_list[[b]] <- res
    
    if (b %% 100 == 0) {
      cat(sprintf("Bootstrap progress: %d/%d (%.1f%%)\n", 
                  b, B, b/B*100))
    }
  }
  
  # check successful rate 
  if (success_count < B * 0.5) {
    warning("More than 50% of bootstrap iterations failed. Results may be unreliable.")
  }
  
  draws <- do.call(rbind, draws_list)
  rownames(draws) <- NULL
  
  # 4) Percentile CI with robust calculation
  if (nrow(draws) == 0L) {
    warning("No successful bootstrap replicates. Returning empty results.")
    ci <- data.frame(Term=character(), Type=character(),
                     est=numeric(), lwr=numeric(), upr=numeric(),
                     check.names = FALSE)
  } else {
    key <- unique(draws[, c("Term","Type"), drop = FALSE])
    ci_rows <- vector("list", nrow(key))
    
    for (i in seq_len(nrow(key))) {
      sel <- draws$Term == key$Term[i] & draws$Type == key$Type[i]
      vals <- draws$Beta_Coefficient[sel]
      vals <- vals[is.finite(vals)]
      
      if (length(vals) < 10) {
        warning("Insufficient bootstrap samples for term: ", key$Term[i])
        ci_rows[[i]] <- data.frame(Term = key$Term[i], Type = key$Type[i],
                                   est = NA_real_, lwr = NA_real_, upr = NA_real_)
      } else {
        est <- stats::median(vals, na.rm = TRUE)
        qs  <- stats::quantile(vals, probs = c(alpha, 1 - alpha), na.rm = TRUE, names = FALSE)
        ci_rows[[i]] <- data.frame(Term = key$Term[i], Type = key$Type[i],
                                   est = est, lwr = qs[1], upr = qs[2])
      }
    }
    ci <- do.call(rbind, ci_rows)
    rownames(ci) <- NULL
  }
  
  list(draws = draws, ci = ci, success_rate = success_count/B)
}
