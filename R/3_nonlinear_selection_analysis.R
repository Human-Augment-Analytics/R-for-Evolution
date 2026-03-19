
# purpose: to estimate quadratic and correlational selection gradients
# quadratic terms to detect stabilizing or disruptive selection
# interaction terms to detect correlational selection


analyze_nonlinear_selection <- function(data, fitness_col, trait_cols, fitness_type) {
  
  # check traits number 
  if (length(trait_cols) < 2) {
    stop("Nonlinear selection requires at least 2 traits")
  }
  
  if (nrow(data) < 20) {
    warning("Small sample size for nonlinear model (n < 20). Results may be unreliable.")
  }
  
  # quadratic terms
  quad <- paste0("I(", trait_cols, "^2)")
  
  # interaction terms (pairwise products)
  inter <- if (length(trait_cols) >= 2) {
    pairs <- utils::combn(trait_cols, 2, simplify = FALSE)
    vapply(pairs, function(p) paste0(p[1], ":", p[2]), "")
  } else character(0)
  

  rhs <- paste(c(trait_cols, quad, inter), collapse = " + ")
  
  if (fitness_type == "continuous") {
    resp <- if ("relative_fitness" %in% names(data)) {
      "relative_fitness"
    } else {
      fitness_col
    }
    
    # binary fitness case
    fml <- stats::as.formula(paste(resp, "~", rhs))
    
    # check if sample size is enough 
    required_params <- length(trait_cols) + length(quad) + length(inter) + 1
    if (nrow(data) < required_params * 2) {
      warning("Sample size may be too small for complex nonlinear model")
    }
    
    fit <- stats::lm(fml, data = data, na.action = stats::na.omit)
    sm  <- summary(fit)
    
    # Type III ANOVA
    if (requireNamespace("car", quietly = TRUE)) {
      an <- tryCatch({
        car::Anova(fit, type = "III")
      }, error = function(e) {
        warning("Type III ANOVA failed: ", e$message)
        NULL
      })
    } else {
      an <- NULL
    }
    
  } else {
    fml <- stats::as.formula(paste(fitness_col, "~", rhs))
    
    required_params <- length(trait_cols) + length(quad) + length(inter) + 1
    if (nrow(data) < required_params * 10) {
      warning("Sample size may be insufficient for binary nonlinear model")
    }
    
    fit <- tryCatch({
      stats::glm(fml, data = data, family = stats::binomial("logit"), na.action = stats::na.omit)
    }, error = function(e) {
      stop("Nonlinear GLM fitting failed: ", e$message)
    })
    
    sm  <- summary(fit)
    
    # check convergence 
    if (!fit$converged) {
      warning("Nonlinear GLM did not converge")
    }
    
    # check variance inflation 
    vif_check <- tryCatch({
      if (requireNamespace("car", quietly = TRUE)) {
        vif_vals <- car::vif(fit)
        if (any(vif_vals > 10)) {
          warning("High variance inflation factors detected (VIF > 10)")
        }
      }
    }, error = function(e) NULL)
    
    # Wald chi-square tests 
    if (requireNamespace("car", quietly = TRUE)) {
      an <- tryCatch({
        car::Anova(fit, type = "III", test.statistic = "Wald")
      }, error = function(e) {
        warning("Wald test for nonlinear model failed: ", e$message)
        NULL
      })
    } else {
      an <- NULL
    }
  }
  
  list(model = fit, summary = sm, anova = an)
}