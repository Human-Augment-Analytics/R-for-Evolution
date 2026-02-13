
# purpose: to calc linear selection gradient for both binary and continuous 
# it includes checks for 
# sample size 
# missing data 
# convergence 
# type III ANOVA 


analyze_linear_selection <- function(data, fitness_col, trait_cols, fitness_type) {
  
  # sample size checking 
  if (nrow(data) < 10) {
    warning("Small sample size (n < 10) may lead to unreliable estimates")
  }
  
  rhs <- paste(trait_cols, collapse = " + ")
  
  if (fitness_type == "continuous") {
    resp <- if ("relative_fitness" %in% names(data)) {
      "relative_fitness"
    } else {
      fitness_col
    }
    
    fml <- stats::as.formula(paste(resp, "~", rhs))
    
    # remove incomplete case for model fitting 
    fit_data <- data[stats::complete.cases(data[, c(resp, trait_cols)]), ]
    if (nrow(fit_data) < length(trait_cols) + 1) {
      stop("Not enough complete cases for linear model")
    }
    
    # fit linear regression model  
    fit <- stats::lm(fml, data = data, na.action = stats::na.omit)
    sm  <- summary(fit)
    
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
    
    # Binary fitness
    fml <- stats::as.formula(paste(fitness_col, "~", rhs))
    
    # remove incomplete cases  
    fit_data <- data[stats::complete.cases(data[, c(fitness_col, trait_cols)]), ]
    if (nrow(fit_data) < length(trait_cols) * 5) {  # at least 5x more 
      warning("Small sample size for binary regression may cause convergence issues")
    }
    
    fit <- tryCatch({
      stats::glm(fml, data = data, family = stats::binomial("logit"), na.action = stats::na.omit)
    }, error = function(e) {
      stop("GLM fitting failed: ", e$message)
    })
    
    sm  <- summary(fit)
    
    # check convergence 
    if (!fit$converged) {
      warning("GLM algorithm did not converge. Results may be unreliable.")
    }
    
    # check separation 
    if (any(abs(coef(fit)) > 10)) {
      warning("Large coefficients detected - possible complete separation")
    }
    
    # chi-square test for logistic regression
    if (requireNamespace("car", quietly = TRUE)) {
      an <- tryCatch({
        car::Anova(fit, type = "III", test.statistic = "Wald")
      }, error = function(e) {
        warning("Wald test failed: ", e$message)
        NULL
      })
    } else {
      an <- NULL
    }
  }
  
  list(model = fit, summary = sm, anova = an)
}