# ============================================================================
# analyze_disruptive_selection
#
# Purpose: 
#   1. Analyze disruptive or stabilizing selection for a single trait
#
# Model: 
#   w ~ z + z²
#   w = α + βz + γz² + ε
#
#   Where:
#     β (linear) = directional selection gradient
#     γ (quadratic) = 2 × β_quad = nonlinear selection gradient
#     γ > 0 = disruptive selection (U-shaped)
#     γ < 0 = stabilizing selection (∩-shaped)
#
# IMPORTANT NOTE:
#   - OLS is ALWAYS used to estimate coefficients (β and γ)
#   - For binary fitness: OLS gives coefficients, logistic GLM gives p-values
#   - For continuous fitness: OLS gives both coefficients and valid p-values
#
# Parameters:
#   data               : data frame with fitness and trait measurements
#   fitness_col        : name of the fitness column
#   trait_col          : name of the trait column
#   fitness_type       : "binary" or "continuous"
#   standardize        : if TRUE, standardize trait to mean 0, SD 1
#   group              : optional grouping variable (e.g., "lake", "year")
#   return_grouped     : if TRUE and group is specified, return data frame
#                        with results per group; if FALSE, return overall result
#
# Returns:
#   Data frame with columns:
#     Term               : trait name or "trait²"
#     Type               : "Linear" or "Quadratic"
#     Beta_Coefficient   : β (linear) or γ (quadratic)
#     Standard_Error     : standard error of estimate
#     P_Value            : statistical significance
#     Variance           : square of standard error
#     Group              : (if return_grouped = TRUE) group identifier
# ============================================================================

#' Analyze disruptive/stabilizing selection
#'
#' Detects disruptive or stabilizing selection on a single trait by estimating linear and quadratic gradients.
#'
#' @param data A data frame containing fitness and trait measurements.
#' @param fitness_col A string specifying the name of the fitness column.
#' @param trait_col A string specifying the name of the single trait column.
#' @param fitness_type A string indicating the fitness type: \code{"binary"} or \code{"continuous"}.
#' @param standardize Logical indicating whether to standardize the trait to mean 0 and SD 1. Default is \code{TRUE}.
#' @param group Optional string specifying a grouping variable.
#' @param return_grouped Boolean indicating whether data frame results are grouped. Default is \code{FALSE}
#'
#' @return A data frame containing selection coefficients and statistics.
#' @export
analyze_disruptive_selection <- function(data, 
                                         fitness_col, 
                                         trait_col, 
                                         fitness_type = c("binary", "continuous"), 
                                         standardize = TRUE, 
                                         group = NULL, 
                                         return_grouped = FALSE) {
  # Input validation
  fitness_type <- match.arg(fitness_type)
  
  if (!trait_col %in% names(data)) {
    stop("Trait column '", trait_col, "' not found in data")
  }
  if (!fitness_col %in% names(data)) {
    stop("Fitness column '", fitness_col, "' not found in data")
  }
  
  # ======================================================
  # CASE 1: return by group
  # ======================================================
  if (return_grouped && !is.null(group)) {
    if (!group %in% names(data)) {
      stop("Group column '", group, "' not found in data")
    }
    
    groups <- unique(data[[group]])
    results_list <- list()
    
    for (g in groups) {
      data_g <- data[data[[group]] == g, ]
      
      res <- analyze_disruptive_selection(
        data = data_g,
        fitness_col = fitness_col,
        trait_col = trait_col,
        fitness_type = fitness_type,
        standardize = standardize,
        group = NULL,
        return_grouped = FALSE
      )
      
      res$Group <- g
      results_list[[as.character(g)]] <- res
    }
    
    all_results <- do.call(rbind, results_list)
    return(all_results)
  }
  
  # ======================================================
  # CASE 2: main analysis (no grouping)
  # ======================================================
  
  # Prepare data (standardization, relative fitness if needed)
  df <- prepare_selection_data(
    data = data,
    fitness_col = fitness_col,
    trait_cols = trait_col,
    standardize = standardize,
    group = group,
    add_relative = (fitness_type == "continuous"),
    na_action = "warn"
  )
  
  # Formula: w ~ z + z²
  fml <- as.formula(
    paste(fitness_col, "~", trait_col, "+ I(", trait_col, "^2)")
  )
  
  # ======================================================
  # Fit OLS (always for coefficient estimates)
  # ======================================================
  fit_ols <- lm(fml, data = df)
  coef_ols <- summary(fit_ols)$coefficients
  
  quad_term <- paste0("I(", trait_col, "^2)")
  
  # Linear term (β) from OLS
  beta_linear <- coef_ols[trait_col, "Estimate"]
  se_linear <- coef_ols[trait_col, "Std. Error"]
  
  # Quadratic term (γ = 2 × β_quad) from OLS
  if (quad_term %in% rownames(coef_ols)) {
    gamma_quad <- 2 * coef_ols[quad_term, "Estimate"]
    se_quad <- 2 * coef_ols[quad_term, "Std. Error"]
  } else {
    gamma_quad <- NA_real_
    se_quad <- NA_real_
  }
  
  # ======================================================
  # Get p-values based on fitness type
  # ======================================================
  if (fitness_type == "continuous") {
    # For continuous fitness: OLS p-values are valid
    p_linear <- coef_ols[trait_col, "Pr(>|t|)"]
    if (quad_term %in% rownames(coef_ols)) {
      p_quad <- coef_ols[quad_term, "Pr(>|t|)"]
    } else {
      p_quad <- NA_real_
    }
  } else {
    # For binary fitness: OLS p-values are NOT valid
    # Use logistic GLM for valid p-values
    fit_glm <- glm(fml, data = df, family = binomial)
    coef_glm <- summary(fit_glm)$coefficients
    
    p_linear <- coef_glm[trait_col, "Pr(>|z|)"]
    
    if (quad_term %in% rownames(coef_glm)) {
      p_quad <- coef_glm[quad_term, "Pr(>|z|)"]
    } else {
      p_quad <- NA_real_
    }
  }
  
  # ======================================================
  # Create results data frame
  # ======================================================
  results <- data.frame(
    Term = c(trait_col, paste0(trait_col, "²")),
    Type = c("Linear", "Quadratic"),
    Beta_Coefficient = c(beta_linear, gamma_quad),
    Standard_Error = c(se_linear, se_quad),
    P_Value = c(p_linear, p_quad),
    stringsAsFactors = FALSE
  )
  
  results$Variance <- results$Standard_Error^2
  
  return(results)
}
