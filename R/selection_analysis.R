# ============================================================================
# selection_analysis
#
# Purpose: Complete selection analysis wrapper that returns full results
#
# This function is a convenience wrapper that runs:
#   1. Data preparation (standardization, relative fitness)
#   2. Linear selection analysis (β)
#   3. Nonlinear selection analysis (γ, γ_ij)
#   4. Coefficient extraction
#
# It returns a comprehensive list with prepared data, models, and results,
# making it suitable for saving as an RDS file for later inspection.
#
# For quick results (just the coefficients table), use selection_coefficients()
#
# Parameters:
#   data         : data frame with fitness and trait measurements
#   fitness_col  : name of the fitness column
#   trait_cols   : vector of trait column names
#   standardize  : if TRUE, standardize traits to mean 0, SD 1
#   group        : optional grouping variable (e.g., "year", "site")
#                  When specified, standardization and relative fitness are
#                  calculated separately within each group.
#   family       : optional GLM family (overrides automatic detection)
#
# Returns:
#   List with:
#     coefficients_table : data frame of selection gradients (β, γ, γ_ij)
#     prepared_data      : cleaned and standardized data
#     fitness_type       : "binary" or "continuous"
#     analysis_date      : date of analysis
#     models             : list(linear = linear_model, nonlinear = nonlinear_model)
#     group_used         : grouping variable used (if any)
# ============================================================================

selection_analysis <- function(data,
                               fitness_col,
                               trait_cols,
                               standardize = TRUE,
                               group = NULL, # ⭐ NEW
                               family = NULL) {
  if (length(trait_cols) < 1) {
    stop("At least 1 trait is required for selection analysis")
  }


  df <- prepare_selection_data(
    data = data,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    standardize = standardize,
    group = group,
    add_relative = TRUE,
    na_action = "warn"
  )

  # Detect family and fitness type
  if (is.null(family)) {
    det <- detect_family(df[[fitness_col]])
    fam <- det$family
    fitness_type <- det$type
  } else {
    fam <- family
    fitness_type <- ifelse(fam$family == "binomial", "binary", "continuous")
  }


  # Returns:
  #   For continuous: model (lm), summary (with p-values), anova, vif
  #   For binary: model$ols (lm), model$glm (logistic), summary$ols, summary$glm, anova
  lin <- analyze_linear_selection(
    data = df,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    fitness_type = fitness_type
  )

  # Returns same structure as linear_result
  nonlin <- analyze_nonlinear_selection(
    data = df,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    fitness_type = fitness_type
  )


  # 5. Extract coefficients
  #   - Binary: OLS coefficients + GLM p-values
  #   - Continuous: all from OLS
  coeff_tab <- extract_linear_coefficients(trait_cols, lin)

  if (length(trait_cols) >= 2) {
    coeff_tab <- rbind(
      coeff_tab,
      extract_quadratic_coefficients(trait_cols, nonlin),
      extract_interaction_coefficients(trait_cols, nonlin)
    )
  }

  linear_model <- if (fitness_type == "binary") {
    list(ols = lin$model$ols, glm = lin$model$glm)
  } else {
    lin$model
  }

  nonlinear_model <- if (fitness_type == "binary") {
    list(ols = nonlin$model_ols, glm = nonlin$model_glm)
  } else {
    nonlin$model_ols
  }

  list(
    coefficients_table = coeff_tab,
    prepared_data = df,
    fitness_type = fitness_type,
    analysis_date = Sys.Date(),
    models = list(
      linear = linear_model,
      nonlinear = nonlinear_model
    ),
    group_used = group
  )
}
