
selection_analysis <- function(data,
                               fitness_col,
                               trait_cols,
                               standardize = TRUE,
                               family = NULL) {
  if (length(trait_cols) < 1) {
    stop("At least 1 trait is required for selection analysis")
  }

  df <- prepare_selection_data(data, fitness_col, trait_cols, standardize)
  fam <- if (is.null(family)) detect_family(df[[fitness_col]]) else family
  fitness_type <- ifelse(fam$family == "binomial", "binary", "continuous")

  lin <- analyze_linear_selection(df, fitness_col, trait_cols, fitness_type)
  nonlin <- analyze_nonlinear_selection(df, fitness_col, trait_cols, fitness_type)

  coeff_tab <- extract_linear_coefficients(trait_cols, lin)
  if (length(trait_cols) >= 2) {
    coeff_tab <- rbind(
      coeff_tab,
      extract_quadratic_coefficients(trait_cols, nonlin),
      extract_interaction_coefficients(trait_cols, nonlin)
    )
  }

  list(
    coefficients_table = coeff_tab,
    prepared_data = df,
    fitness_type = fitness_type,
    analysis_date = Sys.Date(),
    models = list(linear = lin$model, nonlinear = nonlin$model)
  )
}
