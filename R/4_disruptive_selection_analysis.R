analyze_disruptive_selection <- function(
    data,
    fitness_col,
    trait_col,
    fitness_type = c("binary", "continuous"),
    standardize = TRUE
) {
  
  fitness_type <- match.arg(fitness_type)
  
  # --- 1) Prepare data ---
  # Standardize trait (optional) and compute relative fitness
  df <- prepare_selection_data(
    data = data,
    fitness_col = fitness_col,
    trait_cols = trait_col,
    standardize = standardize,
    add_relative = TRUE,
    na_action = "warn"
  )
  
  # Choose response variable
  if (fitness_type == "continuous") {
    response <- if ("relative_fitness" %in% names(df)) {
      "relative_fitness"
    } else {
      fitness_col
    }
    family <- gaussian()
    p_col <- "Pr(>|t|)"
  } else {
    response <- fitness_col
    family <- binomial("logit")
    p_col <- "Pr(>|z|)"
  }
  
  # --- 2) Fit quadratic model ---
  # w ~ z + z^2
  formula_quad <- as.formula(
    paste(response, "~", trait_col, "+ I(", trait_col, "^2)")
  )
  
  model <- glm(formula_quad, data = df, family = family)
  coef_summary <- summary(model)$coefficients
  
  quad_term <- paste0("I(", trait_col, "^2)")
  
  # --- 3) Extract coefficients ---
  beta_linear <- coef_summary[trait_col, "Estimate"]
  se_linear   <- coef_summary[trait_col, "Std. Error"]
  p_linear    <- coef_summary[trait_col, p_col]
  
  # Quadratic term (gamma = 2 * b_z^2)
  if (quad_term %in% rownames(coef_summary)) {
    gamma_quad <- 2 * coef_summary[quad_term, "Estimate"]
    se_quad    <- 2 * coef_summary[quad_term, "Std. Error"]
    p_quad     <- coef_summary[quad_term, p_col]
  } else {
    gamma_quad <- NA_real_
    se_quad    <- NA_real_
    p_quad     <- NA_real_
  }
  
  # --- 4) Assemble results ---
  results <- data.frame(
    Term = c(trait_col, paste0(trait_col, "²")),
    Type = c("Linear", "Quadratic"),
    Beta_Coefficient = c(beta_linear, gamma_quad),
    Standard_Error   = c(se_linear, se_quad),
    P_Value          = c(p_linear, p_quad),
    stringsAsFactors = FALSE
  )
  
  results$Variance <- results$Standard_Error^2
  
  return(results)
}
