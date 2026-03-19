# purpose: 
# follows lande & arnold (1983) integrating 
# 1. data preparation (standardizaiton, relative fitness)
# 2. automatic detect fitness 
# 3. estimation of linear, quadratic, and correlation gradients 

selection_coefficients <- function(data, fitness_col, trait_cols,
                                   fitness_type = c("auto","binary","continuous"),
                                   standardize = TRUE,
                                   use_relative_for_fit = TRUE) {
  
  fitness_type <- match.arg(fitness_type)
  
  # 3 decide which fitness column to model 
  # binary fitness must always be on absolute values 0 or 1 
  # continuous fitness may use relative fitness 
  rel_col <- paste0(fitness_col, "_relative")

  # 1 prepare data
  df <- prepare_selection_data(
    data          = data,
    fitness_col   = fitness_col,
    trait_cols    = trait_cols,
    standardize   = standardize,
    add_relative  = TRUE,   
    na_action     = "warn",
    name_relative = rel_col
  )
  
  # 2 detect family if auto
  det <- detect_family(df[[fitness_col]])
  if (fitness_type == "auto") {
    fitness_type <- det$type
  }
  family_obj <- if (fitness_type == "binary") binomial("logit") else gaussian()
  
  model_fitness_col <-
    if (fitness_type == "binary") {
      if (use_relative_for_fit) {
        message("Binary fitness detected: modeling on absolute 0/1 ")
      }
      fitness_col
    } else {
      if (use_relative_for_fit) {
        if (!rel_col %in% names(df)) {
          stop("Relative fitness column '", rel_col, "' not found. ",
               "Ensure prepare_selection_data(add_relative=TRUE) creates it.")
        }
        rel_col
      } else {
        fitness_col
      }
    }
  
  # 4 run analysis
  linear_result <- analyze_linear_selection(
    data         = df,
    fitness_col  = model_fitness_col,
    trait_cols   = trait_cols,
    fitness_type = fitness_type
  )
  
  nonlinear_result <- analyze_nonlinear_selection(
    data         = df,
    fitness_col  = model_fitness_col,
    trait_cols   = trait_cols,
    fitness_type = fitness_type
  )
  
  # 5 extract coefficients 
  linear_coefs      <- extract_linear_coefficients(trait_cols, linear_result)
  quadratic_coefs   <- extract_quadratic_coefficients(trait_cols, nonlinear_result)
  interaction_coefs <- extract_interaction_coefficients(trait_cols, nonlinear_result)
  
  all_coefs <- rbind(linear_coefs, quadratic_coefs, interaction_coefs)
  
  # compute variance from standard errors for CI 
  all_coefs$Variance <- all_coefs$Standard_Error^2
  
  # 6 store data as attributes 
  attr(all_coefs, "fitness_type_detected") <- det$type
  attr(all_coefs, "model_family_used")     <- if (fitness_type == "binary") "binomial(logit)" else "gaussian"
  attr(all_coefs, "model_fitness_col")     <- model_fitness_col
  attr(all_coefs, "relative_available")    <- rel_col %in% names(df)
  
  return(all_coefs)
}

