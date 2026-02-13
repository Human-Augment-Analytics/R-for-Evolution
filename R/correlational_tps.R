
# purpose： to estimate a two trait fitness surface using a parametric quadratic model 
# the function fits where 
# a logistic regression when fitness is binary 
# a TPS when fitness is continuous 



# ---------- helpers ----------
`%||%` <- function(a, b) if (!is.null(a)) a else b


correlational_tps <- function(
    data,
    fitness_col,
    trait_cols,
    grid_n = 60,
    method = "auto",
    scale_traits = TRUE,
    k = 30,
    use_relative = FALSE
) {
  
  stopifnot(length(trait_cols) == 2L)
  
  if (use_relative && "relative_fitness" %in% names(data)) {
    fitness_col <- "relative_fitness"
  }
  
  need <- c(fitness_col, trait_cols)
  
  if (!all(need %in% names(data))) {
    stop("Missing columns: ", paste(setdiff(need, names(data)), collapse = ", "))
  }
  
  # extract variables and coerce to numeric
  y  <- as.numeric(data[[fitness_col]])
  x1 <- as.numeric(data[[trait_cols[1]]])
  x2 <- as.numeric(data[[trait_cols[2]]])
  
  if(any(!is.numeric(y)) || any(!is.numeric(x1)) || any(!is.numeric(x2))) {
    stop("Non-numeric values detected in fitness or trait columns")
  }
  
  # remove incomplete cases 
  keep <- stats::complete.cases(y, x1, x2)
  y  <- y[keep]; x1 <- x1[keep]; x2 <- x2[keep]
  
  if (length(y) < 10) stop("Too few complete cases: ", length(y), " (<10)")
  
  # detect binary fitness 
  uniq_y <- unique(y)
  is_binary <- length(uniq_y) == 2 && all(sort(uniq_y) == c(0, 1))
  
  if (method == "auto") {
    method <- if (is_binary) "gam" else "tps"
  }
  
  if (!method %in% c("gam", "tps")) stop("method must be 'auto' | 'gam' | 'tps'")
  
  # check trait variation 
  if (length(unique(x1)) < 3 || length(unique(x2)) < 3) {
    stop("Too few unique trait values: ",
         trait_cols[1], " has ", length(unique(x1)),
         " unique values; ", trait_cols[2], " has ", length(unique(x2)), " unique values.")
  }
  
  cat("Data type:", ifelse(is_binary, "binary", "continuous"), "\n")
  cat("Selected method:", method, "\n")
  cat("Data points:", length(y), "\n")
  
  # optional trait standarization 
  scaler <- list(m1 = mean(x1), s1 = stats::sd(x1), m2 = mean(x2), s2 = stats::sd(x2))
  
  if (!is.finite(scaler$s1) || scaler$s1 == 0) scaler$s1 <- 1
  if (!is.finite(scaler$s2) || scaler$s2 == 0) scaler$s2 <- 1
  
  x1s <- if (scale_traits) (x1 - scaler$m1)/scaler$s1 else x1
  x2s <- if (scale_traits) (x2 - scaler$m2)/scaler$s2 else x2
  
  # construct prediction grid in original trait space 
  g1 <- seq(min(x1, na.rm = TRUE), max(x1, na.rm = TRUE), length.out = grid_n)
  g2 <- seq(min(x2, na.rm = TRUE), max(x2, na.rm = TRUE), length.out = grid_n)
  
  grid <- expand.grid(
    g1, g2,
    KEEP.OUT.ATTRS = FALSE
  )
  
  names(grid) <- trait_cols
  
  # scale grid values for prediction if traits were standardized 
  grid_scaled <- grid
  
  if (scale_traits) {
    grid_scaled[[trait_cols[1]]] <- (grid[[trait_cols[1]]] - scaler$m1)/scaler$s1
    grid_scaled[[trait_cols[2]]] <- (grid[[trait_cols[2]]] - scaler$m2)/scaler$s2
  }
  
 
  if (method == "gam") {
    
    if (!requireNamespace("mgcv", quietly = TRUE)) {
      stop("mgcv package required. Please install.packages('mgcv')")
    }
    
    
    if(any(!is.numeric(y)) || any(!is.numeric(x1s)) || any(!is.numeric(x2s))) {
      stop("Non-numeric values in GAM fitting data")
    }
    
    fam <- if (is_binary) stats::binomial("logit") else stats::gaussian()
    

    df_fit <- data.frame(
      .y = as.numeric(y),
      .x1 = as.numeric(x1s),
      .x2 = as.numeric(x2s)
    )
    

    df_fit <- df_fit[complete.cases(df_fit), ]
    
    cat("GAM fitting with", nrow(df_fit), "observations\n")
    
    # try different GAM formulas 
    try_formulas <- list(

      main = .y ~ s(.x1, .x2, bs = "tp", k = min(k, nrow(df_fit)-1)),

      alt1 = .y ~ s(.x1, k = min(floor(k/2), nrow(df_fit)-1)) + s(.x2, k = min(floor(k/2), nrow(df_fit)-1)),

      alt2 = .y ~ .x1 + .x2
    )
    
    fit <- NULL
    formula_used <- NULL
    
    for (form_name in names(try_formulas)) {
      if (is.null(fit)) {
        tryCatch({
          cat("  Trying formula:", form_name, "\n")
          fit <- mgcv::gam(try_formulas[[form_name]], 
                           data = df_fit, 
                           family = fam, 
                           method = "REML")
          formula_used <- form_name
          cat("Success with formula:", form_name, "\n")
          break
        }, error = function(e) {
          cat("ailed with formula", form_name, ":", e$message, "\n")
        })
      }
    }
    
    if (is.null(fit)) {
      stop("All GAM formula attempts failed")
    }
    
    # predict on grid 
    newdat <- data.frame(
      .x1 = as.numeric(grid_scaled[[trait_cols[1]]]),
      .x2 = as.numeric(grid_scaled[[trait_cols[2]]])
    )
    
    .fit <- tryCatch({
      as.numeric(stats::predict(fit, newdata = newdat, type = "response"))
    }, error = function(e) {
      cat("Prediction failed, using mean:", e$message, "\n")
      rep(mean(y, na.rm = TRUE), nrow(newdat))
    })
    
    grid$.fit <- .fit
    
    if (anyNA(grid$.fit)) {
      warning("NA predictions detected, using mean imputation")
      grid$.fit[is.na(grid$.fit)] <- mean(grid$.fit, na.rm = TRUE)
    }
    
    cat("Success Predictions range:", round(range(grid$.fit), 4), "\n")
    return(list(
      model = fit,
      grid = grid,
      method = "gam",
      formula_used = formula_used,
      data_type = ifelse(is_binary, "binary", "continuous"),
      trait_cols = trait_cols,
      fitness_col = fitness_col
    ))
  }
  
  # method == "tps"
  if (!requireNamespace("fields", quietly = TRUE)) {
    stop("For continuous fitness with tps method, install fields: install.packages('fields')")
  }
  
  if (is_binary) {
    warning("Binary data detected but method='tps' chosen. Using Tps on binary outcomes (not ideal).")
  }
  
  Xs <- cbind(as.numeric(x1s), as.numeric(x2s))
  
  tps_model <- tryCatch(
    fields::Tps(Xs, as.numeric(y)),
    error = function(e) {
      warning("Tps failed, retrying with m=2: ", e$message)
      fields::Tps(Xs, as.numeric(y), m = 2)
    }
  )
  
  grid_scaled_mat <- cbind(
    as.numeric(grid_scaled[[trait_cols[1]]]),
    as.numeric(grid_scaled[[trait_cols[2]]])
  )
  
  .fit <- as.numeric(stats::predict(tps_model, grid_scaled_mat))
  grid$.fit <- .fit
  
  if (anyNA(grid$.fit)) {
    warning("NA predictions, using mean imputation")
    grid$.fit[is.na(grid$.fit)] <- mean(grid$.fit, na.rm = TRUE)
  }
  

  return(list(
    model = tps_model,
    grid = grid,
    method = "tps",
    data_type = ifelse(is_binary, "binary", "continuous"),
    trait_cols = trait_cols,
    fitness_col = fitness_col
  ))
}