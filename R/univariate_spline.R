# ======================================================
# univariate_spline.R
# Estimate univariate correlated fitness function
# Important Concept Explanation:
# This function calculates a UNIVARIATE CORRELATED FITNESS FUNCTION
# Definition: Individual fitness ~ Individual phenotype (single trait)
# Formula: w ~ f(z)
# This is a special case of correlated fitness surface (1D instead of 2D)
# Adaptive landscape would require: Mean fitness ~ Population mean phenotype
# ======================================================

# Fit univariate spline for correlated fitness function
univariate_spline <- function(data,
                              fitness_col,
                              trait_col,
                              fitness_type = c("binary", "continuous"),
                              k = 10) {
  fitness_type <- match.arg(fitness_type)

  # Input validation
  if (length(trait_col) != 1L) {
    stop("`trait_col` must be a single column name.")
  }
  if (!trait_col %in% names(data)) {
    stop("`trait_col` not found in `data`.")
  }
  if (!is.numeric(data[[trait_col]])) {
    stop("`trait_col` must be numeric (standardize upstream if needed).")
  }

  # Prepare fitness variable based on type
  if (fitness_type == "continuous") {
    # For continuous fitness, use relative fitness if available
    if ("relative_fitness" %in% names(data)) {
      y <- data[["relative_fitness"]]
      fit_note <- "Using relative_fitness column"
    } else {
      y <- data[[fitness_col]] / mean(data[[fitness_col]], na.rm = TRUE)
      fit_note <- "Computed relative fitness on the fly"
    }
    fam <- stats::gaussian()
    family_name <- "gaussian"
  } else {
    # Binary fitness - use original 0/1 values
    y <- data[[fitness_col]]
    fam <- stats::binomial("logit")
    family_name <- "binomial(logit)"

    # Check if really binary
    unique_vals <- unique(y[!is.na(y)])
    if (!all(unique_vals %in% c(0, 1))) {
      warning(
        "fitness_type = 'binary' but values are not all 0/1. ",
        "Proceeding but results may be unreliable."
      )
    }
    fit_note <- "Using original binary fitness"
  }

  # Prepare data frame for modeling
  df <- data
  df[[".y"]] <- y

  # Create formula with smooth term
  fml <- stats::as.formula(paste0(".y ~ s(", trait_col, ", k = ", k, ")"))

  # Fit GAM with error handling
  fit <- tryCatch(
    {
      mgcv::gam(fml,
        data = df,
        family = fam,
        method = "REML",
        na.action = stats::na.omit
      )
    },
    error = function(e) {
      stop(
        "GAM fitting failed: ", e$message,
        "\nTry reducing k (currently k = ", k, ")"
      )
    }
  )

  # Check convergence
  if (!is.null(fit$converged) && !fit$converged) {
    warning("GAM algorithm did not fully converge")
  }

  # Create prediction grid across observed trait range
  rng <- range(df[[trait_col]], na.rm = TRUE)
  grid <- data.frame(seq(rng[1], rng[2], length.out = 200))
  names(grid) <- trait_col

  # Predict on link scale, then transform to response scale
  pr <- stats::predict(fit, newdata = grid, se.fit = TRUE, type = "link")
  linkinv <- fit$family$linkinv

  grid$fit <- linkinv(pr$fit)
  grid$lwr <- linkinv(pr$fit - 1.96 * pr$se.fit)
  grid$upr <- linkinv(pr$fit + 1.96 * pr$se.fit)

  # Prepare return object with metadata
  result <- list(
    model = fit,
    grid = grid,
    trait = trait_col,
    fitness_type = fitness_type,
    family = family_name,
    k = k,
    n_obs = sum(complete.cases(df[[trait_col]], y)),
    fit_note = fit_note,
    # Add type identifier
    surface_type = "correlated_fitness_univariate",
    note = "Univariate correlated fitness function (individual fitness)"
  )

  class(result) <- "univariate_fitness"

  return(result)
}


# Print method for univariate_fitness objects
print.univariate_fitness <- function(x, ...) {
  cat("Univariate Correlated Fitness Function\n")
  cat("======================================\n")
  cat("Trait:", x$trait, "\n")
  cat("Fitness type:", x$fitness_type, "\n")
  cat("Family:", x$family, "\n")
  cat("Smooth term k =", x$k, "\n")
  cat("Observations:", x$n_obs, "\n")
  cat("Note:", x$fit_note, "\n")
  cat("\n", x$note, "\n")

  cat("\nFitness range on grid: [",
    round(min(x$grid$fit), 3), ", ",
    round(max(x$grid$fit), 3), "]\n",
    sep = ""
  )
}


# Summary method for univariate_fitness objects
summary.univariate_fitness <- function(object, ...) {
  cat("Univariate Correlated Fitness Function - Summary\n")
  cat("================================================\n")
  print.univariate_fitness(object)

  cat("\nModel Summary:\n")
  print(summary(object$model))
}
