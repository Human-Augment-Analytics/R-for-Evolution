# ======================================================
# univariate_spline.R
# Estimate univariate correlated fitness function
#
# Important Concept Explanation:
# This function calculates a UNIVARIATE CORRELATED FITNESS FUNCTION
# Definition: Individual fitness ~ Individual phenotype (single trait)
# Formula: w ~ f(z)
#
# This is a special case of correlated fitness surface (1D instead of 2D)
# Adaptive landscape would require: Mean fitness ~ Population mean phenotype
#
# IMPORTANT NOTES
#   - Traits should be standardized BEFORE calling this function
#   - Use prepare_selection_data() with standardize = TRUE and optional group
#   - The GAM smooth term estimates the shape of the fitness function
#   - DO NOT use scale() within this function (double standardization)
#   - k = NULL (default) triggers adaptive k selection based on n_unique
#   - k can be overridden by the user for manual control
# ======================================================

univariate_spline <- function(data,
                              fitness_col,
                              trait_col,
                              fitness_type = c("binary", "continuous"),
                              group = NULL,
                              k = NULL) {
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

  # ======================================================
  # Adaptive k selection (before group splitting) based on full dataset unique values
  # ======================================================
  if (is.null(k)) {
    n_unique_all <- length(unique(data[[trait_col]][complete.cases(data[[trait_col]])]))
    k <- min(10, max(5, n_unique_all - 1))
    message("Adaptive k selected: k = ", k, " (n_unique = ", n_unique_all, ")")
  }

  # ======================================================
  # If group is specified, run separate analyses for each group
  # ======================================================
  if (!is.null(group)) {
    if (!group %in% names(data)) {
      stop("Group column '", group, "' not found in data")
    }

    groups <- unique(data[[group]])
    results_list <- list()

    for (g in groups) {
      data_g <- data[data[[group]] == g, ]

      # Recursively call without group, passing resolved k
      res <- univariate_spline(
        data         = data_g,
        fitness_col  = fitness_col,
        trait_col    = trait_col,
        fitness_type = fitness_type,
        group        = NULL,
        k            = k # k is now a number, not NULL
      )

      # Add group identifier
      res$group_value <- g
      results_list[[as.character(g)]] <- res
    }

    # Return list of results with group info
    attr(results_list, "grouped") <- TRUE
    attr(results_list, "groups") <- groups
    attr(results_list, "group_col") <- group
    return(results_list)
  }

  # ======================================================
  # Main analysis (no grouping)
  # ======================================================

  # Check if trait appears to be standardized
  z_mean <- mean(data[[trait_col]], na.rm = TRUE)
  z_sd <- sd(data[[trait_col]], na.rm = TRUE)

  message("IMPORTANT: Traits should already be standardized (mean = 0, SD = 1).")
  message("           Do NOT apply scale() again within this function.")

  if (abs(z_mean) > 0.1 || abs(z_sd - 1) > 0.1) {
    warning(
      "Trait '", trait_col, "' does not appear standardized ",
      "(mean = ", round(z_mean, 3), ", SD = ", round(z_sd, 3), "). ",
      "Consider using prepare_selection_data() first."
    )
  } else {
    message("Trait appears standardized (mean ≈ 0, SD ≈ 1)")
  }

  if (fitness_type == "continuous") {
    # For continuous fitness, use relative fitness if available
    if ("relative_fitness" %in% names(data)) {
      y <- data[["relative_fitness"]]
      fit_note <- "Using relative_fitness column"
    } else {
      y <- data[[fitness_col]] / mean(data[[fitness_col]], na.rm = TRUE)
      fit_note <- "Computed relative fitness on the fly (pooled)"
    }
    fam <- stats::gaussian()
    family_name <- "gaussian"
  } else {
    # Binary fitness - use original 0/1 values
    y <- data[[fitness_col]]
    fam <- stats::binomial("logit")
    family_name <- "binomial(logit)"

    unique_vals <- unique(y[!is.na(y)])
    if (!all(unique_vals %in% c(0, 1))) {
      warning(
        "fitness_type = 'binary' but values are not all 0/1. ",
        "Proceeding but results may be unreliable."
      )
    }
    fit_note <- "Using original binary fitness"
  }

  df <- data
  df[[".y"]] <- y

  # ======================================================
  # Safety check: cap k at subset-level unique values
  # ======================================================
  n_unique <- length(unique(df[[trait_col]][complete.cases(df[[trait_col]])]))
  n_obs <- sum(complete.cases(df[[trait_col]], y))

  k_adj <- min(k, max(5, n_unique - 1))
  if (k_adj < k) {
    warning(
      "Reducing k from ", k, " to ", k_adj,
      " (only ", n_unique, " unique values in this subset)"
    )
    k <- k_adj
  }

  if (n_obs < k * 2) {
    warning(
      "Sample size (", n_obs, ") may be insufficient for k = ", k,
      ". Consider reducing k."
    )
  }

  # Create formula with smooth term
  fml <- stats::as.formula(paste0(".y ~ s(", trait_col, ", k = ", k, ")"))

  # Fit GAM
  fit <- tryCatch(
    {
      mgcv::gam(fml,
        data      = df,
        family    = fam,
        method    = "REML",
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

  # Create prediction grid
  rng <- range(df[[trait_col]], na.rm = TRUE)
  grid <- data.frame(seq(rng[1], rng[2], length.out = 200))
  names(grid) <- trait_col

  # Predict on response scale with 95% CI
  pr <- stats::predict(fit, newdata = grid, se.fit = TRUE, type = "link")
  linkinv <- fit$family$linkinv

  grid$fit <- linkinv(pr$fit)
  grid$lwr <- linkinv(pr$fit - 1.96 * pr$se.fit)
  grid$upr <- linkinv(pr$fit + 1.96 * pr$se.fit)

  if (fitness_type == "binary") {
    grid$lwr <- pmax(grid$lwr, 0)
    grid$upr <- pmin(grid$upr, 1)
  }

  result <- list(
    model        = fit,
    grid         = grid,
    trait        = trait_col,
    fitness_type = fitness_type,
    family       = family_name,
    k            = k,
    n_obs        = n_obs,
    fit_note     = fit_note,
    group_used   = NULL,
    trait_mean   = z_mean,
    trait_sd     = z_sd,
    surface_type = "correlated_fitness_univariate",
    note         = "Univariate correlated fitness function (individual fitness)"
  )

  class(result) <- "univariate_fitness"

  return(result)
}
