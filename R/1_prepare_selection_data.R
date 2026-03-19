# prepare_selection_data()
#
# purpose:
# 1. check fitness and traits columns
# 2. handle missing values
# 3. standardize trait variables
# 4. compute relative fitness
#
# example of using this function:
# df <- data.frame(
# offspring = c(2,4,0,3),
# height = c(10.2, 11.5, 9.8, 10.9),
# mass = c(5.1, 5.8, 4.9, 5.4)
# )
# df_prepared <- prepare_selection_data(
#   data = df,
#   fitness_col = "offspring",
#   trait_cols = c("height", "mass"),
#   na_action = "drop"
# )
# results:
# offspring  height   mass  relative_fitness
# 1         2  -0.54   -0.53        0.89
# 2         4   1.22    1.32        1.78
# 3         0  -1.08   -1.06        0.00
# 4         3   0.41    0.26        1.33
#
#
# mean of c(2,4,0,3) = 2.25
# relative fitness is (2,4,0,3)/2.25 = (0.89, 1.78, 0.00, 1.33)
# fitness tell who succeed and traits tell why they succeed
# selection is about traits associated with fitness, fitness itself not inherited, traits (genes) are, selections only leads to evolution if
# traits affect fitness or traits are heritable


prepare_selection_data <- function(data,
                                   fitness_col,
                                   trait_cols,
                                   standardize = TRUE,
                                   add_relative = TRUE,
                                   na_action = c("warn", "drop", "none"),
                                   name_relative = "relative_fitness") {
  # NA handling option
  na_action <- match.arg(na_action)

  df <- data.frame(data, check.names = FALSE)

  if (!is.character(fitness_col) || length(fitness_col) != 1L) {
    stop("`fitness_col` must be a single column name (character)")
  }

  if (!all(c(fitness_col, trait_cols) %in% names(df))) {
    stop("`fitness_col` or some `trait_cols` not found in data")
  }

  # Coerce fitness to numeric if it's logical or integer
  if (is.logical(df[[fitness_col]]) || is.integer(df[[fitness_col]])) {
    df[[fitness_col]] <- as.numeric(df[[fitness_col]])
  }

  # check rows with NA in fitness or trait columns
  cols_check <- c(fitness_col, trait_cols)
  na_rows <- !stats::complete.cases(df[, cols_check, drop = FALSE])

  if (any(na_rows)) {
    n_bad <- sum(na_rows)
    msg <- sprintf(
      "Detected %d row(s) with NA in fitness/traits: %s",
      n_bad, paste(cols_check, collapse = ", ")
    )
    if (na_action == "warn") {
      warning(msg)
    } else if (na_action == "drop") {
      df <- df[!na_rows, , drop = FALSE]
    } # if "none": do nothing
  }

  # Standardize traits
  if (standardize) {
    for (t in trait_cols) {
      # Keep vectorized scale, strip attributes
      df[[t]] <- as.numeric(scale(df[[t]]))
    }
  }

  # Add relative fitness
  if (add_relative) {
    mean_fit <- mean(df[[fitness_col]], na.rm = TRUE)

    if (!is.finite(mean_fit) || mean_fit == 0) {
      warning("Mean fitness is zero or non-finite; cannot compute relative fitness. Skipping.")
    } else {
      df[[name_relative]] <- df[[fitness_col]] / mean_fit
    }
  }

  df
}
