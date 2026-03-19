plot_correlated_fitness <- function(tps, trait_cols, bins = 12) {
  stopifnot(is.list(tps), "grid" %in% names(tps))

  df <- tps$grid

  if (length(trait_cols) != 2) {
    stop("trait_cols must be length 2.")
  }

  if (!all(trait_cols %in% names(df))) {
    stop("Trait columns not found in tps$grid.")
  }

  fitness_col <- NULL

  if (".fit" %in% names(df)) {
    fitness_col <- ".fit"
  } else if ("fitness" %in% names(df)) {
    fitness_col <- "fitness"
  } else if ("pred" %in% names(df)) {
    fitness_col <- "pred"
  } else {
    stop("No fitness column found in grid.")
  }

  ggplot2::ggplot(df) +
    ggplot2::geom_contour_filled(
      ggplot2::aes(
        x = .data[[trait_cols[1]]],
        y = .data[[trait_cols[2]]],
        z = .data[[fitness_col]]
      ),
      bins = bins,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_contour(
      ggplot2::aes(
        x = .data[[trait_cols[1]]],
        y = .data[[trait_cols[2]]],
        z = .data[[fitness_col]]
      ),
      color = "black",
      alpha = 0.3,
      inherit.aes = FALSE
    ) +
    ggplot2::labs(
      x = trait_cols[1],
      y = trait_cols[2],
      fill = "Fitness",
      title = "Correlated Fitness Surface"
    ) +
    ggplot2::theme_minimal()
}

# fitness surface + original individuals + optimum point
plot_correlated_fitness_enhanced <- function(
  tps,
  trait_cols = NULL,
  original_data = NULL,
  fitness_col = NULL,
  bins = 12
) {
  df <- tps$grid

  if (!is.null(trait_cols) && length(trait_cols) == 2) {
    trait1 <- trait_cols[1]
    trait2 <- trait_cols[2]
    cat("Using provided traits:", trait1, trait2, "\n")
  } else {
    possible_traits <- setdiff(names(df), c(".fit", "fitness", "pred", "fit", "lwr", "upr"))
    if (length(possible_traits) >= 2) {
      trait1 <- possible_traits[1]
      trait2 <- possible_traits[2]
      cat("Inferred traits:", trait1, trait2, "\n")
    } else {
      stop("Cannot determine trait columns. Please provide trait_cols.")
    }
  }

  fit_col <- if (".fit" %in% names(df)) {
    ".fit"
  } else if ("fitness" %in% names(df)) {
    "fitness"
  } else {
    stop("No fitness column found in grid")
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_contour_filled(
      data = df,
      ggplot2::aes(
        x = .data[[trait1]],
        y = .data[[trait2]],
        z = .data[[fit_col]]
      ),
      bins = bins
    ) +
    ggplot2::geom_contour(
      data = df,
      ggplot2::aes(
        x = .data[[trait1]],
        y = .data[[trait2]],
        z = .data[[fit_col]]
      ),
      color = "black",
      alpha = 0.3
    )


  if (!is.null(original_data) && !is.null(fitness_col)) {
    if (all(c(trait1, trait2, fitness_col) %in% names(original_data))) {
      unique_vals <- unique(original_data[[fitness_col]])
      is_binary <- length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))

      if (is_binary) {
        p <- p +
          ggplot2::geom_point(
            data = original_data,
            ggplot2::aes(
              x = .data[[trait1]],
              y = .data[[trait2]],
              color = as.factor(.data[[fitness_col]])
            ),
            size = 2,
            alpha = 0.7
          ) +
          ggplot2::scale_color_manual(
            values = c("0" = "red", "1" = "green"),
            labels = c("0" = "Perished", "1" = "Survived"),
            name = "Outcome"
          )
      } else {
        p <- p +
          ggplot2::geom_point(
            data = original_data,
            ggplot2::aes(
              x = .data[[trait1]],
              y = .data[[trait2]],
              color = .data[[fitness_col]]
            ),
            size = 2,
            alpha = 0.7
          ) +
          ggplot2::scale_color_viridis_c(name = "Fitness")
      }
    } else {
      warning("Required columns not found in original_data")
    }
  }

  opt <- df[which.max(df[[fit_col]]), ]

  p +
    ggplot2::geom_point(
      data = opt,
      ggplot2::aes(
        x = .data[[trait1]],
        y = .data[[trait2]]
      ),
      color = "yellow",
      size = 4,
      shape = 18
    ) +
    ggplot2::labs(
      x = trait1,
      y = trait2,
      title = "Correlated Fitness Surface (Enhanced)"
    ) +
    ggplot2::theme_minimal()
}
