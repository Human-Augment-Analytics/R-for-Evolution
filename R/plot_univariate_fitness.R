# ======================================================
# plot_univariate_fitness.R
# Visualize univariate correlated fitness function
# ======================================================

plot_univariate_fitness <- function(uni, trait_col, title = NULL) {
  # Check input type
  if (!inherits(uni, "univariate_fitness")) {
    warning("Object is not of class 'univariate_fitness'")
  }

  stopifnot(is.list(uni), "grid" %in% names(uni))
  grid <- uni$grid
  if (!all(c(trait_col, "fit", "lwr", "upr") %in% names(grid))) {
    stop("`uni$grid` must contain columns: trait, fit, lwr, upr")
  }

  # Set default title if not provided
  if (is.null(title)) {
    title <- "Univariate Correlated Fitness Function"
  }

  # Create plot with clear labeling
  ggplot2::ggplot(
    grid,
    ggplot2::aes(x = .data[[trait_col]], y = .data[["fit"]])
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data[["lwr"]],
        ymax = .data[["upr"]]
      ),
      alpha = 0.2, fill = "steelblue"
    ) +
    ggplot2::geom_line(linewidth = 0.9, color = "steelblue") +
    ggplot2::labs(
      x = trait_col,
      y = "Predicted fitness",
      title = title,
      subtitle = "Individual fitness (correlated fitness function)"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}
