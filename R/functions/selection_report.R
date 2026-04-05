# ============================================================================
# selection_report
#
# Purpose: Generate a comprehensive report with coefficients, univariate splines,
#          and bivariate fitness surfaces
#
# This function creates:
#   1. Selection coefficients table (β, γ, γ_ij)
#   2. Univariate fitness functions (spline-based)
#   3. Correlated fitness surfaces (TPS or GAM)
#   4. Optional saving of plots to disk
#
# Parameters:
#   data         : data frame with fitness and trait measurements
#   fitness_col  : name of the fitness column
#   trait_cols   : vector of trait column names
#   fitness_type : "binary" or "continuous"
#   standardize  : if TRUE, standardize traits to mean 0, SD 1
#   group        : optional grouping variable (e.g., "year", "site")
#                  When specified, standardization and relative fitness are
#                  calculated separately within each group.
#   spline_k     : basis dimension for univariate spline
#   bins         : number of contour bins for bivariate surfaces
#   save_dir     : optional directory to save plots (if NULL, no saving)
#
# Returns:
#   List with:
#     table         : selection coefficients data frame
#     uni_models    : list of univariate spline models
#     uni_plots     : list of univariate fitness plots
#     tps_models    : list of bivariate surface models
#     tps_plots     : list of bivariate surface plots
#     prepared_data : cleaned and standardized data
# ============================================================================

#' Generate a comprehensive selection analysis report
#'
#' Runs standard analyses and produces a table of selection coefficients, univariate splines, and bivariate fitness surfaces.
#'
#' @param data A data frame containing fitness and trait measurements.
#' @param fitness_col A string specifying the name of the fitness column.
#' @param trait_cols A character vector of trait column names.
#' @param fitness_type A string indicating the fitness type: \code{"binary"} or \code{"continuous"}.
#' @param standardize Logical indicating whether traits should be standardized. Default is \code{TRUE}.
#' @param group Optional string specifying a grouping variable.
#' @param spline_k Integer specifying the basis dimension for univariate splines. Default is 10.
#' @param bins Integer specifying the number of contour bins for bivariate surfaces. Default is 12.
#' @param save_dir Optional string path. If provided, plots and tables will be saved to this directory.
#'
#' @return A list containing the coefficients table, models, plots, and prepared data.
#' @export
#'
#' @examples
#' \dontrun{
#' report <- selection_report(my_data, "fitness", c("trait1", "trait2"))
#' }
selection_report <- function(data,
                             fitness_col,
                             trait_cols,
                             fitness_type = c("binary", "continuous"),
                             standardize = TRUE,
                             group = NULL,
                             spline_k = 10,
                             bins = 12,
                             save_dir = NULL) {
  fitness_type <- match.arg(fitness_type)


  df <- prepare_selection_data(
    data = data,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    standardize = standardize,
    group = group,
    add_relative = TRUE,
    na_action = "warn"
  )


  coef_tab <- selection_coefficients(
    data = df,
    fitness_col = fitness_col,
    trait_cols = trait_cols,
    fitness_type = fitness_type,
    standardize = FALSE, # Already standardized in prepare_selection_data
    group = group,
    use_relative_for_fit = (fitness_type == "continuous")
  )

  # Add variance column (already computed in selection_coefficients, but ensure)
  coef_tab$Variance <- coef_tab$Standard_Error^2

  # Univariate spline models and plots
  uni_models <- list()
  uni_plots <- list()

  for (t in trait_cols) {
    um <- univariate_spline(
      data = df,
      fitness_col = fitness_col,
      trait_col = t,
      fitness_type = fitness_type,
      k = spline_k
    )
    uni_models[[t]] <- um$model

    # Use the updated plot function name
    if (exists("plot_univariate_fitness")) {
      up <- plot_univariate_fitness(
        uni = um,
        trait_col = t,
        title = paste("Univariate fitness function:", t)
      )
    } else if (exists("univariate_surface")) {
      # Fallback for backward compatibility
      up <- univariate_surface(um, t, title = paste("Univariate spline:", t))
    } else {
      up <- NULL
    }
    uni_plots[[t]] <- up
  }

  # Correlated fitness surfaces
  tps_models <- list()
  tps_plots <- list()

  if (length(trait_cols) >= 2) {
    pairs <- utils::combn(trait_cols, 2, simplify = FALSE)

    for (pr in pairs) {
      key <- paste(pr, collapse = "_")

      if (exists("correlated_fitness_surface")) {
        tp <- correlated_fitness_surface(
          data = df,
          fitness_col = fitness_col,
          trait_cols = pr,
          method = ifelse(fitness_type == "binary", "gam", "tps"),
          scale_traits = FALSE # Already standardized
        )
      } else if (exists("correlational_tps")) {
        # Fallback for backward compatibility
        tp <- correlational_tps(
          data = df,
          fitness_col = fitness_col,
          trait_cols = pr,
          use_relative = (fitness_type == "continuous")
        )
      } else {
        next
      }

      tps_models[[key]] <- tp$model

      if (exists("plot_correlated_fitness")) {
        tp_plot <- plot_correlated_fitness(tp, pr, bins = bins)
      } else if (exists("correlation_surface")) {
        tp_plot <- correlation_surface(tp, pr, bins = bins)
      } else {
        tp_plot <- NULL
      }
      tps_plots[[key]] <- tp_plot
    }
  }

  if (!is.null(save_dir)) {
    if (!dir.exists(save_dir)) {
      dir.create(save_dir, recursive = TRUE)
    }

    # Save univariate plots
    for (nm in names(uni_plots)) {
      if (!is.null(uni_plots[[nm]])) {
        ggplot2::ggsave(
          filename = file.path(save_dir, paste0("univariate_", nm, ".png")),
          plot = uni_plots[[nm]],
          width = 6, height = 4, dpi = 300
        )
      }
    }

    # Save bivariate plots
    for (nm in names(tps_plots)) {
      if (!is.null(tps_plots[[nm]])) {
        ggplot2::ggsave(
          filename = file.path(save_dir, paste0("correlational_", nm, ".png")),
          plot = tps_plots[[nm]],
          width = 6, height = 5, dpi = 300
        )
      }
    }

    write.csv(coef_tab, file.path(save_dir, "selection_coefficients.csv"), row.names = FALSE)

    cat("Report saved to:", save_dir, "\n")
    cat("  - selection_coefficients.csv\n")
    cat("  - univariate_*.png (", length(uni_plots), " files)\n", sep = "")
    cat("  - correlational_*.png (", length(tps_plots), " files)\n", sep = "")
  }

  list(
    table = coef_tab,
    uni_models = uni_models,
    uni_plots = uni_plots,
    tps_models = tps_models,
    tps_plots = tps_plots,
    prepared_data = df,
    group_used = group,
    fitness_type = fitness_type
  )
}
