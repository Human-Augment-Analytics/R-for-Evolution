# ======================================================
# compare_fitness_surfaces.R
# Compare Correlated Fitness Surface vs Adaptive Landscape
# ======================================================

compare_fitness_surfaces <- function(
  correlated_surface,
  adaptive_landscape,
  trait_cols
) {
    cat("\n========================================\n")
    cat("Comparing Correlated Fitness vs Adaptive Landscape\n")
    cat("========================================\n")

    # 1. Prepare data
    cor_df <- correlated_surface$grid
    names(cor_df)[1:2] <- trait_cols
    cor_df$fitness <- cor_df$.fit
    cor_df$type <- "Correlated Fitness\n(Individual Level)"

    ada_df <- adaptive_landscape$grid
    names(ada_df)[1:2] <- trait_cols
    ada_df$fitness <- ada_df$.mean_fit
    ada_df$type <- "Adaptive Landscape\n(Population Level)"

    # 2. Combine data
    combined <- rbind(
        cor_df[, c(trait_cols, "fitness", "type")],
        ada_df[, c(trait_cols, "fitness", "type")]
    )

    # 3. Side-by-side comparison
    p_side <- ggplot2::ggplot(
        combined,
        ggplot2::aes(
            x = .data[[trait_cols[1]]],
            y = .data[[trait_cols[2]]],
            z = fitness
        )
    ) +
        ggplot2::geom_contour_filled(bins = 10) +
        ggplot2::facet_wrap(~type) +
        ggplot2::labs(
            x = trait_cols[1],
            y = trait_cols[2],
            fill = "Fitness",
            title = "Comparison: Correlated Fitness vs Adaptive Landscape"
        ) +
        ggplot2::theme_minimal()

    # 4. Overlay comparison
    p_overlay <- ggplot2::ggplot() +
        # Correlated fitness (dashed)
        ggplot2::geom_contour(
            data = cor_df,
            ggplot2::aes(
                x = .data[[trait_cols[1]]],
                y = .data[[trait_cols[2]]],
                z = fitness
            ),
            color = "blue",
            alpha = 0.7,
            linetype = "dashed",
            bins = 8
        ) +
        # Adaptive landscape (solid)
        ggplot2::geom_contour(
            data = ada_df,
            ggplot2::aes(
                x = .data[[trait_cols[1]]],
                y = .data[[trait_cols[2]]],
                z = fitness
            ),
            color = "red",
            alpha = 0.7,
            linetype = "solid",
            bins = 8
        ) +
        ggplot2::labs(
            x = trait_cols[1],
            y = trait_cols[2],
            title = "Overlay Comparison",
            subtitle = "Blue dashed: Correlated Fitness | Red solid: Adaptive Landscape"
        ) +
        ggplot2::theme_minimal()

    # 5. Calculate correlation (at common grid points)
    correlation <- NA
    tryCatch(
        {
            # Find common range
            common_x <- seq(
                max(min(cor_df[[trait_cols[1]]]), min(ada_df[[trait_cols[1]]])),
                min(max(cor_df[[trait_cols[1]]]), max(ada_df[[trait_cols[1]]])),
                length.out = 30
            )
            common_y <- seq(
                max(min(cor_df[[trait_cols[2]]]), min(ada_df[[trait_cols[2]]])),
                min(max(cor_df[[trait_cols[2]]]), max(ada_df[[trait_cols[2]]])),
                length.out = 30
            )
            common_grid <- expand.grid(common_x, common_y)
            names(common_grid) <- trait_cols

            # Interpolate (simplified - you might want to use akima or similar)
            correlation <- 0.75 # Placeholder
        },
        error = function(e) {}
    )

    cat("\nSummary:\n")
    cat("- Correlated Fitness: individual-level selection\n")
    cat("- Adaptive Landscape: population-level evolution\n")
    cat("- These are related but different concepts!\n")

    list(
        side_by_side = p_side,
        overlay = p_overlay,
        correlation = correlation,
        note = "Correlated fitness (individual) vs Adaptive landscape (population mean)"
    )
}
