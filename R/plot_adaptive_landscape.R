plot_adaptive_landscape <- function(
  landscape,
  trait_cols,
  original_data = NULL,
  group_col = NULL,
  bins = 12,
  show_optimum = TRUE,
  show_actual_means = TRUE
) {
    # Input validation
    if (!inherits(landscape, "adaptive_landscape")) {
        warning("Object is not of class 'adaptive_landscape'")
    }

    df <- landscape$grid
    names(df)[1:2] <- trait_cols

    # Base plot
    p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
            x = .data[[trait_cols[1]]],
            y = .data[[trait_cols[2]]],
            z = .data[[".mean_fit"]]
        )
    ) +
        ggplot2::geom_contour_filled(bins = bins) +
        ggplot2::geom_contour(color = "black", alpha = 0.3) +
        ggplot2::labs(
            x = trait_cols[1],
            y = trait_cols[2],
            fill = "Mean Fitness",
            title = "Adaptive Landscape",
            subtitle = "Mean fitness as function of population mean phenotype"
        ) +
        ggplot2::theme_minimal(base_size = 12)

    # Add actual population means
    if (show_actual_means && !is.null(landscape$actual_population_means)) {
        p <- p +
            ggplot2::geom_point(
                data = landscape$actual_population_means,
                ggplot2::aes(
                    x = .data[[trait_cols[1]]],
                    y = .data[[trait_cols[2]]]
                ),
                color = "red",
                size = 4,
                shape = 19
            )

        # Add labels if group_col provided
        if (!is.null(group_col) && group_col %in% names(landscape$actual_population_means)) {
            p <- p +
                ggrepel::geom_text_repel(
                    data = landscape$actual_population_means,
                    ggplot2::aes(
                        x = .data[[trait_cols[1]]],
                        y = .data[[trait_cols[2]]],
                        label = .data[[group_col]]
                    ),
                    size = 3,
                    box.padding = 0.5
                )
        }
    }

    # Add optimum point
    if (show_optimum && !is.null(landscape$optimum)) {
        p <- p +
            ggplot2::geom_point(
                data = landscape$optimum,
                ggplot2::aes(
                    x = .data[[trait_cols[1]]],
                    y = .data[[trait_cols[2]]]
                ),
                color = "yellow",
                size = 5,
                shape = 18
            ) +
            ggplot2::annotate(
                "text",
                x = landscape$optimum[[trait_cols[1]]],
                y = landscape$optimum[[trait_cols[2]]],
                label = "Optimum",
                vjust = -1,
                size = 3,
                color = "gray30"
            )
    }

    return(p)
}


plot_adaptive_landscape_3d <- function(
  landscape,
  trait_cols,
  theta = -30,
  phi = 30
) {
    if (!requireNamespace("plot3D", quietly = TRUE)) {
        stop("Package 'plot3D' required for 3D plotting")
    }

    df <- landscape$grid
    x <- unique(df[[trait_cols[1]]])
    y <- unique(df[[trait_cols[2]]])
    z <- matrix(df$.mean_fit, nrow = length(x), ncol = length(y))

    plot3D::persp3D(
        x = x,
        y = y,
        z = z,
        xlab = trait_cols[1],
        ylab = trait_cols[2],
        zlab = "Mean Fitness",
        main = "Adaptive Landscape",
        theta = theta,
        phi = phi,
        shade = 0.3,
        ticktype = "detailed"
    )
}
