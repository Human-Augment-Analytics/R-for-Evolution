# ======================================================
# test_bumpus_sparrows.R
# Integration test using Bumpus sparrow dataset
# ======================================================

cat("\n========================================\n")
cat("BUMPUS SPARROW FUNCTION TEST\n")
cat("========================================\n")

cat("Working directory:", getwd(), "\n")

# ------------------------------------------------------
# 1 Initialize environment
# ------------------------------------------------------

# Determine relative paths based on current directory
proj_root <- ifelse(basename(getwd()) == "temp", "..", ".")
r_dir <- file.path(proj_root, "R")

if (file.exists(file.path(r_dir, "0.0_initialize.R"))) {
    source(file.path(r_dir, "0.0_initialize.R"))
}

# ======================================================
# 2 Load scripts
# ======================================================

cat("\nLoading script files...\n")

script_files <- list.files(
    r_dir,
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in script_files) {
    if (basename(f) != "0.0_initialize.R") {
        source(f)
        cat("Loaded script:", basename(f), "\n")
    }
}

# ======================================================
# 3 Load functions
# ======================================================

cat("\nLoading function files...\n")

fn_files <- list.files(
    r_dir,
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in fn_files) {
    source(f)
    cat("Loaded:", basename(f), "\n")
}

# ======================================================
# 4 Load plotting
# ======================================================

cat("\nLoading plotting functions...\n")

plot_files <- list.files(
    r_dir,
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in plot_files) {
    source(f)
    cat("Loaded plot:", basename(f), "\n")
}

# ======================================================
# 5 Output directories (defined AFTER scripts)
# ======================================================

rm(list = intersect(ls(), c("output_dir", "figure_dir", "table_dir", "model_dir")))

output_dir <- file.path(r_dir, "bumpus_sparrows_results")
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
model_dir <- file.path(output_dir, "models")

dirs <- c(output_dir, figure_dir, table_dir, model_dir)
lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)

cat("\nResults saved to:\n")
cat("  Figures:", figure_dir, "\n")
cat("  Tables: ", table_dir, "\n")
cat("  Models: ", model_dir, "\n")


# ------------------------------------------------------
# 4 Load Bumpus data
# ------------------------------------------------------

cat("\nLoading Bumpus dataset...\n")

data_path <- file.path(proj_root, "temp", "data", "Bumpus_data.csv")

if (!file.exists(data_path)) {
    stop(paste("Cannot find data file at:", data_path))
}

bumpus <- read.csv(data_path)

bumpus$Survived <- as.numeric(bumpus$surv == "alive")
bumpus$TL <- bumpus$totlen
bumpus$HL <- bumpus$humer
bumpus$WT <- bumpus$wgt
bumpus$KL <- bumpus$stern

# Filter for adults to match original study (if AG column exists)
if ("AG" %in% names(bumpus)) {
    cat("Filtering for adults only (AG == 'adult')...\n")
    bumpus <- bumpus[bumpus$AG == "adult", ]
}

cat("Rows:", nrow(bumpus), "\n")
cat("Survival rate:", mean(bumpus$Survived), "\n")

# ------------------------------------------------------
# 5 Define traits
# ------------------------------------------------------

TRAITS <- c("TL", "HL", "WT", "KL")
FITNESS <- "Survived"


# ------------------------------------------------------
# 6 selection_coefficients
# ------------------------------------------------------

cat("\nTesting selection_coefficients...\n")

selection_results <- list()

for (k in seq_along(TRAITS)) {
    traits <- TRAITS[1:k]
    name <- paste(traits, collapse = "_")

    tryCatch(
        {
            res <- selection_coefficients(
                data = bumpus,
                fitness_col = FITNESS,
                trait_cols = traits,
                fitness_type = "binary",
                standardize = TRUE
            )

            selection_results[[name]] <- res

            write.csv(
                res,
                file.path(table_dir, paste0("selection_", name, ".csv")),
                row.names = FALSE
            )
        },
        error = function(e) {
            cat("FAILED:", name, "-", e$message, "\n")
        }
    )
}

# ------------------------------------------------------
# 7 selection_differential
# ------------------------------------------------------

cat("\nTesting selection_differential...\n")

sel_diffs <- list()

for (trait in TRAITS) {
    tryCatch(
        {
            d <- selection_differential(
                data = bumpus,
                fitness_col = FITNESS,
                trait_col = trait,
                standardized = FALSE
            )

            sel_diffs[[trait]] <- d
            cat(trait, "=", round(d, 4), "\n")
        },
        error = function(e) {
            cat("FAILED:", trait, "-", e$message, "\n")
        }
    )
}

sel_df <- data.frame(
    trait = names(sel_diffs),
    differential = unlist(sel_diffs)
)

write.csv(
    sel_df,
    file.path(table_dir, "selection_differentials.csv"),
    row.names = FALSE
)

# ------------------------------------------------------
# 8 univariate_spline + plotting
# ------------------------------------------------------
cat("\nTesting univariate spline + plotting...\n")

for (trait in TRAITS) {
    tryCatch(
        {
            spline <- univariate_spline(
                data = bumpus,
                fitness_col = FITNESS,
                trait_col = trait,
                fitness_type = "binary"
            )

            saveRDS(
                spline,
                file.path(model_dir, paste0("spline_", trait, ".rds"))
            )

            if (exists("plot_univariate_fitness")) {
                p <- plot_univariate_fitness(
                    uni = spline,
                    trait_col = trait,
                    title = paste("Survival vs", trait)
                )

                print(p)

                ggplot2::ggsave(
                    file.path(figure_dir, paste0("univariate_", trait, ".png")),
                    p,
                    width = 7,
                    height = 5,
                    dpi = 300
                )

                cat("Plot saved for:", trait, "\n")
            } else {
                cat("plot_univariate_fitness function not found\n")
            }

            cat("Spline OK:", trait, "\n")
        },
        error = function(e) {
            cat("FAILED:", trait, "-", e$message, "\n")
        }
    )
}


# ------------------------------------------------------
# 9 correlated_fitness_surface + plots
# ------------------------------------------------------

cat("\nTesting correlated fitness surfaces...\n")

pairs <- combn(TRAITS, 2, simplify = FALSE)

cfs_results <- list()

for (pair in pairs) {
    name <- paste(pair, collapse = "_")

    complete_cases <- sum(complete.cases(bumpus[, c(FITNESS, pair)]))
    cat("\n---", name, "---\n")
    cat("  Complete cases:", complete_cases, "\n")

    if (complete_cases < 10) {
        cat("  SKIP: Insufficient complete cases (<10)\n")
        next
    }

    tryCatch(
        {
            res <- correlated_fitness_surface(
                data = bumpus,
                fitness_col = FITNESS,
                trait_cols = pair,
                grid_n = 30,
                method = "gam"
            )

            cfs_results[[name]] <- res

            saveRDS(
                res,
                file.path(model_dir, paste0("cfs_", name, ".rds"))
            )

            if (exists("plot_correlated_fitness")) {
                p <- plot_correlated_fitness(res, pair)
                print(p)

                ggsave(
                    file.path(figure_dir, paste0("cfs_", name, ".png")),
                    p,
                    width = 8,
                    height = 6,
                    dpi = 300
                )

                if (exists("plot_correlated_fitness_enhanced")) {
                    p_enhanced <- plot_correlated_fitness_enhanced(
                        res,
                        pair,
                        original_data = bumpus,
                        fitness_col = FITNESS
                    )

                    ggsave(
                        file.path(figure_dir, paste0("cfs_", name, "_enhanced.png")),
                        p_enhanced,
                        width = 8,
                        height = 6,
                        dpi = 300
                    )
                }
            }
        },
        error = function(e) {
            cat("  FAILED:", e$message, "\n")
        }
    )
}

# ------------------------------------------------------
# 10 analyze_disruptive_selection
# ------------------------------------------------------

cat("\nTesting disruptive selection...\n")

disruptive_results <- list()

for (trait in TRAITS) {
    tryCatch(
        {
            res <- analyze_disruptive_selection(
                data = bumpus,
                fitness_col = FITNESS,
                trait_col = trait,
                fitness_type = "binary",
                standardize = TRUE
            )

            disruptive_results[[trait]] <- res

            write.csv(
                res,
                file.path(table_dir, paste0("disruptive_", trait, ".csv")),
                row.names = FALSE
            )

            cat("\n---", trait, "---\n")
            print(res)

            gamma <- res$Beta_Coefficient[2]
            p_quad <- res$P_Value[2]

            if (gamma < 0 && p_quad < 0.05) {
                cat("Stabilizing selection (γ =", round(gamma, 4), ", P =", round(p_quad, 4), ")\n")
            } else if (gamma > 0 && p_quad < 0.05) {
                cat("Disruptive selection (γ =", round(gamma, 4), ", P =", round(p_quad, 4), ")\n")
            } else {
                cat("No significant nonlinear selection (γ =", round(gamma, 4), ", P =", round(p_quad, 4), ")\n")
            }
        },
        error = function(e) {
            cat("FAILED:", trait, "-", e$message, "\n")
        }
    )
}


if (length(disruptive_results) > 0) {
    summary_df <- data.frame(
        trait = names(disruptive_results),
        linear = sapply(disruptive_results, function(x) x$Beta_Coefficient[1]),
        quadratic = sapply(disruptive_results, function(x) x$Beta_Coefficient[2]),
        p_linear = sapply(disruptive_results, function(x) x$P_Value[1]),
        p_quadratic = sapply(disruptive_results, function(x) x$P_Value[2])
    )

    write.csv(
        summary_df,
        file.path(table_dir, "disruptive_selection_summary.csv"),
        row.names = FALSE
    )

    cat("\n=== Disruptive Selection Summary ===\n")
    print(summary_df)
}

# ------------------------------------------------------
# 11 adaptive landscape
# ------------------------------------------------------
if (exists("adaptive_landscape")) {
    cat("\n=== Testing Adaptive Landscape ===\n")

    bumpus_std <- prepare_selection_data(
        data = bumpus,
        fitness_col = "Survived",
        trait_cols = c("TL", "HL", "WT", "KL"),
        standardize = TRUE,
        add_relative = FALSE
    )

    bumpus_std_points <- bumpus
    bumpus_std_points$TL <- scale(bumpus_std_points$TL)
    bumpus_std_points$KL <- scale(bumpus_std_points$KL)

    fitness_model <- mgcv::gam(
        Survived ~ s(TL, KL),
        family = binomial,
        data = bumpus_std
    )

    landscape <- adaptive_landscape(
        data = bumpus_std,
        fitness_model = fitness_model,
        trait_cols = c("TL", "KL"),
        simulation_n = 500,
        grid_n = 50
    )

    saveRDS(
        landscape,
        file.path(model_dir, "adaptive_landscape.rds")
    )

    # 2D adaptive landscape plot
    if (exists("plot_adaptive_landscape")) {
        cat("\nGenerating 2D adaptive landscape...\n")

        p2d <- plot_adaptive_landscape(
            landscape = landscape,
            trait_cols = c("TL", "KL"),
            original_data = bumpus_std_points,
            bins = 12
        )

        ggsave(
            file.path(figure_dir, "adaptive_landscape_2d.png"),
            p2d,
            width = 8,
            height = 6,
            dpi = 300
        )
    }

    # 3D adaptive landscape plot
    if (exists("plot_adaptive_landscape_3d")) {
        png(
            file.path(figure_dir, "adaptive_landscape_3d.png"),
            width = 8,
            height = 6,
            units = "in",
            res = 300
        )

        plot_adaptive_landscape_3d(
            landscape = landscape,
            trait_cols = c("TL", "KL"),
            theta = -30,
            phi = 30,
            grid_n = 200
        )

        dev.off()

        angles <- list(
            c(-30, 30), # default
            c(0, 30), # front view
            c(-60, 20), # side view
            c(30, 40), # back-right view
            c(-90, 30) # left side view
        )

        for (i in seq_along(angles)) {
            png(
                file.path(figure_dir, paste0("adaptive_landscape_3d_angle", i, ".png")),
                width = 8,
                height = 6,
                units = "in",
                res = 300
            )

            plot_adaptive_landscape_3d(
                landscape = landscape,
                trait_cols = c("TL", "KL"),
                theta = angles[[i]][1],
                phi = angles[[i]][2],
                grid_n = 200
            )

            dev.off()
        }
    }

    cat("\n=== Adaptive Landscape Summary ===\n")
    cat("Optimal population mean:\n")
    cat("  TL (Beak Length):", round(landscape$optimum$TL, 3), "\n")
    cat("  KL (Beak Depth):", round(landscape$optimum$KL, 3), "\n")
    cat("  Mean fitness at optimum:", round(landscape$optimum$.mean_fit, 3), "\n")

    summary_df <- data.frame(
        Metric = c("Optimal TL", "Optimal KL", "Optimal Fitness"),
        Value = c(
            round(landscape$optimum$TL, 3),
            round(landscape$optimum$KL, 3),
            round(landscape$optimum$.mean_fit, 3)
        )
    )
    write.csv(summary_df, file.path(table_dir, "adaptive_landscape_summary.csv"), row.names = FALSE)
    cat("\n  Summary saved to:", file.path(table_dir, "adaptive_landscape_summary.csv"), "\n")
} else {
    cat("\n Skipping adaptive_landscape (function not found)\n")
}

# ------------------------------------------------------
# 12 compare_fitness_surfaces
# ------------------------------------------------------
if (exists("compare_fitness_surfaces_data") &&
    exists("plot_fitness_surfaces_comparison") &&
    exists("cfs_results") &&
    length(cfs_results) > 0 &&
    exists("landscape")) {
    cat("\n=== Testing Surface Comparison ===\n")

    tryCatch(
        {
            all_pairs <- names(cfs_results)
            comparison_results <- list()

            for (pair_name in all_pairs) {
                cat("\n--- Comparing", pair_name, "---\n")

                trait_cols <- cfs_results[[pair_name]]$trait_cols

                comparison_data <- compare_fitness_surfaces_data(
                    correlated_surface = cfs_results[[pair_name]],
                    adaptive_landscape = landscape,
                    trait_cols = trait_cols
                )

                plots <- plot_fitness_surfaces_comparison(
                    comparison_data = comparison_data,
                    bins = 10,
                    title = paste("Comparison:", paste(trait_cols, collapse = " vs "))
                )

                # side-by-side plot
                if (!is.null(plots$side_by_side)) {
                    ggsave(
                        file.path(figure_dir, paste0("comparison_", pair_name, "_side_by_side.png")),
                        plots$side_by_side,
                        width = 12,
                        height = 5,
                        dpi = 300
                    )
                }

                # overlay plot
                if (!is.null(plots$overlay)) {
                    ggsave(
                        file.path(figure_dir, paste0("comparison_", pair_name, "_overlay.png")),
                        plots$overlay,
                        width = 8,
                        height = 6,
                        dpi = 300
                    )
                }

                # Calculate correlation between surfaces
                cor_df <- comparison_data$combined_data
                individual_fit <- cor_df$fitness[cor_df$type == "Correlated Fitness (Individual)"]
                population_fit <- cor_df$fitness[cor_df$type == "Adaptive Landscape (Population)"]

                cor_val <- cor(individual_fit, population_fit, use = "complete.obs")
                cat("  Correlation:", round(cor_val, 4), "\n")

                comparison_results[[pair_name]] <- list(
                    correlation = cor_val,
                    comparison_data = comparison_data,
                    plots = plots
                )

                write.csv(
                    data.frame(
                        trait1 = trait_cols[1],
                        trait2 = trait_cols[2],
                        correlation = cor_val,
                        n_points = length(individual_fit)
                    ),
                    file.path(table_dir, paste0("comparison_correlation_", pair_name, ".csv")),
                    row.names = FALSE
                )
            }

            cat("\n=== Comparison Summary ===\n")
            summary_df <- data.frame()
            for (name in names(comparison_results)) {
                cor_val <- comparison_results[[name]]$correlation
                cat(name, ": correlation =", round(cor_val, 4), "\n")
                summary_df <- rbind(summary_df, data.frame(
                    Trait_Pair = name,
                    Correlation = round(cor_val, 4)
                ))
            }

            write.csv(summary_df, file.path(table_dir, "comparison_summary.csv"), row.names = FALSE)
        },
        error = function(e) {
            cat("Comparison FAILED:", e$message, "\n")
        }
    )
} else {
    cat("\nSkipping surface comparison (required objects not found)\n")
}

# ------------------------------------------------------
# 13 Final summary
# ------------------------------------------------------

cat("\n========================================\n")
cat("TESTING COMPLETE\n")
cat("========================================\n")

cat("\nResults saved to:\n")
cat("  Tables:", table_dir, "\n")
cat("  Figures:", figure_dir, "\n")
cat("  Models:", model_dir, "\n")

# List generated files
cat("\nGenerated files:\n")
cat("\nTables:\n")
print(list.files(table_dir))

cat("\nFigures:\n")
print(list.files(figure_dir))

cat("\nModels:\n")
print(list.files(model_dir))

cat("\n========================================\n")
