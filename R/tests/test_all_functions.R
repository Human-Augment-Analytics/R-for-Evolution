# ======================================================
# test_all_functions.R
# Full integration test for evolutionary selection package
# ======================================================

# ------------------------------------------------------
#Load here package for file path 
# ------------------------------------------------------
required_packages <- c(
  "here"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
  cat("Loaded package:", pkg, "\n")
}

cat("Project Root:", here(), "\n")

cat("\n========================================\n")
cat("RUNNING FULL FUNCTION TEST SUITE\n")
cat("========================================\n")

# ======================================================
# Load scripts (pipeline helpers)
# ======================================================

cat("\nLoading script files...\n")

script_files <- list.files(
    here("R","scripts"),
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in script_files) {
    source(f)

    cat("Loaded script:", basename(f), "\n")
}

# ======================================================
# 2 Output directories
# ======================================================

output_dir <- here("R", "results", "test_all_functions_results")

figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
model_dir <- file.path(output_dir, "models")

dirs <- c(output_dir, figure_dir, table_dir, model_dir)

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("Results saved to:", normalizePath(output_dir), "\n")

# ======================================================
# 3 Load functions
# ======================================================

cat("\nLoading function files...\n")

fn_files <- list.files(
    here("R","functions"),
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
    here("R","plotting"),
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in plot_files) {
    source(f)
    cat("Loaded plot:", basename(f), "\n")
}

# ======================================================
# 5 Create test data
# ======================================================

cat("\nCreating synthetic test data...\n")

set.seed(42)

df_binary <- data.frame(
    fitness = rbinom(80, 1, 0.6),
    size = rnorm(80, 5, 1),
    speed = rnorm(80, 10, 2),
    color = rnorm(80, 3, 0.6)
)

df_continuous <- data.frame(
    fitness = rpois(80, 3) + 1,
    size = rnorm(80, 5, 1),
    speed = rnorm(80, 10, 2),
    color = rnorm(80, 3, 0.6)
)

cat("Binary data:", dim(df_binary), "\n")
cat("Continuous data:", dim(df_continuous), "\n")

TRAITS <- c("size", "speed", "color")
FITNESS <- "fitness"

# ======================================================
# 6 selection_coefficients
# ======================================================

cat("\nTesting selection_coefficients...\n")

test_binary <- selection_coefficients(
    data = df_binary,
    fitness_col = FITNESS,
    trait_cols = c("size", "speed"),
    fitness_type = "binary",
    standardize = TRUE
)

test_continuous <- selection_coefficients(
    data = df_continuous,
    fitness_col = FITNESS,
    trait_cols = c("size", "speed"),
    fitness_type = "continuous",
    standardize = TRUE
)

write.csv(test_binary, file.path(table_dir, "selection_binary.csv"), row.names = FALSE)
write.csv(test_continuous, file.path(table_dir, "selection_continuous.csv"), row.names = FALSE)

cat("Selection coefficients Done\n")

# ======================================================
# 7 selection_differential
# ======================================================

cat("\nTesting selection_differential...\n")

sel_diff <- selection_differential(
    data = df_continuous,
    fitness_col = FITNESS,
    trait_col = "size"
)

cat("Selection differential:", round(sel_diff, 4), "\n")

# ======================================================
# 8 univariate spline + plot
# ======================================================

cat("\nTesting univariate surfaces...\n")

univariate_plots <- list()

for (trait in TRAITS) {
    spline <- univariate_spline(
        data = df_continuous,
        fitness_col = FITNESS,
        trait_col = trait,
        fitness_type = "continuous",
        k = 8
    )

    if (exists("plot_univariate_fitness")) {
        plot <- plot_univariate_fitness(
            uni = spline,
            trait_col = trait,
            title = paste("Univariate Correlated Fitness:", trait)
        )

        univariate_plots[[trait]] <- plot

        ggsave(
            file.path(figure_dir, paste0("univariate_", trait, ".png")),
            plot,
            width = 8,
            height = 6,
            dpi = 300
        )
    } else {
        cat("plot_univariate_fitness not found, using univariate_surface\n")
        # Fallback to old name if exists
        if (exists("univariate_surface")) {
            plot <- univariate_surface(
                uni = spline,
                trait_col = trait
            )
            univariate_plots[[trait]] <- plot
            ggsave(
                file.path(figure_dir, paste0("univariate_", trait, ".png")),
                plot,
                width = 8,
                height = 6,
                dpi = 300
            )
        }
    }
}

cat("Univariate surfaces Done\n")

# ======================================================
# 9 correlated fitness surfaces
# ======================================================

cat("\nTesting correlated fitness surfaces...\n")

if (!exists("trait_pairs")) {
    trait_pairs <- list(c("size", "speed"), c("size", "color"), c("speed", "color"))
}

cfs_plots <- list()

for (pair in trait_pairs) {
    cfs <- correlated_fitness_surface(
        data = df_continuous,
        fitness_col = FITNESS,
        trait_cols = pair,
        grid_n = 30
    )

    plot <- plot_correlated_fitness(
        tps = cfs,
        trait_cols = pair
    )

    name <- paste(pair, collapse = "_")
    cfs_plots[[name]] <- plot

    ggsave(
        file.path(figure_dir, paste0("cfs_", name, ".png")),
        plot,
        width = 8,
        height = 6,
        dpi = 300
    )

    if (exists("plot_correlated_fitness_enhanced")) {
        plot_enhanced <- plot_correlated_fitness_enhanced(
            tps = cfs,
            original_data = df_continuous,
            fitness_col = FITNESS
        )

        ggsave(
            file.path(figure_dir, paste0("cfs_", name, "_enhanced.png")),
            plot_enhanced,
            width = 8,
            height = 6,
            dpi = 300
        )
    }
}

cat("Correlated fitness surfaces Done\n")

# ======================================================
# 10 bootstrap
# ======================================================

if (exists("bootstrap_selection")) {
    cat("\nTesting bootstrap_selection...\n")

    boot <- bootstrap_selection(
        data = df_continuous,
        fitness_col = FITNESS,
        trait_cols = c("size", "speed"),
        fitness_type = "continuous",
        B = 50,
        seed = 42
    )

    write.csv(boot$ci, file.path(table_dir, "bootstrap_ci.csv"), row.names = FALSE)

    cat("Bootstrap Done\n")
} else {
    cat("\nSkipping bootstrap_selection (function not found)\n")
}

# ======================================================
# 11 adaptive landscape
# ======================================================

if (exists("adaptive_landscape")) {
    cat("\nTesting adaptive landscape...\n")

    fitness_model <- mgcv::gam(
        fitness ~ s(size, speed),
        data = df_continuous
    )

    landscape <- adaptive_landscape(
        data = df_continuous,
        fitness_model = fitness_model,
        trait_cols = c("size", "speed"),
        simulation_n = 400,
        grid_n = 30
    )

    saveRDS(
        landscape,
        file.path(model_dir, "adaptive_landscape.rds")
    )

    if (exists("plot_adaptive_landscape")) {
        p <- plot_adaptive_landscape(
            landscape = landscape,
            trait_cols = c("size", "speed"),
            original_data = df_continuous
        )

        ggsave(
            file.path(figure_dir, "adaptive_landscape.png"),
            p,
            width = 8,
            height = 6,
            dpi = 300
        )
    }

    cat("Adaptive landscape Done\n")
} else {
    cat("\nSkipping adaptive_landscape (function not found)\n")
}

# ======================================================
# 12 compare_fitness_surfaces (NEW)
# ======================================================

if (exists("compare_fitness_surfaces") &&
    exists("cfs_plots") &&
    length(cfs_plots) > 0 &&
    exists("landscape")) {
    cat("\nTesting surface comparison...\n")

    tryCatch(
        {
            # Use first pair for comparison
            first_pair <- names(cfs_plots)[1]
            first_cfs <- switch(first_pair,
                "size_speed" = correlated_fitness_surface(df_continuous, FITNESS, c("size", "speed")),
                "size_color" = correlated_fitness_surface(df_continuous, FITNESS, c("size", "color")),
                "speed_color" = correlated_fitness_surface(df_continuous, FITNESS, c("speed", "color"))
            )

            comparison <- compare_fitness_surfaces(
                correlated_surface = first_cfs,
                adaptive_landscape = landscape,
                trait_cols = strsplit(first_pair, "_")[[1]]
            )

            # Save comparison plots
            if (!is.null(comparison$side_by_side)) {
                ggsave(
                    file.path(figure_dir, "comparison_side_by_side.png"),
                    comparison$side_by_side,
                    width = 12,
                    height = 5,
                    dpi = 300
                )
            }

            if (!is.null(comparison$overlay)) {
                ggsave(
                    file.path(figure_dir, "comparison_overlay.png"),
                    comparison$overlay,
                    width = 8,
                    height = 6,
                    dpi = 300
                )
            }

            cat("Surface comparison Done\n")
        },
        error = function(e) {
            cat("Surface comparison failed:", e$message, "\n")
        }
    )
}

# ======================================================
# 13 Validation
# ======================================================

validate <- function(obj, type) {
    if (is.null(obj)) {
        return(FALSE)
    }

    if (type == "data.frame") {
        return(is.data.frame(obj))
    }
    if (type == "list") {
        return(is.list(obj))
    }
    if (type == "plot") {
        return(inherits(obj, "gg") || inherits(obj, "ggplot"))
    }
    return(TRUE)
}

tests <- c(
    validate(test_binary, "data.frame"),
    validate(test_continuous, "data.frame"),
    validate(univariate_plots[[1]], "plot"),
    validate(cfs_plots[[1]], "plot"),
    if (exists("boot")) validate(boot, "list") else FALSE,
    if (exists("landscape")) validate(landscape, "list") else FALSE
)

cat("\nValidation results:", sum(tests), "/", length(tests), "passed\n")

# ======================================================
# 14 Final summary
# ======================================================

cat("\n========================================\n")
cat("FUNCTION TESTING COMPLETE\n")
cat("========================================\n")

cat("\nFunctions loaded:", length(fn_files), "\n")
cat("Plots created:", length(univariate_plots) + length(cfs_plots), "\n")

cat("\nFiles generated:\n")
cat("  Figures:", length(list.files(figure_dir)), "\n")
cat("  Tables:", length(list.files(table_dir)), "\n")
cat("  Models:", length(list.files(model_dir)), "\n")

cat("\nResults saved to:\n")
cat("  Figures:", figure_dir, "\n")
cat("  Tables:", table_dir, "\n")
cat("  Models:", model_dir, "\n")

cat("\n========================================\n")
