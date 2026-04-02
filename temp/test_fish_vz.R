# ======================================================
# test_fish.R
# Integration test using pupfish dataset (Crescent Pond)
# ======================================================

cat("\n========================================\n")
cat("PUPFISH DATASET FUNCTION TEST\n")
cat("========================================\n")

cat("Working directory:", getwd(), "\n")

# ------------------------------------------------------
# 1 Initialize environment
# ------------------------------------------------------

if (file.exists("R/scripts/0.0_initialize.R")) {
    source("R/scripts/0.0_initialize.R")
}

# ======================================================
# 2 Load scripts
# ======================================================

cat("\nLoading script files...\n")

script_files <- list.files(
    "R/scripts",
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
    "R/functions",
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
    "R/plotting",
    pattern = "\\.R$",
    full.names = TRUE
)

for (f in plot_files) {
    source(f)
    cat("Loaded plot:", basename(f), "\n")
}

# ======================================================
# 5 Output directories
# ======================================================

library(here)

output_dir <- here("R", "results", "pupfish_results")
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
# 6 Load pupfish data
# ------------------------------------------------------

cat("\nLoading pupfish dataset...\n")

# Find Crescent Pond data
data_dirs <- c(here("R", "data"), here("R", "test_data"))
crescent_files <- list.files(data_dirs,
    pattern = "Crescent.*Pond.*\\.csv$",
    full.names = TRUE, recursive = TRUE
)

if (length(crescent_files) == 0) {
    stop("Crescent Pond data not found")
}

crescent_data <- read.csv(crescent_files[1])
cat("Loaded:", basename(crescent_files[1]), "\n")
cat("Rows:", nrow(crescent_data), "\n")
cat("Columns:", ncol(crescent_data), "\n")

# ------------------------------------------------------
# 7 Define traits and fitness
# ------------------------------------------------------

FITNESS_BINARY <- "survival"
FITNESS_CONTINUOUS <- "ln.growth"
TRAITS <- c("jaw", "eye", "body", "nasal", "mouth", "SL")

# Filter to complete cases
pupfish_data <- crescent_data %>%
    dplyr::select(dplyr::all_of(c(FITNESS_BINARY, FITNESS_CONTINUOUS, TRAITS))) %>%
    dplyr::filter(complete.cases(.))

cat("\nComplete observations:", nrow(pupfish_data), "\n")
cat("Traits:", paste(TRAITS, collapse = ", "), "\n")

# ------------------------------------------------------
# 8 Data preparation
# ------------------------------------------------------

cat("\nPreparing data...\n")

# Binary fitness
prepared_binary <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_BINARY,
    trait_cols = TRAITS,
    standardize = TRUE,
    add_relative = FALSE,
    na_action = "drop"
)

# Continuous fitness
prepared_continuous <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_CONTINUOUS,
    trait_cols = TRAITS,
    standardize = TRUE,
    add_relative = TRUE,
    na_action = "drop"
)

cat("Binary data: n =", nrow(prepared_binary), "\n")
cat("Continuous data: n =", nrow(prepared_continuous), "\n")

# ------------------------------------------------------
# 9 Selection coefficients (binary)
# ------------------------------------------------------

cat("\n=== Selection Coefficients (Binary Fitness) ===\n")

selection_binary <- selection_coefficients(
    data = prepared_binary,
    fitness_col = FITNESS_BINARY,
    trait_cols = TRAITS,
    fitness_type = "binary",
    standardize = FALSE
)

# Save
write.csv(selection_binary, file.path(table_dir, "selection_binary.csv"), row.names = FALSE)

# Display significant results
sig_binary <- selection_binary[selection_binary$P_Value < 0.05, ]
if (nrow(sig_binary) > 0) {
    cat("\nSignificant gradients (binary):\n")
    print(sig_binary)
} else {
    cat("\nNo significant gradients detected\n")
}

# ------------------------------------------------------
# 10 Selection coefficients (continuous)
# ------------------------------------------------------

cat("\n=== Selection Coefficients (Continuous Fitness) ===\n")

selection_continuous <- selection_coefficients(
    data = prepared_continuous,
    fitness_col = FITNESS_CONTINUOUS,
    trait_cols = TRAITS,
    fitness_type = "continuous",
    standardize = FALSE
)

# Save
write.csv(selection_continuous, file.path(table_dir, "selection_continuous.csv"), row.names = FALSE)

# Display significant results
sig_continuous <- selection_continuous[selection_continuous$P_Value < 0.05, ]
if (nrow(sig_continuous) > 0) {
    cat("\nSignificant gradients (continuous):\n")
    print(sig_continuous)
} else {
    cat("\nNo significant gradients detected\n")
}



# ------------------------------------------------------
# 10 Selection Differential
# ------------------------------------------------------

cat("\n=== Selection Differential ===\n")

# Binary fitness
sel_diff_binary <- list()
for (trait in TRAITS) {
    d <- tryCatch(
        {
            selection_differential(
                data = prepared_binary,
                fitness_col = FITNESS_BINARY,
                trait_col = trait,
                standardized = FALSE,
                use_relative = FALSE
            )
        },
        error = function(e) NA
    )

    sel_diff_binary[[trait]] <- d
    cat("  ", trait, ":", round(d, 4), "\n")
}

# Continuous fitness
sel_diff_continuous <- list()
for (trait in TRAITS) {
    d <- tryCatch(
        {
            selection_differential(
                data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS,
                trait_col = trait,
                standardized = FALSE,
                use_relative = TRUE
            )
        },
        error = function(e) NA
    )

    sel_diff_continuous[[trait]] <- d
    cat("  ", trait, ":", round(d, 4), "\n")
}

sel_diff_df <- data.frame(
    trait = TRAITS,
    binary = unlist(sel_diff_binary),
    continuous = unlist(sel_diff_continuous)
)
write.csv(sel_diff_df, file.path(table_dir, "selection_differentials.csv"), row.names = FALSE)


# ------------------------------------------------------
# 11 Univariate spline (Binary Fitness)
# ------------------------------------------------------

cat("\n=== Univariate Fitness Functions (Binary) ===\n")

for (trait in TRAITS) {
    cat("  ", trait, "...")

    spline <- tryCatch(
        {
            univariate_spline(
                data = prepared_binary,
                fitness_col = FITNESS_BINARY,
                trait_col = trait,
                fitness_type = "binary",
                k = 6
            )
        },
        error = function(e) NULL
    )

    if (!is.null(spline)) {
        if (exists("plot_univariate_fitness")) {
            p <- plot_univariate_fitness(
                uni = spline,
                trait_col = trait,
                title = paste("Fitness Function (Binary):", trait)
            )

            ggsave(
                file.path(figure_dir, paste0("univariate_binary_", trait, ".png")),
                p,
                width = 7,
                height = 5,
                dpi = 300
            )
        }
    } else {
        cat("\n")
    }
}

# ------------------------------------------------------
# 12 Univariate spline (Continuous Fitness)
# ------------------------------------------------------

cat("\n=== Univariate Fitness Functions (Continuous) ===\n")

for (trait in TRAITS) {
    cat("  ", trait, "...")

    spline <- tryCatch(
        {
            univariate_spline(
                data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS,
                trait_col = trait,
                fitness_type = "continuous",
                k = 6
            )
        },
        error = function(e) NULL
    )

    if (!is.null(spline)) {
        if (exists("plot_univariate_fitness")) {
            p <- plot_univariate_fitness(
                uni = spline,
                trait_col = trait,
                title = paste("Fitness Function (Continuous):", trait)
            )

            ggsave(
                file.path(figure_dir, paste0("univariate_continuous_", trait, ".png")),
                p,
                width = 7,
                height = 5,
                dpi = 300
            )
        }
    } else {
        cat("\n")
    }
}

# ------------------------------------------------------
# 12 Bivariate fitness surfaces (Binary) - All Pairs
# ------------------------------------------------------

cat("\n=== Bivariate Fitness Surfaces (Binary) ===\n")

trait_pairs <- combn(TRAITS, 2, simplify = FALSE)

for (pair in trait_pairs) {
    pair_name <- paste(pair, collapse = "_")
    cat("  ", pair_name, "...")

    cfs <- tryCatch(
        {
            correlated_fitness_surface(
                data = prepared_binary,
                fitness_col = FITNESS_BINARY,
                trait_cols = pair,
                grid_n = 30,
                method = "gam"
            )
        },
        error = function(e) NULL
    )

    if (!is.null(cfs) && exists("plot_correlated_fitness")) {
        p <- plot_correlated_fitness(cfs, pair) +
            labs(title = paste("Binary Fitness:", paste(pair, collapse = " × ")))

        ggsave(
            file.path(figure_dir, paste0("cfs_binary_", pair_name, ".png")),
            p,
            width = 8,
            height = 6,
            dpi = 300
        )

        # Enhanced version
        if (exists("plot_correlated_fitness_enhanced")) {
            p_enhanced <- plot_correlated_fitness_enhanced(
                cfs, pair,
                original_data = prepared_binary,
                fitness_col = FITNESS_BINARY
            )

            ggsave(
                file.path(figure_dir, paste0("cfs_binary_", pair_name, "_enhanced.png")),
                p_enhanced,
                width = 8,
                height = 6,
                dpi = 300
            )
        }
    } else {
        cat("\n")
    }
}

# ------------------------------------------------------
# 13 Bivariate fitness surfaces (Continuous) - All Pairs
# ------------------------------------------------------

cat("\n=== Bivariate Fitness Surfaces (Continuous) ===\n")

for (pair in trait_pairs) {
    pair_name <- paste(pair, collapse = "_")
    cat("  ", pair_name, "...")

    cfs <- tryCatch(
        {
            correlated_fitness_surface(
                data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS,
                trait_cols = pair,
                grid_n = 30,
                method = "tps"
            )
        },
        error = function(e) NULL
    )

    if (!is.null(cfs) && exists("plot_correlated_fitness")) {
        p <- plot_correlated_fitness(cfs, pair) +
            labs(title = paste("Continuous Fitness:", paste(pair, collapse = " × ")))

        ggsave(
            file.path(figure_dir, paste0("cfs_continuous_", pair_name, ".png")),
            p,
            width = 8,
            height = 6,
            dpi = 300
        )

        # Enhanced version
        if (exists("plot_correlated_fitness_enhanced")) {
            p_enhanced <- plot_correlated_fitness_enhanced(
                cfs, pair,
                original_data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS
            )

            ggsave(
                file.path(figure_dir, paste0("cfs_continuous_", pair_name, "_enhanced.png")),
                p_enhanced,
                width = 8,
                height = 6,
                dpi = 300
            )
        }
    } else {
        cat(" \n")
    }
}

# ------------------------------------------------------
# 14 Disruptive Selection (Univariate)
# ------------------------------------------------------

cat("\n=== Disruptive Selection ===\n")

disruptive_binary <- list()
disruptive_continuous <- list()

for (trait in TRAITS) {
    cat("\n---", trait, "---\n")

    # Binary fitness (survival)
    cat("  Binary...")
    disrupt_bin <- tryCatch(
        {
            analyze_disruptive_selection(
                data = prepared_binary,
                fitness_col = FITNESS_BINARY,
                trait_col = trait,
                fitness_type = "binary",
                standardize = TRUE
            )
        },
        error = function(e) NULL
    )

    if (!is.null(disrupt_bin) && nrow(disrupt_bin) >= 2) {
        disruptive_binary[[trait]] <- disrupt_bin
        gamma <- disrupt_bin$Beta_Coefficient[2]
        p_val <- disrupt_bin$P_Value[2]
        cat(" γ =", round(gamma, 4), "(p =", round(p_val, 4), ")\n")
    } else {
        cat("\n")
    }

    # Continuous fitness (growth)
    cat("  Continuous...")
    disrupt_cont <- tryCatch(
        {
            analyze_disruptive_selection(
                data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS,
                trait_col = trait,
                fitness_type = "continuous",
                standardize = TRUE
            )
        },
        error = function(e) NULL
    )

    if (!is.null(disrupt_cont) && nrow(disrupt_cont) >= 2) {
        disruptive_continuous[[trait]] <- disrupt_cont
        gamma <- disrupt_cont$Beta_Coefficient[2]
        p_val <- disrupt_cont$P_Value[2]
        cat(" γ =", round(gamma, 4), "(p =", round(p_val, 4), ")\n")
    } else {
        cat("\n")
    }
}

# ------------------------------------------------------
# 15 Disruptive Selection Summary
# ------------------------------------------------------
# ======================================================
# 14 Disruptive Selection (Univariate)
# ======================================================

cat("\n=== Disruptive Selection ===\n")

disruptive_binary <- list()
disruptive_continuous <- list()

for (trait in TRAITS) {
    cat("\n---", trait, "---\n")

    # Binary
    cat("  Binary...")
    disrupt_bin <- tryCatch(
        {
            analyze_disruptive_selection(
                data = prepared_binary,
                fitness_col = FITNESS_BINARY,
                trait_col = trait,
                fitness_type = "binary",
                standardize = TRUE
            )
        },
        error = function(e) NULL
    )

    if (!is.null(disrupt_bin) && nrow(disrupt_bin) >= 2) {
        disruptive_binary[[trait]] <- disrupt_bin
        cat(
            " γ =", round(disrupt_bin$Beta_Coefficient[2], 4),
            "(p =", round(disrupt_bin$P_Value[2], 4), ")\n"
        )
    } else {
        cat(" ✗\n")
    }

    # Continuous
    cat("  Continuous...")
    disrupt_cont <- tryCatch(
        {
            analyze_disruptive_selection(
                data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS,
                trait_col = trait,
                fitness_type = "continuous",
                standardize = TRUE
            )
        },
        error = function(e) NULL
    )

    if (!is.null(disrupt_cont) && nrow(disrupt_cont) >= 2) {
        disruptive_continuous[[trait]] <- disrupt_cont
        cat(
            " γ =", round(disrupt_cont$Beta_Coefficient[2], 4),
            "(p =", round(disrupt_cont$P_Value[2], 4), ")\n"
        )
    } else {
        cat(" ✗\n")
    }
}

# ======================================================
# 15 Disruptive Selection Summary
# ======================================================

cat("\n=== Disruptive Selection Summary ===\n")

disruptive_summary <- data.frame()

for (trait in TRAITS) {
    # Binary
    if (!is.null(disruptive_binary[[trait]])) {
        df <- disruptive_binary[[trait]]
        disruptive_summary <- rbind(disruptive_summary, data.frame(
            Trait = trait,
            Fitness = "Binary",
            Beta = df$Beta_Coefficient[1],
            Gamma = df$Beta_Coefficient[2],
            P_Linear = df$P_Value[1],
            P_Quadratic = df$P_Value[2],
            stringsAsFactors = FALSE
        ))
    }

    # Continuous
    if (!is.null(disruptive_continuous[[trait]])) {
        df <- disruptive_continuous[[trait]]
        disruptive_summary <- rbind(disruptive_summary, data.frame(
            Trait = trait,
            Fitness = "Continuous",
            Beta = df$Beta_Coefficient[1],
            Gamma = df$Beta_Coefficient[2],
            P_Linear = df$P_Value[1],
            P_Quadratic = df$P_Value[2],
            stringsAsFactors = FALSE
        ))
    }
}


write.csv(disruptive_summary, file.path(table_dir, "disruptive_selection_summary.csv"), row.names = FALSE)

cat("\n=== Disruptive Summary Table ===\n")
print(disruptive_summary)

cat("\nSummary saved to:", file.path(table_dir, "disruptive_selection_summary.csv"), "\n")


# ======================================================
# 16 Adaptive Landscape (with 3D)
# ======================================================

if (exists("adaptive_landscape")) {
    cat("\n=== Adaptive Landscape ===\n")

    # ======================================================
    # Binary Fitness (Survival) - 3 combinations
    # ======================================================

    # 1. nasal × SL
    cat("\n--- Binary: nasal × SL ---\n")
    trait_pair_binary1 <- c("nasal", "SL")

    gam_binary1 <- mgcv::gam(
        as.formula(paste(FITNESS_BINARY, "~ s(", trait_pair_binary1[1], ",", trait_pair_binary1[2], ")")),
        family = binomial,
        data = prepared_binary
    )

    landscape_binary1 <- adaptive_landscape(
        data = prepared_binary,
        fitness_model = gam_binary1,
        trait_cols = trait_pair_binary1,
        simulation_n = 500,
        grid_n = 50
    )

    saveRDS(landscape_binary1, file.path(model_dir, "adaptive_landscape_binary_nasal_SL.rds"))

    if (exists("plot_adaptive_landscape")) {
        p2d <- plot_adaptive_landscape(landscape_binary1, trait_pair_binary1, prepared_binary, bins = 12)
        ggsave(file.path(figure_dir, "adaptive_landscape_binary_nasal_SL_2d.png"), p2d, width = 8, height = 6, dpi = 300)
    }
    if (exists("plot_adaptive_landscape_3d")) {
        png(file.path(figure_dir, "adaptive_landscape_binary_nasal_SL_3d.png"), width = 8, height = 6, units = "in", res = 300)
        plot_adaptive_landscape_3d(landscape_binary1, trait_pair_binary1, theta = -30, phi = 30, grid_n = 200)
        dev.off()
    }

    # 2. jaw × SL
    cat("\n--- Binary: jaw × SL ---\n")
    trait_pair_binary2 <- c("jaw", "SL")

    gam_binary2 <- mgcv::gam(
        as.formula(paste(FITNESS_BINARY, "~ s(", trait_pair_binary2[1], ",", trait_pair_binary2[2], ")")),
        family = binomial,
        data = prepared_binary
    )

    landscape_binary2 <- adaptive_landscape(
        data = prepared_binary,
        fitness_model = gam_binary2,
        trait_cols = trait_pair_binary2,
        simulation_n = 500,
        grid_n = 50
    )

    saveRDS(landscape_binary2, file.path(model_dir, "adaptive_landscape_binary_jaw_SL.rds"))

    if (exists("plot_adaptive_landscape")) {
        p2d <- plot_adaptive_landscape(landscape_binary2, trait_pair_binary2, prepared_binary, bins = 12)
        ggsave(file.path(figure_dir, "adaptive_landscape_binary_jaw_SL_2d.png"), p2d, width = 8, height = 6, dpi = 300)
    }
    if (exists("plot_adaptive_landscape_3d")) {
        png(file.path(figure_dir, "adaptive_landscape_binary_jaw_SL_3d.png"), width = 8, height = 6, units = "in", res = 300)
        plot_adaptive_landscape_3d(landscape_binary2, trait_pair_binary2, theta = -30, phi = 30, grid_n = 200)
        dev.off()
    }

    # 3. body × mouth
    cat("\n--- Binary: body × mouth ---\n")
    trait_pair_binary3 <- c("body", "mouth")

    gam_binary3 <- mgcv::gam(
        as.formula(paste(FITNESS_BINARY, "~ s(", trait_pair_binary3[1], ",", trait_pair_binary3[2], ")")),
        family = binomial,
        data = prepared_binary
    )

    landscape_binary3 <- adaptive_landscape(
        data = prepared_binary,
        fitness_model = gam_binary3,
        trait_cols = trait_pair_binary3,
        simulation_n = 500,
        grid_n = 50
    )

    saveRDS(landscape_binary3, file.path(model_dir, "adaptive_landscape_binary_body_mouth.rds"))

    if (exists("plot_adaptive_landscape")) {
        p2d <- plot_adaptive_landscape(landscape_binary3, trait_pair_binary3, prepared_binary, bins = 12)
        ggsave(file.path(figure_dir, "adaptive_landscape_binary_body_mouth_2d.png"), p2d, width = 8, height = 6, dpi = 300)
    }
    if (exists("plot_adaptive_landscape_3d")) {
        png(file.path(figure_dir, "adaptive_landscape_binary_body_mouth_3d.png"), width = 8, height = 6, units = "in", res = 300)
        plot_adaptive_landscape_3d(landscape_binary3, trait_pair_binary3, theta = -30, phi = 30, grid_n = 200)
        dev.off()
    }

    # ======================================================
    # Continuous Fitness (Growth) - 3 combinations
    # ======================================================

    # 1. jaw × mouth
    cat("\n--- Continuous: jaw × mouth ---\n")
    trait_pair_cont1 <- c("jaw", "mouth")

    gam_cont1 <- mgcv::gam(
        as.formula(paste(FITNESS_CONTINUOUS, "~ s(", trait_pair_cont1[1], ",", trait_pair_cont1[2], ")")),
        family = gaussian,
        data = prepared_continuous
    )

    landscape_cont1 <- adaptive_landscape(
        data = prepared_continuous,
        fitness_model = gam_cont1,
        trait_cols = trait_pair_cont1,
        simulation_n = 500,
        grid_n = 50
    )

    saveRDS(landscape_cont1, file.path(model_dir, "adaptive_landscape_continuous_jaw_mouth.rds"))

    if (exists("plot_adaptive_landscape")) {
        p2d <- plot_adaptive_landscape(landscape_cont1, trait_pair_cont1, prepared_continuous, bins = 12)
        ggsave(file.path(figure_dir, "adaptive_landscape_continuous_jaw_mouth_2d.png"), p2d, width = 8, height = 6, dpi = 300)
    }
    if (exists("plot_adaptive_landscape_3d")) {
        png(file.path(figure_dir, "adaptive_landscape_continuous_jaw_mouth_3d.png"), width = 8, height = 6, units = "in", res = 300)
        plot_adaptive_landscape_3d(landscape_cont1, trait_pair_cont1, theta = -30, phi = 30, grid_n = 200)
        dev.off()
    }

    # 2. mouth × SL
    cat("\n--- Continuous: mouth × SL ---\n")
    trait_pair_cont2 <- c("mouth", "SL")

    gam_cont2 <- mgcv::gam(
        as.formula(paste(FITNESS_CONTINUOUS, "~ s(", trait_pair_cont2[1], ",", trait_pair_cont2[2], ")")),
        family = gaussian,
        data = prepared_continuous
    )

    landscape_cont2 <- adaptive_landscape(
        data = prepared_continuous,
        fitness_model = gam_cont2,
        trait_cols = trait_pair_cont2,
        simulation_n = 500,
        grid_n = 50
    )

    saveRDS(landscape_cont2, file.path(model_dir, "adaptive_landscape_continuous_mouth_SL.rds"))

    if (exists("plot_adaptive_landscape")) {
        p2d <- plot_adaptive_landscape(landscape_cont2, trait_pair_cont2, prepared_continuous, bins = 12)
        ggsave(file.path(figure_dir, "adaptive_landscape_continuous_mouth_SL_2d.png"), p2d, width = 8, height = 6, dpi = 300)
    }
    if (exists("plot_adaptive_landscape_3d")) {
        png(file.path(figure_dir, "adaptive_landscape_continuous_mouth_SL_3d.png"), width = 8, height = 6, units = "in", res = 300)
        plot_adaptive_landscape_3d(landscape_cont2, trait_pair_cont2, theta = -30, phi = 30, grid_n = 200)
        dev.off()
    }

    # 3. jaw × SL
    cat("\n--- Continuous: jaw × SL ---\n")
    trait_pair_cont3 <- c("jaw", "SL")

    gam_cont3 <- mgcv::gam(
        as.formula(paste(FITNESS_CONTINUOUS, "~ s(", trait_pair_cont3[1], ",", trait_pair_cont3[2], ")")),
        family = gaussian,
        data = prepared_continuous
    )

    landscape_cont3 <- adaptive_landscape(
        data = prepared_continuous,
        fitness_model = gam_cont3,
        trait_cols = trait_pair_cont3,
        simulation_n = 500,
        grid_n = 50
    )

    saveRDS(landscape_cont3, file.path(model_dir, "adaptive_landscape_continuous_jaw_SL.rds"))

    if (exists("plot_adaptive_landscape")) {
        p2d <- plot_adaptive_landscape(landscape_cont3, trait_pair_cont3, prepared_continuous, bins = 12)
        ggsave(file.path(figure_dir, "adaptive_landscape_continuous_jaw_SL_2d.png"), p2d, width = 8, height = 6, dpi = 300)
    }
    if (exists("plot_adaptive_landscape_3d")) {
        png(file.path(figure_dir, "adaptive_landscape_continuous_jaw_SL_3d.png"), width = 8, height = 6, units = "in", res = 300)
        plot_adaptive_landscape_3d(landscape_cont3, trait_pair_cont3, theta = -30, phi = 30, grid_n = 200)
        dev.off()
    }
} else {
    cat("\nSkipping adaptive_landscape (function not found)\n")
}

# ======================================================
# 17 Compare Fitness Surfaces (Fixed)
# ======================================================

if (exists("compare_fitness_surfaces_data") &&
    exists("plot_fitness_surfaces_comparison")) {
    cat("\n=== Compare Fitness Surfaces ===\n")

    # ======================================================
    # Binary: nasal × SL
    # ======================================================
    cat("\n--- Binary: nasal × SL ---\n")

    cfs_binary_nasal_SL <- correlated_fitness_surface(
        data = prepared_binary,
        fitness_col = FITNESS_BINARY,
        trait_cols = c("nasal", "SL"),
        grid_n = 50,
        method = "gam"
    )

    landscape_binary_nasal_SL <- readRDS(file.path(model_dir, "adaptive_landscape_binary_nasal_SL.rds"))

    comparison_binary1 <- compare_fitness_surfaces_data(
        correlated_surface = cfs_binary_nasal_SL,
        adaptive_landscape = landscape_binary_nasal_SL,
        trait_cols = c("nasal", "SL")
    )


    plots_binary1 <- plot_fitness_surfaces_comparison(
        comparison_data = comparison_binary1,
        bins = 10,
        title = "Binary: nasal × SL - Individual vs Population"
    )


    if (!is.null(plots_binary1$side_by_side)) {
        ggsave(file.path(figure_dir, "comparison_binary_nasal_SL_side_by_side.png"),
            plots_binary1$side_by_side,
            width = 12, height = 5, dpi = 300
        )
    }
    if (!is.null(plots_binary1$overlay)) {
        ggsave(file.path(figure_dir, "comparison_binary_nasal_SL_overlay.png"),
            plots_binary1$overlay,
            width = 8, height = 6, dpi = 300
        )
    }


    cor_df <- comparison_binary1$combined_data
    individual_fit <- cor_df$fitness[cor_df$type == "Correlated Fitness (Individual)"]
    population_fit <- cor_df$fitness[cor_df$type == "Adaptive Landscape (Population)"]


    if (length(individual_fit) == length(population_fit)) {
        cor_val <- cor(individual_fit, population_fit, use = "complete.obs")
        cat("  Correlation:", round(cor_val, 4), "\n")


        write.csv(data.frame(
            Fitness = "Binary",
            Trait1 = "nasal",
            Trait2 = "SL",
            Correlation = cor_val,
            N = length(individual_fit)
        ), file.path(table_dir, "comparison_correlation_binary_nasal_SL.csv"), row.names = FALSE)
    } else {
        cat(
            "  Warning: Length mismatch - individual:", length(individual_fit),
            "population:", length(population_fit), "\n"
        )
    }

    # ======================================================
    # Binary: jaw × SL
    # ======================================================
    cat("\n--- Binary: jaw × SL ---\n")

    cfs_binary_jaw_SL <- correlated_fitness_surface(
        data = prepared_binary,
        fitness_col = FITNESS_BINARY,
        trait_cols = c("jaw", "SL"),
        grid_n = 50,
        method = "gam"
    )

    landscape_binary_jaw_SL <- readRDS(file.path(model_dir, "adaptive_landscape_binary_jaw_SL.rds"))

    comparison_binary2 <- compare_fitness_surfaces_data(
        correlated_surface = cfs_binary_jaw_SL,
        adaptive_landscape = landscape_binary_jaw_SL,
        trait_cols = c("jaw", "SL")
    )

    plots_binary2 <- plot_fitness_surfaces_comparison(
        comparison_data = comparison_binary2,
        bins = 10,
        title = "Binary: jaw × SL - Individual vs Population"
    )

    if (!is.null(plots_binary2$side_by_side)) {
        ggsave(file.path(figure_dir, "comparison_binary_jaw_SL_side_by_side.png"),
            plots_binary2$side_by_side,
            width = 12, height = 5, dpi = 300
        )
    }
    if (!is.null(plots_binary2$overlay)) {
        ggsave(file.path(figure_dir, "comparison_binary_jaw_SL_overlay.png"),
            plots_binary2$overlay,
            width = 8, height = 6, dpi = 300
        )
    }

    cor_df <- comparison_binary2$combined_data
    individual_fit <- cor_df$fitness[cor_df$type == "Correlated Fitness (Individual)"]
    population_fit <- cor_df$fitness[cor_df$type == "Adaptive Landscape (Population)"]

    if (length(individual_fit) == length(population_fit)) {
        cor_val <- cor(individual_fit, population_fit, use = "complete.obs")
        cat("  Correlation:", round(cor_val, 4), "\n")

        write.csv(data.frame(
            Fitness = "Binary",
            Trait1 = "jaw",
            Trait2 = "SL",
            Correlation = cor_val,
            N = length(individual_fit)
        ), file.path(table_dir, "comparison_correlation_binary_jaw_SL.csv"), row.names = FALSE)
    } else {
        cat("  Warning: Length mismatch\n")
    }

    # ======================================================
    # Continuous: jaw × mouth
    # ======================================================
    cat("\n--- Continuous: jaw × mouth ---\n")

    cfs_continuous_jaw_mouth <- correlated_fitness_surface(
        data = prepared_continuous,
        fitness_col = FITNESS_CONTINUOUS,
        trait_cols = c("jaw", "mouth"),
        grid_n = 50,
        method = "tps"
    )

    landscape_continuous_jaw_mouth <- readRDS(file.path(model_dir, "adaptive_landscape_continuous_jaw_mouth.rds"))

    comparison_cont1 <- compare_fitness_surfaces_data(
        correlated_surface = cfs_continuous_jaw_mouth,
        adaptive_landscape = landscape_continuous_jaw_mouth,
        trait_cols = c("jaw", "mouth")
    )

    plots_cont1 <- plot_fitness_surfaces_comparison(
        comparison_data = comparison_cont1,
        bins = 10,
        title = "Continuous: jaw × mouth - Individual vs Population"
    )

    if (!is.null(plots_cont1$side_by_side)) {
        ggsave(file.path(figure_dir, "comparison_continuous_jaw_mouth_side_by_side.png"),
            plots_cont1$side_by_side,
            width = 12, height = 5, dpi = 300
        )
    }
    if (!is.null(plots_cont1$overlay)) {
        ggsave(file.path(figure_dir, "comparison_continuous_jaw_mouth_overlay.png"),
            plots_cont1$overlay,
            width = 8, height = 6, dpi = 300
        )
    }

    cor_df <- comparison_cont1$combined_data
    individual_fit <- cor_df$fitness[cor_df$type == "Correlated Fitness (Individual)"]
    population_fit <- cor_df$fitness[cor_df$type == "Adaptive Landscape (Population)"]

    if (length(individual_fit) == length(population_fit)) {
        cor_val <- cor(individual_fit, population_fit, use = "complete.obs")
        cat("  Correlation:", round(cor_val, 4), "\n")

        write.csv(data.frame(
            Fitness = "Continuous",
            Trait1 = "jaw",
            Trait2 = "mouth",
            Correlation = cor_val,
            N = length(individual_fit)
        ), file.path(table_dir, "comparison_correlation_continuous_jaw_mouth.csv"), row.names = FALSE)
    }

    # ======================================================
    # Continuous: mouth × SL
    # ======================================================
    cat("\n--- Continuous: mouth × SL ---\n")

    cfs_continuous_mouth_SL <- correlated_fitness_surface(
        data = prepared_continuous,
        fitness_col = FITNESS_CONTINUOUS,
        trait_cols = c("mouth", "SL"),
        grid_n = 50,
        method = "tps"
    )

    landscape_continuous_mouth_SL <- readRDS(file.path(model_dir, "adaptive_landscape_continuous_mouth_SL.rds"))

    comparison_cont2 <- compare_fitness_surfaces_data(
        correlated_surface = cfs_continuous_mouth_SL,
        adaptive_landscape = landscape_continuous_mouth_SL,
        trait_cols = c("mouth", "SL")
    )

    plots_cont2 <- plot_fitness_surfaces_comparison(
        comparison_data = comparison_cont2,
        bins = 10,
        title = "Continuous: mouth × SL - Individual vs Population"
    )

    if (!is.null(plots_cont2$side_by_side)) {
        ggsave(file.path(figure_dir, "comparison_continuous_mouth_SL_side_by_side.png"),
            plots_cont2$side_by_side,
            width = 12, height = 5, dpi = 300
        )
    }
    if (!is.null(plots_cont2$overlay)) {
        ggsave(file.path(figure_dir, "comparison_continuous_mouth_SL_overlay.png"),
            plots_cont2$overlay,
            width = 8, height = 6, dpi = 300
        )
    }

    cor_df <- comparison_cont2$combined_data
    individual_fit <- cor_df$fitness[cor_df$type == "Correlated Fitness (Individual)"]
    population_fit <- cor_df$fitness[cor_df$type == "Adaptive Landscape (Population)"]

    if (length(individual_fit) == length(population_fit)) {
        cor_val <- cor(individual_fit, population_fit, use = "complete.obs")
        cat("  Correlation:", round(cor_val, 4), "\n")

        write.csv(data.frame(
            Fitness = "Continuous",
            Trait1 = "mouth",
            Trait2 = "SL",
            Correlation = cor_val,
            N = length(individual_fit)
        ), file.path(table_dir, "comparison_correlation_continuous_mouth_SL.csv"), row.names = FALSE)
    }
} else {
    cat("\nSkipping surface comparison (required functions not found)\n")
}


# ------------------------------------------------------
# 14 Final summary
# ------------------------------------------------------

cat("\n========================================\n")
cat("TESTING COMPLETE\n")
cat("========================================\n")

cat("\nResults saved to:\n")
cat("  Tables:", table_dir, "\n")
cat("  Figures:", figure_dir, "\n")
cat("  Models:", model_dir, "\n")

cat("\nGenerated files:\n")
cat("\nTables:\n")
print(list.files(table_dir))

cat("\nFigures:\n")
print(list.files(figure_dir))

cat("\nModels:\n")
print(list.files(model_dir))

cat("\nSample size:", nrow(prepared), "\n")
cat("Traits analyzed:", length(TRAITS), "\n")
cat("Significant gradients:", sum(selection_results$P_Value < 0.05, na.rm = TRUE), "\n")

cat("\n========================================\n")
