# ======================================================
# test_fish.R
# Integration test using pupfish datasets (Crescent Pond + Little Lake)
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
# 6 Load pupfish datasets
# ------------------------------------------------------
cat("\n=== Loading Pupfish Datasets ===\n")

data_dirs <- c(here("R", "data"), here("R", "test_data"))

# ======================================================
# 6.1 Crescent Pond data (main dataset)
# ======================================================
crescent_files <- list.files(data_dirs,
    pattern = "Crescent.*Pond.*\\.csv$",
    full.names = TRUE, recursive = TRUE
)

crescent_data <- read.csv(crescent_files[1])
cat("\n--- Crescent Pond ---\n")
cat("File:", basename(crescent_files[1]), "\n")
cat("Rows:", nrow(crescent_data), "\n")
cat("Columns:", ncol(crescent_data), "\n")

# ======================================================
# 6.2 Little Lake data (comparison dataset)
# ======================================================
little_files <- list.files(data_dirs,
    pattern = "Little.*Lake.*\\.csv$",
    full.names = TRUE, recursive = TRUE
)

little_data <- read.csv(little_files[1])
cat("\n--- Little Lake ---\n")
cat("File:", basename(little_files[1]), "\n")
cat("Rows:", nrow(little_data), "\n")
cat("Columns:", ncol(little_data), "\n")

# ======================================================
# 6.3 Combine datasets (for lake comparison)
# ======================================================

crescent_data$lake <- "Crescent Pond"
little_data$lake <- "Little Lake"

all_pupfish_data <- rbind(crescent_data, little_data)

cat("\n=== Combined Dataset ===\n")
cat("Total rows:", nrow(all_pupfish_data), "\n")
cat("Crescent Pond:", sum(all_pupfish_data$lake == "Crescent Pond"), "\n")
cat("Little Lake:", sum(all_pupfish_data$lake == "Little Lake"), "\n")
cat("Lakes:", paste(unique(all_pupfish_data$lake), collapse = ", "), "\n")

main_data <- all_pupfish_data

# checking
cat("\nColumn names in combined data:\n")
print(names(main_data))

required_cols <- c(FITNESS_BINARY, FITNESS_CONTINUOUS, TRAITS, "lake")
missing_cols <- required_cols[!required_cols %in% names(main_data)]
if (length(missing_cols) > 0) {
    cat("\nMissing columns:", paste(missing_cols, collapse = ", "), "\n")
} else {
    cat("\nAll required columns present!\n")
}

cat("\nFirst few rows:\n")
print(head(main_data[, c(TRAITS[1:3], "lake")]))

# ------------------------------------------------------
# 7 Define traits and fitness
# ------------------------------------------------------

FITNESS_BINARY <- "survival"
FITNESS_CONTINUOUS <- "ln.growth"
TRAITS <- c("jaw", "eye", "body", "nasal", "mouth", "SL")
GROUP <- "lake"

pupfish_data <- main_data %>%
    dplyr::select(dplyr::all_of(c(FITNESS_BINARY, FITNESS_CONTINUOUS, TRAITS, GROUP))) %>%
    dplyr::filter(complete.cases(.))

cat("\nComplete observations:", nrow(pupfish_data), "\n")
cat("Traits:", paste(TRAITS, collapse = ", "), "\n")
cat("Lakes:", paste(unique(pupfish_data[[GROUP]]), collapse = ", "), "\n")

# ------------------------------------------------------
# 8 Data preparation
# ------------------------------------------------------

prepared_binary <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_BINARY,
    trait_cols = TRAITS,
    standardize = TRUE,
    group = NULL,
    add_relative = FALSE,
    na_action = "drop"
)

prepared_binary_group <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_BINARY,
    trait_cols = TRAITS,
    standardize = TRUE,
    group = GROUP,
    add_relative = FALSE,
    na_action = "drop"
)

prepared_continuous <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_CONTINUOUS,
    trait_cols = TRAITS,
    standardize = TRUE,
    group = NULL,
    add_relative = TRUE,
    na_action = "drop"
)

prepared_continuous_group <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_CONTINUOUS,
    trait_cols = TRAITS,
    standardize = TRUE,
    group = GROUP,
    add_relative = TRUE,
    na_action = "drop"
)

cat("\n--- No grouping ---\n")
cat("Binary data (overall): n =", nrow(prepared_binary), "\n")
cat("Continuous data (overall): n =", nrow(prepared_continuous), "\n")

cat("\n--- With grouping by lake ---\n")
cat("Binary data (grouped): n =", nrow(prepared_binary_group), "\n")
cat("Continuous data (grouped): n =", nrow(prepared_continuous_group), "\n")

cat("\n--- Sample size by group ---\n")
cat("Binary grouped data:\n")
print(table(prepared_binary_group[[GROUP]]))

cat("\nContinuous grouped data:\n")
print(table(prepared_continuous_group[[GROUP]]))

# ======================================================
# 9 Selection Differential
# ======================================================

sel_diff_all <- list()


for (trait in TRAITS) {
    cat("\n--- Trait:", trait, "---\n")

    # ======================================================
    # Binary Fitness
    # ======================================================

    # Without group
    sel_diff_bin <- selection_differential(
        data = prepared_binary,
        fitness_col = FITNESS_BINARY,
        trait_col = trait,
        standardized = TRUE,
        use_relative = FALSE
    )
    cat("  Binary (no group):", round(sel_diff_bin, 4), "\n")

    # With group
    sel_diff_bin_grouped <- selection_differential(
        data = prepared_binary_group,
        fitness_col = FITNESS_BINARY,
        trait_col = trait,
        group = GROUP,
        return_grouped = TRUE,
        standardized = TRUE,
        use_relative = FALSE
    )

    cat("  Binary (by group):\n")
    print(sel_diff_bin_grouped)

    # With group (return weighted mean)
    sel_diff_bin_weighted <- selection_differential(
        data = prepared_binary_group,
        fitness_col = FITNESS_BINARY,
        trait_col = trait,
        group = GROUP,
        return_grouped = FALSE,
        standardized = TRUE,
        use_relative = FALSE
    )
    cat("  Binary (weighted mean):", round(sel_diff_bin_weighted, 4), "\n")

    # ======================================================
    # Continuous Fitness
    # ======================================================

    # Without group
    sel_diff_cont <- selection_differential(
        data = prepared_continuous,
        fitness_col = FITNESS_CONTINUOUS,
        trait_col = trait,
        standardized = TRUE,
        use_relative = TRUE
    )
    cat("  Continuous (no group):", round(sel_diff_cont, 4), "\n")

    # With group
    sel_diff_cont_grouped <- selection_differential(
        data = prepared_continuous_group,
        fitness_col = FITNESS_CONTINUOUS,
        trait_col = trait,
        group = GROUP,
        return_grouped = TRUE,
        standardized = TRUE,
        use_relative = TRUE
    )

    cat("  Continuous (by group):\n")
    print(sel_diff_cont_grouped)

    sel_diff_cont_weighted <- selection_differential(
        data = prepared_continuous_group,
        fitness_col = FITNESS_CONTINUOUS,
        trait_col = trait,
        group = GROUP,
        return_grouped = FALSE,
        standardized = TRUE,
        use_relative = TRUE
    )
    cat("  Continuous (weighted mean):", round(sel_diff_cont_weighted, 4), "\n")

    sel_diff_all[[trait]] <- list(
        binary = list(
            overall = sel_diff_bin,
            grouped = sel_diff_bin_grouped,
            weighted = sel_diff_bin_weighted
        ),
        continuous = list(
            overall = sel_diff_cont,
            grouped = sel_diff_cont_grouped,
            weighted = sel_diff_cont_weighted
        )
    )


    # Save grouped results (binary)
    write.csv(sel_diff_bin_grouped,
        file.path(table_dir, paste0("selection_differentials_binary_", trait, "_grouped.csv")),
        row.names = FALSE
    )

    # Save grouped results (continuous)
    write.csv(sel_diff_cont_grouped,
        file.path(table_dir, paste0("selection_differentials_continuous_", trait, "_grouped.csv")),
        row.names = FALSE
    )

    # Save overall results
    overall_bin_df <- data.frame(
        trait = trait,
        selection_differential = sel_diff_bin,
        fitness = "binary",
        method = "overall"
    )
    write.csv(overall_bin_df,
        file.path(table_dir, paste0("selection_differentials_binary_", trait, "_overall.csv")),
        row.names = FALSE
    )

    overall_cont_df <- data.frame(
        trait = trait,
        selection_differential = sel_diff_cont,
        fitness = "continuous",
        method = "overall"
    )
    write.csv(overall_cont_df,
        file.path(table_dir, paste0("selection_differentials_continuous_", trait, "_overall.csv")),
        row.names = FALSE
    )

    # Save weighted mean results
    weighted_bin_df <- data.frame(
        trait = trait,
        selection_differential = sel_diff_bin_weighted,
        fitness = "binary",
        method = "weighted_mean"
    )
    write.csv(weighted_bin_df,
        file.path(table_dir, paste0("selection_differentials_binary_", trait, "_weighted.csv")),
        row.names = FALSE
    )

    weighted_cont_df <- data.frame(
        trait = trait,
        selection_differential = sel_diff_cont_weighted,
        fitness = "continuous",
        method = "weighted_mean"
    )
    write.csv(weighted_cont_df,
        file.path(table_dir, paste0("selection_differentials_continuous_", trait, "_weighted.csv")),
        row.names = FALSE
    )
}


cat("\n=== Selection Differential Summary ===\n")

summary_all <- data.frame()

for (trait in TRAITS) {
    res <- sel_diff_all[[trait]]

    # Binary overall
    summary_all <- rbind(summary_all, data.frame(
        Trait = trait,
        Fitness = "Binary",
        Analysis = "Overall",
        S = res$binary$overall,
        stringsAsFactors = FALSE
    ))

    # Binary weighted
    summary_all <- rbind(summary_all, data.frame(
        Trait = trait,
        Fitness = "Binary",
        Analysis = "Weighted Mean",
        S = res$binary$weighted,
        stringsAsFactors = FALSE
    ))

    # Binary by group
    if (is.data.frame(res$binary$grouped)) {
        for (i in 1:nrow(res$binary$grouped)) {
            summary_all <- rbind(summary_all, data.frame(
                Trait = trait,
                Fitness = "Binary",
                Analysis = res$binary$grouped[[GROUP]][i],
                S = res$binary$grouped$S[i],
                stringsAsFactors = FALSE
            ))
        }
    }

    # Continuous overall
    summary_all <- rbind(summary_all, data.frame(
        Trait = trait,
        Fitness = "Continuous",
        Analysis = "Overall",
        S = res$continuous$overall,
        stringsAsFactors = FALSE
    ))

    # Continuous weighted
    summary_all <- rbind(summary_all, data.frame(
        Trait = trait,
        Fitness = "Continuous",
        Analysis = "Weighted Mean",
        S = res$continuous$weighted,
        stringsAsFactors = FALSE
    ))

    # Continuous by group
    if (is.data.frame(res$continuous$grouped)) {
        for (i in 1:nrow(res$continuous$grouped)) {
            summary_all <- rbind(summary_all, data.frame(
                Trait = trait,
                Fitness = "Continuous",
                Analysis = res$continuous$grouped[[GROUP]][i],
                S = res$continuous$grouped$S[i],
                stringsAsFactors = FALSE
            ))
        }
    }
}


write.csv(summary_all, file.path(table_dir, "selection_differentials_all.csv"), row.names = FALSE)


# ======================================================
# 10 Selection Coefficients
# ======================================================

selection_coef_all <- list()

for (trait in TRAITS) {
    cat("\n--- Trait:", trait, "---\n")

    # ======================================================
    # Binary - Overall
    # ======================================================
    selection_bin_overall_single <- tryCatch(
        {
            selection_coefficients(
                data = prepared_binary,
                fitness_col = FITNESS_BINARY,
                trait_cols = trait,
                fitness_type = "binary",
                standardize = TRUE
            )
        },
        error = function(e) {
            cat("  Warning (binary overall):", e$message, "\n")
            data.frame(
                Term = trait,
                Type = "Linear",
                Beta_Coefficient = NA,
                Standard_Error = NA,
                P_Value = NA,
                Variance = NA,
                stringsAsFactors = FALSE
            )
        }
    )

    if (is.data.frame(selection_bin_overall_single) && nrow(selection_bin_overall_single) > 0) {
        write.csv(selection_bin_overall_single,
            file.path(table_dir, paste0("selection_binary_", trait, "_overall.csv")),
            row.names = FALSE
        )
    }

    # ======================================================
    # Binary - By Lake
    # ======================================================
    selection_bin_by_lake_single <- tryCatch(
        {
            selection_coefficients(
                data = prepared_binary_group,
                fitness_col = FITNESS_BINARY,
                trait_cols = trait,
                fitness_type = "binary",
                standardize = TRUE,
                group = GROUP,
                return_grouped = TRUE
            )
        },
        error = function(e) {
            cat("  Warning (binary by lake):", e$message, "\n")
            # 返回空数据框
            data.frame()
        }
    )

    if (is.data.frame(selection_bin_by_lake_single) && nrow(selection_bin_by_lake_single) > 0) {
        write.csv(selection_bin_by_lake_single,
            file.path(table_dir, paste0("selection_binary_", trait, "_by_lake.csv")),
            row.names = FALSE
        )
    }

    # ======================================================
    # Continuous - Overall
    # ======================================================
    selection_cont_overall_single <- tryCatch(
        {
            selection_coefficients(
                data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS,
                trait_cols = trait,
                fitness_type = "continuous",
                standardize = TRUE
            )
        },
        error = function(e) {
            cat("  Warning (continuous overall):", e$message, "\n")
            data.frame(
                Term = trait,
                Type = "Linear",
                Beta_Coefficient = NA,
                Standard_Error = NA,
                P_Value = NA,
                Variance = NA,
                stringsAsFactors = FALSE
            )
        }
    )

    if (is.data.frame(selection_cont_overall_single) && nrow(selection_cont_overall_single) > 0) {
        write.csv(selection_cont_overall_single,
            file.path(table_dir, paste0("selection_continuous_", trait, "_overall.csv")),
            row.names = FALSE
        )
    }

    # ======================================================
    # Continuous - By Lake
    # ======================================================
    selection_cont_by_lake_single <- tryCatch(
        {
            selection_coefficients(
                data = prepared_continuous_group,
                fitness_col = FITNESS_CONTINUOUS,
                trait_cols = trait,
                fitness_type = "continuous",
                standardize = TRUE,
                group = GROUP,
                return_grouped = TRUE
            )
        },
        error = function(e) {
            cat("  Warning (continuous by lake):", e$message, "\n")
            data.frame()
        }
    )

    if (is.data.frame(selection_cont_by_lake_single) && nrow(selection_cont_by_lake_single) > 0) {
        write.csv(selection_cont_by_lake_single,
            file.path(table_dir, paste0("selection_continuous_", trait, "_by_lake.csv")),
            row.names = FALSE
        )
    }

    selection_coef_all[[trait]] <- list(
        binary = list(
            overall = selection_bin_overall_single,
            by_lake = selection_bin_by_lake_single
        ),
        continuous = list(
            overall = selection_cont_overall_single,
            by_lake = selection_cont_by_lake_single
        )
    )

    if (is.data.frame(selection_bin_overall_single) && nrow(selection_bin_overall_single) > 0) {
        beta_bin <- selection_bin_overall_single$Beta_Coefficient[selection_bin_overall_single$Term == trait]
        if (length(beta_bin) > 0 && !is.na(beta_bin)) {
            cat("  Binary (overall): β =", round(beta_bin, 4), "\n")
        } else {
            cat("  Binary (overall): NA\n")
        }
    } else {
        cat("  Binary (overall): NA\n")
    }

    if (is.data.frame(selection_cont_overall_single) && nrow(selection_cont_overall_single) > 0) {
        beta_cont <- selection_cont_overall_single$Beta_Coefficient[selection_cont_overall_single$Term == trait]
        if (length(beta_cont) > 0 && !is.na(beta_cont)) {
            cat("  Continuous (overall): β =", round(beta_cont, 4), "\n")
        } else {
            cat("  Continuous (overall): NA\n")
        }
    } else {
        cat("  Continuous (overall): NA\n")
    }
}


cat("\n--- Multi-trait Analysis ---\n")

# Binary - Overall
selection_bin_multi_overall <- tryCatch(
    {
        selection_coefficients(
            data = prepared_binary,
            fitness_col = FITNESS_BINARY,
            trait_cols = TRAITS,
            fitness_type = "binary",
            standardize = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait binary overall failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_bin_multi_overall)) {
    write.csv(selection_bin_multi_overall,
        file.path(table_dir, "selection_binary_multi_overall.csv"),
        row.names = FALSE
    )
    cat("  Binary multi-trait saved\n")
}

# Binary - By Lake
selection_bin_multi_by_lake <- tryCatch(
    {
        selection_coefficients(
            data = prepared_binary_group,
            fitness_col = FITNESS_BINARY,
            trait_cols = TRAITS,
            fitness_type = "binary",
            standardize = TRUE,
            group = GROUP,
            return_grouped = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait binary by lake failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_bin_multi_by_lake)) {
    write.csv(selection_bin_multi_by_lake,
        file.path(table_dir, "selection_binary_multi_by_lake.csv"),
        row.names = FALSE
    )
    cat("  Binary multi-trait by lake saved\n")
}

# Continuous - Overall
selection_cont_multi_overall <- tryCatch(
    {
        selection_coefficients(
            data = prepared_continuous,
            fitness_col = FITNESS_CONTINUOUS,
            trait_cols = TRAITS,
            fitness_type = "continuous",
            standardize = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait continuous overall failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_cont_multi_overall)) {
    write.csv(selection_cont_multi_overall,
        file.path(table_dir, "selection_continuous_multi_overall.csv"),
        row.names = FALSE
    )
    cat("  Continuous multi-trait saved\n")
}

# Continuous - By Lake
selection_cont_multi_by_lake <- tryCatch(
    {
        selection_coefficients(
            data = prepared_continuous_group,
            fitness_col = FITNESS_CONTINUOUS,
            trait_cols = TRAITS,
            fitness_type = "continuous",
            standardize = TRUE,
            group = GROUP,
            return_grouped = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait continuous by lake failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_cont_multi_by_lake)) {
    write.csv(selection_cont_multi_by_lake,
        file.path(table_dir, "selection_continuous_multi_by_lake.csv"),
        row.names = FALSE
    )
    cat("  Continuous multi-trait by lake saved\n")
}


cat("\n=== Selection Coefficients Summary ===\n")

summary_coef <- data.frame()

for (trait in TRAITS) {
    res <- selection_coef_all[[trait]]

    # Binary overall
    df_bin <- res$binary$overall
    if (is.data.frame(df_bin) && nrow(df_bin) > 0) {
        beta_bin <- df_bin$Beta_Coefficient[df_bin$Term == trait]
        p_bin <- df_bin$P_Value[df_bin$Term == trait]

        if (length(beta_bin) > 0) {
            summary_coef <- rbind(summary_coef, data.frame(
                Trait = trait,
                Fitness = "Binary",
                Analysis = "Overall",
                Beta = beta_bin[1],
                P_Value = p_bin[1],
                stringsAsFactors = FALSE
            ))
        }
    }

    # Binary by lake
    df_bin_lake <- res$binary$by_lake
    if (is.data.frame(df_bin_lake) && nrow(df_bin_lake) > 0) {
        for (i in 1:nrow(df_bin_lake)) {
            if (df_bin_lake$Term[i] == trait) {
                summary_coef <- rbind(summary_coef, data.frame(
                    Trait = trait,
                    Fitness = "Binary",
                    Analysis = as.character(df_bin_lake[[GROUP]][i]),
                    Beta = df_bin_lake$Beta_Coefficient[i],
                    P_Value = df_bin_lake$P_Value[i],
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    # Continuous overall
    df_cont <- res$continuous$overall
    if (is.data.frame(df_cont) && nrow(df_cont) > 0) {
        beta_cont <- df_cont$Beta_Coefficient[df_cont$Term == trait]
        p_cont <- df_cont$P_Value[df_cont$Term == trait]

        if (length(beta_cont) > 0) {
            summary_coef <- rbind(summary_coef, data.frame(
                Trait = trait,
                Fitness = "Continuous",
                Analysis = "Overall",
                Beta = beta_cont[1],
                P_Value = p_cont[1],
                stringsAsFactors = FALSE
            ))
        }
    }

    # Continuous by lake
    df_cont_lake <- res$continuous$by_lake
    if (is.data.frame(df_cont_lake) && nrow(df_cont_lake) > 0) {
        for (i in 1:nrow(df_cont_lake)) {
            if (df_cont_lake$Term[i] == trait) {
                summary_coef <- rbind(summary_coef, data.frame(
                    Trait = trait,
                    Fitness = "Continuous",
                    Analysis = as.character(df_cont_lake[[GROUP]][i]),
                    Beta = df_cont_lake$Beta_Coefficient[i],
                    P_Value = df_cont_lake$P_Value[i],
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
}


write.csv(summary_coef, file.path(table_dir, "selection_coefficients_summary.csv"), row.names = FALSE)



# ======================================================
# 10 Selection Coefficients
# ======================================================

selection_coef_all <- list()

for (trait in TRAITS) {
    cat("\n--- Trait:", trait, "---\n")

    # ======================================================
    # Binary - Overall
    # ======================================================
    bin_overall <- tryCatch(
        {
            analyze_disruptive_selection(
                data = prepared_binary,
                fitness_col = FITNESS_BINARY,
                trait_col = trait,
                fitness_type = "binary",
                standardize = TRUE
            )
        },
        error = function(e) {
            cat("  Warning (binary overall):", e$message, "\n")
            NULL
        }
    )

    if (!is.null(bin_overall) && nrow(bin_overall) > 0) {
        write.csv(bin_overall,
            file.path(table_dir, paste0("selection_binary_", trait, "_overall.csv")),
            row.names = FALSE
        )
    }

    # ======================================================
    # Binary - By Lake
    # ======================================================
    bin_by_lake <- data.frame()
    for (lake in unique(prepared_binary_group[[GROUP]])) {
        data_lake <- prepared_binary_group[prepared_binary_group[[GROUP]] == lake, ]

        res <- tryCatch(
            {
                analyze_disruptive_selection(
                    data = data_lake,
                    fitness_col = FITNESS_BINARY,
                    trait_col = trait,
                    fitness_type = "binary",
                    standardize = TRUE
                )
            },
            error = function(e) {
                NULL
            }
        )

        if (!is.null(res) && nrow(res) > 0) {
            res$Group <- lake
            bin_by_lake <- rbind(bin_by_lake, res)
        }
    }

    if (nrow(bin_by_lake) > 0) {
        write.csv(bin_by_lake,
            file.path(table_dir, paste0("selection_binary_", trait, "_by_lake.csv")),
            row.names = FALSE
        )
    }

    # ======================================================
    # Continuous - Overall
    # ======================================================
    cont_overall <- tryCatch(
        {
            analyze_disruptive_selection(
                data = prepared_continuous,
                fitness_col = FITNESS_CONTINUOUS,
                trait_col = trait,
                fitness_type = "continuous",
                standardize = TRUE
            )
        },
        error = function(e) {
            cat("  Warning (continuous overall):", e$message, "\n")
            NULL
        }
    )

    if (!is.null(cont_overall) && nrow(cont_overall) > 0) {
        write.csv(cont_overall,
            file.path(table_dir, paste0("selection_continuous_", trait, "_overall.csv")),
            row.names = FALSE
        )
    }

    # ======================================================
    # Continuous - By Lake
    # ======================================================
    cont_by_lake <- data.frame()
    for (lake in unique(prepared_continuous_group[[GROUP]])) {
        data_lake <- prepared_continuous_group[prepared_continuous_group[[GROUP]] == lake, ]

        res <- tryCatch(
            {
                analyze_disruptive_selection(
                    data = data_lake,
                    fitness_col = FITNESS_CONTINUOUS,
                    trait_col = trait,
                    fitness_type = "continuous",
                    standardize = TRUE
                )
            },
            error = function(e) {
                NULL
            }
        )

        if (!is.null(res) && nrow(res) > 0) {
            res$Group <- lake
            cont_by_lake <- rbind(cont_by_lake, res)
        }
    }

    if (nrow(cont_by_lake) > 0) {
        write.csv(cont_by_lake,
            file.path(table_dir, paste0("selection_continuous_", trait, "_by_lake.csv")),
            row.names = FALSE
        )
    }


    selection_coef_all[[trait]] <- list(
        binary = list(
            overall = bin_overall,
            by_lake = bin_by_lake
        ),
        continuous = list(
            overall = cont_overall,
            by_lake = cont_by_lake
        )
    )


    if (!is.null(bin_overall) && nrow(bin_overall) > 0) {
        beta_bin <- bin_overall$Beta_Coefficient[bin_overall$Type == "Linear"]
        if (length(beta_bin) > 0) {
            cat("  Binary (overall): β =", round(beta_bin[1], 4), "\n")
        }
    }

    if (!is.null(cont_overall) && nrow(cont_overall) > 0) {
        beta_cont <- cont_overall$Beta_Coefficient[cont_overall$Type == "Linear"]
        if (length(beta_cont) > 0) {
            cat("  Continuous (overall): β =", round(beta_cont[1], 4), "\n")
        }
    }
}


cat("\n--- Multi-trait Analysis ---\n")

# Binary - Overall
selection_bin_multi_overall <- tryCatch(
    {
        selection_coefficients(
            data = prepared_binary,
            fitness_col = FITNESS_BINARY,
            trait_cols = TRAITS,
            fitness_type = "binary",
            standardize = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait binary overall failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_bin_multi_overall)) {
    write.csv(selection_bin_multi_overall,
        file.path(table_dir, "selection_binary_multi_overall.csv"),
        row.names = FALSE
    )
    cat("  Binary multi-trait saved\n")
}

# Binary - By Lake
selection_bin_multi_by_lake <- tryCatch(
    {
        selection_coefficients(
            data = prepared_binary_group,
            fitness_col = FITNESS_BINARY,
            trait_cols = TRAITS,
            fitness_type = "binary",
            standardize = TRUE,
            group = GROUP,
            return_grouped = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait binary by lake failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_bin_multi_by_lake)) {
    write.csv(selection_bin_multi_by_lake,
        file.path(table_dir, "selection_binary_multi_by_lake.csv"),
        row.names = FALSE
    )
    cat("  Binary multi-trait by lake saved\n")
}

# Continuous - Overall
selection_cont_multi_overall <- tryCatch(
    {
        selection_coefficients(
            data = prepared_continuous,
            fitness_col = FITNESS_CONTINUOUS,
            trait_cols = TRAITS,
            fitness_type = "continuous",
            standardize = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait continuous overall failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_cont_multi_overall)) {
    write.csv(selection_cont_multi_overall,
        file.path(table_dir, "selection_continuous_multi_overall.csv"),
        row.names = FALSE
    )
    cat("  Continuous multi-trait saved\n")
}

# Continuous - By Lake
selection_cont_multi_by_lake <- tryCatch(
    {
        selection_coefficients(
            data = prepared_continuous_group,
            fitness_col = FITNESS_CONTINUOUS,
            trait_cols = TRAITS,
            fitness_type = "continuous",
            standardize = TRUE,
            group = GROUP,
            return_grouped = TRUE
        )
    },
    error = function(e) {
        cat("Multi-trait continuous by lake failed:", e$message, "\n")
        NULL
    }
)

if (!is.null(selection_cont_multi_by_lake)) {
    write.csv(selection_cont_multi_by_lake,
        file.path(table_dir, "selection_continuous_multi_by_lake.csv"),
        row.names = FALSE
    )
    cat("  Continuous multi-trait by lake saved\n")
}


cat("\n=== Selection Coefficients Summary (Univariate) ===\n")

summary_coef <- data.frame()

for (trait in TRAITS) {
    res <- selection_coef_all[[trait]]

    # Binary overall
    if (!is.null(res$binary$overall) && nrow(res$binary$overall) > 0) {
        df <- res$binary$overall
        beta <- df$Beta_Coefficient[df$Type == "Linear"]
        p_val <- df$P_Value[df$Type == "Linear"]
        gamma <- df$Beta_Coefficient[df$Type == "Quadratic"]
        p_gamma <- df$P_Value[df$Type == "Quadratic"]

        if (length(beta) > 0) {
            summary_coef <- rbind(summary_coef, data.frame(
                Trait = trait,
                Fitness = "Binary",
                Analysis = "Overall",
                Beta = beta[1],
                P_Beta = p_val[1],
                Gamma = ifelse(length(gamma) > 0, gamma[1], NA),
                P_Gamma = ifelse(length(p_gamma) > 0, p_gamma[1], NA),
                stringsAsFactors = FALSE
            ))
        }
    }

    # Continuous overall
    if (!is.null(res$continuous$overall) && nrow(res$continuous$overall) > 0) {
        df <- res$continuous$overall
        beta <- df$Beta_Coefficient[df$Type == "Linear"]
        p_val <- df$P_Value[df$Type == "Linear"]
        gamma <- df$Beta_Coefficient[df$Type == "Quadratic"]
        p_gamma <- df$P_Value[df$Type == "Quadratic"]

        if (length(beta) > 0) {
            summary_coef <- rbind(summary_coef, data.frame(
                Trait = trait,
                Fitness = "Continuous",
                Analysis = "Overall",
                Beta = beta[1],
                P_Beta = p_val[1],
                Gamma = ifelse(length(gamma) > 0, gamma[1], NA),
                P_Gamma = ifelse(length(p_gamma) > 0, p_gamma[1], NA),
                stringsAsFactors = FALSE
            ))
        }
    }
}

write.csv(summary_coef, file.path(table_dir, "selection_coefficients_summary.csv"), row.names = FALSE)


# ======================================================
# 10 Selection Coefficients
# ======================================================
