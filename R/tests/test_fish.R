# ======================================================
# test_fish.R
# Integration test using pupfish datasets (Crescent Pond + Little Lake)
# ======================================================

rm(list = ls())
gc()

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

output_dir <- file.path("R", "results", "pupfish_results")
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

data_dirs <- c(file.path("R", "data"), file.path("R", "test_data"))

# ======================================================
# 6.1 Crescent Pond data (main dataset)
# ======================================================
crescent_files <- list.files(
    data_dirs,
    pattern    = "Crescent.*Pond.*\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
)

crescent_data <- read.csv(crescent_files[1])
cat("\n--- Crescent Pond ---\n")
cat("File:", basename(crescent_files[1]), "\n")
cat("Rows:", nrow(crescent_data), "\n")
cat("Columns:", ncol(crescent_data), "\n")

# ======================================================
# 6.2 Little Lake data (comparison dataset)
# ======================================================
little_files <- list.files(
    data_dirs,
    pattern    = "Little.*Lake.*\\.csv$",
    full.names = TRUE,
    recursive  = TRUE
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

cat("\nColumn names in combined data:\n")
print(names(main_data))

# ------------------------------------------------------
# 7 Define traits and fitness
# ------------------------------------------------------

FITNESS_BINARY <- "survival"
FITNESS_CONTINUOUS <- "ln.growth"
TRAITS <- c("jaw", "pmx", "nasal", "noseangle", "body", "eye")
GROUP <- "lake"

pupfish_data <- main_data %>%
    dplyr::select(dplyr::all_of(c(FITNESS_BINARY, FITNESS_CONTINUOUS, TRAITS, GROUP))) %>%
    dplyr::filter(complete.cases(.))

cat("\nComplete observations:", nrow(pupfish_data), "\n")
cat("Traits:", paste(TRAITS, collapse = ", "), "\n")
cat("Lakes:", paste(unique(pupfish_data[[GROUP]]), collapse = ", "), "\n")


pupfish_data <- main_data %>%
    dplyr::select(dplyr::all_of(c(FITNESS_BINARY, FITNESS_CONTINUOUS, TRAITS, GROUP, "density"))) %>%
    dplyr::filter(density == "H") %>%     
    dplyr::filter(complete.cases(.)) %>%
    dplyr::select(-density)                 

cat("\nComplete observations:", nrow(pupfish_data), "\n")
cat("By lake:\n")
print(table(pupfish_data[[GROUP]]))

# main_data %>% dplyr::filter(density == "H") %>% nrow()

# main_data %>% 
#     dplyr::filter(density == "H") %>%
#     dplyr::select(all_of(c(FITNESS_BINARY, TRAITS, GROUP))) %>%
#     dplyr::filter(complete.cases(.)) %>% nrow()

# main_data %>%
#     dplyr::filter(density == "H") %>%
#     dplyr::select(all_of(c(FITNESS_CONTINUOUS, TRAITS, GROUP))) %>%
#     dplyr::filter(complete.cases(.)) %>% nrow()


# ------------------------------------------------------
# 8 Data preparation
# ------------------------------------------------------

pupfish_continuous <- pupfish_data %>%
    dplyr::filter(ln.growth != 0)   # ← 改成 != 0，不是 > 0

cat("Survivors with growth data:", nrow(pupfish_continuous), "\n")
print(table(pupfish_continuous[[GROUP]]))


prepared_binary <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_BINARY,
    trait_cols = TRAITS,
    standardize = FALSE,
    group = NULL,
    add_relative = FALSE,
    na_action = "drop"
)

prepared_binary_group <- prepare_selection_data(
    data = pupfish_data,
    fitness_col = FITNESS_BINARY,
    trait_cols = TRAITS,
    standardize = FALSE,
    group = GROUP,
    add_relative = FALSE,
    na_action = "drop"
)

prepared_continuous <- prepare_selection_data(
    data         = pupfish_continuous,
    fitness_col  = FITNESS_CONTINUOUS,
    trait_cols   = TRAITS,
    standardize  = FALSE,
    group        = NULL,
    add_relative = TRUE,
    na_action    = "drop"
)

prepared_continuous_group <- prepare_selection_data(
    data         = pupfish_continuous,
    fitness_col  = FITNESS_CONTINUOUS,
    trait_cols   = TRAITS,
    standardize  = FALSE,
    group        = GROUP,
    add_relative = TRUE,
    na_action    = "drop"
)

cat("Continuous n:", nrow(prepared_continuous), "\n")
print(table(prepared_continuous_group[[GROUP]]))

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

    # Binary - Without group
    sel_diff_bin <- selection_differential(
        data         = prepared_binary,
        fitness_col  = FITNESS_BINARY,
        trait_col    = trait,
        standardized = TRUE,
        use_relative = FALSE
    )
    cat("  Binary (no group):", round(sel_diff_bin, 4), "\n")

    # Binary - With group
    sel_diff_bin_grouped <- selection_differential(
        data           = prepared_binary_group,
        fitness_col    = FITNESS_BINARY,
        trait_col      = trait,
        group          = GROUP,
        return_grouped = TRUE,
        standardized   = TRUE,
        use_relative   = FALSE
    )
    cat("  Binary (by group):\n")
    print(sel_diff_bin_grouped)

    # Binary - Weighted mean
    sel_diff_bin_weighted <- selection_differential(
        data           = prepared_binary_group,
        fitness_col    = FITNESS_BINARY,
        trait_col      = trait,
        group          = GROUP,
        return_grouped = FALSE,
        standardized   = TRUE,
        use_relative   = FALSE
    )
    cat("  Binary (weighted mean):", round(sel_diff_bin_weighted, 4), "\n")

    # Continuous - Without group
    sel_diff_cont <- selection_differential(
        data         = prepared_continuous,
        fitness_col  = FITNESS_CONTINUOUS,
        trait_col    = trait,
        standardized = TRUE,
        use_relative = TRUE
    )
    cat("  Continuous (no group):", round(sel_diff_cont, 4), "\n")

    # Continuous - With group
    sel_diff_cont_grouped <- selection_differential(
        data           = prepared_continuous_group,
        fitness_col    = FITNESS_CONTINUOUS,
        trait_col      = trait,
        group          = GROUP,
        return_grouped = TRUE,
        standardized   = TRUE,
        use_relative   = TRUE
    )
    cat("  Continuous (by group):\n")
    print(sel_diff_cont_grouped)

    # Continuous - Weighted mean
    sel_diff_cont_weighted <- selection_differential(
        data           = prepared_continuous_group,
        fitness_col    = FITNESS_CONTINUOUS,
        trait_col      = trait,
        group          = GROUP,
        return_grouped = FALSE,
        standardized   = TRUE,
        use_relative   = TRUE
    )
    cat("  Continuous (weighted mean):", round(sel_diff_cont_weighted, 4), "\n")

    sel_diff_all[[trait]] <- list(
        binary     = list(overall = sel_diff_bin, grouped = sel_diff_bin_grouped, weighted = sel_diff_bin_weighted),
        continuous = list(overall = sel_diff_cont, grouped = sel_diff_cont_grouped, weighted = sel_diff_cont_weighted)
    )
}

# Summary table
summary_all <- data.frame()

for (trait in TRAITS) {
    res <- sel_diff_all[[trait]]

    summary_all <- rbind(summary_all, data.frame(Trait = trait, Fitness = "Binary",     Analysis = "Overall",       S = res$binary$overall,     stringsAsFactors = FALSE))
    summary_all <- rbind(summary_all, data.frame(Trait = trait, Fitness = "Binary",     Analysis = "Weighted Mean", S = res$binary$weighted,    stringsAsFactors = FALSE))
    summary_all <- rbind(summary_all, data.frame(Trait = trait, Fitness = "Continuous", Analysis = "Overall",       S = res$continuous$overall,  stringsAsFactors = FALSE))
    summary_all <- rbind(summary_all, data.frame(Trait = trait, Fitness = "Continuous", Analysis = "Weighted Mean", S = res$continuous$weighted, stringsAsFactors = FALSE))

    if (is.data.frame(res$binary$grouped)) {
        for (i in seq_len(nrow(res$binary$grouped))) {
            summary_all <- rbind(summary_all, data.frame(
                Trait = trait, Fitness = "Binary",
                Analysis = res$binary$grouped[[GROUP]][i],
                S = res$binary$grouped$S[i], stringsAsFactors = FALSE))
        }
    }

    if (is.data.frame(res$continuous$grouped)) {
        for (i in seq_len(nrow(res$continuous$grouped))) {
            summary_all <- rbind(summary_all, data.frame(
                Trait = trait, Fitness = "Continuous",
                Analysis = res$continuous$grouped[[GROUP]][i],
                S = res$continuous$grouped$S[i], stringsAsFactors = FALSE))
        }
    }
}

write.csv(summary_all,
    file.path(table_dir, "selection_differentials_all.csv"),
    row.names = FALSE
)

cat("\n=== Selection Differential Summary ===\n")
print(summary_all)

# ======================================================
# 10 Selection Coefficients
# ======================================================

trait_pairs        <- combn(TRAITS, 2, simplify = FALSE)
selection_coef_all <- list()

# Binary - Without group
cat("\n--- Binary (Overall) ---\n")
for (pair in trait_pairs) {
    pair_name <- paste(pair, collapse = "_")
    result <- suppressWarnings(selection_coefficients(
        data         = prepared_binary,
        fitness_col  = FITNESS_BINARY,
        trait_cols   = pair,
        fitness_type = "binary",
        standardize  = FALSE
    ))
    write.csv(result, file.path(table_dir, paste0("selection_binary_", pair_name, ".csv")), row.names = FALSE)
    selection_coef_all[[paste0("binary_", pair_name)]] <- result
}

# Continuous - Without group
cat("\n--- Continuous (Overall) ---\n")
for (pair in trait_pairs) {
    pair_name <- paste(pair, collapse = "_")
    result <- suppressWarnings(selection_coefficients(
        data         = prepared_continuous,
        fitness_col  = FITNESS_CONTINUOUS,
        trait_cols   = pair,
        fitness_type = "continuous",
        standardize  = FALSE
    ))
    write.csv(result, file.path(table_dir, paste0("selection_continuous_", pair_name, ".csv")), row.names = FALSE)
    selection_coef_all[[paste0("continuous_", pair_name)]] <- result
}

# Binary - With group
cat("\n--- Binary (By Group) ---\n")
for (pair in trait_pairs) {
    pair_name <- paste(pair, collapse = "_")
    result <- suppressWarnings(selection_coefficients(
        data           = prepared_binary_group,
        fitness_col    = FITNESS_BINARY,
        trait_cols     = pair,
        fitness_type   = "binary",
        standardize    = FALSE,
        group          = GROUP,
        return_grouped = TRUE
    ))
    write.csv(result, file.path(table_dir, paste0("selection_binary_", pair_name, "_group.csv")), row.names = FALSE)
    selection_coef_all[[paste0("binary_", pair_name, "_group")]] <- result
}

# Continuous - With group
cat("\n--- Continuous (By Group) ---\n")
for (pair in trait_pairs) {
    pair_name <- paste(pair, collapse = "_")
    result <- suppressWarnings(selection_coefficients(
        data           = prepared_continuous_group,
        fitness_col    = FITNESS_CONTINUOUS,
        trait_cols     = pair,
        fitness_type   = "continuous",
        standardize    = FALSE,
        group          = GROUP,
        return_grouped = TRUE
    ))
    write.csv(result, file.path(table_dir, paste0("selection_continuous_", pair_name, "_group.csv")), row.names = FALSE)
    selection_coef_all[[paste0("continuous_", pair_name, "_group")]] <- result
}

cat("\nTotal results stored:", length(selection_coef_all), "\n")

summary_coef <- data.frame()

for (pair in trait_pairs) {
    pair_name <- paste(pair, collapse = "_")

    for (fitness_label in c("binary", "continuous")) {
        key <- paste0(fitness_label, "_", pair_name)
        df  <- selection_coef_all[[key]]
        if (is.null(df)) next

        for (type_label in c("Linear", "Quadratic", "Correlational")) {
            rows <- df[df$Type == type_label, ]
            if (nrow(rows) == 0) next
            for (i in seq_len(nrow(rows))) {
                summary_coef <- rbind(summary_coef, data.frame(
                    Trait1      = pair[1],
                    Trait2      = pair[2],
                    Term        = rows$Term[i],
                    Fitness     = tools::toTitleCase(fitness_label),
                    Analysis    = "Overall",
                    Type        = type_label,
                    Coefficient = round(rows$Beta_Coefficient[i], 4),
                    P_Value     = rows$P_Value[i],
                    stringsAsFactors = FALSE
                ))
            }
        }
    }

    for (fitness_label in c("binary", "continuous")) {
        key <- paste0(fitness_label, "_", pair_name, "_group")
        df  <- selection_coef_all[[key]]
        if (is.null(df)) next

        linear_rows <- df[df$Type == "Linear", ]
        if (nrow(linear_rows) == 0) next
        for (i in seq_len(nrow(linear_rows))) {
            group_name <- linear_rows[[GROUP]][i]
            summary_coef <- rbind(summary_coef, data.frame(
                Trait1      = pair[1],
                Trait2      = pair[2],
                Term        = linear_rows$Term[i],
                Fitness     = tools::toTitleCase(fitness_label),
                Analysis    = paste0("Group: ", group_name),
                Type        = "Linear",
                Coefficient = round(linear_rows$Beta_Coefficient[i], 4),
                P_Value     = linear_rows$P_Value[i],
                stringsAsFactors = FALSE
            ))
        }
    }
}

write.csv(summary_coef,
    file.path(table_dir, "selection_coefficients_summary_all.csv"),
    row.names = FALSE
)

cat("Summary table rows:", nrow(summary_coef), "\n")
head(summary_coef, 10)

# summary_coef %>%
#     dplyr::filter(grepl("body", Term), 
#                   Type == "Linear", 
#                   Analysis == "Overall") %>%
#     dplyr::arrange(Fitness, P_Value)

summary_coef %>%
    dplyr::filter(Type == "Linear", 
                  Analysis == "Overall",
                  P_Value < 0.05) %>%
    dplyr::arrange(Fitness, P_Value)

summary_coef %>%
    dplyr::filter(Type %in% c("Quadratic", "Correlational"),
                  Analysis == "Overall",
                  P_Value < 0.05) %>%
    dplyr::arrange(Fitness, Type, P_Value)

# ======================================================
# 11 Disruptive Selection
# ======================================================

disruptive_binary <- list()
disruptive_binary_group <- list()
disruptive_continuous <- list()
disruptive_continuous_group <- list()

for (trait in TRAITS) {
    # Binary - Without group
    disruptive_binary[[trait]] <- analyze_disruptive_selection(
        data         = prepared_binary,
        fitness_col  = FITNESS_BINARY,
        trait_col    = trait,
        fitness_type = "binary",
        standardize  = FALSE    # ← already standardized
    )
    write.csv(disruptive_binary[[trait]],
        file.path(table_dir, paste0("disruptive_binary_", trait, ".csv")),
        row.names = FALSE
    )

    # Binary - With group
    disruptive_binary_group[[trait]] <- analyze_disruptive_selection(
        data           = prepared_binary_group,
        fitness_col    = FITNESS_BINARY,
        trait_col      = trait,
        fitness_type   = "binary",
        standardize    = FALSE,    # ← already standardized
        group          = GROUP,
        return_grouped = TRUE
    )
    write.csv(disruptive_binary_group[[trait]],
        file.path(table_dir, paste0("disruptive_binary_", trait, "_group.csv")),
        row.names = FALSE
    )

    # Continuous - Without group
    disruptive_continuous[[trait]] <- analyze_disruptive_selection(
        data         = prepared_continuous,
        fitness_col  = FITNESS_CONTINUOUS,
        trait_col    = trait,
        fitness_type = "continuous",
        standardize  = FALSE    # ← already standardized
    )
    write.csv(disruptive_continuous[[trait]],
        file.path(table_dir, paste0("disruptive_continuous_", trait, ".csv")),
        row.names = FALSE
    )

    # Continuous - With group
    disruptive_continuous_group[[trait]] <- analyze_disruptive_selection(
        data           = prepared_continuous_group,
        fitness_col    = FITNESS_CONTINUOUS,
        trait_col      = trait,
        fitness_type   = "continuous",
        standardize    = FALSE,    # ← already standardized
        group          = GROUP,
        return_grouped = TRUE
    )
    write.csv(disruptive_continuous_group[[trait]],
        file.path(table_dir, paste0("disruptive_continuous_", trait, "_group.csv")),
        row.names = FALSE
    )
}

cat("\n=== Disruptive Selection Summary Table ===\n")

summary_df <- data.frame()

for (trait in TRAITS) {
    for (fitness_label in c("binary", "continuous")) {
        df <- if (fitness_label == "binary") disruptive_binary[[trait]] else disruptive_continuous[[trait]]
        if (is.null(df)) next

        linear_row    <- df[df$Type == "Linear", ]
        quadratic_row <- df[df$Type == "Quadratic", ]

        summary_df <- rbind(summary_df, data.frame(
            Trait     = trait,
            Fitness   = tools::toTitleCase(fitness_label),
            Analysis  = "Overall",
            Beta      = round(linear_row$Beta_Coefficient, 4),
            P_Beta    = linear_row$P_Value,
            Gamma     = round(quadratic_row$Beta_Coefficient, 4),
            P_Gamma   = quadratic_row$P_Value,
            Selection = ifelse(quadratic_row$P_Value < 0.05,
                ifelse(quadratic_row$Beta_Coefficient > 0, "Disruptive", "Stabilizing"),
                "None"
            ),
            stringsAsFactors = FALSE
        ))
    }
}

write.csv(summary_df,
    file.path(table_dir, "disruptive_selection_summary.csv"),
    row.names = FALSE
)

print(summary_df)


# ======================================================
# 12 Univariate Spline — Example: body depth
# ======================================================

# Binary - Overall
spline_body_binary <- univariate_spline(
    data         = prepared_binary,
    fitness_col  = FITNESS_BINARY,
    trait_col    = "body",
    fitness_type = "binary"
)

# Continuous - Overall
spline_body_continuous <- univariate_spline(
    data         = prepared_continuous,
    fitness_col  = FITNESS_CONTINUOUS,
    trait_col    = "body",
    fitness_type = "continuous"
)

p1 <- plot_univariate_fitness(
    uni       = spline_body_binary,
    trait_col = "body",
    title     = "Body Depth — Survival Probability"
) + 
ggplot2::labs(subtitle = NULL) + 
ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin      = ggplot2::margin(20, 20, 20, 20)
)

p2 <- plot_univariate_fitness(
    uni       = spline_body_continuous,
    trait_col = "body",
    title     = "Body Depth — Growth Rate"
) +
ggplot2::labs(subtitle = NULL) + 
ggplot2::theme(
    plot.background  = ggplot2::element_rect(fill = "white", color = NA),
    plot.margin      = ggplot2::margin(20, 20, 20, 20)
)
ggsave(file.path(figure_dir, "univariate_spline_body_continuous.png"),
       p2, width = 7, height = 5, dpi = 300, bg = "white")

ggsave(file.path(figure_dir, "univariate_spline_body_binary.png"),
       p1, width = 7, height = 5, dpi = 300, bg = "white")


# Binary - By lake
spline_body_binary_cp <- univariate_spline(
    data         = prepared_binary_group %>% dplyr::filter(lake == "Crescent Pond"),
    fitness_col  = FITNESS_BINARY,
    trait_col    = "body",
    fitness_type = "binary"
)

spline_body_binary_ll <- univariate_spline(
    data         = prepared_binary_group %>% dplyr::filter(lake == "Little Lake"),
    fitness_col  = FITNESS_BINARY,
    trait_col    = "body",
    fitness_type = "binary"
)

# Continuous - By lake
spline_body_cont_cp <- univariate_spline(
    data         = prepared_continuous_group %>% dplyr::filter(lake == "Crescent Pond"),
    fitness_col  = FITNESS_CONTINUOUS,
    trait_col    = "body",
    fitness_type = "continuous"
)

spline_body_cont_ll <- univariate_spline(
    data         = prepared_continuous_group %>% dplyr::filter(lake == "Little Lake"),
    fitness_col  = FITNESS_CONTINUOUS,
    trait_col    = "body",
    fitness_type = "continuous"
)

# Plots
p_bin_cp <- plot_univariate_fitness(spline_body_binary_cp, "body",
    title = "Body Depth — Survival (Crescent Pond)") +
    ggplot2::labs(subtitle = NULL) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))

p_bin_ll <- plot_univariate_fitness(spline_body_binary_ll, "body",
    title = "Body Depth — Survival (Little Lake)") +
    ggplot2::labs(subtitle = NULL) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))

p_cont_cp <- plot_univariate_fitness(spline_body_cont_cp, "body",
    title = "Body Depth — Growth Rate (Crescent Pond)") +
    ggplot2::labs(subtitle = NULL) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))

ggsave(file.path(figure_dir, "spline_body_continuous_CrescentPond.png"),
       p_cont_cp, width = 7, height = 5, dpi = 300, bg = "white")

p_cont_ll <- plot_univariate_fitness(spline_body_cont_ll, "body",
    title = "Body Depth — Growth Rate (Little Lake)") +
    ggplot2::labs(subtitle = NULL) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))

# Save
ggsave(file.path(figure_dir, "spline_body_binary_CrescentPond.png"),
       p_bin_cp, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(file.path(figure_dir, "spline_body_binary_LittleLake.png"),
       p_bin_ll, width = 7, height = 5, dpi = 300, bg = "white")

ggsave(file.path(figure_dir, "spline_body_continuous_LittleLake.png"),
       p_cont_ll, width = 7, height = 5, dpi = 300, bg = "white")


sel_diff_all[["body"]]$continuous$grouped

prepared_continuous_group %>%
    dplyr::filter(lake == "Crescent Pond") %>%
    dplyr::select(body, ln.growth, relative_fitness) %>%
    summary()

prepared_continuous_group %>%
    dplyr::filter(lake == "Crescent Pond") %>%
    ggplot2::ggplot(ggplot2::aes(x = body, y = relative_fitness)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth() +
    ggplot2::labs(title = "Crescent Pond: body vs relative fitness")


# ======================================================
# 13 Correlated Fitness Surfaces
# ======================================================
cat("\n=== Correlated Fitness Surfaces ===\n")

# Only the 4 most biologically meaningful pairs
focal_pairs <- list(
    c("body", "eye"),        # strongest selection signals + negative correlational selection
    c("jaw",  "body"),       # jaw stabilizing + body directional
    c("nasal", "noseangle"), # nasal functional module, negative correlational selection
    c("jaw",  "nasal")       # key pair from Martin & Wainwright (2013) Figure 5/6
)

for (fitness_label in c("binary", "continuous")) {
    fitness_col   <- if (fitness_label == "binary") FITNESS_BINARY else FITNESS_CONTINUOUS
    prepared_data <- if (fitness_label == "binary") prepared_binary else prepared_continuous
    prepared_grp  <- if (fitness_label == "binary") prepared_binary_group else prepared_continuous_group

    cat("\n---", tools::toTitleCase(fitness_label), "(Overall) ---\n")

    for (pair in focal_pairs) {
        pair_name <- paste(pair, collapse = "_")
        cat("  Fitting:", pair_name, "\n")

        cfs <- suppressMessages(correlated_fitness_surface(
            data        = prepared_data,
            fitness_col = fitness_col,
            trait_cols  = pair,
            grid_n      = 50,
            method      = "auto",
            scale_traits = FALSE,
            group       = NULL
        ))

        saveRDS(cfs,
            file.path(model_dir, paste0("cfs_", fitness_label, "_", pair_name, ".rds"))
        )

        p <- plot_correlated_fitness(tps = cfs, trait_cols = pair)
        ggplot2::ggsave(
            file.path(figure_dir, paste0("cfs_", fitness_label, "_", pair_name, ".png")),
            p, width = 8, height = 6, dpi = 300, bg = "white"
        )

        p_enhanced <- plot_correlated_fitness_enhanced(
            tps           = cfs,
            trait_cols    = pair,
            original_data = prepared_data,
            fitness_col   = fitness_col,
            bins          = 12
        )
        ggplot2::ggsave(
            file.path(figure_dir, paste0("cfs_", fitness_label, "_", pair_name, "_enhanced.png")),
            p_enhanced, width = 8, height = 6, dpi = 300, bg = "white"
        )
    }

    cat("\n---", tools::toTitleCase(fitness_label), "(By Lake) ---\n")

    for (pair in focal_pairs) {
        pair_name <- paste(pair, collapse = "_")
        cat("  Fitting:", pair_name, "\n")

        cfs_list <- suppressMessages(correlated_fitness_surface(
            data        = prepared_grp,
            fitness_col = fitness_col,
            trait_cols  = pair,
            grid_n      = 50,
            method      = "auto",
            scale_traits = FALSE,
            group       = GROUP,
            k           = 20
        ))

        for (lake in names(cfs_list)) {
            lake_label <- gsub(" ", "_", lake)

            saveRDS(cfs_list[[lake]],
                file.path(model_dir, paste0("cfs_", fitness_label, "_", pair_name, "_", lake_label, ".rds"))
            )

            p <- plot_correlated_fitness(tps = cfs_list[[lake]], trait_cols = pair)
            ggplot2::ggsave(
                file.path(figure_dir, paste0("cfs_", fitness_label, "_", pair_name, "_", lake_label, ".png")),
                p, width = 8, height = 6, dpi = 300, bg = "white"
            )

            p_enhanced <- plot_correlated_fitness_enhanced(
                tps           = cfs_list[[lake]],
                trait_cols    = pair,
                original_data = prepared_grp %>% dplyr::filter(lake == !!lake),
                fitness_col   = fitness_col,
                bins          = 12
            )
            ggplot2::ggsave(
                file.path(figure_dir, paste0("cfs_", fitness_label, "_", pair_name, "_", lake_label, "_enhanced.png")),
                p_enhanced, width = 8, height = 6, dpi = 300, bg = "white"
            )
        }
    }
}

# ======================================================
# 14 Adaptive Landscape
# ======================================================

cat("\n=== Adaptive Landscape Analysis ===\n")

sites <- unique(prepared_binary_group[[GROUP]])
site_info <- data.frame(
    name = sites,
    label = gsub(" ", "_", sites),
    stringsAsFactors = FALSE
)

trait_pairs <- combn(TRAITS, 2, simplify = FALSE)

for (fitness_label in c("binary", "continuous")) {
    fitness_col <- if (fitness_label == "binary") FITNESS_BINARY else FITNESS_CONTINUOUS
    prepared_data <- if (fitness_label == "binary") prepared_binary else prepared_continuous
    prepared_grp <- if (fitness_label == "binary") prepared_binary_group else prepared_continuous_group
    gam_family <- if (fitness_label == "binary") binomial else gaussian

    # Overall
    cat("\n--- ", tools::toTitleCase(fitness_label), "(Overall) ---\n")
    for (pair in trait_pairs) {
        pair_name <- paste(pair, collapse = "_")
        formula_str <- paste(fitness_col, "~ s(", pair[1], ",", pair[2], ")")

        fitness_model <- mgcv::gam(
            as.formula(formula_str),
            family = gam_family,
            data   = prepared_data
        )

        landscape <- adaptive_landscape(
            data          = prepared_data,
            fitness_model = fitness_model,
            trait_cols    = pair,
            simulation_n  = 500,
            grid_n        = 50 # matches CFS grid_n
        )
        saveRDS(
            landscape,
            file.path(model_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_overall.rds"))
        )

        if (exists("plot_adaptive_landscape")) {
            p2d <- plot_adaptive_landscape(
                landscape     = landscape,
                trait_cols    = pair,
                original_data = prepared_data[, c(fitness_col, pair)],
                bins          = 12
            )
            ggplot2::ggsave(
                file.path(figure_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_overall_2d.png")),
                p2d,
                width = 8, height = 6, dpi = 300
            )
        }

        if (exists("plot_adaptive_landscape_3d")) {
            png(file.path(figure_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_overall_3d.png")),
                width = 8, height = 6, units = "in", res = 300
            )
            plot_adaptive_landscape_3d(
                landscape = landscape, trait_cols = pair,
                theta = -30, phi = 30, grid_n = 200
            )
            dev.off()
        }
    }

    # By site
    cat("\n--- ", tools::toTitleCase(fitness_label), "(By Site) ---\n")
    for (pair in trait_pairs) {
        pair_name <- paste(pair, collapse = "_")

        for (i in seq_len(nrow(site_info))) {
            site <- site_info$name[i]
            site_label <- site_info$label[i]
            cat("  Processing:", site, "\n")

            site_data <- prepared_grp[prepared_grp[[GROUP]] == site, ]
            formula_str <- paste(fitness_col, "~ s(", pair[1], ",", pair[2], ")")
            fitness_model <- mgcv::gam(
                as.formula(formula_str),
                family = gam_family,
                data   = site_data
            )

            landscape <- adaptive_landscape(
                data          = site_data,
                fitness_model = fitness_model,
                trait_cols    = pair,
                simulation_n  = 500,
                grid_n        = 50
            )
            saveRDS(
                landscape,
                file.path(model_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_", site_label, ".rds"))
            )

            if (exists("plot_adaptive_landscape")) {
                p2d <- plot_adaptive_landscape(
                    landscape     = landscape,
                    trait_cols    = pair,
                    original_data = site_data[, c(fitness_col, pair)],
                    bins          = 12
                )
                ggplot2::ggsave(
                    file.path(figure_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_", site_label, "_2d.png")),
                    p2d,
                    width = 8, height = 6, dpi = 300
                )
            }

            if (exists("plot_adaptive_landscape_3d")) {
                png(file.path(figure_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_", site_label, "_3d.png")),
                    width = 8, height = 6, units = "in", res = 300
                )
                plot_adaptive_landscape_3d(
                    landscape = landscape, trait_cols = pair,
                    theta = -30, phi = 30, grid_n = 200
                )
                dev.off()
            }
        }
    }
}

# Adaptive landscape summary table
summary_landscape <- data.frame()

for (fitness_label in c("binary", "continuous")) {
    for (pair in trait_pairs) {
        pair_name <- paste(pair, collapse = "_")

        # Overall
        file_path <- file.path(model_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_overall.rds"))
        if (file.exists(file_path)) {
            landscape <- readRDS(file_path)
            summary_landscape <- rbind(summary_landscape, data.frame(
                Trait1 = pair[1],
                Trait2 = pair[2],
                Fitness = tools::toTitleCase(fitness_label),
                Site = "Overall",
                Optimum_Trait1 = round(landscape$optimum[[pair[1]]], 4),
                Optimum_Trait2 = round(landscape$optimum[[pair[2]]], 4),
                Max_Fitness = round(landscape$optimum$.mean_fit, 4),
                stringsAsFactors = FALSE
            ))
        }

        # By site
        for (i in seq_len(nrow(site_info))) {
            site_label <- site_info$label[i]
            file_path <- file.path(model_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_", site_label, ".rds"))
            if (file.exists(file_path)) {
                landscape <- readRDS(file_path)
                summary_landscape <- rbind(summary_landscape, data.frame(
                    Trait1 = pair[1],
                    Trait2 = pair[2],
                    Fitness = tools::toTitleCase(fitness_label),
                    Site = site_info$name[i],
                    Optimum_Trait1 = round(landscape$optimum[[pair[1]]], 4),
                    Optimum_Trait2 = round(landscape$optimum[[pair[2]]], 4),
                    Max_Fitness = round(landscape$optimum$.mean_fit, 4),
                    stringsAsFactors = FALSE
                ))
            }
        }
    }
}

write.csv(summary_landscape,
    file.path(table_dir, "adaptive_landscape_summary.csv"),
    row.names = FALSE
)

# ======================================================
# 15 Compare Fitness Surfaces
# ======================================================

cat("\n=== Compare Fitness Surfaces ===\n")

trait_pairs <- combn(TRAITS, 2, simplify = FALSE)
comparison_results <- list()

for (fitness_label in c("binary", "continuous")) {
    cat("\n--- ", tools::toTitleCase(fitness_label), "---\n")

    for (pair in trait_pairs) {
        pair_name <- paste(pair, collapse = "_")

        # Overall
        cfs_file <- file.path(model_dir, paste0("cfs_", fitness_label, "_", pair_name, ".rds"))
        al_file <- file.path(model_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_overall.rds"))

        if (!file.exists(cfs_file) || !file.exists(al_file)) {
            cat("  Skipping", pair_name, "Overall - missing files\n")
        } else {
            cat("  Comparing:", pair_name, "(Overall)\n")
            cfs_overall <- readRDS(cfs_file)
            al_overall <- readRDS(al_file)

            comparison_overall <- compare_fitness_surfaces_data(
                correlated_surface    = cfs_overall,
                adaptive_landscape    = al_overall,
                trait_cols            = pair,
                calculate_correlation = TRUE
            )

            plots_overall <- plot_fitness_surfaces_comparison(
                comparison_data = comparison_overall,
                bins            = 10,
                title           = paste("Fitness Surfaces -", pair_name, "(Overall)")
            )

            if (!is.null(plots_overall$side_by_side)) {
                ggplot2::ggsave(
                    file.path(figure_dir, paste0("comparison_", fitness_label, "_", pair_name, "_overall_side_by_side.png")),
                    plots_overall$side_by_side,
                    width = 12, height = 5, dpi = 300
                )
            }
            if (!is.null(plots_overall$overlay)) {
                ggplot2::ggsave(
                    file.path(figure_dir, paste0("comparison_", fitness_label, "_", pair_name, "_overall_overlay.png")),
                    plots_overall$overlay,
                    width = 8, height = 6, dpi = 300
                )
            }

            write.csv(
                data.frame(
                    trait1            = pair[1],
                    trait2            = pair[2],
                    fitness           = fitness_label,
                    site              = "Overall",
                    correlation       = comparison_overall$correlation,
                    distance_optima   = comparison_overall$distance_between_optima,
                    n_points          = nrow(cfs_overall$grid)
                ),
                file.path(table_dir, paste0("comparison_", fitness_label, "_", pair_name, "_overall_summary.csv")),
                row.names = FALSE
            )

            comparison_results[[paste0(fitness_label, "_", pair_name, "_overall")]] <- list(
                correlation     = comparison_overall$correlation,
                distance_optima = comparison_overall$distance_between_optima,
                site            = "Overall"
            )
        }

        # By site
        for (i in seq_len(nrow(site_info))) {
            site <- site_info$name[i]
            site_label <- site_info$label[i]

            cfs_site_file <- file.path(model_dir, paste0("cfs_", fitness_label, "_", pair_name, "_", site_label, ".rds"))
            al_site_file <- file.path(model_dir, paste0("adaptive_landscape_", fitness_label, "_", pair_name, "_", site_label, ".rds"))

            if (!file.exists(cfs_site_file) || !file.exists(al_site_file)) {
                cat("    Skipping", site, "- missing files\n")
                next
            }

            cat("    Comparing:", pair_name, "-", site, "\n")
            cfs_site <- readRDS(cfs_site_file)
            al_site <- readRDS(al_site_file)

            comparison_site <- compare_fitness_surfaces_data(
                correlated_surface    = cfs_site,
                adaptive_landscape    = al_site,
                trait_cols            = pair,
                calculate_correlation = TRUE
            )

            plots_site <- plot_fitness_surfaces_comparison(
                comparison_data = comparison_site,
                bins            = 10,
                title           = paste("Fitness Surfaces -", pair_name, "-", site)
            )

            if (!is.null(plots_site$side_by_side)) {
                ggplot2::ggsave(
                    file.path(figure_dir, paste0("comparison_", fitness_label, "_", pair_name, "_", site_label, "_side_by_side.png")),
                    plots_site$side_by_side,
                    width = 12, height = 5, dpi = 300
                )
            }
            if (!is.null(plots_site$overlay)) {
                ggplot2::ggsave(
                    file.path(figure_dir, paste0("comparison_", fitness_label, "_", pair_name, "_", site_label, "_overlay.png")),
                    plots_site$overlay,
                    width = 8, height = 6, dpi = 300
                )
            }

            write.csv(
                data.frame(
                    trait1          = pair[1],
                    trait2          = pair[2],
                    fitness         = fitness_label,
                    site            = site,
                    correlation     = comparison_site$correlation,
                    distance_optima = comparison_site$distance_between_optima,
                    n_points        = nrow(cfs_site$grid)
                ),
                file.path(table_dir, paste0("comparison_", fitness_label, "_", pair_name, "_", site_label, "_summary.csv")),
                row.names = FALSE
            )

            comparison_results[[paste0(fitness_label, "_", pair_name, "_", site_label)]] <- list(
                correlation     = comparison_site$correlation,
                distance_optima = comparison_site$distance_between_optima,
                site            = site
            )
        }
    }
}

# Comparison summary table
comparison_summary <- data.frame()

for (name in names(comparison_results)) {
    res <- comparison_results[[name]]
    comparison_summary <- rbind(comparison_summary, data.frame(
        Comparison = name,
        Correlation = round(res$correlation %||% NA, 4),
        Distance_Optima = round(res$distance_optima, 4),
        Site = res$site,
        stringsAsFactors = FALSE
    ))
}

write.csv(comparison_summary,
    file.path(table_dir, "fitness_surfaces_comparison_summary.csv"),
    row.names = FALSE
)

cat("\n========================================\n")
cat("PUPFISH TEST COMPLETE\n")
cat("========================================\n")
cat("Results saved to:", output_dir, "\n")
