## 1. SET WORKING DIRECTORY AND LOAD DATA -------------------------------------

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

# ------------------------------------------------------
# 1 Initialize environment
# ------------------------------------------------------
if (file.exists(here("R", "scripts", "0.0_initialize.R"))) {
  source(here("R", "scripts", "0.0_initialize.R"))
}

# ======================================================
# Define Output Directories
# ======================================================

output_dir <- here("R", "results", "test_birds")
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
model_dir <- file.path(output_dir, "models")

dirs <- c(output_dir, figure_dir, table_dir, model_dir)

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("Results saved to:", normalizePath(output_dir), "\n")

data_path <- here("R", "data", "bird_data.csv")

if (file.exists(data_path)) {
  bird_study <- read.csv(data_path, stringsAsFactors = FALSE)
  cat("bird_data.csv loaded from", data_path, "\n")
} else {
  # Try fallback location if R/test_data doesn't exist
  data_path_fallback <- here("test_data", "bird_data.csv")
  if (file.exists(data_path_fallback)) {
    bird_study <- read.csv(data_path_fallback, stringsAsFactors = FALSE)
    cat("bird_data.csv loaded from", data_path_fallback, "\n")
  } else {
    stop("bird_data.csv not found at ", data_path, " or ", data_path_fallback)
  }
}

# Remove duplicate X.y.* columns
bird_study <- bird_study[, !grepl("^X\\.y\\.", names(bird_study))]

cat("Data dimensions:", dim(bird_study), "\n")
cat("Column names:\n")
print(names(bird_study))

## 1A. BUILD A MORE "PAPER-LIKE" BEAK AXIS (Beak_PC1) -------------------------

beak_cols <- c("MedianBeakLength", "MedianBeakWidth", "MedianBeakDepth")
if (all(beak_cols %in% names(bird_study))) {
  cat("\nFound raw beak traits, building Beak_PC1 via PCA\n")
  
  beak_complete <- bird_study[, beak_cols]
  ok <- complete.cases(beak_complete)
  
  if (sum(ok) > 10) {
    pca_fit <- prcomp(beak_complete[ok, ], scale. = TRUE)
    pc1_scores <- rep(NA_real_, nrow(bird_study))
    pc1_scores[ok] <- pca_fit$x[, 1]
    
    # Ensure Beak_PC1 is positively correlated with depth: a larger value = a larger beak.
    cor_depth <- suppressWarnings(cor(pc1_scores[ok], beak_complete[ok, "MedianBeakDepth"]))
    if (!is.na(cor_depth) && cor_depth < 0) {
      pc1_scores <- -pc1_scores
    }
    
    bird_study$Beak_PC1 <- pc1_scores
    cat("Added Beak_PC1 (PCA on length/width/depth)\n")
  } else {
    cat("Not enough complete cases for beak PCA, skipping Beak_PC1\n")
  }
} else {
  cat("\nSome raw beak columns missing, will use existing PC1 if available\n")
}

## 2. LOAD REQUIRED PACKAGES ---------------------------------------------------

required_packages <- c("ggplot2", "dplyr", "tidyr", "mgcv", "fields", "purrr", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
  cat("pass", pkg, "\n")
}

## 3. LOAD ALL FUNCTIONS -------------------------------------------------------

# ------------------------------------------------------
# 3 Load function files and plotting files
# ------------------------------------------------------
cat("\nLoading function files and plotting files...\n")

fn_files <- list.files(
  here("R", "functions"),
  pattern = "\\.R$",
  full.names = TRUE
)

for (f in fn_files) {
  source(f)
  cat("Loaded:", basename(f), "\n")
}

script_files <- list.files(
  here("R", "scripts"),
  pattern = "\\.R$",
  full.names = TRUE
)
# Exclude 0.0_initialize.R since it is sourced above
script_files <- script_files[basename(script_files) != "0.0_initialize.R"]

for (f in script_files) {
  source(f)
  cat("Loaded script:", basename(f), "\n")
}

plot_files <- list.files(
  here("R", "plotting"),
  pattern = "\\.R$",
  full.names = TRUE
)

for (f in plot_files) {
  source(f)
  cat("Loaded plot:", basename(f), "\n")
}

## 4. DATA PREPARATION FUNCTION -----------------------------------------------

year_cols <- names(bird_study)[grepl("y\\.", names(bird_study))]
if (length(year_cols) > 0) {
  cat("Found year columns (first few):", paste(head(year_cols), collapse = ", "), "\n")
} else {
  cat("No y.[year] columns found; survival definition may fail\n")
}

# according to paper: survival：year X capture & X+1...last_year if captured in any year, fitness = 1
prepare_basic_survival_data <- function(year, last_year = 2011) {
  current_col <- paste0("y.", year)
  later_cols  <- paste0("y.", (year + 1):last_year)
  
  needed_cols <- c(current_col, later_cols)
  missing_cols <- needed_cols[!needed_cols %in% names(bird_study)]
  if (length(missing_cols) > 0) {
    warning("Missing columns for years: ", paste(missing_cols, collapse = ", "))
    return(NULL)
  }
  
  survival_data <- bird_study %>%
    dplyr::filter(.data[[current_col]] == 1) %>%
    dplyr::mutate(
      fitness = as.numeric(rowSums(dplyr::across(dplyr::all_of(later_cols)),
                                   na.rm = TRUE) > 0),
      year = year
    )
  
  # Beak_PC1 > PC1 > raw beak
  trait_candidates <- c("Beak_PC1", "PC1", "PC.body1",
                        "MedianBeakLength", "MedianBeakWidth", "MedianBeakDepth")
  available_traits <- trait_candidates[trait_candidates %in% names(survival_data)]
  
  id_col <- if ("BANDFINAL" %in% names(survival_data)) "BANDFINAL" else NULL
  
  survival_data <- survival_data %>%
    dplyr::select(dplyr::all_of(c("year", "fitness", available_traits, id_col)))
  
  return(survival_data)
}


## 5. CHECK DATA AVAILABILITY ACROSS YEARS ------------------------------------

test_years <- 2003:2011
available_years <- c()

for (year in test_years) {
  test_data <- prepare_basic_survival_data(year)
  if (!is.null(test_data) && nrow(test_data) >= 20) {
    available_years <- c(available_years, year)
    cat("Year", year, ": n =", nrow(test_data),
        ", survival =", round(mean(test_data$fitness), 3), "\n")
  }
}

if (length(available_years) == 0) {
  stop("No years with sufficient data found.")
}

# choose the year with the largest sample size as the main test year.
best_year <- available_years[which.max(sapply(available_years, function(yr) {
  data <- prepare_basic_survival_data(yr)
  if (!is.null(data)) nrow(data) else 0
}))]

cat("Using year", best_year, "for detailed single-year testing\n")

## 6. SINGLE-YEAR COMPREHENSIVE TEST ------------------------------------------

main_data <- prepare_basic_survival_data(best_year)
cat("Sample size:", nrow(main_data), "individuals\n")
cat("Survival rate:", round(mean(main_data$fitness), 3), "\n")

numeric_traits <- names(main_data)[sapply(main_data, is.numeric)]
cat("Numeric traits:", paste(numeric_traits, collapse = ", "), "\n")

# select the beak axis: Prioritize Beak_PC1, then PC1
beak_trait <- if ("Beak_PC1" %in% names(main_data)) {
  "Beak_PC1"
} else if ("PC1" %in% names(main_data)) {
  "PC1"
} else {
  stop("No Beak_PC1 or PC1 found in main_data.")
}
cat("Using beak trait:", beak_trait, "\n")

# prepare standardized data, trait combination: beak + body size (if present)
traits_for_model <- c(beak_trait, "PC.body1")
traits_for_model <- traits_for_model[traits_for_model %in% names(main_data)]

main_data_prepared <- prepare_selection_data(
  data = main_data,
  fitness_col = "fitness",
  trait_cols = traits_for_model,
  standardize = TRUE,
  add_relative = TRUE,
  na_action = "warn"
)

## 6.1 SINGLE-TRAIT SELECTION ANALYSIS ----------------------------------------

# Helper for single-trait fallback (since selection_coefficients requires >=2 traits)
run_manual_single_trait <- function(data, fitness_col, trait_col) {
  df <- data
  # Simple standardization
  df[[trait_col]] <- as.numeric(scale(df[[trait_col]]))
  
  # Fit GLM with quadratic term
  form <- as.formula(paste(fitness_col, "~", trait_col, "+ I(", trait_col, "^2)"))
  m <- glm(form, data = df, family = binomial)
  
  coefs <- summary(m)$coefficients
  # Return formatted like selection_coefficients
  data.frame(
    Term = c(trait_col, paste0(trait_col, "^2")),
    Type = c("Linear", "Quadratic"),
    Beta_Coefficient = coefs[2:3, 1],
    Standard_Error = coefs[2:3, 2],
    P_Value = coefs[2:3, 4]
  )
}

cat("Testing", beak_trait, "(single trait)...\n")

single_beak <- NULL
tryCatch({
  single_beak <- selection_coefficients(
    data = main_data,
    fitness_col = "fitness",
    trait_cols = beak_trait,
    fitness_type = "binary",
    standardize = TRUE
  )
  cat("Single-trait", beak_trait, "analysis successful\n")
  print(single_beak)
}, error = function(e) {
  cat("Single-trait analysis failed:", e$message, "\n")
  if (grepl("requires at least 2 traits", e$message)) {
    cat("-> Attempting manual GLM fallback for single trait...\n")
    single_beak <<- run_manual_single_trait(main_data, "fitness", beak_trait)
    print(single_beak)
  }
})

## 6.2 MULTI-TRAIT SELECTION ANALYSIS -----------------------------------------

multi_trait <- NULL
if (all(c(beak_trait, "PC.body1") %in% names(main_data))) {
  tryCatch({
    multi_trait <- selection_coefficients(
      data = main_data,
      fitness_col = "fitness",
      trait_cols = c(beak_trait, "PC.body1"),
      fitness_type = "binary",
      standardize = TRUE
    )
    cat("Multi-trait analysis successful\n")
    print(multi_trait)
  }, error = function(e) {
    cat("Multi-trait analysis failed:", e$message, "\n")
  })
} else {
  cat("PC.body1 not available; skipping multi-trait test\n")
}

## 6.3 VISUALIZATION FUNCTIONS -------------------------------------------------

cat("Testing univariate_spline and plot_univariate_fitness...\n")
traits_for_vis <- intersect(c(beak_trait, "PC.body1"), names(main_data_prepared))

for (trait in traits_for_vis) {
  cat("Plotting", trait, "...")
  tryCatch({
    spline_result <- univariate_spline(
      data = main_data_prepared,
      fitness_col = "fitness",
      trait_col = trait,
      fitness_type = "binary",
      k = 8
    )
    
    surface_plot <- plot_univariate_fitness(
      uni = spline_result,
      trait_col = trait,
      title = paste("Fitness Surface -", trait, "- Year:", best_year)
    )
    
    print(surface_plot)
    cat(" ✓\n")
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}


if (length(traits_for_model) >= 2) {
  cat("Testing correlated_fitness_surface and plot_correlated_fitness...\n")
  tryCatch({
    tps_result <- correlated_fitness_surface(
      data = main_data_prepared,
      fitness_col = "fitness",
      trait_cols = traits_for_model[1:2],
      grid_n = 35,
      method = "auto"
    )
    
    corr_plot <- plot_correlated_fitness(
      tps = tps_result,
      trait_cols = traits_for_model[1:2]
    ) +
      labs(
        title = "Trait Correlation Surface",
        subtitle = paste("Year:", best_year),
        x = traits_for_model[1],
        y = traits_for_model[2],
        fill = "Relative Fitness"
      ) +
      theme_minimal()
    
    print(corr_plot)
    cat("Correlation surface successful\n")
  }, error = function(e) {
    cat("Correlation surface failed:", e$message, "\n")
  })
  
  # auto / gam / tps
  cat("Testing different correlation methods...\n")
  methods_to_test <- c("auto", "gam", "tps")
  
  for (method in methods_to_test) {
    cat("Method:", method, "...")
    tryCatch({
      method_tps <- correlated_fitness_surface(
        data = main_data_prepared,
        fitness_col = "fitness",
        trait_cols = traits_for_model[1:2],
        grid_n = 25,
        method = method
      )
      
      method_plot <- plot_correlated_fitness(
        tps = method_tps,
        trait_cols = traits_for_model[1:2]
      ) +
        labs(
          title = paste("Correlation Surface -", toupper(method)),
          subtitle = paste("Data type:", method_tps$data_type)
        )
      
      print(method_plot)
      cat(" ✓\n")
    }, error = function(e) {
      cat("Error:", e$message, "\n")
    })
  }
} else {
  cat("Less than two traits; skipping correlation surfaces\n")
}

## 6.4 UTILITY FUNCTIONS -------------------------------------------------------

cat("Testing selection_differential for", beak_trait, "...\n")

tryCatch({
  sel_diff <- selection_differential(
    data = main_data_prepared,
    fitness_col = "fitness",
    trait_col = beak_trait,
    assume_standardized = TRUE,
    use_relative = TRUE
  )
  cat("Selection differential for", beak_trait, ":", round(sel_diff, 4), "\n")
}, error = function(e) {
  cat("Selection differential failed:", e$message, "\n")
})

# detect_family

tryCatch({
  family_result <- detect_family(main_data_prepared$fitness)
  cat("Fitness family detected:", family_result$type, "\n")
}, error = function(e) {
  cat("Detect family failed:", e$message, "\n")
})

# bootstrap_selection
cat("Testing bootstrap_selection on", beak_trait, "...\n")

bootstrap_result <- NULL
tryCatch({
  bootstrap_result <- bootstrap_selection(
    data = main_data_prepared,
    fitness_col = "fitness",
    trait_cols = beak_trait,
    fitness_type = "binary",
    B = 50,
    seed = 42
  )
  cat("Bootstrap completed\n")
  if (!is.null(bootstrap_result) && is.numeric(bootstrap_result$ci$lower)) {
    cat("CI:", round(bootstrap_result$ci$lower[1], 3), "to",
        round(bootstrap_result$ci$upper[1], 3), "\n")
  } else {
    cat("Bootstrap CI not available (likely due to single-trait limitation).\n")
  }
}, error = function(e) {
  cat("Bootstrap failed:", e$message, "\n")
})

## 7. MULTI-YEAR PAPER-STYLE ANALYSIS -----------------------------------------

yearly_results <- list()

cat("Analyzing quadratic selection across available years...\n")

for (year in available_years) {
  cat("Year", year, "...")
  year_data <- prepare_basic_survival_data(year)
  
  if (var(year_data$fitness, na.rm = TRUE) == 0) {
    cat("Skipping (No variation in fitness)\n")
    next
  }
  
  if (!is.null(year_data) && nrow(year_data) >= 20) {
    
    beak_trait_year <- if ("Beak_PC1" %in% names(year_data)) "Beak_PC1" else "PC1"
    if (!beak_trait_year %in% names(year_data)) {
      cat("No beak trait; skipped\n")
      next
    }
    
    year_result <- NULL
    tryCatch({
      year_result <- selection_coefficients(
        data = year_data,
        fitness_col = "fitness",
        trait_cols = beak_trait_year,
        fitness_type = "binary",
        standardize = TRUE 
      )
    }, error = function(e) {
      if (grepl("requires at least 2 traits", e$message)) {
        year_result <<- run_manual_single_trait(year_data, "fitness", beak_trait_year)
      }
    })

    if (!is.null(year_result)) {
      
      quad_coef <- year_result %>%
        dplyr::filter(Type == "Quadratic" & grepl(beak_trait_year, Term))
      
      if (nrow(quad_coef) > 0 && !is.na(quad_coef$Beta_Coefficient)) {
        quad_beta <- quad_coef$Beta_Coefficient
        
        result_summary <- data.frame(
          Year = year,
          Sample_Size = nrow(year_data),
          Survival_Rate = round(mean(year_data$fitness), 3),
          Quadratic_Coefficient = quad_beta,      # β2
          Gamma_Approx = 2 * quad_beta,           # L&A γ 
          P_Value = quad_coef$P_Value,
          Significant = quad_coef$P_Value < 0.05
        )
        yearly_results[[as.character(year)]] <- result_summary
        cat("quadratic coef =", round(quad_beta, 3), "\n")
      } else {
        cat("No quadratic coefficient\n")
      }
    }
  } else {
    cat("Insufficient data\n")
  }
}

if (length(yearly_results) > 0) {
  yearly_summary <- dplyr::bind_rows(yearly_results)
  cat("\nMULTI-YEAR QUADRATIC SELECTION RESULTS:\n")
  print(yearly_summary)
  
  if (nrow(yearly_summary) > 1) {
    temporal_plot <- ggplot(yearly_summary,
                            aes(x = Year, y = Quadratic_Coefficient, color = Significant)) +
      geom_point(aes(size = Sample_Size)) +
      geom_line(alpha = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      labs(
        title = "Temporal Variation in Disruptive Selection",
        subtitle = paste("Quadratic Selection on Beak Axis (", beak_trait, ") Across Years", sep = ""),
        y = "Quadratic Selection Coefficient (single-trait beta2)",
        color = "Statistically\nSignificant",
        size = "Sample Size"
      ) +
      scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
      theme_minimal()
    
    print(temporal_plot)
    cat("Temporal variation plot displayed\n")
  }
}

## 8. SAVE RESULTS -------------------------------------------------------------

analysis_results <- list(
  analysis_info = list(
    analysis_date = Sys.time(),
    test_year = best_year,
    sample_size = nrow(main_data),
    survival_rate = mean(main_data$fitness),
    available_years = available_years,
    beak_trait = beak_trait
  ),
  single_trait_results = single_beak,
  multi_trait_results = multi_trait,
  multi_year_results = if (exists("yearly_summary")) yearly_summary else NULL,
  bootstrap_results = bootstrap_result
)

saveRDS(analysis_results,
        file.path(output_dir, "comprehensive_analysis_results.rds"))
cat("Results saved to:",
    file.path(output_dir, "comprehensive_analysis_results.rds"), "\n")

save.image(file.path(output_dir, "analysis_workspace.RData"))
cat("Workspace saved\n")

## 9. FINAL SUMMARY ------------------------------------------------------------

cat("KEY FINDINGS:\n")
if (exists("yearly_summary") && nrow(yearly_summary) > 0) {
  sig_years <- yearly_summary %>% dplyr::filter(Significant == TRUE)
  if (nrow(sig_years) > 0) {
    cat("Significant disruptive/stabilizing selection in",
        nrow(sig_years), "year(s)\n")
    cat("Years with significant quadratic coefficient:",
        paste(sig_years$Year, collapse = ", "), "\n")
  } else {
    cat("No statistically significant quadratic selection detected\n")
  }
}

cat("Results saved in:", output_dir, "\n")
