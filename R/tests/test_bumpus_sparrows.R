# ======================================================
# test_bumpus_sparrows.R
# Integration test using Bumpus sparrow dataset
# ======================================================

cat("\n========================================\n")
cat("BUMPUS SPARROW FUNCTION TEST\n")
cat("========================================\n")

if (dir.exists("../../R/scripts")) {
  setwd("../..")
}
cat("Working directory:", getwd(), "\n")

# ------------------------------------------------------
# 1 Initialize environment
# ------------------------------------------------------

if (file.exists("R/scripts/0.0_initialize.R")) {
  source("R/scripts/0.0_initialize.R")
}

# ------------------------------------------------------
# 2 Output directories
# ------------------------------------------------------

output_dir <- file.path("R", "results", "bumpus_results")

table_dir <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")
model_dir <- file.path(output_dir, "models")

dirs <- c(output_dir, table_dir, figure_dir, model_dir)

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("Results will be saved to:", normalizePath(output_dir), "\n")

# ------------------------------------------------------
# 3 Load function files and plotting files
# ------------------------------------------------------

cat("\nLoading function files and plotting files...\n")

fn_files <- list.files(
  "R/functions",
  pattern = "\\.R$",
  full.names = TRUE
)

for (f in fn_files) {
  source(f)
  cat("Loaded:", basename(f), "\n")
}

# Explicitly load scripts from R/scripts
scripts_to_load <- c(
  "R/scripts/1_prepare_selection_data.R",
  "R/scripts/2_linear_selection_analysis.R",
  "R/scripts/3_nonlinear_selection_analysis.R"
)

for (script in scripts_to_load) {
  if (file.exists(script)) {
    source(script)
    cat("Loaded:", basename(script), "\n")
  }
}

plot_files <- list.files(
  "R/plotting",
  pattern = "\\.R$",
  full.names = TRUE
)

for (f in plot_files) {
  source(f)
  cat("Loaded plot:", basename(f), "\n")
}

# ------------------------------------------------------
# 4 Load Bumpus data
# ------------------------------------------------------

cat("\nLoading Bumpus dataset...\n")

data_path <- file.path("R", "data", "Bumpus_data.csv")

if (!file.exists(data_path)) {
  stop("Cannot find data/Bumpus_data.csv")
}

bumpus <- read.csv(data_path)

bumpus$Survived <- as.numeric(bumpus$surv == "alive")
bumpus$TL <- bumpus$totlen
bumpus$HL <- bumpus$humer
bumpus$WT <- bumpus$wgt
bumpus$KL <- bumpus$stern

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
        standardize = FALSE
      )

      selection_results[[name]] <- res

      write.csv(
        res,
        file.path(table_dir, paste0("selection_", name, ".csv")),
        row.names = FALSE
      )

      cat("SUCCESS:", name, "\n")
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
        trait_col = trait
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

for (trait in TRAITS[1:2]) {
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
# 9 bootstrap_selection
# ------------------------------------------------------

if (exists("bootstrap_selection")) {
  cat("\nTesting bootstrap_selection...\n")

  tryCatch(
    {
      set.seed(42)

      boot <- bootstrap_selection(
        data = bumpus,
        fitness_col = FITNESS,
        trait_cols = c("TL", "HL"),
        fitness_type = "binary",
        B = 100
      )

      write.csv(
        boot$ci,
        file.path(table_dir, "bootstrap_ci.csv"),
        row.names = FALSE
      )

      saveRDS(
        boot,
        file.path(model_dir, "bootstrap_results.rds")
      )

      cat("Bootstrap OK\n")
    },
    error = function(e) {
      cat("Bootstrap FAILED:", e$message, "\n")
    }
  )
}

# ------------------------------------------------------
# 10 correlated_fitness_surface + plots
# ------------------------------------------------------

cat("\nTesting correlated fitness surfaces...\n")

pairs <- combn(TRAITS, 2, simplify = FALSE)

cfs_results <- list() # UPDATED: variable name

for (pair in pairs) {
  name <- paste(pair, collapse = "_")

  tryCatch(
    {
      # UPDATED: Use new function name correlated_fitness_surface
      res <- correlated_fitness_surface(
        data = bumpus,
        fitness_col = FITNESS,
        trait_cols = pair,
        grid_n = 30
      )

      cfs_results[[name]] <- res

      saveRDS(
        res,
        file.path(model_dir, paste0("cfs_", name, ".rds"))
      )

      if (exists("plot_correlated_fitness")) {
        p <- plot_correlated_fitness(res, pair)

        print(p)

        ggplot2::ggsave(
          file.path(figure_dir, paste0("cfs_", name, ".png")),
          p,
          width = 8,
          height = 6,
          dpi = 300
        )

        # Also try enhanced version if available
        if (exists("plot_correlated_fitness_enhanced")) {
          p_enhanced <- plot_correlated_fitness_enhanced(
            res,
            pair,
            original_data = bumpus,
            fitness_col = FITNESS
          )

          ggplot2::ggsave(
            file.path(figure_dir, paste0("cfs_", name, "_enhanced.png")),
            p_enhanced,
            width = 8,
            height = 6,
            dpi = 300
          )
        }
      }

      cat("CFS OK:", name, "\n")
    },
    error = function(e) {
      cat("FAILED:", name, "-", e$message, "\n")
    }
  )
}

# ------------------------------------------------------
# 11 adaptive landscape
# ------------------------------------------------------

if (exists("adaptive_landscape")) {
  cat("\nTesting adaptive landscape...\n")

  tryCatch(
    {
      # Fit a model first
      model <- mgcv::gam(
        Survived ~ s(TL, HL),
        family = binomial,
        data = bumpus
      )

      # Calculate adaptive landscape
      landscape <- adaptive_landscape(
        data = bumpus,
        fitness_model = model,
        trait_cols = c("TL", "HL"),
        group_col = NULL, # Bumpus data doesn't have groups
        simulation_n = 500 # Smaller for testing
      )

      saveRDS(
        landscape,
        file.path(model_dir, "adaptive_landscape.rds")
      )

      # Plot adaptive landscape
      if (exists("plot_adaptive_landscape")) {
        p <- plot_adaptive_landscape(
          landscape = landscape,
          trait_cols = c("TL", "HL"),
          original_data = bumpus
        )

        print(p)

        ggplot2::ggsave(
          file.path(figure_dir, "adaptive_landscape.png"),
          p,
          width = 8,
          height = 6,
          dpi = 300
        )
      }

      cat("Adaptive landscape Done\n")
    },
    error = function(e) {
      cat("Adaptive landscape FAILED:", e$message, "\n")
    }
  )
}

# ------------------------------------------------------
# 12 compare_fitness_surfaces
# ------------------------------------------------------

if (exists("compare_fitness_surfaces") &&
  exists("cfs_results") &&
  length(cfs_results) > 0 &&
  exists("landscape")) {
  cat("\nTesting surface comparison...\n")

  tryCatch(
    {
      # Compare first CFS result with adaptive landscape
      first_pair <- names(cfs_results)[1]

      comparison <- compare_fitness_surfaces(
        correlated_surface = cfs_results[[first_pair]],
        adaptive_landscape = landscape,
        trait_cols = c("TL", "HL")
      )

      # Save comparison plots
      if (!is.null(comparison$side_by_side)) {
        ggplot2::ggsave(
          file.path(figure_dir, "comparison_side_by_side.png"),
          comparison$side_by_side,
          width = 12,
          height = 5,
          dpi = 300
        )
      }

      if (!is.null(comparison$overlay)) {
        ggplot2::ggsave(
          file.path(figure_dir, "comparison_overlay.png"),
          comparison$overlay,
          width = 8,
          height = 6,
          dpi = 300
        )
      }

      cat("Comparison Done\n")
    },
    error = function(e) {
      cat("Comparison FAILED:", e$message, "\n")
    }
  )
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
