###############################################################################
# FULL FUNCTION TEST AND RESULTS ARCHIVING SCRIPT
#
# Runs the complete analysis pipeline:
#   - data preparation
#   - selection coefficient estimation
#   - univariate fitness surfaces
#   - bivariate (correlational) fitness surfaces
#
# All results and figures are saved to disk.
###############################################################################

## ---- Configuration ---------------------------------------------------------
verbose_output <- TRUE
save_plots     <- TRUE
save_workspace <- TRUE

log_msg <- function(...) {
  if (verbose_output) cat(...)
}

## ---- 1. Output directory ---------------------------------------------------
log_msg("1. Setting up output directory...\n")

results_dir <- "function_test_results"
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  log_msg("   Created output directory: ", results_dir, "\n")
} else {
  log_msg("   Using existing output directory: ", results_dir, "\n")
}

## ---- 2. Load packages ------------------------------------------------------
log_msg("\n2. Loading required packages...\n")

if (!requireNamespace("ggplot2", quietly = TRUE))
  stop("Package 'ggplot2' is required.")

library(ggplot2)

if (!requireNamespace("mgcv", quietly = TRUE))
  log_msg("   Package 'mgcv' not installed (needed for GAM-based surfaces)\n")

if (!requireNamespace("fields", quietly = TRUE))
  log_msg("   Package 'fields' not installed (needed for TPS surfaces)\n")

## ---- 3. Load data ----------------------------------------------------------
log_msg("\n3. Loading Crescent Pond puppyfish data...\n")

crescent_filename <- "Crescent+Pond+-+size-corrected+trait+data+++survival+++growth+++d13C+++d15N.csv"
possible_paths <- c(
  file.path("data", crescent_filename),
  file.path("test_data", crescent_filename),
  file.path("..", "data", crescent_filename),
  file.path("..", "test_data", crescent_filename),
  crescent_filename
)

crescent_path <- NULL
for (p in possible_paths) {
  if (file.exists(p)) {
    crescent_path <- p
    break
  }
}

if (is.null(crescent_path)) {
  message("Data file not found. Checked: ", paste(possible_paths, collapse=", "), ". Skipping analysis.")
  quit(status = 0)
}
crescent_data <- read.csv(crescent_path)

fitness_binary     <- "survival"
fitness_continuous <- "ln.growth"
traits <- c("jaw", "eye", "body", "nasal", "mouth", "SL")

test_data <- crescent_data[, c(fitness_binary, fitness_continuous, traits)]
test_data <- test_data[complete.cases(test_data), ]

log_msg("   Observations used: ", nrow(test_data), "\n")

## ---- 4. Load analysis functions -------------------------------------------
log_msg("\n4. Loading analysis functions...\n")

function_files <- c(
  "prepare_selection_data.R",
  "selection_coefficients.R",
  "univariate_spline.R",
  "univariate_surface.R",
  "correlational_tps.R",
  "correlation_surface.R"
)

for (f in function_files) {
  if (!file.exists(f)) stop("Missing function file: ", f)
  source(f)
  log_msg("   Loaded: ", f, "\n")
}

## ---- 5. Initialize results container --------------------------------------
all_results <- list(
  metadata = list(
    date = Sys.time(),
    data_source = "Crescent Pond puppyfish",
    n = nrow(test_data),
    traits = traits
  ),
  preparation = list(),
  selection = list(),
  surfaces = list()
)

## ---- 6. Data preparation ---------------------------------------------------
log_msg("\n5. Preparing data...\n")

prep_binary <- prepare_selection_data(
  data = test_data,
  fitness_col = fitness_binary,
  trait_cols = traits,
  standardize = TRUE
)

prep_continuous <- prepare_selection_data(
  data = test_data,
  fitness_col = fitness_continuous,
  trait_cols = traits,
  standardize = TRUE
)

all_results$preparation$binary     <- dim(prep_binary)
all_results$preparation$continuous <- dim(prep_continuous)

## ---- 7. Selection coefficient analysis ------------------------------------
log_msg("\n6. Estimating selection coefficients...\n")

sel_binary <- selection_coefficients(
  data = prep_binary,
  fitness_col = fitness_binary,
  trait_cols = traits,
  fitness_type = "binary",
  standardize = FALSE
)

sel_continuous <- selection_coefficients(
  data = prep_continuous,
  fitness_col = fitness_continuous,
  trait_cols = traits,
  fitness_type = "continuous",
  standardize = FALSE
)

all_results$selection$binary     <- sel_binary
all_results$selection$continuous <- sel_continuous

## ---- 8. Univariate fitness surfaces ---------------------------------------
log_msg("\n7. Estimating univariate fitness surfaces...\n")

uni_surfaces <- list()

for (trait in traits[1:3]) {
  uni <- univariate_spline(
    data = prep_binary,
    fitness_col = fitness_binary,
    trait_col = trait,
    fitness_type = "binary",
    k = 6
  )
  
  uni_surfaces[[trait]] <- uni
  
  if (save_plots) {
    p <- univariate_surface(uni, trait_col = trait) +
      ggtitle(paste("Fitness vs", trait))
    ggsave(
      filename = file.path(results_dir, paste0("univariate_", trait, ".png")),
      plot = p,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}

all_results$surfaces$univariate <- names(uni_surfaces)

## ---- 9. Bivariate (correlational) fitness surface --------------------------
log_msg("\n8. Estimating bivariate fitness surface...\n")

trait_pair <- traits[1:2]

tps <- correlational_tps(
  data = prep_binary,
  fitness_col = fitness_binary,
  trait_cols = trait_pair,
  method = "auto",
  grid_n = 25
)

all_results$surfaces$correlational <- list(
  traits = trait_pair,
  grid_dim = dim(tps$grid)
)

if (save_plots) {
  p <- correlation_surface(tps, trait_cols = trait_pair) +
    ggtitle("Bivariate Fitness Surface")
  ggsave(
    filename = file.path(results_dir, "correlation_surface.png"),
    plot = p,
    width = 10,
    height = 8,
    dpi = 300
  )
}

## ---- 10. Save results ------------------------------------------------------
log_msg("\n9. Saving results...\n")

saveRDS(all_results, file.path(results_dir, "analysis_results.rds"))

write.csv(
  sel_binary,
  file.path(results_dir, "selection_coefficients_binary.csv"),
  row.names = FALSE
)

write.csv(
  sel_continuous,
  file.path(results_dir, "selection_coefficients_continuous.csv"),
  row.names = FALSE
)

if (save_workspace) {
  save.image(file.path(results_dir, "workspace.RData"))
}

## ---- 11. Final summary -----------------------------------------------------
log_msg("\nAnalysis complete.\n")
log_msg("Results directory: ", normalizePath(results_dir), "\n")
