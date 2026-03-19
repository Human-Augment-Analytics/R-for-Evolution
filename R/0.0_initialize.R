# ======================================================
# 0.0_initialize.R
# Project initialization script
#
# Purpose:
#   - Load required packages
#   - Set working directories
#   - Create output folders
#   - Set reproducibility seed
# ======================================================

cat("\n========================================\n")
cat("Initializing R Environment\n")
cat("========================================\n")

# ------------------------------------------------------
# 1. Set reproducibility seed
# ------------------------------------------------------

set.seed(42)

# ------------------------------------------------------
# 2. Load required packages
# ------------------------------------------------------

required_packages <- c(
    "mgcv", # GAM models
    "fields", # Thin plate splines (Tps)
    "MASS", # multivariate normal simulation
    "Matrix", # matrix operations
    "ggplot2", # plotting
    "dplyr", # data manipulation
    "tidyr"
)

load_or_install <- function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

invisible(lapply(required_packages, load_or_install))

cat("Packages loaded\n")

# ------------------------------------------------------
# 3. Define project directories
# ------------------------------------------------------

project_dir <- getwd()

data_dir <- file.path(project_dir, "data")

results_dir <- file.path(project_dir, "results")
output_dir <- file.path(results_dir, "models")
figure_dir <- file.path(results_dir, "figures")
table_dir <- file.path(results_dir, "tables")

# ------------------------------------------------------
# 4. Create directories if they do not exist
# ------------------------------------------------------

dirs <- c(results_dir, output_dir, figure_dir, table_dir)

for (d in dirs) {
    if (!dir.exists(d)) {
        dir.create(d, recursive = TRUE)
    }
}

# ------------------------------------------------------
# 5. Print environment info
# ------------------------------------------------------

cat("\nWorking directory:\n")
print(project_dir)

cat("\nR version:\n")
print(R.version.string)
