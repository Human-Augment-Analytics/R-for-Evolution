# Find the project root. When testthat runs, it temporarily sets the 
# working directory to the `tests/testthat/` directory.
project_root <- normalizePath("../../", mustWork = FALSE)

# Fallback if tests are run from the project root instead
if (!dir.exists(file.path(project_root, "R"))) {
  project_root <- normalizePath(".", mustWork = FALSE)
}

# 1. Source initialization to load required packages
init_script <- file.path(project_root, "R/scripts/0.0_initialize.R")
if (file.exists(init_script)) {
  source(init_script, local = TRUE)
}

# 2. Source all required R files into the test environment
dirs_to_source <- c("R/functions", "R/scripts", "R/plotting")

for (d in dirs_to_source) {
  files <- list.files(file.path(project_root, d), pattern = "\\.R$", full.names = TRUE)
  files <- files[!grepl("0.0_initialize\\.R", basename(files))]
  for (f in files) {
    source(f, local = TRUE)
  }
}