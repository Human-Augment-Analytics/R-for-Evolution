#!/usr/bin/env Rscript

# ==============================================================================
# Smoke Test Runner
# This script attempts to source every .R file in the 'R' directory to ensure
# they execute without errors.
# ==============================================================================

# Configuration
SCRIPT_DIR <- "R"  # Directory containing your R scripts

# Check if directory exists
if (!dir.exists(SCRIPT_DIR)) {
  stop(paste("Directory", SCRIPT_DIR, "not found. Please run this from the repo root."))
}

# Find all .R files recursively
r_files <- list.files(SCRIPT_DIR, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
r_files <- normalizePath(r_files)

if (length(r_files) == 0) {
  message("No .R files found to test.")
  quit(status = 0)
}

message(paste("Found", length(r_files), "files. Starting execution test..."))

failures <- character()

for (file in r_files) {
  message(paste("\nTesting:", file))
  
  old_wd <- getwd()
  
  # Strategy: Run everything from the 'R' directory.
  # This ensures that scripts can source their siblings (e.g. source("utils.R")).
  # For scripts that need data (which is usually in ../data relative to R/),
  # we create a temporary symlink 'R/data' -> '../data'.
  
  run_wd <- file.path(old_wd, SCRIPT_DIR)
  
  # Setup data symlink
  data_src <- file.path(old_wd, "data")
  data_link <- file.path(run_wd, "data")
  
  # Setup test_data symlink
  test_data_src <- file.path(old_wd, "test_data")
  test_data_link <- file.path(run_wd, "test_data")
  
  # Setup R symlink (allows source("R/script.R") to work from within R/)
  r_link <- file.path(run_wd, "R")
  
  created_links <- character()
  
  if (dir.exists(data_src) && !file.exists(data_link)) {
    tryCatch({
      file.symlink(data_src, data_link)
      created_links <- c(created_links, data_link)
    }, error = function(e) {
      # Proceed even if symlink fails
    })
  }
  
  if (dir.exists(test_data_src) && !file.exists(test_data_link)) {
    tryCatch({
      file.symlink(test_data_src, test_data_link)
      created_links <- c(created_links, test_data_link)
    }, error = function(e) {
      # Proceed even if symlink fails
    })
  }
  
  if (!file.exists(r_link)) {
    tryCatch({
      file.symlink(run_wd, r_link)
      created_links <- c(created_links, r_link)
    }, error = function(e) {})
  }
  
  # Create a test environment to mock functions
  test_env <- new.env(parent = globalenv())
  
  # Mock setwd to prevent scripts from breaking the runner or failing on non-existent paths
  assign("setwd", function(dir) {
    message(paste("  [Mock] setwd(", dir, ") intercepted and ignored."))
  }, envir = test_env)
  
  # Mock quit to prevent scripts from terminating the runner
  assign("quit", function(save = "default", status = 0, runLast = TRUE) {
    if (status == 0) {
      stop("MOCK_QUIT_SUCCESS")
    } else {
      stop(paste("MOCK_QUIT_FAILURE:", status))
    }
  }, envir = test_env)
  
  # Try to source the file in a local environment
  tryCatch({
    setwd(run_wd)
    source(file, local = test_env)
    message("  -> SUCCESS")
  }, error = function(e) {
    if (identical(conditionMessage(e), "MOCK_QUIT_SUCCESS")) {
      message("  -> SKIPPED (Script requested quit)")
    } else {
      message("  -> FAILED")
      message(paste("     Error:", e$message))
      message(paste("     Working Directory:", getwd()))
      failures <<- c(failures, file)
    }
  }, finally = {
    for (link in created_links) {
      if (file.exists(link)) file.remove(link)
    }
    setwd(old_wd)
  })
}

message("\n================ SUMMARY ================")
if (length(failures) > 0) {
  message(paste(length(failures), "files failed to run:"))
  message(paste("-", failures, collapse = "\n"))
  quit(status = 1)
} else {
  message("All files executed successfully.")
  quit(status = 0)
}