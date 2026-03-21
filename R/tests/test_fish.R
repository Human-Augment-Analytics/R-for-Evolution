# =============================================================================
# PART 1: SETUP AND INITIALIZATION
# =============================================================================

cat("1. Working directory and package initialization\n")

# Initialize environment
if (file.exists("R/scripts/0.0_initialize.R")) {
  source("R/scripts/0.0_initialize.R")
  cat("   Sourced: 0.0_initialize.R\n")
}

required_packages <- c(
  "dplyr", "tidyr", "ggplot2", "mgcv", "fields",
  "purrr", "patchwork", "viridis", "scales", "knitr", "here"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
  cat("   Loaded package:", pkg, "\n")
}

cat("Project Root:", here(), "\n")

# =============================================================================
# PART 2: LOADING FUNCTIONS
# =============================================================================

cat("\n2. LOADING FUNCTIONS...\n")

cat("\nLoading function files and plotting files...\n")

fn_files <- list.files(
  here("R", "functions"),
  pattern = "\\.R$",
  full.names = TRUE
)

for (f in fn_files) {
  source(f)
  cat("   Loaded:", basename(f), "\n")
}

plot_files <- list.files(
  here("R", "plotting"),
  pattern = "\\.R$",
  full.names = TRUE
)

for (f in plot_files) {
  source(f)
  cat("   Loaded plot:", basename(f), "\n")
}

script_files <- list.files(
  here("R", "scripts"),
  pattern = "\\.R$",
  full.names = TRUE
)

# Exclude initialization script
script_files <- script_files[!grepl("0.0_initialize\\.R$", script_files)]

for (f in script_files) {
  source(f)
  cat("   Loaded script:", basename(f), "\n")
}

# =============================================================================
# PART 3: DATA LOADING AND PREPARATION
# =============================================================================

cat("\n3. DATA LOADING AND PREPARATION...\n")

# Define data directories to search
data_dirs <- c(here("R", "data"), here("R", "test_data"))

# Helper function to find files in directories
find_data_file <- function(filenames, dirs) {
  for (d in dirs) {
    if (dir.exists(d)) {
      for (f in filenames) {
        p <- file.path(d, f)
        if (file.exists(p)) return(p)
      }
    }
  }
  return(NULL)
}

# 1. Load Crescent Pond puppyfish dataset
crescent_names <- c(
  "Crescent+Pond+-+size-corrected+trait+data+++survival+++growth+++d13C+++d15N.csv",
  "Crescent Pond - size-corrected trait data + survival + growth + d13C + d15N.csv"
)
crescent_path <- find_data_file(crescent_names, data_dirs)

# Fallback: fuzzy search if not found
if (is.null(crescent_path)) {
  for (d in data_dirs) {
    if (dir.exists(d)) {
      candidates <- list.files(d, pattern = "Crescent.*Pond.*\\.csv$", full.names = TRUE)
      if (length(candidates) > 0) {
        crescent_path <- candidates[1]
        break
      }
    }
  }
}

if (is.null(crescent_path)) {
  stop("Data file 'Crescent Pond...' not found in data directories.")
}
cat("   Using data file:", crescent_path, "\n")
crescent_data <- read.csv(crescent_path)

# 2. Load Little Lake puppyfish dataset
ll_names <- c(
  "Little+Lake+-+size-corrected+trait+data+++survival+++growth+++d13C+++d15N.csv",
  "Little Lake - size-corrected trait data + survival + growth + d13C + d15N.csv"
)
ll_path <- find_data_file(ll_names, data_dirs)
if (!is.null(ll_path)) {
  cat("   Using data file:", ll_path, "\n")
  little_lake_data <- read.csv(ll_path)
} else {
  cat("   [INFO] Little Lake data not found.\n")
}

# 3. Load Isotopes dataset
iso_path <- find_data_file("isotopes.csv", data_dirs)
if (!is.null(iso_path)) {
  cat("   Using data file:", iso_path, "\n")
  isotopes_data <- read.csv(iso_path)
} else {
  cat("   [INFO] Isotopes data not found.\n")
}

# 4. Load Raw Data
raw_path <- find_data_file("rawdata.csv", data_dirs)
if (!is.null(raw_path)) {
  cat("   Using data file:", raw_path, "\n")
  raw_data <- read.csv(raw_path)
} else {
  cat("   [INFO] Raw data not found.\n")
}

# Define fitness variables and trait set
fitness_binary     <- "survival"
fitness_continuous <- "ln.growth"
morphological_traits <- c("jaw", "eye", "body", "nasal", "mouth", "SL")

# Assemble analysis dataset
test_data <- crescent_data %>%
  select(all_of(c(fitness_binary, fitness_continuous, morphological_traits))) %>%
  filter(complete.cases(.))

cat("   Test dataset contains", nrow(test_data), "complete observations\n")

# =============================================================================
# PART 4: RUN ANALYSES AND RECORD RESULTS
# =============================================================================

cat("\n4. RUNNING ANALYSES AND RECORDING RESULTS...\n")

results_dir <- here("R", "results", "test_fish_results")
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
  cat("   Created output directory:", results_dir, "\n")
} else {
  cat("   Using existing output directory:", results_dir, "\n")
}

# Initialize master results object
all_results <- list(
  test_info = list(
    date = Sys.time(),
    data_source = "Crescent Pond puppyfish",
    n_observations = nrow(test_data),
    fitness_variables = c(binary = fitness_binary,
                          continuous = fitness_continuous),
    traits_analyzed = morphological_traits
  ),
  data_preparation = list(),
  selection_analysis = list(),
  fitness_surfaces = list(),
  validation_summary = list()
)

## 4.1 Data preparation -------------------------------------------------------
cat("   4.1 DATA PREPARATION...\n")

if (exists("prepare_selection_data")) {
  
  # Binary fitness preparation
  prepared_binary <- tryCatch({
    result <- prepare_selection_data(
      data = test_data,
      fitness_col = fitness_binary,
      trait_cols = morphological_traits,
      standardize = TRUE
    )
    all_results$data_preparation$binary <- list(
      status = "success",
      dimensions = dim(result)
    )
    result
  }, error = function(e) {
    all_results$data_preparation$binary <- list(
      status = "failed",
      error = e$message
    )
    NULL
  })
  
  # Continuous fitness preparation
  prepared_continuous <- tryCatch({
    result <- prepare_selection_data(
      data = test_data,
      fitness_col = fitness_continuous,
      trait_cols = morphological_traits,
      standardize = TRUE
    )
    all_results$data_preparation$continuous <- list(
      status = "success",
      dimensions = dim(result)
    )
    result
  }, error = function(e) {
    all_results$data_preparation$continuous <- list(
      status = "failed",
      error = e$message
    )
    NULL
  })
  
  cat("   Data preparation completed\n")
}

## 4.2 Selection coefficient analysis ----------------------------------------
cat("   4.2 SELECTION COEFFICIENT ANALYSIS...\n")

if (exists("selection_coefficients")) {
  
  # Binary fitness model
  if (!is.null(prepared_binary)) {
    selection_binary <- tryCatch({
      result <- selection_coefficients(
        data = prepared_binary,
        fitness_col = fitness_binary,
        trait_cols = morphological_traits,
        fitness_type = "binary",
        standardize = FALSE
      )
      all_results$selection_analysis$binary <- list(
        status = "success",
        results = result
      )
      result
    }, error = function(e) {
      all_results$selection_analysis$binary <- list(
        status = "failed",
        error = e$message
      )
      NULL
    })
  }
  
  # Continuous fitness model
  if (!is.null(prepared_continuous)) {
    selection_continuous <- tryCatch({
      result <- selection_coefficients(
        data = prepared_continuous,
        fitness_col = fitness_continuous,
        trait_cols = morphological_traits,
        fitness_type = "continuous",
        standardize = FALSE
      )
      all_results$selection_analysis$continuous <- list(
        status = "success",
        results = result
      )
      result
    }, error = function(e) {
      all_results$selection_analysis$continuous <- list(
        status = "failed",
        error = e$message
      )
      NULL
    })
  }
  
  cat("   Selection coefficient analysis completed\n")
}

## 4.3 Fitness surface estimation --------------------------------------------
cat("   4.3 FITNESS SURFACE ESTIMATION...\n")

if (exists("univariate_spline") && !is.null(prepared_binary)) {
  
  # Univariate fitness surfaces
  univariate_results <- list()
  for (trait in morphological_traits[1:3]) {
    spline_result <- tryCatch({
      univariate_spline(
        data = prepared_binary,
        fitness_col = fitness_binary,
        trait_col = trait,
        fitness_type = "binary",
        k = 6
      )
    }, error = function(e) NULL)
    
    if (!is.null(spline_result)) {
      univariate_results[[trait]] <- spline_result
      
      if (exists("plot_univariate_fitness")) {
        plot_file <- file.path(
          results_dir,
          paste0("univariate_surface_", trait, ".png")
        )
        png(plot_file, width = 800, height = 600)
        print(
          plot_univariate_fitness(spline_result, trait_col = trait) +
            labs(title = paste("Fitness Surface:", trait))
        )
        dev.off()
      }
    }
  }
  
  all_results$fitness_surfaces$univariate <- list(
    traits_tested = names(univariate_results),
    n_surfaces = length(univariate_results)
  )
  
  cat("   Univariate fitness surfaces generated (",
      length(univariate_results), " traits)\n", sep = "")
}

# Bivariate (correlational) fitness surface
if (exists("correlated_fitness_surface") && !is.null(prepared_binary)) {
  trait_pair <- morphological_traits[1:2]
  
  tps_result <- tryCatch({
    result <- correlated_fitness_surface(
      data = prepared_binary,
      fitness_col = fitness_binary,
      trait_cols = trait_pair,
      method = "auto",
      grid_n = 20
    )
    
    if (exists("plot_correlated_fitness")) {
      plot_file <- file.path(results_dir, "correlation_surface.png")
      png(plot_file, width = 1000, height = 800)
      print(
        plot_correlated_fitness(result, trait_cols = trait_pair) +
          labs(title = "Bivariate Fitness Surface")
      )
      dev.off()
    }
    
    all_results$fitness_surfaces$correlational <- list(
      status = "success",
      traits = trait_pair,
      grid_dimensions = dim(result$grid)
    )
    result
  }, error = function(e) {
    all_results$fitness_surfaces$correlational <- list(
      status = "failed",
      error = e$message
    )
    NULL
  })
  
  cat("   Correlational fitness surface completed\n")
}

# =============================================================================
# PART 5: SAVE SUMMARY
# =============================================================================

cat("\n5. SAVING SUMMARY...\n")

# Save the results object
saveRDS(all_results, file = file.path(results_dir, "all_results.rds"))

if (!is.null(all_results$selection_analysis$binary$results)) {
  cat("\nBinary Selection Results Summary (Top 5):\n")
  print(head(all_results$selection_analysis$binary$results))
}

cat("\nAnalysis complete. Results saved to:", results_dir, "\n")
