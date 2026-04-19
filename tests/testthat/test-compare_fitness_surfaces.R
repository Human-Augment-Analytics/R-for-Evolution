library(testthat)

test_that("compare_fitness_surfaces_data handles aligned grids", {
  cor_grid <- data.frame(
    z1 = rep(1:5, 5),
    z2 = rep(1:5, each = 5),
    .fit = runif(25)
  )
  ada_grid <- data.frame(
    z1 = rep(1:5, 5),
    z2 = rep(1:5, each = 5),
    .mean_fit = runif(25)
  )
  
  cor_surf <- list(grid = cor_grid)
  ada_surf <- list(grid = ada_grid)
  
  suppressWarnings({
    res <- compare_fitness_surfaces_data(cor_surf, ada_surf, c("z1", "z2"))
  })
  
  expect_equal(nrow(res$combined_data), 50)
})