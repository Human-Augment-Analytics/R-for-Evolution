library(testthat)

test_that("correlated_fitness_surface computes GAM grids", {
  set.seed(42)
  df <- data.frame(
    w = rnorm(50, 1, 0.1),
    z1 = rnorm(50),
    z2 = rnorm(50)
  )
  
  df$z1 <- as.numeric(scale(df$z1))
  df$z2 <- as.numeric(scale(df$z2))
  
  res <- correlated_fitness_surface(df, "w", c("z1", "z2"), grid_n = 10, method = "gam")
  expect_equal(res$method, "gam")
  expect_true(".fit" %in% names(res$grid))
})