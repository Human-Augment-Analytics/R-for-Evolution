library(testthat)

test_that("adaptive_landscape calculates mean fitness on a grid", {
  set.seed(42)
  df <- data.frame(
    w = rnorm(50, 1, 0.1),
    z1 = rnorm(50),
    z2 = rnorm(50)
  )
  
  df$z1 <- as.numeric(scale(df$z1))
  df$z2 <- as.numeric(scale(df$z2))
  
  fit_model <- mgcv::gam(w ~ s(z1, z2), data = df)
  
  res <- adaptive_landscape(df, fit_model, c("z1", "z2"), simulation_n = 10, grid_n = 5)
  expect_s3_class(res, "adaptive_landscape")
  expect_true(".mean_fit" %in% names(res$grid))
  expect_equal(nrow(res$grid), 25)
})