library(testthat)

test_that("univariate_spline generates valid splines", {
  set.seed(42)
  df <- data.frame(
    w = rnorm(50, 1, 0.1),
    z = rnorm(50)
  )
  
  df$z <- as.numeric(scale(df$z))
  
  res <- univariate_spline(df, "w", "z", fitness_type = "continuous", k = 3)
  expect_s3_class(res, "univariate_fitness")
  expect_true(all(c("fit", "lwr", "upr") %in% names(res$grid)))
})