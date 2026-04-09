library(testthat)

test_that("extract_linear_coefficients gets OLS coefficients correctly", {
  set.seed(42)
  df <- data.frame(
    w = rnorm(50, 1, 0.1),
    z1 = rnorm(50),
    z2 = rnorm(50)
  )
  
  res <- analyze_linear_selection(df, "w", c("z1", "z2"), "continuous")
  
  coefs <- extract_linear_coefficients(c("z1", "z2"), res)
  expect_equal(nrow(coefs), 2)
})