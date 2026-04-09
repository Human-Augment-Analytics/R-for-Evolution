library(testthat)

test_that("analyze_disruptive_selection computes linear and quadratic gradients", {
  set.seed(42)
  df <- data.frame(
    w = rnorm(50, 1, 0.1),
    z = rnorm(50)
  )
  
  res <- analyze_disruptive_selection(df, "w", "z", "continuous", standardize = FALSE)
  expect_true(all(c("Linear", "Quadratic") %in% res$Type))
  expect_equal(nrow(res), 2)
})