library(testthat)

test_that("analyze_linear_selection works with continuous and binary data", {
  set.seed(42)
  df <- data.frame(
    w_cont = rnorm(50, 1, 0.1),
    w_bin = rbinom(50, 1, 0.5),
    z1 = rnorm(50),
    z2 = rnorm(50)
  )
  
  res_cont <- analyze_linear_selection(df, "w_cont", c("z1", "z2"), "continuous")
  expect_equal(res_cont$fitness_type, "continuous")
  
  res_bin <- analyze_linear_selection(df, "w_bin", c("z1", "z2"), "binary")
  expect_equal(res_bin$fitness_type, "binary")
})