library(testthat)

test_that("initialize script ensures required packages are available", {
  expect_true(requireNamespace("mgcv", quietly = TRUE))
  expect_true(requireNamespace("ggplot2", quietly = TRUE))
  expect_true(requireNamespace("fields", quietly = TRUE))
})