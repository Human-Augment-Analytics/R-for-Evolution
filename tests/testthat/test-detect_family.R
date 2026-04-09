library(testthat)

test_that("detect_family correctly identifies fitness types", {
  expect_equal(detect_family(c(0, 1, 0, 1, 0))$type, "binary")
  expect_equal(detect_family(c(0, 1, 2, 3, 4))$type, "count")
  expect_equal(detect_family(seq(0, 1, length.out = 10))$type, "continuous")
  expect_equal(detect_family(rnorm(10, 1, 0.05))$type, "continuous")
  expect_error(detect_family(c(NA, NA)), "No non-NA values")
})