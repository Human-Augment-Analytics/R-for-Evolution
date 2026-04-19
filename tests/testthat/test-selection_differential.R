library(testthat)

test_that("selection_differential calculates S correctly", {
  df <- data.frame(
    w = c(0.5, 1.0, 1.5),
    z = c(-1, 0, 1) # Mean 0, SD 1
  )
  
  # S = Cov(z, w)
  res <- selection_differential(df, "w", "z", standardized = TRUE, use_relative = FALSE)
  expect_equal(res, 1/3)
})