library(testthat)

test_that("prepare_selection_data standardizes traits and computes relative fitness", {
  df <- data.frame(
    w = c(2, 4, 6, 8),
    t1 = c(10, 20, 30, 40),
    group = c("A", "A", "B", "B")
  )
  
  res <- prepare_selection_data(df, "w", "t1", standardize = TRUE, add_relative = TRUE)
  
  expect_equal(mean(res$t1), 0)
  expect_equal(sd(res$t1), 1)
  expect_equal(mean(res$relative_fitness), 1)
  
  # Test with groups
  res_group <- prepare_selection_data(df, "w", "t1", standardize = TRUE, add_relative = TRUE, group = "group")
  expect_equal(res_group$t1[1:2], c(-0.7071068, 0.7071068), tolerance = 1e-6)
})