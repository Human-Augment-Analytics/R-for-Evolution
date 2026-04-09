library(testthat)

test_that("selection_coefficients extracts linear, quadratic, and correlational terms", {
  set.seed(42)
  df <- data.frame(
    w = rnorm(50, 5, 1),
    z1 = rnorm(50),
    z2 = rnorm(50)
  )
  
  res <- selection_coefficients(df, "w", c("z1", "z2"), fitness_type = "continuous")
  
  expect_true(any(res$Type == "Linear"))
  expect_true(all(c("Beta_Coefficient", "Standard_Error", "P_Value") %in% names(res)))
})