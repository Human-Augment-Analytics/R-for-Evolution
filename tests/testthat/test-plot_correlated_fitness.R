library(testthat)

test_that("plot_correlated_fitness returns ggplot object", {
  cfs <- list(
    grid = data.frame(z1 = rep(1:5, 5), z2 = rep(1:5, each = 5), .fit = runif(25))
  )
  
  p <- plot_correlated_fitness(cfs, c("z1", "z2"), show_optimum = FALSE)
  expect_s3_class(p, "ggplot")
})