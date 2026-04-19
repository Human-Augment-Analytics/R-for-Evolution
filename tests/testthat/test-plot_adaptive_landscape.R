library(testthat)

test_that("plot_adaptive_landscape returns ggplot object", {
  ada <- list(
    grid = data.frame(z1 = rep(1:5, 5), z2 = rep(1:5, each = 5), .mean_fit = runif(25))
  )
  class(ada) <- "adaptive_landscape"
  
  p <- plot_adaptive_landscape(ada, c("z1", "z2"), show_optimum = FALSE)
  expect_s3_class(p, "ggplot")
})