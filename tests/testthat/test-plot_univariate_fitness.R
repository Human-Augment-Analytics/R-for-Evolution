library(testthat)

test_that("plot_univariate_fitness returns ggplot object", {
  uni <- list(
    grid = data.frame(z1 = seq(-2, 2, 0.1), fit = runif(41), lwr = runif(41), upr = runif(41)),
    fitness_type = "continuous"
  )
  class(uni) <- "univariate_fitness"
  p <- plot_univariate_fitness(uni, "z1")
  expect_s3_class(p, "ggplot")
})