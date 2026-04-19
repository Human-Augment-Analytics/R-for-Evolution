library(testthat)

test_that("plot_fitness_surfaces_comparison returns plot list", {
  df <- data.frame(
    z1 = rep(1:5, 10),
    z2 = rep(1:5, each = 10),
    fitness = runif(50),
    type = rep(c("Correlated Fitness (Individual)", "Adaptive Landscape (Population)"), each = 25)
  )
  
  comp_data <- list(
    combined_data = df,
    trait_cols = c("z1", "z2")
  )
  
  plots <- plot_fitness_surfaces_comparison(comp_data)
  expect_s3_class(plots$overlay, "ggplot")
})