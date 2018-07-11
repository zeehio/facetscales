context("test-facet_grid_sc.R")

test_that("facet_grid_sc() accepts custom scales per facet", {
  sc_y <- list(barpercent = ggplot2::scale_y_continuous(labels = scales::percent_format()),
               barscientific = ggplot2::scale_y_continuous(labels = scales::scientific_format()))
  grid <- facet_grid_sc(ggplot2::vars(foo), scales = list(x = "fixed", y = sc_y))
  expect_identical(grid$params$scales$x, NULL)
  expect_identical(grid$params$scales$y, sc_y)
})

