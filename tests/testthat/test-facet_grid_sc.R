context("test-facet_grid_sc.R")

test_that("facet_grid_sc() accepts custom scales per facet", {
  sc_y <- list(barpercent = ggplot2::scale_y_continuous(labels = scales::percent_format()),
               barscientific = ggplot2::scale_y_continuous(labels = scales::scientific_format()))
  grid <- facet_grid_sc(ggplot2::vars(foo), scales = list(x = "fixed", y = sc_y))
  expect_identical(grid$params$scales$x, NULL)
  expect_identical(grid$params$scales$y, sc_y)
})

test_that("facet_grid_sc() accepts transformations", {
  sc_y <- list(setosa = ggplot2::scale_y_reverse(),
               versicolor = ggplot2::scale_y_sqrt(),
               virginica = ggplot2::scale_y_continuous())

  ctrl <- ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Sepal.Width,
                                             colour = Species)) +
    ggplot2::geom_point()
  test <- ctrl + facet_grid_sc(Species ~ ., scales = list(x = "fixed", y = sc_y))
  ctrl <- ctrl + ggplot2::facet_grid(Species ~ ., scales = "free_y")

  ctrl <- ggplot2::layer_data(ctrl)
  test <- ggplot2::layer_data(test)

  expect_identical(ctrl[ctrl$PANEL == 1, "y"] * -1,
                   test[test$PANEL == 1, "y"])

  expect_identical(ctrl[ctrl$PANEL == 2, "y"] ^ 0.5,
                   test[test$PANEL == 2, "y"])

  expect_identical(ctrl[ctrl$PANEL == 3, "y"],
                   test[test$PANEL == 3, "y"])
})
