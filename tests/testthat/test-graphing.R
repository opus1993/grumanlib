library(ggplot2)

test_that("we can do something", {

  fil <- scale_fill_viridis_d()
  col <- scale_fill_viridis_d()

  th <- theme_jim(grid = "XY", axis = "xy")

  expect_equal(th$plot.title$family,
               expected = if (.Platform$OS.type == "windows") "Titillium Web" else "Titillium Web Bold")

  invisible(theme_jim(grid = FALSE))
  invisible(theme_jim(grid = "XY"))
  invisible(theme_jim(grid = "xy"))

  invisible(theme_jim(axis = TRUE))

  invisible(theme_jim(axis = FALSE))
  invisible(theme_jim(axis = "xy"))
  invisible(theme_jim(axis = ""))

  expect_equal(col$palette(1), "#440154FF")
  expect_equal(fil$palette(1), "#440154FF")

})
