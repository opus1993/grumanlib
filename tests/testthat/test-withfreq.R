library(testthat)
library(dplyr)

set.seed(2021)
x <- factor(state.name[rpois(100, 25)])

test_that("String wrap with frequency", {

  expect_error(p1 <- withfreq(x), NA)
  expect_identical(as.character(p1[1]), "Mississippi (10)")

  expect_error(p2 <- withfreq(x, 5), NA)
  expect_identical(as.character(p2[5]), "New\nHampshire (4)")

})
