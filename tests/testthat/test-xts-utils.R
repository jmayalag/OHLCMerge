context("Test xts utils")

test_that("combine_xts should combine two xts with overlap", {
  require(xts)

  data(sample_matrix)
  x <- as.xts(sample_matrix)

  a <- head(x, 10)
  b <- head(x, 5) * 2

  expected <- head(x, 10)
  comb <- combine_xts(a, b)
  expect_equal(comb, expected)
})

test_that("combine_xts should combine two xts with overlap, updating values", {
  require(xts)

  data(sample_matrix)
  x <- as.xts(sample_matrix)

  a <- head(x, 10)
  b <- head(x, 10) * 2

  expected <- head(x, 10) * 2
  comb <- combine_xts(a, b, update = TRUE)
  expect_equal(comb, expected)
})


test_that("combine_xts should combine two xts without overlap", {
  require(xts)

  data(sample_matrix)
  x <- as.xts(sample_matrix)
  a <- head(x, 10)
  b <- tail(x, 10)

  comb <- combine_xts(a, b)
  expected <- c(head(x, 10), tail(x, 10))
  expect_equal(comb, expected)
})

test_that("compatible_periodicity should be true for compatible periodicities", {
  require(xts)

  data(sample_matrix)
  x <- as.xts(sample_matrix)
  a <- periodicity_df(head(x))
  b <- periodicity_df(tail(x))
  expect_true(compatible_periodicity(a, b))
})

test_that("compatible_periodicity should be false for incompatible periodicities", {
  require(xts)

  data(sample_matrix)
  x <- as.xts(sample_matrix)
  a <- periodicity_df(head(x))
  b <- periodicity_df(tail(x))
  b$units <- "minutes"
  expect_false(compatible_periodicity(a, b))
})
