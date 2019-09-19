context("Test csv functions")

filepath <- function() {
  system.file("extdata", "IG_AUDJPY_D1.csv", package = "OHLCMerge")
}

test_that("read_ohlcv should read the csv with '%Y.%m.%d' date format", {
  csv_file <- filepath()
  df <- read_ohlcv(csv_file, date_format = c("%Y.%m.%d"))

  expect_equal(nrow(df), 7607)
  expect_equal(as.character(start(df)), "1993-05-16")
  expect_equal(as.character(end(df)), "2019-02-28")
  expect_equal(sum(df$Volume), 309304521)
})

test_that("read_ohlcv should read the csv with the default date formats", {
  csv_file <- filepath()
  df <- read_ohlcv(csv_file)

  expect_equal(nrow(df), 7607)
  expect_equal(as.character(start(df)), "1993-05-16")
  expect_equal(as.character(end(df)), "2019-02-28")
  expect_equal(sum(df$Volume), 309304521)
})

test_that("read_ohlcv should fail to read the csv with '%Y-%m-%d' date format", {
  csv_file <- filepath()
  expect_error(read_ohlcv(csv_file, date_format = c("%Y-%m-%d")), "character string is not in a standard unambiguous format")
})

test_that("as_posixct_compat should work with a character vector of datetime formats (R >= 3.5)", {
  if (!uses_try_formats()) return()
  formats <- c("%Y-%m-%d", "%Y.%m.%d")
  expected <- as.POSIXct("2019-01-01")

  d <- as_posixct_compat("2019-01-01", try_formats = formats)
  expect_equal(d, expected)

  d <- as_posixct_compat("2019.01.01", try_formats = formats)
  expect_equal(d, expected)
})

test_that("as_posixct_compat should work with a character vector of datetime formats (R < 3.5)", {
  if (uses_try_formats()) return()
  formats <- c("%Y-%m-%d", "%Y.%m.%d")
  expected <- as.POSIXct("2019-01-01")

  d <- as_posixct_compat("2019-01-01", try_formats = formats)
  expect_equal(d, expected)

  d <- as_posixct_compat("2019.01.01", try_formats = formats)
  expect_equal(d, expected)
})
