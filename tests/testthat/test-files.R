context("Test files.R")

test_that("auto_group_files should group files by file name prefix", {
  files <- c("AAPL-1.csv", "AAPL-2.csv", "AMZN_1.csv", "AMZN_2.csv", "IG_AUDJPY_D1-1.csv", "IG_AUDJPY_D1-2.csv", "IG_EURUSD_D1_1.csv", "IG_EURUSD_D1_2.csv")
  grouped <- auto_group_files(files)
  expect_equal(grouped$dataset, c("AAPL", "AAPL", "AMZN", "AMZN", "IG_AUDJPY_D1", "IG_AUDJPY_D1", "IG_EURUSD_D1", "IG_EURUSD_D1"))
})

test_that("group_files should group files by the same dataset", {
  grouped <- group_files(c("PreciosApple2016.csv", "AAPL2017.csv", "StockApple2018.csv"), "AAPL")
  expect_equal(grouped$dataset, c("AAPL", "AAPL", "AAPL"))
})
