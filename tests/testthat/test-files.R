context("Test files.R")

test_that("auto_group_files should group files by file name prefix", {
  files <- c("AAPL_1.csv", "AAPL_2.csv", "AMZN_1.csv", "AMZN_2.csv")
  grouped <- auto_group_files(files)
  expect_equal(grouped$dataset, c("AAPL", "AAPL", "AMZN", "AMZN"))
})

test_that("group_files should group files by the same dataset", {
  grouped <- group_files(c("PreciosApple2016.csv", "AAPL2017.csv", "StockApple2018.csv"), "AAPL")
  expect_equal(grouped$dataset, c("AAPL", "AAPL", "AAPL"))
})
