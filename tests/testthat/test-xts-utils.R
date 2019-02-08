context("Test xts utils")

test_that("combine_xts should combine two xts", {
  require(xts)

  data(sample_matrix)
  a <- head(as.xts(sample_matrix))
  b <- head(as.xts(sample_matrix), 30)
  b[5:10, ] <- b[5:10, ] * 2

  comb <- combine_xts(a, b)
  expect_equal(round(sum(comb$Close)), 1707)
})
