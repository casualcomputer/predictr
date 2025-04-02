# tests/testthat/test_helpers.R

library(testthat)
source("R/helpers.R")

test_that("label_unknown replaces NAs in factor columns", {
  df <- data.frame(a = factor(c("x", NA, "y")), b = c(1,2,NA), stringsAsFactors = FALSE)
  df <- label_unknown(df)
  expect_true("Unknown" %in% levels(df$a))
  expect_false(any(is.na(df$a)))
})

test_that("impute_data works with median method", {
  df <- data.frame(a = c(1, NA, 3), b = c("A", NA, "B"), stringsAsFactors = FALSE)
  df <- as.data.frame(lapply(df, function(x) if(is.character(x)) as.factor(x) else x))
  result <- impute_data(df, df, method =
