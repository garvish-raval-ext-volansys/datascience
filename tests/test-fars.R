library(testthat)

test_that("fars_read", {
  print("Testing Far Read Functon")
  read_file <- fars_read("accident_2013.csv.bz2")
  expect_that(read_file, is_a("data.frame"))
})

test_that("make_filename", {
  print("Testing Make Filename Functon")
  read_file <- make_filename("2015")
  expect_that(read_file, equals("accident_2015.csv.bz2"))

  read_file <- make_filename(2015)
  expect_that(read_file, equals("accident_2015.csv.bz2"))

  read_file <- make_filename(c(2015))
  expect_that(read_file, equals("accident_2015.csv.bz2"))

  read_file <- make_filename(c("2015"))
  expect_that(read_file, equals("accident_2015.csv.bz2"))
})
