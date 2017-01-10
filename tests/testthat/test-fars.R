context("Test the basic functionality of the package\n\n")

library(dplyr)
library(maps)

setwd(system.file("extdata", package = "farreport"))

test_that("fars_read", {
  print("Testing Far Read Functon")
  read_file <- fars_read("accident_2013.csv.bz2")
  expect_that(read_file, is_a("data.frame"))
  expect_error(fars_read("accident_2020.csv.bz2"))
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

test_that("fars_read_years", {
  print("Testing Fars Read Functon")
  read_file <- fars_read_years("2015")
  expect_is(read_file, "list")
})

test_that("fars_summarize_years", {
  print("Testing Fars summarize Functon")
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
  expect_is(fars_summarize_years(list(2013, 2014)), "tbl_df")
  expect_equal(names(fars_summarize_years(2013:2015)), c("MONTH", 2013:2015))
})

test_that("fars_map_state", {
  print("Testing Fars Map Functon")
  expect_silent(fars_map_state(5, 2015))
  expect_error(fars_map_state(100, 2015))
  expect_error(fars_map_state(1, 2016))
})
