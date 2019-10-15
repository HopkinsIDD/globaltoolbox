# Test that the package handles a few simple use cases.

source("help-functionality_check.R")
context("General Use Case Tests")

# Startup

test_that("TOGO loads from GADM",{
  initialize_database()
  expect_error({
    load_gadm("TGO", dbname = tdbn)
  },
  NA)
  expect_equal({
    standardize_name("TOGO", dbname = tdbn)
  },
  setNames("::tgo", "TOGO"))
  expect_equal({
    telescoping_standardize("TOGO::oti", dbname = tdbn)
  },
  setNames("::tgo::savanes::oti", "TOGO::oti"))
})
