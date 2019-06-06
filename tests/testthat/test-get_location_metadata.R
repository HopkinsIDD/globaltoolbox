# Test that the package correctly identifies country names

source("help-functionality_check.R")

context("get_location_metadata")

test_that("get_location_metadata works", {
  initialize_database()
  create_test_database()
  expect_error(get_location_metadata(dbname = tdbn), NA)
  expect_error(get_location_metadata(location = "::tst", dbname = tdbn), NA)
  expect_equal(
    nrow(get_location_metadata(location = "::tst", dbname = tdbn)),
    2
  )
  expect_equal(
    ncol(get_location_metadata(location = "::tst2", dbname = tdbn)) >= 4,
    TRUE
  )
  expect_equal({
    all(
      c("id", "name", "readable_name") %in%
        colnames(get_location_metadata(location = "::tst", dbname = tdbn))
    )
  },
  TRUE
  )
  expect_equal(
    all(
      c("test") %in%
        colnames(get_location_metadata(location = "::tst", dbname = tdbn))
    ),
    TRUE
  )
})
