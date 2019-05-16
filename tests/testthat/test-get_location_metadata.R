# Test that the package correctly identifies country names

context("get_location_metadata")

test_that("get_location_metadata works", {
    expect_error(get_location_metadata(), NA)
    expect_error(get_location_metadata(location = "AFR"), NA)
    expect_equal(nrow(get_location_metadata(location = "TST")), 1)
    expect_equal(ncol(get_location_metadata(location = "TST")), 4)
    expect_equal(
      colnames(get_location_metadata(location = "TST")),
      c("id", "name", "readable_name", "test")
    )
})
