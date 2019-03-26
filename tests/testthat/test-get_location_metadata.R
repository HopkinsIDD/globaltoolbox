# Test that the package correctly identifies country names

context("get_location_metadata")

test_that("get_location_metadata works",{
    expect_error(get_location_metadata(),NA)
    expect_error(get_location_metadata(location="AFR"),NA)
    expect_equal(nrow(get_location_metadata(location="AFR")),1)
    expect_equal(ncol(get_location_metadata(location="AFR")),3)
    expect_equal(colnames(get_location_metadata(location="AFR")),c('id','name','type'))
})
