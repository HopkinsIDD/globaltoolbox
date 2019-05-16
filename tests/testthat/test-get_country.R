# Test that the package correctly identifies country names

context("Standardizing country names")


data("test_country_names", package = "globaltoolbox")


test_that("get_country identified correct standardized country name", {
  x <- test_country_names
})
