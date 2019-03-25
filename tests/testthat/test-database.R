# Test that the package correctly identifies country names

context("Database Functionality")

test_that("Database Creation works",{
  expect_error({create_database()},NA)
  expect_error({create_database()},NA)
  skip_on_cran()
  expect_error({create_database('tmpdatabase.sqlite')},NA)
})

test_that("Database Reset works",{
  expect_error({reset_database()},NA)
  expect_error({reset_database()})
  skip_on_cran()
  expect_error({create_database('tmpdatabase.sqlite')},NA)
  expect_error({reset_database('tmpdatabase.sqlite')},NA)
})

test_that("Database low level interface works",{
  tryCatch({
    create_database()
  },
  error = function(e){
    skip_if(TRUE)
  })
  expect_error({database_add_location(name = 'TST', standard = TRUE, metadata = list('test'=1))},NA)
  expect_error({database_add_location(name = 'TST2', standard = TRUE, metadata = list('test'=1,test2='foo'))},NA)
  # Can't have multiple standard locations with the same name
  expect_error({database_add_location(name = 'TST', standard = TRUE, metadata = list('test'=1))})
  # Can't have standard location with no metadata
  expect_error({database_add_location(name = 'TST', standard = TRUE, metadata = NULL)})
  # Can't have non-standard location with metadata
  expect_error({database_add_location(name = 'TST', standard = FALSE, metadata = list('test'=1))})

  # Custom database
  tryCatch({
    create_database("tmpdatabase.sqlite")
  },
  error = function(e){
    skip_if(TRUE)
  })
  expect_error({database_add_location(name = 'TST', standard = TRUE, metadata = list('test'=1))},dbname='tmpdatabase.sqlite',NA)
  expect_error({database_add_location(name = 'TST',dbname="fakedatabase.sqlite")})
})
