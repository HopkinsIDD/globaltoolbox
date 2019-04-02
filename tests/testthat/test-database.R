# Test that the package correctly identifies country names

context("Database Functionality")

try({reset_database()})
try({reset_database('tmpdatabase.sqlite')})

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

test_that("Database low level interface: add_location works",{
  tryCatch({
    create_database()
  },
  error = function(e){
    skip_if(TRUE)
  })
  expect_error({database_add_location(name = 'TST', readable_name='Test',metadata = list('test'=1))},NA)
  expect_error({database_add_location(name = 'TST2', readable_name='Test', metadata = list('test'=1,test2='foo'))},NA)
  expect_error({database_add_location(name = 'TST3', readable_name='Test', metadata = list("test"="John's Data"))},NA)
  # Can't have multiple standard locations with the same name
  expect_error({database_add_location(name = 'TST', readable_name='Test', metadata = list('test'=1))})
  # Can't have standard location with no metadata
  expect_error({database_add_location(name = 'TST', readable_name='Test', metadata = NULL)})

  # Custom database
  tryCatch({
    create_database("tmpdatabase.sqlite")
  },
  error = function(e){
    skip_if(TRUE)
  })
  expect_error({
      database_add_location(
          name = 'TST',
          readable_name='Test',
          metadata = list('test'=1),
         dbname='tmpdatabase.sqlite'
      )
  },NA)
  expect_error({
      database_add_location(name = 'TST',readable_name='Test', dbname="fakedatabase.sqlite")
  })
})
