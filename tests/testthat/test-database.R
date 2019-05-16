# Test that the package correctly identifies country names

context("Database Low Level Interface")

# Startup
tdbn = 'testdatabase.sqlite'
fdbn = 'fakedatabase.sqlite'
attempt_fun <- function(FUN,NAME,...){
  tryCatch(FUN(...),error=function(e){skip_if(TRUE,message=paste(NAME,'failed with error',e$message))})
}
initialize_database <-  function(){
  attempt_fun(reset_database,'reset_database',dbname=tdbn)
  attempt_fun(create_database,'create_database',dbname=tdbn)
}
create_locations = function(){
  attempt_fun(
    FUN=database_add_location,
    NAME='add_location',
    name='TST',
    readable_name='Test',
    metadata=list('test'=1),
    tdbn
  )
  attempt_fun(FUN=database_add_location,
    NAME='add_location',
    name='TST2',
    readable_name='Test2',
    metadata=list('test'=2),
    tdbn
  )
  attempt_fun(FUN=database_add_location,
    NAME='add_location',
    name='TST::TST',
    readable_name='Test',
    metadata=list('test'=3),
    tdbn
  )
}
try({reset_database()},silent=T)
try({reset_database(tdbn)},silent=T)

test_that("default_database_filename",{
  expect_error({default_database_filename()},NA)
  expect_equal({class(default_database_filename())},'character')
})

test_that("Database Reset",{
  expect_error({create_database(tdbn)},NA)
  expect_error({reset_database(tdbn)},NA)
  expect_equal(file.exists(tdbn),FALSE)
  expect_error({reset_database(tdbn)},NA)
  expect_equal(file.exists(tdbn),FALSE)
})

test_that("Database Creation",{
  skip_on_cran()
  expect_error({create_database(tdbn)},NA)
  expect_error({create_database(tdbn)},NA)
})

test_that("Add Location",{
  initialize_database()
  expect_error({database_add_location(name = 'TST', readable_name='Test',metadata = list('test'=1),tdbn)},NA)
  expect_error({database_add_location(name = 'TST2', readable_name='Test', metadata = list('test'=1,test2='foo'),tdbn)},NA)
  expect_error({database_add_location(name = 'TST3', readable_name='Test', metadata = list("test"="John's Data"),tdbn)},NA)
  # Can't have multiple standard locations with the same name
  expect_error({database_add_location(name = 'TST', readable_name='Test', metadata = list('test'=1))})
  # Can't have standard location with no metadata
  expect_error({database_add_location(name = 'TST', readable_name='Test', metadata = NULL)})

  # Custom database

  initialize_database()
  expect_error({
      database_add_location(
          name = 'TST',
          readable_name='Test',
          metadata = list('test'=1),
         dbname=tdbn
      )
  },NA)
  expect_error({
      database_add_location(name = 'TST',readable_name='Test', dbname=fdbn)
  })
})

test_that("Add Hierarchy",{
  initialize_database()
  create_locations()
  expect_error({database_add_location_hierarchy(1,3,1,tdbn)},NA)
  expect_error({database_add_location_hierarchy(1,3,1,tdbn)})
})

test_that("Add Alias",{
  initialize_database()
  create_locations()
  expect_error({database_add_location_alias(1,"Alias",tdbn)},NA)
  expect_error({database_add_location_alias(1,"Alias",tdbn)})
  expect_error({database_add_location_alias(1,"Test",tdbn)},NA)
  expect_error({database_add_location_alias(1,"Test",tdbn)})
  expect_error({database_add_location_alias(3,"Alias",tdbn)},NA)
})

test_that("Add Geometry",{
  initialize_database()
  create_locations()
  skip_if(!require(sf),"Geometry operations require the sf package")
  skip_if(!require(lubridate),"Geometry operations require the lubridate package")
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now() - lubridate::years(1),
      lubridate::now(),
      st_sfc(st_point(c(0, 1))),
      tdbn
    )
  }, NA)
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now() - lubridate::years(1),
      lubridate::now(),
      st_sfc(st_point(c(1, 1))),
      tdbn
    )
  })
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now(),
      lubridate::now() + lubridate::years(1),
      st_sfc(st_point(c(1, 1))),
      tdbn
    )
  }, NA)
  expect_error({
    database_add_location_geometry(
      1,
      lubridate::now() - lubridate::years(1),
      lubridate::now() + lubridate::years(1),
      st_sfc(st_point(c(1, 1))),
      tdbn
    )
  })
  expect_error({
      database_add_location_geometry(
        2,
        lubridate::now() - lubridate::years(1),
        lubridate::now() + lubridate::years(1),
        st_sfc(st_point(c(1, 1))),
        tdbn
      )
    }, NA)
})

test_that("Retrieve ID", {
  expect_equal(get_database_id_from_name("TST", dbname = tdbn), 1)
  expect_equal(get_database_id_from_name("TST2", dbname = tdbn), 2)
  expect_equal(get_database_id_from_name("TST::TST", dbname = tdbn), 3)
  expect_equal(
    get_database_id_from_name(c("TST", "TST::TST", "TST2"), dbname = tdbn),
    setNames(c(1, 3, 2), c("TST", "TST::TST", "TST2"))
  )
})
