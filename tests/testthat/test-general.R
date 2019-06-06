# Test that the package handles a few simple use cases.

context("General Use Case Tests")

# Startup
tdbn <- "testdatabase.sqlite"
fdbn <- "fakedatabase.sqlite"
attempt_fun <- function(FUN,NAME,...){
  tryCatch(
    FUN(...),
    error = function(e){
      skip_if(TRUE, message = paste(NAME, "failed with error", e$message))
    }
  )
}
initialize_database <-  function(){
  attempt_fun(reset_database, "reset_database", dbname = tdbn)
  attempt_fun(create_database, "create_database", dbname = tdbn)
}
try({reset_database()},silent=T)
try({reset_database(tdbn)},silent=T)

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
