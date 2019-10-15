# Startup
tdbn <- "testdatabase"
fdbn <- "fakedatabase"
attempt_fun <- function(FUN, NAME, ...){
  tryCatch(
    FUN(...),
    error = function(e){
      skip_if(TRUE, paste(NAME, "failed with error", e$message))
    }
  )
}
initialize_database <-  function(){
  attempt_fun(create_database, "create_database", dbname = tdbn)
  attempt_fun(reset_database, "reset_database", dbname = tdbn)
  attempt_fun(create_database, "create_database", dbname = tdbn)
}
create_locations <- function(){
  attempt_fun(
    FUN = database_add_location,
    NAME = "add_location",
    name = "::tst",
    readable_name = "Test",
    metadata = list("test" = 1),
    tdbn
  )
  attempt_fun(
    FUN = database_add_location,
    NAME = "add_location",
    name = "::tst2",
    readable_name = "Test2",
    metadata = list("test" = 2),
    tdbn
  )
  attempt_fun(FUN = database_add_location,
    NAME = "add_location",
    name = "::tst::tst",
    readable_name = "Test",
    metadata = list("test" = 3),
    tdbn
  )
  attempt_fun(FUN = database_add_location,
    NAME = "add_location",
    name = "::tst2::tst",
    readable_name = "Test 2",
    metadata = list("test2" = 3),
    tdbn
  )
}
create_hierarchy <- function(){
  attempt_fun(
    FUN = database_add_hierarchy,
    NAME = "add_hierarchy",
    parent_id = 1,
    descendant_id = 2,
    depth = 1,
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_hierarchy,
    NAME = "add_hierarchy",
    parent_id = 1,
    descendant_id = 3,
    depth = 1,
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_hierarchy,
    NAME = "add_hierarchy",
    parent_id = 1,
    descendant_id = 4,
    depth = 2,
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_hierarchy,
    NAME = "add_hierarchy",
    parent_id = 2,
    descendant_id = 2,
    depth = 0,
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_hierarchy,
    NAME = "add_hierarchy",
    parent_id = 3,
    descendant_id = 3,
    depth = 0,
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_hierarchy,
    NAME = "add_hierarchy",
    parent_id = 4,
    descendant_id = 4,
    depth = 0,
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_hierarchy,
    NAME = "add_hierarchy",
    parent_id = 2,
    descendant_id = 4,
    depth = 1,
    dbname = tdbn
  )
}
create_test_database <- function(){
  attempt_fun(
    FUN = database_add_descendant_id,
    NAME = "add_descendant_id",
    standardized_parent_name = "",
    readable_descendant_id_name = "tst",
    metadata = list("test" = 1,"test3" = 1),
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_descendant_id,
    NAME = "add_descendant_id",
    standardized_parent_name = "",
    readable_descendant_id_name = "tst2",
    metadata = list("test" = 2, "test2"= 1),
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_descendant_id,
    NAME = "add_descendant_id",
    standardized_parent_name = "::tst",
    readable_descendant_id_name = "tst3",
    metadata = list("test" = 3),
    dbname = tdbn
  )
  attempt_fun(
    FUN = database_add_descendant_id,
    NAME = "add_descendant_id",
    standardized_parent_name = "::tst2",
    readable_descendant_id_name = "tst3",
    metadata = list("test" = 3),
    dbname = tdbn
  )
}

try({
    reset_database()
  },
  silent = T
)
try({
    reset_database(tdbn)
  },
  silent = T
)

create_names_b_data <- function(){
  names <- c(
    "test",
    "test",
    "test",
    "usa",
    "baltimore",
    "baltimore",
    "tanzania",
    "md",
    "tza",
    "bal",
    "bra",
    "phiction"
  )
  depths <- c(
    1,
    1,
    1,
    1,
    1,
    2,
    1,
    1,
    1,
    1,
    1,
    1
  )

  return(data.frame(
    id = 1:length(names),
    name = names,
    depth = depths,
    id_tmp = 1:length(names)
  ))
}
