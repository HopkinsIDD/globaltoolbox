#' @include string_manipulation.R
default_database_filename <- function(){
  system.file("extdata", "globaltoolbox.sqlite", package = "globaltoolbox")
}



#' @name reset_database
#' @title reset_database
#' @description Setup the database for holding the location tree and shapefiles.
#'   Requires postgres database already set up.  For instructions on how to set
#'   up a postgres database, see https://www.r-bloggers.com/getting-started-with-postgresql-in-r
#' @param dbname The name of the database to connect to
#' @export
reset_database <- function(dbname = default_database_filename()){
  ## Create Tables
  if (file.exists(dbname)){
    file.remove(dbname)
  }
  file.create(dbname)
  return()
}



#' @name create_database
#' @title create_database
#' @description Setup the database for holding the location tree and shapefiles.
#'   Requires postgres database already set up.  For instructions on how to set
#'   up a postgres database, see https://www.r-bloggers.com/getting-started-with-postgresql-in-r
#' @param dbname The name of the database to connect to
#' @param ... Other parameters for RPostgreSQL::dbConnect
#' @export
create_database <- function(dbname = default_database_filename()){
  if (!file.exists(dbname)){
    file.create(dbname)
  }
  ## Create Tables

  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)

  ## nolint start
  ## The first table holds the locations and any metadata
  ##  Name          | Type               | Description                                        | Constraints
  ##  id            | SERIAL PRIMARY KEY | A unique id per location                           | SERIAL
  ##  name          | text               | A name for the location                            | NOT NULL
  ##  readable name | text               | A human readable name for the location             | NOT NULL
  ##  metadata      | blob (json)        | this is a json object with any additional metadata | NOT NULL
  ## nolint end

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TABLE IF NOT EXISTS locations(
         id INTEGER PRIMARY KEY,
         name text UNIQUE NOT NULL,
         readable_name text NOT NULL,
         metadata blob NOT NULL
       );"
    ))

  ## nolint start
  ## The second table holds the tree structure of containment
  ## | Name          | Type    | Description                                         | Constraints                                   |
  ## |---------------|---------|-----------------------------------------------------|-----------------------------------------------|
  ## | parent_id     | integer | The id of the parent of this part of the tree       | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | descendent_id | integer | The id of the descendent of this part of the tree   | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | depth         | integer | How many tree nodes are in the path connecting them | NOT NULL                                      |
  ## nolint end

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TABLE IF NOT EXISTS location_hierarchy(
         parent_id integer NOT NULL,
         descendent_id integer NOT NULL,
         depth integer NOT NULL,
         PRIMARY KEY(parent_id,descendent_id)
         FOREIGN KEY(parent_id) REFERENCES locations(id),
         FOREIGN KEY(descendent_id) REFERENCES locations(id)
       );"
    )
  )

  ## nolint start
  ## The third table holds the geometric information
  ## | Name        | Type           | Description                                           | Constraints                                   |
  ## |-------------|----------------|-------------------------------------------------------|-----------------------------------------------|
  ## | id          | SERIAL         | A unique id per shapefile                             |                                               |
  ## | location_id | integer        | Which location this is a shapefile for                | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | time_left   | date           | The first time this shapefile represents the location | NOT NULL                                      |
  ## | time_right  | date           | The last time this shapefile represents the location  | NOT NULL                                      |
  ## | geometry    | blob (geojson) | The geometry this row represents                      | NOT NULL                                      |
  ## nolint end
  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TABLE IF NOT EXISTS location_geometries(
         id integer PRIMARY KEY,
         location_id integer NOT NULL,
         time_left date NOT NULL,
         time_right date NOT NULL CHECK (time_left <= time_right),
         geometry blob NOT NULL,
         UNIQUE(location_id,time_left,time_right)
         FOREIGN KEY(location_id) REFERENCES locations(id)
       );"
    )
  )
  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TRIGGER IF NOT EXISTS no_overlaps BEFORE INSERT
       ON location_geometries
       BEGIN
         SELECT CASE
         WHEN EXISTS(
           SELECT location_id,time_left,time_right
           FROM location_geometries as OLD
           WHERE
             OLD.location_id = NEW.location_id AND
             OLD.time_left < NEW.time_right AND
             OLD.time_right >= NEW.time_right
         )
         THEN
           RAISE(ABORT, 'overlapping intervals')
         END;
         SELECT CASE
         WHEN EXISTS(
           SELECT location_id,time_left,time_right
           FROM location_geometries as OLD
           WHERE
             OLD.location_id = NEW.location_id AND
             OLD.time_left <= NEW.time_left AND
             OLD.time_right > NEW.time_left
         )
         THEN
           RAISE(ABORT, 'overlapping intervals')
         END;
       END"
    )
  )

  ## nolint start
  ## The first table holds the locations and any metadata
  ## | Name         | Type               | Description                                        | Constraints                                   |
  ## |--------------|--------------------|----------------------------------------------------|-----------------------------------------------|
  ## | id           | SERIAL PRIMARY KEY | A unique id per location                           | SERIAL                                        |
  ## | alias        | text               | A name for the location                            | NOT NULL                                      |
  ## | location_id  | blob (json)        | this is a json object with any additional metadata | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## nolint end

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TABLE IF NOT EXISTS location_aliases(
         alias text,
         location_id integer NOT NULL,
         PRIMARY KEY(alias,location_id)
         FOREIGN KEY(location_id) REFERENCES locations(id)
       );"
    )
  )
  ## Populate with WHO regions
  RSQLite::dbDisconnect(con)
  try({
    loc_id <- database_add_location("", "", NULL, dbname)
    database_add_hierarchy(loc_id, loc_id, 0, dbname = dbname)
    database_add_location_alias(loc_id, "", dbname = dbname)
  },silent=T)
  return()
}



#' @name database_add_location
#' @title database_add_location
#' @description Wrapper for the sql code to create a location.  This function should not be called directly in most circumstances.  See database_add_descendent and database_add_alias instead.
#' @param name location name to add
#' @param readable_name common readable name for the location
#' @param metadata Additional data that may be useful to identify the location name
#' @param dbname database name to put location information.
#' @return standardized database code which can be used to identify other data
#' @export
database_add_location <- function(
  name,
  readable_name,
  metadata=NULL,
  dbname = default_database_filename()
){
  if (is.numeric(name)){
    stop("This should not happen")
  }
  name <- standardize_location_strings(name)
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  metadata <- as.character(jsonlite::toJSON(metadata))
  query <- "INSERT INTO locations
      (name,readable_name,metadata)
    VALUES
      ({name},{readable_name},{metadata})"

  DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con = con, query)))
  rc <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT last_insert_rowid() FROM locations"
  )
  RSQLite::dbDisconnect(con)
  return(return(rc))
}



#' @name database_add_location_hierarchy
#' @title database_add_location_hierarchy
#' @description Wrapper for the sql code to create a relationship in the location hierarchy.  This function should not be called directly in most circumstances.  See database_add_descendent and database_add_alias instead.
#' @export
database_add_hierarchy <- function(
  parent_id,
  descendent_id,
  depth, dbname = default_database_filename()
){
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  query <- "INSERT INTO location_hierarchy
      (parent_id, descendent_id,depth)
    VALUES ({parent_id},{descendent_id},{depth})"
  DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con = con, query)))
  RSQLite::dbDisconnect(con)
  return()
}



#' @name database_add_location_geometry
#' @title database_add_location_geometry
#' @description Wrapper for the sql code to create a geometry associated with a location at a time period.
#' @export
database_add_location_geometry <- function(
  location_id,
  time_left,
  time_right,
  geometry,
  dbname = default_database_filename()
){
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  geometry <- geojsonsf::sfc_geojson(geometry)
  query <- "INSERT INTO location_geometries
      (location_id, time_left, time_right, geometry)
    VALUES
      ({location_id},{time_left},{time_right},{geometry})"
  DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con = con, query)))
  RSQLite::dbDisconnect(con)
  return()
}



#' @name database_add_location_alias
#' @title database_add_location_alias
#' @description Wrapper for the sql code to create an alias for a location.
#' @export
database_add_location_alias <- function(
  location_id,
  alias,
  dbname = default_database_filename()
){
  alias <- standardize_location_strings(alias)
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  query <- "INSERT INTO location_aliases
      (location_id, alias)
    VALUES
      ({location_id},{alias})"
  DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con = con, query)))
  RSQLite::dbDisconnect(con)
  return()
}



#' @name get_database_id_from_name
#' @title get_database_id_from_name
#' @description Get the id of a location by looking up it's name.
#' @param name Standardized name of a location
#' @param dbname Name of the database. Defaults to default location.
#' @export
get_database_id_from_name <- function(
  name,
  dbname = default_database_filename()
){
  if (length(name) > 1){
    return(sapply(name, get_database_id_from_name, dbname = dbname))
  }
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  query <- "SELECT
    id
  FROM locations
    WHERE name is {name}"
  rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))

  if (length(rc$id) != 1){
    stop(paste("Ambiguous location", name, "has", nrow(rc), "location_ids"))
  }
  RSQLite::dbDisconnect(con)
  return(rc$id)
}

#' @name database_merge_location
#' @title database_merge_locations
#' @description combine two locations by replacing all references to one with the other, and adding the first as an alias for the second.
#' @param from The location id to delete from the database
#' @param to The location id to merge \\code{from} into
#' @param dbname Name of the database. Defaults to default location.
database_merge_locations <- function(
  from,
  to,
  dbname=default_database_filename()
){

  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)

  add_query <- "UPDATE OR IGNORE {`table_name`}
    SET
      {`location_field`} = {to}
    WHERE
      {`location_field`} = {from}"
  drop_query <- "DELETE FROM {`table_name`} WHERE {`location_field`} = {from}"

  all_iterations <- tibble::tibble(
    table_name = c(    "location_aliases", "location_hierarchy", "location_hierarchy", "location_geometries"),
    link_field = c(    "alias",            "parent_id",          "descendent_id",      "id"),
    location_field = c("location_id",      "descendent_id",      "parent_id",          "location_id"),
  )

  for(it in 1:nrow(all_iterations)){
    ## These variables are used, but the lintr doesn't see that use
    table_name <- all_iterations$table_name[it]
    link_field <- all_iterations$link_field[it]
    location_field <- all_iterations$location_field[it]
    location_id <- all_iterations$location_id[it]
    DBI::dbClearResult(DBI::dbSendQuery(
      con,
      glue::glue_sql(.con = con, add_query)
    ))
    DBI::dbClearResult(DBI::dbSendQuery(
      con,
      glue::glue_sql(.con = con, drop_query)
    ))
  }
}
