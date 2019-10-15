#' @include string_manipulation.R

#' @name default_database_filename
#' @title default_database_filename
#' @description Return the filename of the default database
#' @return A filename as a character
#' @export
default_database_filename <- function(){
  return("globaltoolbox")
  system.file("extdata", "globaltoolbox.sqlite", package = "globaltoolbox")
}



#' @name reset_database
#' @title reset_database
#' @description Setup the database for holding the location tree and shapefiles.
#' @param dbname The name of the database to connect to
#' @export
reset_database <- function(dbname = default_database_filename(),...){
  ## Create Tables
  # if (file.exists(dbname)){
  #   file.remove(dbname)
  # }
  # file.create(dbname)
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  suppressWarnings(DBI::dbClearResult(
    DBI::dbSendQuery(con, "DROP TABLE IF EXISTS location_aliases;")
  ))
  suppressWarnings(DBI::dbClearResult(
    DBI::dbSendQuery(con, "DROP TABLE IF EXISTS location_geometries;")
  ))
  suppressWarnings(DBI::dbClearResult(
    DBI::dbSendQuery(con, "DROP TABLE IF EXISTS location_hierarchy;")
  ))
  suppressWarnings(DBI::dbClearResult(
    DBI::dbSendQuery(con, "DROP TABLE IF EXISTS locations;")
  ))
  return()
}



#' @name create_database
#' @title create_database
#' @description Setup the database for holding the location tree and shapefiles.
#' @param dbname The name of the database to connect to
#' @param ... Other parameters for RPostgres::dbConnect
#' @export
create_database <- function(dbname = default_database_filename(),...){
  # if (!file.exists(dbname)){
  #   file.create(dbname)
  # }
  ## Create Tables

  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({

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
         id SERIAL PRIMARY KEY,
         name text UNIQUE NOT NULL,
         readable_name text NOT NULL,
         metadata jsonb NOT NULL
       );"
    ))

  ## nolint start
  ## The second table holds the tree structure of containment
  ## | Name          | Type    | Description                                         | Constraints                                   |
  ## |---------------|---------|-----------------------------------------------------|-----------------------------------------------|
  ## | parent_id     | integer | The id of the parent of this part of the tree       | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | descendant_id | integer | The id of the descendant_id of this part of the tree   | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | depth         | integer | How many tree nodes are in the path connecting them | NOT NULL                                      |
  ## nolint end

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TABLE IF NOT EXISTS location_hierarchy(
         parent_id integer NOT NULL,
         descendant_id integer NOT NULL,
         depth integer NOT NULL,
         PRIMARY KEY(parent_id,descendant_id),
         FOREIGN KEY(parent_id) REFERENCES locations(id),
         FOREIGN KEY(descendant_id) REFERENCES locations(id)
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
  ## | geometry    | geometry       | The geometry this row represents                      | NOT NULL                                      |
  ## nolint end
  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TABLE IF NOT EXISTS location_geometries(
         id SERIAL PRIMARY KEY,
         location_id integer NOT NULL,
         time_left date NOT NULL,
         time_right date NOT NULL CHECK (time_left <= time_right),
         geometry geometry NOT NULL,
         UNIQUE(location_id,time_left,time_right),
         FOREIGN KEY(location_id) REFERENCES locations(id)
       );"
    )
  )
##   DBI::dbClearResult(
##     DBI::dbSendQuery(
##       con,
##       "CREATE TRIGGER no_overlaps BEFORE INSERT
##        ON location_geometries
##        $BODY$
##        BEGIN
##          SELECT CASE
##          WHEN EXISTS(
##            SELECT location_id,time_left,time_right
##            FROM location_geometries as OLD
##            WHERE
##              OLD.location_id = NEW.location_id AND
##              OLD.time_left < NEW.time_right AND
##              OLD.time_right >= NEW.time_right
##          )
##          THEN
##            RAISE(ABORT, 'overlapping intervals')
##          END;
##          SELECT CASE
##          WHEN EXISTS(
##            SELECT location_id,time_left,time_right
##            FROM location_geometries as OLD
##            WHERE
##              OLD.location_id = NEW.location_id AND
##              OLD.time_left <= NEW.time_left AND
##              OLD.time_right > NEW.time_left
##          )
##          THEN
##            RAISE(ABORT, 'overlapping intervals')
##          END;
##        END;
##        $BODY$;"
##     )
##   )

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
         alias text NOT NULL,
         location_id integer NOT NULL,
         PRIMARY KEY(alias,location_id),
         FOREIGN KEY(location_id) REFERENCES locations(id)
       );"
    )
  )
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
  ## Populate with a root
  RSQLite::dbDisconnect(con)
  suppressWarnings(try({
    loc_id <- database_add_location("", "", NULL, dbname = dbname,...)
    database_add_hierarchy(loc_id, loc_id, 0, dbname = dbname,...)
    database_add_location_alias(loc_id, "", dbname = dbname,...)
  },
  silent = T))
  return()
}



#' @name database_add_location
#' @title database_add_location
#' @description Wrapper for the sql code to create a location.  This function should not be called directly in most circumstances.  See database_add_descendant and database_add_alias instead.
#' @param name location name to add
#' @param readable_name common readable name for the location
#' @param metadata Additional data that may be useful to identify the location name.
#' @param dbname database name to put location information.
database_add_location <- function(
  name,
  readable_name,
  metadata = NULL,
  dbname = default_database_filename(),
  ...
){
  rc <- as.integer(NA)
  if (is.numeric(name)){
    stop("This should not happen")
  }
  name <- standardize_location_strings(name)
  metadata <- as.character(jsonlite::toJSON(metadata))
  query <- "INSERT INTO locations
      (name,readable_name,metadata)
    VALUES
      ({name},{readable_name},{metadata})
    RETURNING
      id"

  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
  RSQLite::dbDisconnect(con)
  return(rc)
}



#' @name database_add_location_hierarchy
#' @title database_add_location_hierarchy
#' @description Wrapper for the sql code to create a relationship in the location hierarchy.  This function should not be called directly in most circumstances.  See database_add_descendant and database_add_alias instead.
#' @param parent_id The id of the parent in the relationship.  The id is with respect to the locations table in the database
#' @param descendant_id The id of the descendant_id in the relationship.  The id is with respect to the locations table in the database
#' @param depth The distance between the parent and the descendant_id along the tree.  Should be one more than the number of intermediate locations between the parent and child.
#' @param dbname database name to put location information.
database_add_hierarchy <- function(
  parent_id,
  descendant_id,
  depth,
  dbname = default_database_filename(),...
){
  query <- "INSERT INTO location_hierarchy
      (parent_id, descendant_id,depth)
    VALUES ({parent_id},{descendant_id},{depth})"
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con = con, query)))
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
  RSQLite::dbDisconnect(con)
  return()
}



#' @name database_add_location_geometry
#' @title database_add_location_geometry
#' @description Wrapper for the sql code to create a geometry associated with a location at a time period.
#' @param location_id The id of the location to add a geometry for.  Should be with respect to the locations table.
#' @param time_left The first time this geometry is valid for.  Should be a Date object.
#' @param time_right The last time this geometry is valid for.  Should be a Date object.
#' @param geometry The geometry to add to the database for the given time period.  Should be an sfc object.
#' @param dbname database name to put location information.
database_add_location_geometry <- function(
  location_id,
  time_left,
  time_right,
  geometry,
  dbname = default_database_filename(),
  ...
){
  sf::st_as_text
  query <- "INSERT INTO location_geometries
      (location_id, time_left, time_right, geometry)
    VALUES
      ({location_id},{time_left},{time_right},{sf::st_as_text(geometry)})"
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con = con, query)))
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
  RSQLite::dbDisconnect(con)
  return()
}



#' @name database_add_location_alias
#' @title database_add_location_alias
#' @description Wrapper for the sql code to create an alias for a location.
#' @param location_id The id of the location to add an alias for.  Should be with respect to the locations table.
#' @param alias The alias to add.
#' @param dbname database name to put location information.
database_add_location_alias <- function(
  location_id,
  alias,
  dbname = default_database_filename(),
  ...
){
  alias <- standardize_location_strings(alias)
  query <- "INSERT INTO location_aliases
      (location_id, alias)
    VALUES
      ({location_id},{alias})"
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con = con, query)))
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
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
  dbname = default_database_filename(),
  ...
){
  if (length(name) > 1){
    return(sapply(name, get_database_id_from_name, dbname = dbname,...))
  }
  query <- "SELECT
    id
  FROM locations
    WHERE name = {name}"
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
  RSQLite::dbDisconnect(con)

  if (length(rc$id) != 1){
    stop(paste("Ambiguous location", name, "has", nrow(rc), "location_ids"))
  }
  return(rc$id)
}



#' @name database_merge_locations
#' @title database_merge_locations
#' @description combine two locations by replacing all references to one with the other, and adding the first as an alias for the second.
#' @param from The name of the location to merge into another location (this location will be deleted)
#' @param to The name of the location to merge another location into (this location will contain all data for both locations)
#' @param dbname Name of the database. Defaults to default location.
database_merge_locations <- function(
  from,
  to,
  dbname = default_database_filename(),
  ...
){

  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)

  if(class(from) == 'character'){
    from <- get_database_id_from_name(from, dbname)
  }
  if(class(to) == 'character'){
    to <- get_database_id_from_name(to, dbname)
  }
  add_query <- "UPDATE OR IGNORE {`table_name`}
    SET
      {`location_field`} = {to}
    WHERE
      {`location_field`} = {from}"
  drop_query <- "DELETE FROM {`table_name`} WHERE {`location_field`} = {from}"

  all_iterations <- tibble::tibble(
    table_name = c(    "location_aliases", "location_hierarchy", "location_hierarchy", "location_geometries"),
    link_field = c(    "alias",            "parent_id",          "descendant_id",      "id"),
    location_field = c("location_id",      "descendant_id",      "parent_id",          "location_id"),
  )

  for(it in 1:nrow(all_iterations)){
    ## These variables are used, but the lintr doesn't see that use
    table_name <- all_iterations$table_name[it]
    link_field <- all_iterations$link_field[it]
    location_field <- all_iterations$location_field[it]
    tryCatch({
      DBI::dbClearResult(DBI::dbSendQuery(
        con,
        glue::glue_sql(.con = con, add_query)
      ))
      DBI::dbClearResult(DBI::dbSendQuery(
        con,
        glue::glue_sql(.con = con, drop_query)
      ))
    },
    error = function(e){
      RSQLite::dbDisconnect(con)
      stop(e$message)
    })
    table_name <- "locations"
    location_field <- "id"
    tryCatch({
      DBI::dbClearResult(DBI::dbSendQuery(
        con,
        glue::glue_sql(.con = con, drop_query)
      ))
    },
    error = function(e){
      RSQLite::dbDisconnect(con)
      stop(e$message)
    })
  }

  RSQLite::dbDisconnect(con)
  return()
}
