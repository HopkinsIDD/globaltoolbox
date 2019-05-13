default_database_filename = function(){
  system.file("extdata","globaltoolbox.sqlite",package = "globaltoolbox")
}

#' @name reset_database
#' @description Setup the database for holding the location tree and shapefiles.
#'   Requires postgres database already set up.  For instructions on how to set
#'   up a postgres database, see https://www.r-bloggers.com/getting-started-with-postgresql-in-r
#' @param dbname The name of the database to connect to
#' @export
reset_database <- function(dbname = default_database_filename()){
  ## Create Tables
  
  #' @importFrom RSQLite SQLite
  #' @importFrom DBI dbConnect
  if(file.exists(dbname)){
    file.remove(dbname)
  }
  return()
}

#' @name create_database
#' @description Setup the database for holding the location tree and shapefiles.
#'   Requires postgres database already set up.  For instructions on how to set
#'   up a postgres database, see https://www.r-bloggers.com/getting-started-with-postgresql-in-r
#' @param dbname The name of the database to connect to
#' @param ... Other parameters for RPostgreSQL::dbConnect
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @importFrom RSQLite dbDisconnect
#' @export
create_database <- function(dbname = default_database_filename()){
  ## Create Tables

  con <- dbConnect(drv=SQLite(),dbname)
  
  ## The first table holds the locations and any metadata
  ## | Name          | Type               | Description                                        | Constraints |
  ## |---------------|--------------------|----------------------------------------------------|-------------|
  ## | id            | SERIAL PRIMARY KEY | A unique id per location                           | SERIAL      |
  ## | name          | text               | A name for the location                            | NOT NULL    |
  ## | readable name | text               | A human readable name for the location             | NOT NULL    |
  ## | metadata      | blob (json)        | this is a json object with any additional metadata | NOT NULL    |
  
  #' @importFrom DBI dbSendQuery dbClearResult
  dbClearResult(dbSendQuery(con, "CREATE TABLE IF NOT EXISTS locations(
      id INTEGER PRIMARY KEY,
      name text UNIQUE NOT NULL,
      readable_name text NOT NULL,
      metadata blob NOT NULL
    );"))
  
  ## The second table holds the tree structure of containment
  ## | Name          | Type    | Description                                         | Constraints                                   |
  ## |---------------|---------|-----------------------------------------------------|-----------------------------------------------|
  ## | parent_id     | integer | The id of the parent of this part of the tree       | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | descendent_id | integer | The id of the descendent of this part of the tree   | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | depth         | integer | How many tree nodes are in the path connecting them | NOT NULL                                      |
  #' @importFrom DBI dbSendQuery dbClearResult
  dbClearResult(dbSendQuery(con, "CREATE TABLE IF NOT EXISTS location_hierarchy(
      parent_id integer NOT NULL,
      descendent_id integer NOT NULL,
      depth integer NOT NULL,
      PRIMARY KEY(parent_id,descendent_id)
      FOREIGN KEY(parent_id) REFERENCES locations(id),
      FOREIGN KEY(descendent_id) REFERENCES locations(id)
    );"))

  ## The third table holds the geometric information
  ## | Name        | Type           | Description                                           | Constraints                                   |
  ## |-------------|----------------|-------------------------------------------------------|-----------------------------------------------|
  ## | id          | SERIAL         | A unique id per shapefile                             |                                               |
  ## | location_id | integer        | Which location this is a shapefile for                | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  ## | time_left   | date           | The first time this shapefile represents the location | NOT NULL                                      |
  ## | time_right  | date           | The last time this shapefile represents the location  | NOT NULL                                      |
  ## | geometry    | blob (geojson) | The geometry this row represents                      | NOT NULL                                      |
  #' @importFrom DBI dbSendQuery dbClearResult

  dbClearResult(dbSendQuery(con, "CREATE TABLE IF NOT EXISTS location_geometries(
      id integer PRIMARY KEY,
      location_id integer NOT NULL,
      time_left date NOT NULL,
      time_right date NOT NULL CHECK (time_left <= time_right),
      geometry blob NOT NULL,
      UNIQUE(location_id,time_left,time_right)
      FOREIGN KEY(location_id) REFERENCES locations(id)
    );"))
  
  ## The first table holds the locations and any metadata
  ## | Name         | Type               | Description                                        | Constraints                                   |
  ## |--------------|--------------------|----------------------------------------------------|-----------------------------------------------|
  ## | id           | SERIAL PRIMARY KEY | A unique id per location                           | SERIAL                                        |
  ## | alias        | text               | A name for the location                            | NOT NULL                                      |
  ## | location_id  | blob (json)        | this is a json object with any additional metadata | NOT NULL FOREIGN KEY REFERENCES locations(id) |
  #' @importFrom DBI dbSendQuery dbClearResult
  dbClearResult(dbSendQuery(con, "CREATE TABLE IF NOT EXISTS location_aliases(
      alias text,
      location_id integer NOT NULL,
      PRIMARY KEY(alias,location_id)
      FOREIGN KEY(location_id) REFERENCES locations(id)
    );"))
  ## Populate with WHO regions
                                        # database_add_descendent(standardized_descendent_name = "AFR",readable_descendent_name='Africa',standardized_name=NULL,metadata = list('type'='WHO Region'),dbname = dbname)
                                        # database_add_descendent(standardized_descendent_name = "AMR",readable_descendent_name='Americas',standardized_name=NULL,metadata = list('type'='WHO Region'),dbname = dbname)
                                        # database_add_descendent(standardized_descendent_name = "EMR",readable_descendent_name='Eastern Mediterranean',standardized_name=NULL,metadata = list('type'='WHO Region'),dbname = dbname)
                                        # database_add_descendent(standardized_descendent_name = "EUR",readable_descendent_name='Europe',standardized_name=NULL,metadata = list('type'='WHO Region'),dbname = dbname)
                                        # database_add_descendent(standardized_descendent_name = "SEAR",readable_descendent_name='Southeast Asia',standardized_name=NULL,metadata = list('type'='WHO Region'),dbname = dbname)
                                        # database_add_descendent(standardized_descendent_name = "WPR",readable_descendent_name='Western Pacific',standardized_name=NULL,metadata = list('type'='WHO Region'),dbname = dbname)
  dbDisconnect(con)
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
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @importFrom jsonlite toJSON
#' @importFrom DBI dbGetQuery dbClearResult
#' @importFrom glue glue_sql
#' @importFrom RSQLite dbDisconnect
#' @export
database_add_location <- function(name, readable_name,metadata=NULL,dbname = default_database_filename()){
  if(is.numeric(name)){stop("This should not happen")}
  #' @importFrom DBI dbConnect
  con <- dbConnect(drv=SQLite(),dbname)
  metadata <- as.character(toJSON(metadata))
  query <- "INSERT INTO locations
        (name,readable_name,metadata)
      VALUES
        ({name},{readable_name},{metadata})"

  dbClearResult(dbSendQuery(con,glue_sql(.con=con,query)))
                                        # dbClearResult(dbSendQuery(con, query))
  rc <- dbGetQuery(con, 'SELECT DISTINCT last_insert_rowid() FROM locations')
  dbDisconnect(con)
  return(return(rc))
}

#' @name database_add_location_hierarchy
#' @description Wrapper for the sql code to create a relationship in the location hierarchy.  This function should not be called directly in most circumstances.  See database_add_descendent and database_add_alias instead.
#' @importFrom RSQLite dbDisconnect
#' @export
database_add_location_hierarchy <- function(parent_id, descendent_id,depth,dbname = default_database_filename()){
  #' @importFrom RSQLite SQLite
  #' @importFrom DBI dbConnect
  con <- dbConnect(drv=SQLite(),dbname)

  query <- "INSERT INTO location_hierarchy
      (parent_id, descendent_id,depth)
    VALUES ({parent_id},{descendent_id},{depth})"
  #' @importFrom DBI dbSendQuery dbClearResult
  #' @importFrom glue glue_sql
  dbClearResult(dbSendQuery(con,glue_sql(.con=con,query)))
  dbDisconnect(con)
  return()
}

#' @name database_add_location_geometry
#' @description Wrapper for the sql code to create a geometry associated with a location at a time period.
#' @importFrom RSQLite dbDisconnect
#' @export
database_add_location_geometry <- function(location_id, time_left, time_right, geometry,dbname = default_database_filename()){
  #' @importFrom RSQLite SQLite
  #' @importFrom DBI dbConnect
  con <- dbConnect(drv=SQLite(),dbname)
  #' @importFrom geojsonsf sfc_geojson
  geometry = sfc_geojson(geometry)
  query = "INSERT INTO location_geometries
      (location_id, time_left, time_right, geometry)
    VALUES
      ({location_id},{time_left},{time_right},{geometry})"
  #' @importFrom DBI dbSendQuery dbClearResult
  #' @importFrom glue glue_sql
  dbClearResult(dbSendQuery(con,glue_sql(.con=con,query)))
  dbDisconnect(con)
  return()
}

#' @name database_add_location_alias
#' @description Wrapper for the sql code to create an alias for a location.
#' @importFrom RSQLite dbDisconnect
#' @export
database_add_location_alias <- function(location_id, alias,dbname = default_database_filename()){
  #' @importFrom RSQLite SQLite
  #' @importFrom DBI dbConnect
  con <- dbConnect(drv=SQLite(),dbname)
  query = "INSERT INTO location_aliases
      (location_id, alias)
    VALUES
      ({location_id},{alias})"
  #' @importFrom DBI dbSendQuery dbClearResult
  #' @importFrom glue glue_sql
  dbClearResult(dbSendQuery(con,glue_sql(.con=con,query)))
  dbDisconnect(con)
  return()
}

#' @name get_database_id_from_name
#' @title get_database_id_from_name
#' @description Get the id of a location by looking up it's name.
#' @param name Standardized name of a location
#' @param dbname Name of the database. Defaults to default location.
#' @importFrom RSQLite dbDisconnect
#' @export
get_database_id_from_name <- function(name,dbname = default_database_filename()){
  if(length(name) > 1){return(sapply(name,get_database_id_from_name,dbname=dbname))}
  con <- dbConnect(drv=SQLite(),dbname)
  query = "SELECT
    id
  FROM locations
    WHERE name is {name}"
  rc <- dbGetQuery(con,glue_sql(.con=con,query))

  if(length(rc$id) != 1){
    stop(paste("Ambiguous location",name,"has",nrow(rc),"location_ids"))
  }
  dbDisconnect(con)
  return(rc$id)
}
