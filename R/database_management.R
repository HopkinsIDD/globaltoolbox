default_database_filename = function(){
    system.file("extdata","globaltoolbox.sqlite",package = "globaltoolbox")
}

#' @export
#' @description Setup the database for holding the location tree and shapefiles.
#'   Requires postgres database already set up.  For instructions on how to set
#'   up a postgres database, see https://www.r-bloggers.com/getting-started-with-postgresql-in-r
#' @param dbname The name of the database to connect to
#' @param ... Other parameters for RPostgreSQL::dbConnect
reset_database <- function(dbname = default_database_filename()){
    ## Create Tables
    
    #' @importFrom RSQLite SQLite
    #' @importFrom DBI dbConnect
    con <- dbConnect(drv=SQLite(),dbname)
    #' @importFrom DBI dbSendQuery dbClearResult
    dbClearResult(dbSendQuery(con, "DROP TABLE locations"))
    dbClearResult(dbSendQuery(con, "DROP TABLE location_hierarchy"))
    dbClearResult(dbSendQuery(con, "DROP TABLE location_aliases"))
    dbClearResult(dbSendQuery(con, "DROP TABLE location_geometries"))
}

#' @export
#' @description Setup the database for holding the location tree and shapefiles.
#'   Requires postgres database already set up.  For instructions on how to set
#'   up a postgres database, see https://www.r-bloggers.com/getting-started-with-postgresql-in-r
#' @param dbname The name of the database to connect to
#' @param ... Other parameters for RPostgreSQL::dbConnect
create_database <- function(dbname = default_database_filename(),...){
    ## Create Tables

    #' @importFrom RSQLite SQLite
    #' @importFrom DBI dbConnect
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
    return()
}

#' @description Wrapper for the sql code to create a location.  This function should not be called directly in most circumstances.  See database_add_descendent and database_add_alias instead.
database_add_location <- function(name, readable_name,metadata=NULL,dbname = default_database_filename()){
    #' @importFrom RSQLite SQLite
    #' @importFrom DBI dbConnect
    con <- dbConnect(drv=SQLite(),dbname)
    #' @importFrom jsonlite toJSON
    metadata <- as.character(toJSON(metadata))
    query <- "INSERT INTO locations
        (name,readable_name,metadata)
      VALUES
        ({name},{readable_name},{metadata})"
    #' @importFrom DBI dbGetQuery dbClearResult
    #' @importFrom glue glue_sql
    dbClearResult(dbSendQuery(con,glue_sql(.con=con,query)))
    # dbClearResult(dbSendQuery(con, query))
    rc <- dbGetQuery(con, 'SELECT DISTINCT last_insert_rowid() FROM locations')
    return(return(rc))
}

#' @description Wrapper for the sql code to create a relationship in the location hierarchy.  This function should not be called directly in most circumstances.  See database_add_descendent and database_add_alias instead.
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
    return()
}

#' @description Wrapper for the sql code to create a geometry associated with a location at a time period.
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
    return()
}

#' @description Wrapper for the sql code to create an alias for a location.
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
    return()
}

#' @export
#' @description Get the id of a location by looking up it's name.
#' @param name The name of the location to search for
#' @param source A standardized location name or id number.  If not NULL, the search will be limited to locations within the source
#' @param depth How deep under the source should the search extent. (Not yet implemented)
#' @param dbname The name of the database.  Defaults to the database associated with the package
database_lookup_location_by_name <- function(name,source=NULL,standard=TRUE,depth=NA,dbname = default_database_filename()){
    #' @importFrom RSQLite SQLite
    #' @importFrom DBI dbConnect
    con <- dbConnect(drv=SQLite(),dbname)
    if(!is.na(depth)){warning("The depth argument has not been implemented yet.")}

    query = "SELECT id FROM locations"
    if(!is.null(source)){
        parent_id = as.numeric(source)
        if(is.na(parent_id)){
            parent_id = database_lookup_location_by_name(name=source,standard=TRUE,dbname=dbname,source=NULL)
        }
        query = paste0(query," inner join location_hierarchy ON location_hierarchy.descendent_id = locations.id where parent_id ='",parent_id,"' and")
    } else {
        query = paste0(query,' where')
    }
    query = paste0(query," name is '",name,"'")
    if(!standard){
        stop("This is not yet working")
        # query = paste0(query,' and standard = 1')
    }
    
    #' @importFrom DBI dbGetQuery
    rc = dbGetQuery(con,query)
    if(length(rc) != 1){
        stop(paste("Ambiguous location",name,"has",length(rc),"location_ids"))
    }
    return(rc$id)
}

#' @description Add a new location as a descendent of another location and update the location hierarchy.
database_add_descendent <- function(standardized_name,standardized_descendent_name,readable_descendent_name,metadata,dbname=default_database_filename()){
    #' @importFrom RSQLite SQLite
    #' @importFrom DBI dbConnect
    con <- dbConnect(drv=SQLite(),dbname)
    if(!is.null(standardized_name)){
        parent_id = database_lookup_location_by_name(name=standardized_name,source=NULL,dbname=dbname)
    }
    descendent_id = database_add_location(
        name=standardized_descendent_name,
        readable_name=readable_descendent_name,
        metadata=metadata,dbname=dbname
    )
    database_add_location_hierarchy(descendent_id,descendent_id,0,dbname=dbname)
    if(!is.null(standardized_name)){
        ## Add all ancestors of parent as ancestors here
        query = "INSERT INTO location_hierarchy
          (parent_id,descendent_id, depth)
            SELECT
              parent_id,
              {descendent_id} as descendent_id,
              depth+1 as depth
            FROM
              location_hierarchy
            WHERE
              descendent_id == {parent_id}"
        #' @importFrom DBI dbSendQuery dbClearResult
        #' @importFrom glue glue_sql
        dbClearResult(dbSendQuery(con,glue_sql(.con=con,query)))
    }
    return(descendent_id)
}
