#' @include string_manipulation.R

#' @export
#' @name default_database_filename
#' @title default_database_filename
#' @description Return the filename of the default database
#' @return A filename as a character
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
    DBI::dbSendQuery(con, "DROP TABLE IF EXISTS location_aliases CASCADE;")
  ))
  suppressWarnings(DBI::dbClearResult(
    DBI::dbSendQuery(con, "DROP TABLE IF EXISTS location_geometries CASCADE;")
  ))
  suppressWarnings(DBI::dbClearResult(
    DBI::dbSendQuery(con, "DROP VIEW IF EXISTS location_hierarchy CASCADE;")
  ))
  suppressWarnings(DBI::dbClearResult(
    DBI::dbSendQuery(con, "DROP TABLE IF EXISTS locations CASCADE;")
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
  ## The third table holds the geometric information
  ## | Name       | Type     | Description                                           | Constraints |
  ## |------------|----------|-------------------------------------------------------|-------------|
  ## | id         | SERIAL   | A unique id per shapefile                             |             |
  ## | name       | text     | A name of for the geometry                            | NOT NULL    |
  ## | time_left  | date     | The first time this shapefile represents the location | NOT NULL    |
  ## | time_right | date     | The last time this shapefile represents the location  | NOT NULL    |
  ## | geometry   | geometry | The geometry this row represents                      | NOT NULL    |
  ## nolint end
  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE TABLE IF NOT EXISTS location_geometries(
         id SERIAL PRIMARY KEY,
         name text NOT NULL,
         readable_name text NOT NULL,
         time_left date,
         time_right date CHECK (time_left <= time_right),
         inner_geometry geometry NOT NULL,
         outer_geometry geometry NOT NULL,
         source text NOT NULL
       );"
    )
  )


  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE INDEX location_geometries_igix ON location_geometries USING GIST(inner_geometry);"
    )
  )
  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE INDEX location_geometries_ogix ON location_geometries USING GIST(outer_geometry);"
    )
  )

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
      "DROP VIEW IF EXISTS location_hierarchy"
    )
  )

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      # "CREATE VIEW location_hierarchy AS
      "CREATE VIEW pre_location_hierarchy AS
      (SELECT
        table_1.id as ancestor,
        table_2.id as descendant,
        table_1.name as ancestor_name,
        table_2.name as descendant_name,
        table_1.inner_geometry as ancestor_geometry,
        table_2.inner_geometry as descendant_geometry,
        greatest(table_1.time_left , table_2.time_left ) as time_left,
        least   (table_1.time_right, table_2.time_right) as time_right
      FROM
        location_geometries as table_1
      LEFT JOIN
        location_geometries as table_2
      ON
        table_1.id <> table_2.id AND
        table_1.time_left  <= table_2.time_right AND
        table_1.time_right >= table_1.time_left
      WHERE
        ST_CONTAINS(table_1.outer_geometry,table_2.inner_geometry) AND
        greatest(table_1.time_left , table_2.time_left ) <= least   (table_1.time_right, table_2.time_right)
      );
      "
    )
  )

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE MATERIALIZED VIEW location_hierarchy as (
        SELECT
          ancestor,
          descendant,
          time_left,
          time_right
        FROM
          pre_location_hierarchy
        WHERE
        (
          st_area(st_difference(descendant_geometry, ancestor_geometry)) /
          st_area(descendant_geometry)
        ) < (0.1)::double precision
      )"
    )
  )

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE VIEW graph_duplicate_locations AS
        (SELECT
          lhs.ancestor as lhs,
          lhs.descendant as rhs
        FROM
          location_hierarchy as lhs
        INNER JOIN
          location_hierarchy as rhs
        ON
          lhs.ancestor = rhs.descendant AND
          lhs.descendant = rhs.ancestor
        WHERE
          lhs.ancestor != lhs.descendant
      );"
    )
  )

  DBI::dbClearResult(
    DBI::dbSendQuery(
      con,
      "CREATE VIEW geometry_duplicate_locations AS
        (SELECT
          lhs.ancestor as lhs,
          lhs.descendant as rhs,
          lhs.ancestor_geometry as lhs_geometry,
          lhs.descendant_geometry as rhs_geometry
        FROM
          pre_location_hierarchy as lhs
        INNER JOIN
          pre_location_hierarchy as rhs
        ON
          lhs.ancestor = rhs.descendant AND
          lhs.descendant = rhs.ancestor
        WHERE
          (
            (
              st_area(st_difference(lhs.ancestor_geometry,lhs.descendant_geometry)) +
              st_area(st_difference(lhs.descendant_geometry,lhs.ancestor_geometry))
            ) /
              st_area(st_union(lhs.ancestor_geometry,lhs.descendant_geometry))
          ) < .01
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
         geometry_id integer NOT NULL,
         PRIMARY KEY(alias,geometry_id),
         FOREIGN KEY(geometry_id) REFERENCES location_geometries(id)
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
  database_add_location_geometry(
    name = '',
    readable_name = 'World',
    time_left = '0001-01-01',
    time_right = '3000-01-01',
    source_name = 'globaltoolbox',
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(-180,-90,-180,90,180,90,180,-90,-180,-90),ncol=2,byrow=T)))),
    dbname = dbname
  )
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
  return(rc$id)
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
  name,
  readable_name,
  source_name,
  time_left,
  time_right,
  geometry,
  dbname = default_database_filename(),
  ...
){
  invalid_idx <- is.na(name) | is.na(readable_name)
  sf::st_as_text
  if(any(invalid_idx)){
    if(sum(!invalid_idx) == 0){return(c())}
    readable_name <- readable_name[!invalid_idx]
    name <- name[!invalid_idx]
    time_left <- time_left[!invalid_idx]
    time_right <- time_right[!invalid_idx]
    geometry <- geometry[!invalid_idx]
  }
  if(class(time_left) == 'character'){
    time_left <- lubridate::ymd(time_left)
  }
  if(class(time_right) == 'character'){
    time_right <- lubridate::ymd(time_right)
  }
  query_head <- "INSERT INTO location_geometries
      (name, readable_name, time_left, time_right, inner_geometry,outer_geometry,source)
    VALUES"
  query_body <- "({name},{readable_name},{time_left},{time_right},ST_MAKEVALID({sf::st_as_text(geometry)}),ST_MAKEVALID(ST_BUFFER({sf::st_as_text(geometry)},.01)),{source_name})"
  query_tail <- "
    RETURNING
      id"
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    query <- paste(query_head,paste(glue::glue_sql(.con=con,query_body),collapse=', '),query_tail)
    geometry_id <- DBI::dbGetQuery(con, query)
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    if(grepl("TopologyException",e$message)){
      database_add_location_geometry(
        name=name,
        readable_name=readable_name,
        source_name=source_name,
        time_left=time_left,
        time_right=time_right,
        geometry = lwgeom::st_make_valid(geometry),
        dbname = dbname,
        ...
      )
    } else {
      stop(e$message)
    }
  })
  RSQLite::dbDisconnect(con)
  database_add_location_alias(
    geometry_id = geometry_id$id,
    alias = name
  )
  return(geometry_id$id)
}



#' @name database_add_location_alias
#' @title database_add_location_alias
#' @description Wrapper for the sql code to create an alias for a location.
#' @param location_id The id of the location to add an alias for.  Should be with respect to the locations table.
#' @param alias The alias to add.
#' @param dbname database name to put location information.
database_add_location_alias <- function(
  geometry_id,
  alias,
  dbname = default_database_filename(),
  ...
){
  alias <- standardize_location_strings(alias)
  geometry_id <- geometry_id[!is.na(alias)]
  alias <- alias[!is.na(alias)]
  if(length(alias) == 0){
    return()
  }
  query_head <- "INSERT INTO location_aliases
      (geometry_id, alias)
    VALUES"
  query_body <- "({geometry_id},{alias})"
  query_tail <- "ON CONFLICT DO NOTHING"
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    query <- paste(query_head,paste(glue::glue_sql(.con=con,query_body),collapse=', '),query_tail)
    DBI::dbClearResult(DBI::dbSendQuery(con, query))
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
  RSQLite::dbDisconnect(con)
  return()
}



#' @name database_merge_locations
#' @title database_merge_locations
#' @description combine two locations by replacing all references to one with the other, and adding the first as an alias for the second.
#' @param from The name of the location to merge into another location (this location will be deleted)
#' @param to The name of the location to merge another location into (this location will contain all data for both locations)
#' @param dbname Name of the database. Defaults to default location.
database_merge_location_geometries <- function(
  from,
  to,
  dbname = default_database_filename(),
  ...
){

  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  if(length(to) != 1){
    stop("Only merge into a single location at a time")
  }
  query_1 <- "SELECT alias from location_aliases where geometry_id = {from}"
  query_2 <- "SELECT min(time_left),max(time_right) from location_geometries where id in ({from},{to})"
  query_3 <- "UPDATE location_geometries SET time_left = {time_left},time_right = {time_right} WHERE id = {to}"
  drop_query_1 <- "DELETE from location_aliases where geometry_id = {from}"
  drop_query_2 <- "DELETE from location_geometries where id = {from}"

  tryCatch({
    rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query_1))$alias
    times <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query_2))
    time_left <- times$min
    time_right <- times$max

  },
  error = function(e){
    DBI::dbDisconnect(con)
    stop(e$message)
  })
  database_add_location_alias(geometry_id = rep(to,times=length(rc)), alias=rc,dbname=dbname,...)
  tryCatch({
    DBI::dbClearResult(DBI::dbSendQuery(con,glue::glue_sql(.con = con, query_3)))
    DBI::dbClearResult(DBI::dbSendQuery(con,glue::glue_sql(.con = con, drop_query_1)))
    DBI::dbClearResult(DBI::dbSendQuery(con,glue::glue_sql(.con = con, drop_query_2)))
  },
  error = function(e){
    DBI::dbDisconnect(con)
    stop(e$message)
  })
  RSQLite::dbDisconnect(con)
  return()
}



disable_location_hierarchy_update_trigger <- function(dbname = default_database_filename(),...){
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "DROP TRIGGER tg_refresh_my_mv on location_geometries;"
      )
    )
  }, error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
}



refresh_location_hierarchy <- function(dbname = default_database_filename(),...){
  message("Refreshing the location hierarchy.  This may take some time.")
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  fix_problematic_geometries(dbname=dbname)
  merge_all_geometric_duplicates(dbname=dbname)
  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "REFRESH MATERIALIZED VIEW location_hierarchy"
      )
    )
  }, error = function(e){
    DBI::dbDisconnect(con)
    stop(e$message)
  })
}



merge_all_geometric_duplicates <- function(limit = Inf,dbname = default_database_filename(),...){
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)
  fix_problematic_geometries(dbname=dbname,...)
  if(is.finite(limit)){
    query <- "SELECT lhs,rhs FROM geometry_duplicate_locations LIMIT {limit}"
  } else {
    query <- "SELECT lhs,rhs FROM geometry_duplicate_locations"
  }
  rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))
  from_keys= c()
  to_keys = c()
  if(nrow(rc) == 0){return()}
  for(idx in 1:nrow(rc)){
    print(paste(idx,"/",nrow(rc)))
    from_key <- max(rc[idx,])
    to_key   <- min(rc[idx,])
    while(from_key %in% from_keys){
      from_key <- to_keys[from_keys == from_key][1]
    }
    while(to_key %in% from_keys){
      to_key <- to_keys[from_keys == to_key]
    }
    if(to_key == from_key){next}
    database_merge_location_geometries(
      from_key,
      to_key,
      dbname=dbname
    )
    from_keys[length(from_keys) + 1] <- from_key
    to_keys[length(to_keys) + 1] <- to_key
  }
  refresh_location_hierarchy(dbname=dbname,...)
}



fix_problematic_geometries <- function(dbname = default_database_filename(),...){
  con <- DBI::dbConnect(drv = RPostgres::Postgres(), dbname = dbname,...)

  tryCatch({
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "UPDATE location_geometries
         SET
           inner_geometry = ST_MAKEVALID(inner_geometry),
           outer_geometry = ST_BUFFER(inner_geometry,0.1)
         WHERE
           NOT ST_ISVALID(inner_geometry);"
      )
    )
    DBI::dbClearResult(
      DBI::dbSendQuery(
        con,
        "UPDATE location_geometries
         SET
           inner_geometry = ST_COLLECTIONEXTRACT(inner_geometry,3),
           outer_geometry = ST_BUFFER(inner_geometry,0.1)
         WHERE
           ST_GEOMETRYTYPE(inner_geometry) = 'ST_GeometryCollection';"
      )
    )
  }, error = function(e){
    DBI::dbDisconnect(con)
    stop(e$message)
  })
}
