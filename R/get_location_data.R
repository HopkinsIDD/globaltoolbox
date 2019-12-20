#' @include database_management.R



#' @name get_location_metadata
#' @title get_location_metadata
#' @description Pull metadata associated with a particular location or set of locations
#' @param location The name of the location to search for.  If there are multiple matches, all will be returned.  If this is NULL, every location below the source will be returned.
#' @param source A location to only search within.  Only locations contained within this location will be searched.  If this is NULL, every location will be searched.
#' @param metadata_names The column names of metadata to pull.  The default is to pull all metadata.
#' @param aliases Whether or not to also pull metadata for aliases of locations.
#' @param strict_scope Whether to exclude the source from the results.
#' @param depth How far down the tree from the source to look.  If NA, then the entire subtree will be searched.
#' @param dbname The name of the database to connect to
#' @return A data.frame containing the metadata for the appropriate locations.
#' @export
get_location_metadata <- function(
  location=NULL,
  source="",
  metadata_names=NULL,
  aliases=TRUE,
  strict_scope=TRUE,
  depth=NA,
  dbname = default_database_filename()
){
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  query <- "SELECT
    *
  FROM
    (locations
      LEFT JOIN
    location_hierarchy
      ON
        locations.id = location_hierarchy.descendant_id
    ) INNER JOIN
    location_aliases
      ON
      locations.id = location_aliases.location_id
    WHERE
      1=1"
   if(!is.null(location)){
      query <- paste(query, "AND name is {location}")
   }
  if(!aliases){
    query <- paste(query, 'AND alias is name')
  }
  if(is.null(source)){
    query <- paste(query, 'AND depth = 0')
  } else {
    parent_id <- as.numeric(source)
    if(is.na(parent_id)){
      parent_id <-
        globaltoolbox::get_database_id_from_name(name = source, dbname = dbname)
    }
    query <- paste(query, 'AND parent_id = {parent_id}')
    if(strict_scope){
      query <- paste(query, 'AND depth > 0')
    }
    if(!is.na(depth)){
      query <- paste(query, 'AND depth <= {depth}')
    }
  }

  rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))
  rc$depth_from_source <- rc$depth
  rc$depth <- NULL
  rc$location_id <- NULL
  rc$descendent_id <- NULL
  metadata.frame <- dplyr::bind_rows(
    lapply(rc$metadata, process_single_metadata_frame)
  )
  rc <- dplyr::bind_cols(
    rc[, -which(colnames(rc) %in% c('standard', 'metadata'))],
    metadata.frame
  )
  if(!is.null(metadata_names)){
    cnames <- c('id', 'name', 'readable_name', metadata_names, 'alias')
    cnames <- cnames[cnames %in% colnames(rc)]
    rc <- rc[, cnames]
  }
  RSQLite::dbDisconnect(con)
  return(rc)
}



#' @name process_single_metadata_frame
#' @title process_single_metadata_frame
#' @description Take a single metadata string and turn it into a data.frame
#' @param frame A json string representing the metadata for an object.
#' @return A data.frame
process_single_metadata_frame <- function(frame){
  if(is.na(frame)){
    return(data.frame(missing = TRUE))
  }
  frame <- as.data.frame(jsonlite::fromJSON(frame))
  ## frame <- dplyr::mutate_all(frame, dplyr::funs(as.character(.)))
  for(col in colnames(frame)){
    frame[[col]] <- as.character(frame[[col]])
  }
  return(frame)
}



#' @name get_all_aliases
#' @title get_all_aliases
#' @description Pull aliases associated with a particular location or set of locations
#' @param location The name of the location to search for.  If there are multiple matches, all will be returned.  If this is NULL, every location below the source will be returned.
#' @param source A location to only search within.  Only locations contained within this location will be searched.  If this is "", every location will be searched.
#' @param strict_scope Whether to exclude the source from the results.
#' @param depth How far down the tree from the source to look.  If NA, then the entire subtree will be searched.
#' @param dbname The name of the database to connect to
#' @return A data.frame containing the metadata for the appropriate locations.
#' @export
get_all_aliases <- function(
  location=NULL,
  source="",
  strict_scope=TRUE,
  depth=NA,
  dbname = default_database_filename()
){
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  query <- "SELECT
    name,
    alias,
    readable_name,
    location_id as id,
    max(location_hierarchy.depth) as depth_from_source,
    metadata
  FROM
    (locations
      LEFT JOIN
    location_hierarchy
      ON
        locations.id = location_hierarchy.descendant_id
    ) INNER JOIN
    location_aliases
      ON
      locations.id = location_aliases.location_id
    WHERE
      1=1"
  if(!is.null(location)){
    query <- paste(query, "AND name is {location}")
  }
  if(!is.null(source)){
    parent_id <- NA
    if(is.numeric(source)){
      parent_id <- as.numeric(source)
    } else {
      parent_id <- globaltoolbox::get_database_id_from_name(
        name = source,
        dbname = dbname
      )
    }
    query <- paste(query, "AND parent_id = {parent_id}")
    if(strict_scope){
      query <- paste(query, "AND depth > 0")
    }
    if(!is.na(depth)){
      query <- paste(query, 'AND depth <= {depth}')
    }
  }
  query <- paste(query, "GROUP BY name,alias,location_id")
  rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))

  RSQLite::dbDisconnect(con)
  return(rc)
}



#' @name get_location_geometry
#' @title get_location_geometry
#' @description Pull geometry associated with a particular location or set of locations at particular times
#' @param location The name of the location to search for.  If there are multiple matches, all will be returned.  If this is NULL, every location below the source will be returned.
#' @param source A location to only search within.  Only locations contained within this location will be searched.  If this is "", every location will be searched.
#' @param strict_scope Whether to exclude the source in the results
#' @param depth How far down the tree from the source to search.  If NA, the entire subtree will be searched.
#' @param time_left Only pull geometries who are valid at a time after this time
#' @param time_right Only pull geometries who are valid at a time before this time
#' @param dbname The name of the database to connect to
#' @export
get_location_geometry <- function(
  location=NULL,
  source="",
  strict_scope=TRUE,
  depth=NA,
  time_left = NA,
  time_right = NA,
  dbname = default_database_filename()
){
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  query <- "SELECT
    locations.name,
    locations.id as location_id,
    location_geometries.id as geometry_id,
    location_geometries.time_left,
    location_geometries.time_right,
    location_geometries.geometry,
    location_hierarchy.depth
  FROM
    (locations
      LEFT JOIN
    location_hierarchy
      ON
        locations.id = location_hierarchy.descendant_id
    ) INNER JOIN
    location_geometries
      ON
      locations.id = location_geometries.location_id
    WHERE
      1=1"
  if(!is.null(location)){
     query <- paste(query, "AND name is {location}")
  }
  if(!is.na(time_left)){
    query <- paste(query, "AND {time_left} < time_right")
  }
  if(!is.na(time_right)){
    query <- paste(query, "AND time_left <= {time_right}")
  }
  if(is.null(source)){
    query <- paste(query, "AND depth = 0")
  } else {
    parent_id <- as.numeric(source)
    if(is.na(parent_id)){
      parent_id <-
        globaltoolbox::get_database_id_from_name(name = source, dbname = dbname)
    }
    query <- paste(query, 'AND parent_id = {parent_id}')
    if(strict_scope){
      query <- paste(query, 'AND depth > 0')
    }
    if(!is.na(depth)){
      query <- paste(query, 'AND depth <= {depth}')
    }
  }

  rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))
  RSQLite::dbDisconnect(con)

  rc$depth_from_source <- rc$depth
  rc$depth <- NULL
  if(nrow(rc) > 0){
    rc$geometry <- geojsonsf::geojson_sf(rc$geometry)$geometry
  }
  rc <- sf::st_as_sf(rc)
  return(rc)
}


#' @name get_parents
#' @title get_parents
#' @description Pull the names of all locations which are parents of the given location in the tree
#' @param location The standardized name of the location to get the parents of
#' @param time_left Only pull geometries who are valid at a time after this time
#' @param time_right Only pull geometries who are valid at a time before this time
#' @param dbname The name of the database to connect to
#" @export
get_parents <- function(location,dbname = default_database_filename()){
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  query <- "SELECT
    locations.id,
    locations.name,
    locations.readable_name,
    parents.id as parent_id,
    parents.name as parent_name,
    parents.readable_name as readable_parent_name,
    depth
  FROM
    locations
  LEFT JOIN
    location_hierarchy
  ON
    locations.id = location_hierarchy.descendant_id
  LEFT JOIN
    locations as parents
  ON
    parents.id = location_hierarchy.parent_id
  WHERE
    locations.name is {location}"
  query <- paste(query, "GROUP BY locations.name,locations.id,parents.id")
  rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))

  RSQLite::dbDisconnect(con)
  return(rc)
}
