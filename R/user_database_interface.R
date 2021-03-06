#' @include database_management.R standardize_name.R

#' @name database_add_descendant_id
#' @title database_add_descendant_id
#' @description Add a new location as a descendant_id of another location and update the location hierarchy.
#' @param redable_descendant_id_name The human readable name of the location to add.
#' @param standardized_parent_name The standardized name of the parent of the location to add.
#' @param metadata Any data the user wants associated with the location.  Should be in the form of a data.frame
#' @param dbname The name of the database.  Defaults to the database associated with the package
#' @export
database_add_descendant_id <- function(
  standardized_parent_name,
  readable_descendant_id_name,
  metadata,
  dbname=default_database_filename()
){
  ## Testing connection to fail fast
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  RSQLite::dbDisconnect(con)

  standardized_descendant_id_name <- create_standardized_name(
    name = readable_descendant_id_name,
    parent = standardized_parent_name,
    check_aliases = FALSE,
    dbname = dbname
  )

  if (!is.null(standardized_parent_name)){
    parent_id <- get_database_id_from_name(
      name = standardized_parent_name,
      dbname = dbname
    )
  }

  descendant_id <- database_add_location(
    name = standardized_descendant_id_name,
    readable_name = readable_descendant_id_name,
    metadata = metadata,
    dbname = dbname
  )
  database_add_hierarchy(
    descendant_id,
    descendant_id,
    0,
    dbname = dbname
  )
  if (grepl(":", standardized_descendant_id_name)){
    database_add_location_alias(
      location_id = descendant_id,
      alias = gsub(".*:", "", standardized_descendant_id_name),
      dbname = dbname
    )
  }
  database_add_location_alias(
    location_id = descendant_id,
    alias = standardized_descendant_id_name,
    dbname = dbname
  )
  if (!is.null(standardized_parent_name)){
    ## Add all ancestors of parent as ancestors here
    query <- "INSERT INTO location_hierarchy
          (parent_id,descendant_id, depth)
            SELECT
              parent_id,
              {descendant_id} as descendant_id,
              depth+1 as depth
            FROM
              location_hierarchy
            WHERE
              descendant_id == {parent_id}"
    con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
    tryCatch({
      DBI::dbClearResult(
        DBI::dbSendQuery(con, glue::glue_sql(.con = con, query))
      )
    },
    error = function(e){
      RSQLite::dbDisconnect(con)
      stop(e$message)
    })
    RSQLite::dbDisconnect(con)
  }
  return(descendant_id)
}



#' @name database_standardize_name
#' @title database_standardize_name
#' @description Get the id of a location by looking up it's name.
#' @param name The name of the location to search for
#' @param source A standardized location name or id number.  If not NULL, the search will be limited to locations within the source
#' @param standard Whether or not to check aliases.
#' @param depth How deep under the source should the search extent. (Not yet implemented)
#' @param dbname The name of the database.  Defaults to the database associated with the package
database_standardize_name <- function(
  name,
  source="",
  aliases=TRUE,
  strict_scope=source != "",
  depth=NA,
  dbname = default_database_filename()
){
  ## Testing the connection to fail fast
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  RSQLite::dbDisconnect(con)

  query <- "SELECT
    name
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
      alias is {name}"
  if(!aliases){
    query <- paste(query, 'AND alias is name')
  }
  if(is.null(source)){
    query <- paste(query, 'AND depth = 0')
  } else {
    parent_id <- as.numeric(source)
    if(is.na(parent_id)){
      parent_id <- get_database_id_from_name(
        name = source,
        dbname = dbname
      )
    }
    query <- paste(query, 'AND parent_id = {parent_id}')
    if(strict_scope){
      query <- paste(query, 'AND depth > 0')
    }
    if(!is.na(depth)){
      query <- paste(query, 'AND depth <= {depth}')
    }
  }

  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)
  tryCatch({
    rc <- DBI::dbGetQuery(con, glue::glue_sql(.con = con, query))
  },
  error = function(e){
    RSQLite::dbDisconnect(con)
    stop(e$message)
  })
  RSQLite::dbDisconnect(con)

  rc <- dplyr::filter(rc, !duplicated(name))

  if(length(rc$name) != 1){
    stop(paste("Ambiguous location", name, "has", nrow(rc), "location_ids"))
  }
  return(rc$name)
}
