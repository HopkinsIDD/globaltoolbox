#' @name database_add_descendent
#' @title database_add_descendent
#' @description Add a new location as a descendent of another location and update the location hierarchy.
#' @include database_management.R standardize_name.R
#' @param redable_descendent_name The human readable name of the location to add.
#' @param standardized_name The standardized name of the parent of the location to add.
#' @param dbname The name of the database.  Defaults to the database associated with the package
#' @export
database_add_descendent <- function(
  standardized_name,
  readable_descendent_name,
  metadata, 
  dbname=default_database_filename()
){
  
  warning("Not checking for aliases as name matching is too sensitive")
  standardized_descendent_name = create_standardized_name(
    name=readable_descendent_name,
    parent=standardized_name,
    check_aliases=FALSE,
    dbname=dbname
  )

  con <- DBI::dbConnect(drv=RSQLite::SQLite(),dbname)
  if(!is.null(standardized_name)){
    parent_id = get_database_id_from_name(name=standardized_name,dbname=dbname)
  }
  
  descendent_id = database_add_location(
    name=standardized_descendent_name,
    readable_name=readable_descendent_name,
    metadata=metadata,dbname=dbname
  )
  database_add_location_hierarchy(descendent_id,descendent_id,0,dbname=dbname)
  if(grepl(':',standardized_descendent_name)){
    database_add_location_alias(
      location_id=descendent_id,
      alias=gsub('.*:','',standardized_descendent_name),
      dbname=dbname
    )
  }
  database_add_location_alias(
    location_id=descendent_id,
    alias=standardized_descendent_name,
    dbname=dbname
  )
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
    DBI::dbClearResult(DBI::dbSendQuery(con, glue::glue_sql(.con=con,query)))
  }
  RSQLite::dbDisconnect(con)
  return(descendent_id)
}

#' @name database_standardize_name
#' @title database_standardize_name
#' @description Get the id of a location by looking up it's name.
#' @param name The name of the location to search for
#' @param source A standardized location name or id number.  If not NULL, the search will be limited to locations within the source
#' @param standard Whether or not to check aliases.
#' @param depth How deep under the source should the search extent. (Not yet implemented)
#' @param dbname The name of the database.  Defaults to the database associated with the package
#' @export
database_standardize_name <- function(
  name,
  source=NULL,
  aliases=TRUE,
  strict_scope=TRUE,
  depth=NA,
  dbname = default_database_filename()
){
  con <- DBI::dbConnect(drv=RSQLite::SQLite(),dbname)
  query = "SELECT
    name
  FROM
    (locations
      LEFT JOIN
    location_hierarchy
      ON
        locations.id = location_hierarchy.descendent_id
    ) INNER JOIN
    location_aliases
      ON
      locations.id = location_aliases.location_id
    WHERE
      alias is {name}"
  if(!aliases){
    query = paste(query,'AND alias is name')
  }
  if(is.null(source)){
    query = paste(query,'AND depth = 0')
  } else {
    parent_id = as.numeric(source)
    if(is.na(parent_id)){
      parent_id = get_database_id_from_name(name=source,dbname=dbname)
    }
    query = paste(query,'AND parent_id = {parent_id}')
    if(strict_scope){
      query = paste(query,'AND depth > 0')
    }
    if(!is.na(depth)){
      query = paste(query,'AND depth <= {depth}')
    }
  }
  rc <- DBI::dbGetQuery(con,glue::glue_sql(.con=con,query))
  rc <- dplyr::filter(rc,!duplicated(name))

  if(length(rc$name) != 1){
    stop(paste("Ambiguous location",name,"has",nrow(rc),"location_ids"))
  }
  RSQLite::dbDisconnect(con)
  return(rc$name)
}
