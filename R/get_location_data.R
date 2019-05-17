#' @name get_location_metadata
#' @include database_management.R
#' @description Pull metadata associated with a particular location or set of locations
#' @param location The name of the location to search for.  If there are multiple matches, all will be returned.  If this is NULL, every location below the source will be returned.
#' @param source A location to only search within.  Only locations contained within this location will be searched.  If this is NULL, every location will be searched.
#' @param metadata_names The column names of metadata to pull.  The default is to pull all metadata.
#' @param aliases Whether or not to also pull metadata for aliases of locations.
#' @param dbname The name of the database to connect to
#' @export
## get_location_metadata(location)
## get_location_metadata(location,metadata)
## get_location_metadata(location,database)
## get_location_metadata(location,metadata,database)
## get_metadata(source)
## get_metadata(source,metadata)
## get_metadata(source,database)
## get_metadata(source,metadata,database)
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbConnect
#' @importFrom glue glue_sql
#' @importFrom RSQLite dbDisconnect
#' @importFrom DBI dbGetQuery
get_location_metadata <- function(
  location=NULL,
  source=NULL,
  metadata_names=NULL,
  aliases=TRUE,
  strict_scope=TRUE,
  depth=NA,
  dbname = default_database_filename()
){
  con <- dbConnect(drv=SQLite(),dbname)
  query = "SELECT
    *
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
      1=1"
   if(!is.null(location)){
      query = paste(query,"AND alias is {location}")
   }
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
  #' @importFrom glue glue_sql
  rc <- dbGetQuery(con,glue_sql(.con=con,query))
  rc$depth_from_source = rc$depth
  rc$depth = NULL
  metadata = lapply(rc$metadata,process_single_metadata_frame)
  #' @importFrom dplyr bind_rows
  metadata.frame <- bind_rows(lapply(rc$metadata,process_single_metadata_frame)) 
  #' @importFrom dplyr bind_cols
  rc <- bind_cols(
    rc[,-which(colnames(rc) %in% c('standard','metadata'))],
    metadata.frame
  )
  if(!is.null(metadata_names)){
    cnames = c('id','name','readable_name',metadata_names,'alias')
    cnames = cnames[cnames %in% colnames(rc)]
    rc = rc[,cnames]
  }
  dbDisconnect(con)
  return(rc)
}



process_single_metadata_frame <- function(frame){
    #' @importFrom jsonlite fromJSON
    if(is.na(frame)){return(data.frame(missing=TRUE))}
    frame = as.data.frame(fromJSON(frame))
    #' @importFrom dplyr mutate_all
    #' @importFrom dplyr funs
    frame = mutate_all(frame,funs(as.character(.)))
    return(frame)
}



#' @export
#' @importFrom glue glue_sql
get_all_aliases <- function(
  location=NULL,
  source=NULL,
  metadata_names=NULL,
  aliases=TRUE,
  strict_scope=TRUE,
  depth=NA,
  dbname = default_database_filename()
){
  con <- dbConnect(drv=SQLite(),dbname)
  query = "SELECT
    name,alias,readable_name,location_id as id,max(location_hierarchy.depth) as depth_from_source
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
      1=1"
   if(!is.null(location)){
      query = paste(query,"AND alias is {location}")
   }
  if(!aliases){
    query = paste(query,'AND alias is name')
  }
  if(!is.null(source)){
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
  query = paste(query,'GROUP BY name,alias,location_id')
  rc <- dbGetQuery(con,glue_sql(.con=con,query))
  dbDisconnect(con)
  return(rc)
}
