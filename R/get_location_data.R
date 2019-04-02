#' @export
#' @description Pull metadata associated with a particular location or set of locations
#' @param location The name of the location to search for.  If there are multiple matches, all will be returned.  If this is NULL, every location below the source will be returned.
#' @param source A location to only search within.  Only locations contained within this location will be searched.  If this is NULL, every location will be searched.
#' @param metadata_names The column names of metadata to pull.  The default is to pull all metadata.
#' @param dbname The name of the database to connect to
## get_location_metadata(location)
## get_location_metadata(location,metadata)
## get_location_metadata(location,database)
## get_location_metadata(location,metadata,database)
## get_metadata(source)
## get_metadata(source,metadata)
## get_metadata(source,database)
## get_metadata(source,metadata,database)
get_location_metadata <- function(location=NULL,source=NULL,metadata_names=NULL,dbname = default_database_filename()){
  #' @importFrom RSQLite SQLite
  #' @importFrom DBI dbConnect
  con <- dbConnect(drv=SQLite(),dbname)
  #' @importFrom DBI dbGetQuery
  
  query = "SELECT * FROM locations"
  if(!is.null(source)){
    parent_id = as.numeric(source)
    if(is.na(parent_id)){
      parent_id = database_lookup_location_by_name(name=source,standard=TRUE,dbname=dbname,source=NULL)
    }
    query = paste0(query,
      "INNER JOIN
        location_hierarchy
      ON
        location_hierarchy.descendent_id = locations.id
      WHERE
        parent_id = {parent_id}")
    if((!is.null(location))){
      query = paste0(query,' AND')
    }
  } else {
    if((!is.null(location))){
      query = paste0(query,' WHERE')
    }
  }
  if(!is.null(location)){
    query = paste0(query," name is {location}")
  }
  
    #' @importFrom glue glue_sql
  results <- dbGetQuery(con,glue_sql(.con=con,query))
  #' @importFrom dplyr bind_rows
  metadata.frame <- bind_rows(lapply(results$metadata,process_single_metadata_frame)) 
  #' @importFrom dplyr bind_cols
  results <- bind_cols(results[,-which(colnames(results) %in% c('standard','metadata'))],metadata.frame)
  return(results)
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
