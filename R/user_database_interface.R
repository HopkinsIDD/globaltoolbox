#' @include database_management.R standardize_name.R


#' @export
#' @description Add a new location to the database as a descendent of another region.
#' @param redable_descendent_name The human readable name of the location to add.
#' @param standardized_name The standardized name of the parent of the location to add.
#' @param dbname The name of the database.  Defaults to the database associated with the package
#' @description Add a new location as a descendent of another location and update the location hierarchy.
database_add_descendent <- function(
  standardized_name,
  readable_descendent_name,
  metadata,dbname=default_database_filename()
){
  standardized_descendent_name = create_standardized_name(
    name=readable_descendent_name,
    parent=standardized_name,
    check_aliases=TRUE,
    dbname=dbname
  )

  #' @importFrom RSQLite SQLite
  #' @importFrom DBI dbConnect
  con <- dbConnect(drv=SQLite(),dbname)
  if(!is.null(standardized_name)){

    parent_id = get_database_id_from_name(name=standardized_name,dbname=dbname)
  }
  
  descendent_id = database_add_location(
    name=standardized_descendent_name,
    readable_name=readable_descendent_name,
    metadata=metadata,dbname=dbname
  )
  database_add_location_hierarchy(descendent_id,descendent_id,0,dbname=dbname)
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
    #' @importFrom DBI dbSendQuery dbClearResult
    #' @importFrom glue glue_sql
    dbClearResult(dbSendQuery(con,glue_sql(.con=con,query)))
  }
  return(descendent_id)
}
