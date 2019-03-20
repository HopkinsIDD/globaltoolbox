default_database_filename = function(){
  system.file("extdata","globaltoolbox.sqlite",package = "globaltoolbox")
}
#' @export
#' @description Setup the database for holding the location tree and shapefiles.
#'   Requires postgres database already set up.  For instructions on how to set
#'   up a postgres database, see https://www.r-bloggers.com/getting-started-with-postgresql-in-r
#' @param dbname The name of the database to connect to
#' @param ... Other parameters for RPostgreSQL::dbConnect
create_database <- function(dbname = 'globaltoolbox',...){
  ## Create Tables

  #' @importFrom RPostgreSQL dbConnect dbDriver
  con <- dbConnect(drv=dbDriver("PostgreSQL"),default_database_filename(),...)
  
  ## The first table holds the locations and any metadata
  dbGetQuery(con, "CREATE TABLE IF NOT EXISTS public.locations ( id SERIAL, name varchar COLLATE
  pg_catalog.default, latitude real, longitude real, data jsonb NOT
  NULL DEFAULT '{}'::jsonb );")

  ## The second table holds the tree structure of containment
  dbGetQuery(con, "CREATE TABLE IF NOT EXISTS public.location_hierarchies ( ancestor_id integer NOT
  NULL, descendant_id integer NOT NULL, generations integer NOT NULL
  );")

  ## The third table holds the geometric information
  dbGetQuery(con, "CREATE TABLE IF NOT EXISTS location_geometries( id SERIAL, location_id integer NOT
  NULL, time_left date NOT NULL, time_right date NOT NULL, geometry
  jsonb NOT NULL );")
  
  ## Populate with WHO regions
  database_create_location("AFR",5,25,{})
  database_create_location("AMR",15,-87,{})
  database_create_location("EUR",NA,NA,{})
  database_create_location("SEAR",NA,NA,{})
  database_create_location("EMR",NA,NA,{})
  database_create_location("WPR",NA,NA,{})
}

insert_into_database <- function(){}

database_create_location <- function(name,latitude,longitude,data){
  #' @importFrom jsonlite toJSON
  dbGetQuery(con, paste("INSERT INTO public.locations (name, latitude, longitude) VALUES (",name,", ",latitude,", ",longitude,")"))
  return()
}
