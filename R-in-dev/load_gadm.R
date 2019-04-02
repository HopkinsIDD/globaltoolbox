rm(list=ls())
devtools::document()
if(require(globaltoolbox,quiet=T)){
    unloadNamespace('pkg:globaltoolbox')
    remove.packages("globaltoolbox")
}
rm(list=ls())
install.packages('~/git/globaltoolbox',type='source',repos=NULL)
library(globaltoolbox)
library(lwgeom)
library(sf)
dbname=globaltoolbox:::default_database_filename()
reset_database()
create_database()

country_aliases.csv <- system.file(
    "extdata",
    "country_aliases.csv",
    package = "globaltoolbox"
)
country_aliases <- read.csv(country_aliases.csv)
all_countries <- levels(country_aliases$country_code)

for(ISO_A1 in all_countries){
  try({
      ISO_level = 0
      destination <- tempfile(fileext='.rds')
      website <- paste(
          "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
          ISO_A1,
          "_adm",
          ISO_level,
          ".rds",
          sep=''
      )
      download.file(website,destination,mode='wb')
      #' @importFrom sf st_as_sf
      country_data <- st_as_sf(readRDS(destination))
      # if(!all(suppressWarnings(st_is_valid(country_data)))){
      #    #' @importFrom lwgeom st_make_valid
      #    country_data <- st_make_valid(country_data)
      # }
      country_data$type = 'ISO_A1'
      country_data$depth = 0
      metadata_frame <- as.data.frame(country_data)
      metadata_frame <- metadata_frame[,colnames(metadata_frame) != 'geometry']
      descendent_id <- globaltoolbox:::database_add_descendent(
                                           dbname=dbname,
                                           metadata=metadata_frame,
                                           standardized_name=NULL,
                                           standardized_descendent_name=country_data$ISO
                                       )
      for(alias_idx in which(grepl("NAME",colnames(country_data)))){
          globaltoolbox:::database_add_alias(
                              dbname=dbname,
                              standardized_name=country_data$ISO,
                              name=country_data[[alias_idx]][1]
                          )
      }
      geometry = country_data$geometry
      unlink(destination)
  })
}

get_location_metadata() %>%  dplyr::as_tibble() -> results

load_
