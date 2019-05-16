options(warn=1)
options(error=recover)
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
# dbname=globaltoolbox:::default_database_filename()
dbname = "~/git/globaltoolbox/inst/extdata/globaltoolbox.sqlite"
create_database()
reset_database()
create_database()

country_aliases.csv <- system.file(
  "extdata",
  "country_aliases.csv",
  package = "globaltoolbox"
)
country_aliases <- read.csv(country_aliases.csv)
all_countries <- levels(country_aliases$country_code)

error_messages = c('')

alias_ISO_columns = c(
  "HASC",
  "VARNAME",
  "ISO",
  "NAME",
  "ID",
  "NL_NAME"
)



for(ISO_A1 in all_countries){
  print(ISO_A1)
  destination <- tempfile(fileext='.rds')
  
  # Download GADM for the country
  try({
    ISO_level = 0
    website <- paste(
      "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
      ISO_A1,
      "_adm",
      ISO_level,
      ".rds",
      sep=''
    )
    download.file(website,destination,mode='wb')
  },silent=T)
  
  # if GADM file downloaded correctly process it
  if(file.exists(destination)){
    country_data <- st_as_sf(readRDS(destination))
    country_data$type = 'ISO_A1'
    country_data$depth = 0
    metadata_frame <- as.data.frame(country_data)
    metadata_frame <- metadata_frame[,colnames(metadata_frame) != 'geometry']
    metadata_frame <- metadata_frame[,!grepl("NAME",colnames(metadata_frame))]
    metadata_frame <- metadata_frame[,!(colnames(metadata_frame) %in% c("VALIDFR","VALIDTO"))]
    descendent_id <- globaltoolbox:::database_add_descendent(
      dbname=dbname,
      metadata=metadata_frame,
      standardized_name=NULL,
      readable_descendent_name = country_data$ISO
    )
    for(alias_idx in
      c(
        which(colnames(country_data)=='ISO'),
        which(grepl("NAME",colnames(country_data)))
      )
    ){
      alias=country_data[[alias_idx]][1]
      if(grepl("^\n \r$",alias)){next}
      location_id=descendent_id
      tryCatch({globaltoolbox:::database_add_location_alias(
                             dbname=dbname,
                             location_id=descendent_id,
                             alias=alias
                           )
                             
      },
      error = function(e){
        if(!(
          grepl("UNIQUE constraint failed",e$message)
        )){
          stop("The only way creating an alias should fail is if the alias is already in the database")
        }
      })
    }
    tryCatch({
      geometry = country_data$geometry
      time_left = "1800-01-01"
      time_right = "2020-01-01"
      database_add_location_geometry(
        location_id=location_id,
        time_left=time_left,
        time_right=time_right,
        geometry=geometry,
        dbname=dbname
      )
    },
    error = function(e){
    },silent=T)
    unlink(destination)
  }

  ## Lower level regions:
  toggle = TRUE
  ISO_level = 0
  while(toggle){
    if(length(error_messages) > 11){warning("Over 10 errors")}
    if(length(error_messages) > 101){warning("Over 100 errors")}
    if(length(error_messages) > 1001){warning("Over 1000 errors")}
    if(length(error_messages) > 10001){warning("Over 10000 errors")}
    if(length(error_messages) > 100001){warning("Over 100000 errors")}
    try({
      ISO_level = ISO_level + 1
      website <- paste(
        "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
        ISO_A1,
        "_adm",
        ISO_level,
        ".rds",
        sep=''
      )
      download.file(website,destination,mode='wb')
    },silent=T)
    if(file.exists(destination)){
      country_data <- st_as_sf(readRDS(destination))
      country_data$type = paste0('ISO_A2_L',ISO_level)
      country_data$depth = ISO_level
      parent_name = c()
      for(i in 0:(ISO_level - 1)){
        ## Remove data about containing countries
        parent_name = paste(parent_name,country_data[[paste('NAME',i,sep='_')]],sep='::')
      }
      parent_name = gsub('^::','',parent_name)
      country_data$standardized_parent_name = telescoping_standardize(parent_name,dbname=dbname)
      for(row in 1:nrow(country_data)){
        print(paste(ISO_A1,"level",ISO_level,"location",row,"/",nrow(country_data)))
        tmp_data = country_data[row,]
        metadata_frame <- as.data.frame(tmp_data)
        metadata_frame <- metadata_frame[,!(colnames(metadata_frame) %in% paste(alias_ISO_columns,ISO_level,sep='_'))]
        metadata_frame <- metadata_frame[,colnames(metadata_frame) != 'geometry']
        metadata_frame <- metadata_frame[,colnames(metadata_frame) != 'ISO']
        for(i in 0:(ISO_level - 1)){
          ## Remove data about containing countries
          metadata_frame <- metadata_frame[,!grepl(paste0('_',i,'$'),colnames(metadata_frame))]
          colnames(metadata_frame) = gsub(paste0('_',i,'$'),'',colnames(metadata_frame))
        }
        tryCatch({
            descendent_id <- globaltoolbox:::database_add_descendent(
                                           dbname=dbname,
                                           metadata=metadata_frame,
                                           standardized_name=tmp_data$standardized_parent_name,
                                           readable_descendent_name = tmp_data[[paste('NAME',ISO_level,sep='_')]] 
                                         )
        },
        error = function(e){
            error_messages <<- c(error_messages,paste(ISO_A1,"level",ISO_level,"location",row,"/",nrow(country_data),e$message))
        })
        tmp_alias_ISO_columns = paste(alias_ISO_columns[
          paste(alias_ISO_columns,ISO_level,sep='_') %in%
          colnames(country_data)
        ],
        ISO_level,
        sep='_'
        )
        for(alias_name in tmp_alias_ISO_columns){
          alias=tmp_data[[alias_name]][1]
          if(is.na(alias)){next}
          if(grepl("^\n \r$",alias)){next}
          if(grepl("|",alias,fixed=T)){
            alias = strsplit(alias,'|',fixed=T)[[1]]
          }
          for(this_alias in alias){
            location_id=descendent_id
            tryCatch({globaltoolbox:::database_add_location_alias(
                                        dbname=dbname,
                                        location_id=descendent_id,
                                        alias=this_alias
                                      )
            },
            error = function(e){
              if(!(
                grepl("UNIQUE constraint failed",e$message)
              )){
                stop("The only way creating an alias should fail is if the alias is already in the database")
              }
            })
          }
        }
        geometry=tmp_data$geometry
        time_left = "1800-01-01"
        time_right = "2020-01-01"
        tryCatch({
          geometry = country_data$geometry
          time_left = "1800-01-01"
          time_right = "2020-01-01"
          database_add_location_geometry(
            location_id=location_id,
            time_left=time_left,
            time_right=time_right,
            geometry=geometry,
            dbname=dbname
          )
        },
        error = function(e){
        },silent=T)
        unlink(destination)
      }
    } else {
      toggle = FALSE
    }
  }
}



standardize_gadm_lhs_time <- function(x){
  if(x == 'Present'){return(lubridate::now())}
  if(x == 'Unknown'){return(lubridate::ymd('1900-01-01'))}
  x = gsub('[^1234567890]','',x)
  if(nchar(x) == 4){
    return(lubridate::ymd(paste(x,'01','01')))
  }
  if(nchar(x) == 6){
    return(lubridate::ymd(paste(substr(x,1,4),substr(x,5,6),'01')))
  }
  if(nchar(x) == 8){
    return(lubridate::ymd(paste(substr(x,1,4),substr(x,5,6),substr(x,7,8))))
  }
  return(NA)
}

standardize_gadm_rhs_time <- function(x){
  if(x == 'Present'){return(lubridate::now()+lubridate::years(1))}
  if(x == 'Unknown'){return(lubridate::now()+lubridate::years(1))}
  x = gsub('[^1234567890]','',x)
  if(nchar(x) == 4){
    return(lubridate::ymd(paste(x,'12','31')))
  }
  if(nchar(x) == 6){
    return(lubridate::ymd(paste(substr(x,1,4),substr(x,5,6),'31')))
  }
  if(nchar(x) == 8){
    return(lubridate::ymd(paste(substr(x,1,4),substr(x,5,6),substr(x,7,8))))
  }
  return(NA)
}

install.packages('.',type='source',repos=NULL)
library(globaltoolbox)
library(sf)
ISO_A1='AFG'
ISO_level=0
