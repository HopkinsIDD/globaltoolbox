# devtools::install_github("HopkinsIDD/globaltoolbox")
install.packages('/home/jkaminsky/git/globaltoolbox',type='source',repos=NULL)

library(sf)
library(globaltoolbox)

create_partial_database <- function(
  gaul_directory = "~/GAUL_shp",
  gadm_shapefile = "~/gadm36.shp",
  country = "",
  max_depth = 10,
  dbname = "globaltoolbox"
){
  # reset_database(dbname = dbname)
  # create_database(dbname = dbname)

  ## Load alt_country_names
  # load_country_aliases(filename)

  dirname <- gaul_directory
  all_files <- list.files(dirname,recursive=TRUE)
  all_files <- all_files[grepl(".shp$",all_files)]
  all_files <- paste0(dirname,'/',all_files)
  for(filename in all_files){
    print(filename)
    year <- as.numeric(gsub('.*2015_([1234567890]*).*','\\1',filename))
    if(year > 1900){
      tl <- paste(year,"01","01",sep='-')
      tr <- paste(year+1,"01","01",sep='-')
    } else {
      year <- as.numeric(gsub('^.*([1234567890][1234567890][1234567890][123467890]).*$','\\1',filename))
      if (grepl('_bef_',filename)) {
        tl = '1800-01-01'
        tr <- paste(year+1,"01","01",sep='-')
      } else if (grepl('_now.',filename)) {
        tl <- paste(year,"01","01",sep='-')
        tr = '2019-12-31'
      }
      if(is.na(year)){
        tl = '1800-01-01'
        tr = '2019-12-31'
      }
    }
    print(paste("File: ",filename))
    print(paste(tl,"to",tr))
    shp <- st_read(filename,stringsAsFactors=FALSE)
    levels <- suppressWarnings(max(as.numeric(unique(gsub("_.*","",gsub("ADM","",names(shp))))),na.rm=TRUE))
    hierarchy_column_names <- paste0("ADM",0:levels,"_NAME")
    hierarchy_column_names <- hierarchy_column_names[hierarchy_column_names %in% names(shp)]
    alias_column_names = rep(list(""),times=length(hierarchy_column_names))
    shp_indices <- which(unlist(globaltoolbox:::standardize_string(shp[[hierarchy_column_names[[1]]]])) == unlist(globaltoolbox:::standardize_string(country)))
    shp <- shp[shp_indices,]
    if(length(shp_indices) == 0){
      next
    }
    load_hierarchical_sf(
      shp_files = shp,
      time_left = tl,
      time_right = tr,
      hierarchy_column_names = hierarchy_column_names,
      alias_column_names = alias_column_names,
      source_name = "GAUL",
      geometry = TRUE,
      max_depth = max_depth,
      refresh_on_exit = FALSE
    )
    print(paste("GAUL Shapefile for",country,":",filename,"has",nrow(shp),"lines"))
    print(paste("File: ",filename,"finished"))
    gc()
  }

  shp <- st_read(gadm_shapefile,stringsAsFactors=FALSE)
  hierarchy_column_names = paste("NAME",0:5,sep="_")
  alias_column_names <- lapply(0:5,function(x){
    x = paste(c("GID","HASC","VARNAME","NLAME","VARNAME"),x,sep="_")
    return(x[x %in% names(shp)])
  })
  shp_indices <- which(unlist(globaltoolbox:::standardize_string(shp[[hierarchy_column_names[[1]]]])) == unlist(globaltoolbox:::standardize_string(country)))
  if(length(shp_indices) == 0){
    return()
  }
  shp <- shp[shp_indices,]
  print(paste("GADM Shapefile for",country,"has",nrow(shp),"lines"))
  load_hierarchical_sf(
    shp_files = shp,
    time_left = '1800-01-01',
    time_right = '2019-12-31',
    hierarchy_column_names = paste("NAME",0:5,sep="_"),
    alias_column_names = alias_column_names,
    source_name = "GADM",
    max_depth =  max_depth,
    geometry = TRUE,
    geometry_union = TRUE
  )
  rm(shp)
  gc()
}
