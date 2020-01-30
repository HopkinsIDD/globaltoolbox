# devtools::install_github("HopkinsIDD/globaltoolbox")
install.packages('/home/jkaminsky/git/globaltoolbox',type='source',repos=NULL)

library(sf)
library(globaltoolbox)

create_partial_database <- function(
  gaul_directory = "~/GAUL_shp",
  gadm_shapefile = "~/gadm36.shp",
  country = "",
  dbname = "globaltoolbox"
){
  # reset_database(dbname = dbname)
  # create_database(dbname = dbname)

  ## Load alt_country_names
  # load_country_aliases(filename)

  dirname <- gaul_directory
  all_files <- list.files(dirname)
  all_files <- all_files[!grepl(".zip",all_files)]
  for(file in all_files){
    filename <- paste0(dirname,"/",file,"/",file,".shp")
    backup_folder <- paste0(dirname,"/",file,".shp")
    if(!file.exists(filename)){next;}
    print(filename)
    year <- as.numeric(gsub('.*2015_([1234567890]*).*','\\1',filename))
    print(paste("File: ",filename))
    shp <- st_read(filename)
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
      time_left = paste(year,"01","01",sep='-'),
      time_right = paste(year+1,"01","01",sep='-'),
      hierarchy_column_names = hierarchy_column_names,
      alias_column_names = alias_column_names,
      source_name = "GAUL",
      geometry = TRUE,
      max_depth = 3
    )
    print(paste("GAUL Shapefile for",country,":",filename,"has",nrow(shp),"lines"))
    print(paste("File: ",filename,"finished"))
    gc()
  }

  shp <- st_read(gadm_shapefile)
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
    max_depth =  3,
    geometry = TRUE,
    geometry_union = TRUE
  )
  rm(shp)
  gc()
}
