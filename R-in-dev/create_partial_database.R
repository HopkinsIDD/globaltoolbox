devtools::install_github("HopkinsIDD/globaltoolbox")
# install.packages('/home/jkaminsky/git/globaltoolbox',type='source',repos=NULL)
options(warn=1,error=recover)

library(sf)
library(globaltoolbox)

create_database <- function(filename,gaul_directory,gadm_shapefile,country = ""){
  reset_database(dbname = filename)
  create_database(dbname = filename)

## Load alt_country_names
load_country_aliases(filename)

dirname <- gaul_directory
all_files <- list.files(dirname)
all_files <- all_files[!grepl(".zip",all_files)]
for(file in all_files){
  filename <- paste0(dirname,"/",file,"/",file,".shp")
  print(filename)
  year <- as.numeric(gsub('.*2015_([1234567890]*).*','\\1',filename))
  print(paste("File: ",filename))
  shp <- st_read(filename)
  browser()
  levels <- max(as.numeric(unique(gsub("_.*","",gsub("ADM","",names(shp))))),na.rm=TRUE)
  hierarchy_column_names <- paste0("ADM",0:levels,"_NAME")
  hierarchy_column_names <- hierarchy_column_names[hierarchy_column_names %in% names(shp)]
  alias_column_names = rep(c(),times=length(hierarchy_column_names))
  # alias_column_names = lapply(0:levels,function(x){
  #   x <- paste0("ADM", x, "_CODE")
  #   x <- x[x %in% names(shp)]
  # })
  load_hierarchical_sf(
    filename = filename,
    time_left = paste(year,"01","01",sep='-'),
    time_right = paste(year,"12","31",sep='-'),
    hierarchy_column_names = hierarchy_column_names,
    alias_column_names = alias_column_names,
    source_name = "GAUL",
    geometry = TRUE,
    max_depth = 3,
    dbname = "/home/jkaminsky/GAUL.sqlite"
  )
  print(paste("File: ",filename,"finished"))
}

shp <- st_read(gadm_shapefile)
alias_column_names <- lapply(0:5,function(x){
  x = paste(c("GID","HASC","VARNAME","NLAME","VARNAME"),x,sep="_")
  return(x[x %in% names(shp)])
})
load_hierarchical_sf(
  filename = '/home/jkaminsky/gadm36.shp',
  time_left = '1800-01-01',
  time_right = '2019-12-31',
  hierarchy_column_names = paste("NAME",0:5,sep="_"),
  alias_column_names = alias_column_names,
  source_name = "GADM",
  max_depth =  3,
  geometry = TRUE,
  geometry_union = TRUE,
  dbname = "/home/jkaminsky/GAUL.sqlite"
)
