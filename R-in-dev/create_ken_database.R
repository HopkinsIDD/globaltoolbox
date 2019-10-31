# devtools::install_github("HopkinsIDD/globaltoolbox")


install.packages('~/git/globaltoolbox',type='source',repos=NULL)
options(warn=1,error=recover)

library(sf)
library(globaltoolbox)

# reset_database("globaltoolbox")
# create_database("globaltoolbox")

## Load alt_country_names
# load_country_aliases('globaltoolbox')

dirname <- "~/KEN_shp"
all_files <- list.files(dirname)
all_files <- all_files[grepl(".shp$",all_files)]
for(file in all_files){
  filename <- paste0(dirname,"/",file)
  print(filename)
  year <- as.numeric(gsub('.*2015_([1234567890]*).*','\\1',filename))
  print(paste("File: ",filename))
  shp <- st_read(filename)
  levels <- max(as.numeric(unique(gsub("_.*","",gsub("ADM","",names(shp))))),na.rm=TRUE)
  hierarchy_column_names <- paste0("ADM",0:levels,"_NAME")
  hierarchy_column_names <- hierarchy_column_names[hierarchy_column_names %in% names(shp)]
  alias_column_names = lapply(0:levels,function(x){c("")})
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
    refresh_on_exit = FALSE,
    dbname = "globaltoolbox"
  )
  print(paste("File: ",filename,"finished"))
}

shp <- sf::st_read('~/gadm_ken.shp')
alias_column_names <- lapply(0:5,function(x){
  x = paste(c("GID","HASC","VARNAME","NLAME","VARNAME"),x,sep="_")
  return(x[x %in% names(shp)])
})
load_hierarchical_sf(
  filename = '~/gadm_ken.shp',
  time_left = as.character("NA"),
  time_right = as.character("NA"),
  hierarchy_column_names = paste("NAME",0:5,sep="_"),
  alias_column_names = alias_column_names,
  source_name = "GADM",
  geometry = TRUE,
  geometry_union = TRUE,
  refresh_on_exit = TRUE,
  dbname = "globaltoolbox"
)

merge_all_geometric_duplicates("globaltoolbox")
