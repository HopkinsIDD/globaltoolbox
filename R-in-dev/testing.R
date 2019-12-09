# sample 5 cities from the metro dataset
five_cities <- metro[sample(length(metro), 5), ]





# ggmap package

ggmap::geocode(dengue_locs$city[260:265])



#dbname <- "../gaul_gadm.sqlite"



# Openstreetmaps

## geocoding function using OSM Nominatim API
## details: http://wiki.openstreetmap.org/wiki/Nominatim
## made by: D.Kisler 



nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}

i=2
nominatim_osm(paste(dengue_locs$orig[i], dengue_locs$country[i], sep=", "))

              