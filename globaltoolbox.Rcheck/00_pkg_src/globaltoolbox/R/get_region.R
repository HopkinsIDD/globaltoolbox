data('region_data',package='globaltoolbox')


# Get WHO Regions using ISO
#' @name get.who.region
#' @title get.who.region
#' @description Identify WHO region from ISO
#' @param ISO vector of ISO3 codes
#' @return vector of WHO regions 
#' @export
get.who.region <- function(ISO){
  # Get WHO Regions using ISO
  return(as.character(region_data$who.region[match(toupper(ISO), region_data$ISO3)]))
}


# Get Regions using ISO
#' @name get.region
#' @title get.region
#' @description Identify region from ISO
#' @param ISO vector of ISO3 codes
#' @return vector of regions 
#' @export
get.region <- function(ISO){
  return(as.character(region_data$Region[match(toupper(ISO), region_data$ISO3)]))
}


# Get Sub Regions using ISO
#' @name get.subregion
#' @title get.subregion
#' @description Identify subregion from ISO
#' @param ISO vector of ISO3 codes
#' @return vector of subregions 
#' @export
get.subregion <- function(ISO){
  return(as.character(region_data$Sub.Region[match(toupper(ISO), region_data$ISO3)]))
}

