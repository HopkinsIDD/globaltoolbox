#' @name standardize_string
#' @title standardize_string
#' @description Standardize indivual levels of a location of grouped names, separated by "|", for example "DC|Maryland".
#' This is used by standardize_location_strings.
#' @param string string location name to standardize.
#' @return standardized string of the location names.
#' @export
standardize_string <- function(string){
  
  ranked_encodings <- c('UTF-8', 'LATIN1') # Encodings to try
  string <- as.character(string)
  string <- strsplit(string, '|', fixed = TRUE)
  string <- lapply(string, function(x){
    return(
      stringi::stri_trans_general(
        stringi::stri_trans_general(
          stringi::stri_trans_general(x,"Any-Latn"),
          "Latin-ASCII"
        ),
        "Any-Lower"
      )
    )
  })
  string <- lapply(string, function(x){
      gsub(' ', '', x)
  })
  string <- lapply(string, function(x){
      gsub('[[:punct:]]', '', x)
  })
  string[sapply(string, length) == 0] <- ''
  return(string)
}




#' @name standardize_location_strings
#' @title standardize_location_strings
#' @description Standardize each level of a string name. This is used by others functions that standardize and match names.
#' @param location_name location name to match
#' @return standardized string of the location name.
#' @export
standardize_location_strings <- function(location_name){
  if(length(location_name) == 0){
    return(location_name)
  }
  location_tmp <- location_name
  location_tmp <- gsub('|', 'vertcharacter', location_tmp, fixed = TRUE)
  location_tmp <- gsub('-', 'dashcharacter', location_tmp, fixed = TRUE)
  location_tmp <- gsub('::', 'doublecoloncharacter', location_tmp, fixed = TRUE)
  location_tmp <- standardize_string(location_tmp)
  location_tmp <- gsub('doublecoloncharacter', '::', location_tmp, fixed = TRUE)
  location_tmp <- gsub('dashcharacter', '-', location_tmp, fixed = TRUE)
  location_tmp <- gsub('vertcharacter', '|', location_tmp, fixed = TRUE)
  while(any(
    grepl(pattern = '::$', location_tmp) |
    grepl(pattern = '::NA$', location_tmp) |
    grepl(pattern = '::::', location_tmp)
    )){
    location_tmp <- gsub(':::', ':', location_tmp, fixed = TRUE)
    location_tmp <- gsub('::NA$', '', location_tmp)
    location_tmp <- gsub('::$', '', location_tmp)
  }
  return(location_tmp)
}
