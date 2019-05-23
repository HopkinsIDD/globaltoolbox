#' @name standardize_string
#' @title standardize_string
#' @description Standardize indivual levels of a location of grouped names, separated by "|", for example "DC|Maryland". 
#' This is used by standardize_location_strings.
#' @param string string location name to standardize.
#' @return standardized string of the location names.
#' @export
ranked_encodings <- c('UTF-8', 'LATIN1')
standardize_string <- function(string){
  string <- as.character(string)
  string <- strsplit(string, '|', fixed = TRUE)
  string <- lapply(string, function(x){
      gsub(' ', '', x)
  })
  string <- lapply(string, function(x){
    if(length(x) == 0){
      return(x)
    }
    for(i in 1:length(x)){
      if(Encoding(x[i]) != 'unknown'){
        x[i] <- iconv(from = Encoding(x[i]), to = 'ASCII//TRANSLIT', x[i])
      } else {
          for(encoding in ranked_encodings){
              if(!is.na(x[i])){
                  next
              }
              y <- iconv(
                  from = encoding,
                  to = 'ASCII//TRANSLIT',
                  x[i]
              )
              if(!is.na(y)){
                  x[i] <- y
              }
          }
      }
    }
    return(x)
  })
  string <- lapply(string, function(x){
      gsub('[[:punct:]]', '', x)
  })
  string <- lapply(string, function(x){
      tolower(x)
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
    grepl(pattern = '::$', location_tmp) ||
    grepl(pattern = '::NA$', location_tmp) ||
    grepl(pattern = '::::', location_tmp)
    )){
    location_tmp <- gsub(':::', ':', location_tmp, fixed = TRUE)
    location_tmp <- gsub('::NA$', '', location_tmp)
    location_tmp <- gsub('::$', '', location_tmp)
  }
  return(location_tmp)
}
