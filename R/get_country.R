# Get and Use ISO COdes


# source("R/country_data.R")
# source("R/region_data.R")

data('country_names',package='globaltoolbox')
data('country_codes',package='globaltoolbox')




#' @name match_country
#' @title match_country
#' @description use stringdist to get best match for country name
#' @param a country name to match
#' @param return_ISO TRUE/FALSE
#' @param return_country_name TRUE/FALSE
#' @param return_score TRUE/FALSE
#' @param return_score_matrix TRUE/FALSE
#' @return ISOs, country names, matching scores, full matching distance matrix
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @export
match_country <- function(a, #country_names=country_names, 
                               return_ISO=TRUE, return_country_name=FALSE, 
                               return_score=FALSE, return_score_matrix=FALSE){
  
  if(length(a)>1 | length(a)==0){
    stop("ERROR: 'a' can only be of length 1")
  }
  if(is.na(a)){
    return(NA)
  }

  
  a <- tolower(a)
  a <- str_replace_all(a, "[[:punct:]]", "") # remove all punctuation
  b <- tolower(country_names$names)
  
  methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  dists <- as.data.frame(matrix(NA, nrow=length(b), ncol=length(methods), 
                                dimnames=list(b, methods)))
  for (j in 1:length(methods)){
    dists[,j]  <- suppressWarnings(stringdist(a, b, method=methods[j]))
  }
  dists$score_sums <- rowSums(dists)
  dists$osa <- as.integer(dists$osa)
  
  best_ <- NULL
  # get best from results
  if (any(dists$osa<=1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$jw<=.1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$osa<=3 & dists$jw<=0.31 & dists$soundex == 0)){
    best_ <- which(dists$osa<=3 & dists$jw<=0.31 & dists$soundex==0)
  } 
  
  if (length(best_)==0 & !return_score_matrix){
    return(NA)
  } else if (length(best_)==0 & return_score_matrix){
    return(dists)
  }
  if (length(best_>1)){
    iso <- paste(country_names$ISO3[best_], collapse=", ")
    country <- paste(country_names$Name[best_], collapse=", ")
    score_sum <- paste(dists$score_sums[best_], collapse=", ")
  } else {
    iso <- country_names$ISO3[best_]
    country <- country_names$Name[best_]
    score_sum <- dists$score_sums[best_]
  }
  
  res <- data.frame(iso=iso, country=country, score_sum=score_sum)
  return(res[, which(c(return_ISO, return_country_name, return_score))])
}

# # Example
# data('test_country_names',package='globaltoolbox')
# 
# test_country_name <- sample(test_country_names, 1)
# print(test_country_name)
# 
# match_country(a=test_country_name, country_names=country_names, return_ISO=FALSE,
#                    return_country_name=TRUE, return_score=FALSE, return_score_matrix=FALSE)
# sapply(X=test_country_names, FUN=match_country, country_names=country_names, return_ISO=FALSE,
#         return_country_name=TRUE, return_score=FALSE, return_score_matrix=FALSE)




data('locations_lvl2',package='globaltoolbox')

#' @name match_locs_level2
#' @title match_locs_level2
#' @description use stringdist to get best match for country name, if not official country
#' @param a location name to match
#' @param names locations_lvl2 df loaded with the package
#' @param return_ISO3 TRUE/FALSE
#' @param return_Code TRUE/FALSE
#' @param return_name TRUE/FALSE return standardized name
#' @param return_score TRUE/FALSE
#' @param return_score_matrix TRUE/FALSE
#' @return ISOs, country names, matching scores, full matching distance matrix
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @export
match_locs_level2 <- function(a, names=locations_lvl2, 
                          return_ISO3=FALSE, return_name=TRUE, return_Code=FALSE,
                          return_score=FALSE, return_score_matrix=FALSE){
  
  if(length(a)>1 | length(a)==0){
    stop("ERROR: 'a' can only be of length 1")
  }
  if(is.na(a)){
    return(NA)
  }
  
  a <- tolower(a)
  a <- str_replace_all(a, "[[:punct:]]", "") # remove all punctuation
  b <- tolower(names$Name)
  
  methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  dists <- as.data.frame(matrix(NA, nrow=length(b), ncol=length(methods), 
                                dimnames=list(b, methods)))
  for (j in 1:length(methods)){
    dists[,j]  <- suppressWarnings(stringdist(a, b, method=methods[j]))
  }
  dists$score_sums <- rowSums(dists)
  dists$osa <- as.integer(dists$osa)
  
  best_ <- NULL
  # get best from results
  if (any(dists$osa<=1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$jw<=.1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$osa<=3 & dists$jw<=0.31 & dists$soundex == 0)){
    best_ <- which(dists$osa<=3 & dists$jw<=0.31 & dists$soundex==0)
  } 
  
  if (length(best_)==0 & !return_score_matrix){
    return(NA)
  } else if (length(best_)==0 & return_score_matrix){
    return(dists)
  }
  if (length(best_>1)){
    iso <- paste(names$ISO3[best_], collapse=", ")
    code <- paste(names$Code[best_], collapse=", ")
    name <- paste(names$Name[best_], collapse=", ")
    score_sum <- paste(dists$score_sums[best_], collapse=", ")
  } else {
    iso <- names$ISO3[best_]
    code <- names$Code[best_]
    name <- names$Name[best_]
    score_sum <- dists$score_sums[best_]
  }
  
  res <- data.frame(code=code, iso=iso, name=name, score_sum=score_sum)
  return(res[, c(return_Code, return_ISO3, return_name, return_score)])
}



data('city_data',package='globaltoolbox')

#' @name match_city
#' @title match_city
#' @description use stringdist to get best match for city name
#' @param a location name to match
#' @param names city df loaded with the package
#' @param return_ISO3 TRUE/FALSE
#' @param return_name TRUE/FALSE return standardized name
#' @param return_score TRUE/FALSE
#' @param return_score_matrix TRUE/FALSE
#' @return ISOs, city names, matching scores, full matching distance matrix
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @export
match_city <- function(a, return_ISO3=FALSE, return_name=TRUE,
                          return_score=FALSE, return_score_matrix=FALSE){
  
  if(length(a)>1 | length(a)==0){
    stop("ERROR: 'a' can only be of length 1")
  }
  if(is.na(a)){
    return(NA)
  }
  
  
  a <- tolower(a)
  a <- str_replace_all(a, "[[:punct:]]", "") # remove all punctuation
  b <- tolower(city_data$city_ascii)
  
  methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  dists <- as.data.frame(matrix(NA, nrow=length(b), ncol=length(methods), dimnames=list(b, methods)))
  for (j in 1:length(methods)){
    dists[,j]  <- suppressWarnings(stringdist(a, b, method=methods[j]))
  }
  dists$score_sums <- rowSums(dists)
  dists$osa <- as.integer(dists$osa)
  
  best_ <- NULL
  # get best from results
  if (any(dists$osa<=1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$jw<=.1)){
    best_ <- which.min(dists$score_sums)
  } else if (any(dists$osa<=3 & dists$jw<=0.31 & dists$soundex == 0)){
    best_ <- which(dists$osa<=3 & dists$jw<=0.31 & dists$soundex==0)
  } 
  
  if (length(best_)==0 & !return_score_matrix){
    return(NA)
  } else if (length(best_)==0 & return_score_matrix){
    return(dists)
  }
  if (length(best_>1)){
    iso <- paste(city_data$ISO3[best_], collapse=", ")
    name <- paste(city_data$city_ascii[best_], collapse=", ")
    score_sum <- paste(city_data$score_sums[best_], collapse=", ")
  } else {
    iso <- city_data$ISO3[best_]
    name <- city_data$city_ascii[best_]
    score_sum <- city_data$score_sums[best_]
  }
  
  res <- data.frame(iso=iso, name=name, score_sum=score_sum)
  return(res[, c(return_ISO3, return_name, return_score)])
}








# Get ISO3 from Country Name 
#' @name get.iso
#' @title get.iso
#' @description get the official ISO for a vector of countries. This will identify most likely matched 
#' to the inputted country names used a variety of tools and methods.
#' @param country vector of country names
#' @return vector of ISOs for each country. These can then be used to identify other characteristics of the country
#' @importFrom countrycode countrycode
#' @export
get.iso <- function(country){
  
  country <- tolower(country)
  #country <- str_replace_all(country, "[[:punct:]]", "") # remove all punctuation
  
  # just check the unique countries to speed it up
  country_all <- country
  country <- unique(country)
  country_indexes <- match(country_all, country)
  
  # First try "countrycode" package
  ISO <- suppressWarnings(countrycode(country, 'country.name', 'iso3c'))

  # If not completely successful, try using "match_country" function
  if (sum(is.na(ISO))!=0){
    isos_need_match <- is.na(ISO)    
    match_attempt2 <- as.character(lapply(X=country[isos_need_match], 
                                          FUN=function(X) as.character(match_country(X)) ))
    # fill in new matches
    ISO[isos_need_match] <- match_attempt2
  }
  return(ISO[country_indexes])
}

# # Example
# data('test_country_names',package='globaltoolbox')
# get.iso(country=test_country_names)



# Get Standardized Country Name from Country Name 
#' @name get.country.name.std
#' @title get.country.name.std
#' @description Standard country name.
#' @param country vector of country names
#' @return vector of country names matching each country. These can then be used to identify other characteristics of the country
#' @export
get.country.name.std <- function(country){
  iso3 <- get.iso(country)
  # Get Country using ISO3
  return(as.character(country_codes$Country[match(toupper(iso3), country_codes$ISO3)]))
}

# # Example
# data('test_country_names',package='globaltoolbox')
# get.country.name.std(country=test_country_names)




# Get Standardized Name for Location 
#' @name get.location.std
#' @title get.location.std
#' @description get a standardized name for a vector of locations. This will identify most likely matched 
#' to the inputted names used a variety of tools and methods. This should be used when the data likely have
#' locations that are not only countries (i.e., some sub-countries like England or Dubai)
#' @param location vector of location names
#' @return vector of names for each location. These can then be used to identify other characteristics of the location
#' @export
get.location.std <- function(location){

  #location <- str_replace_all(location, "[[:punct:]]", "") # remove all punctuation
  location <- tolower(location)
  
  # just check the unique countries to speed it up
  location_all <- location
  location <- unique(location)
  location_indexes <- match(location_all, location)
  
  # # First try to match the country using get.iso and get.country.name.std
  loc_stds <- get.country.name.std(location)
  
  # If not completely successful, try using "match_locs_level2" function
  if (sum(is.na(loc_stds))!=0){
    locs_need_match <- is.na(loc_stds)    
    match_attempt2 <- as.character(lapply(X=location[locs_need_match],
                                          FUN=function(X) as.character(match_locs_level2(X)) ))
    # fill in new matches
    loc_stds[locs_need_match] <- match_attempt2
  }
  return(loc_stds[location_indexes])
}

# # Example
# data('test_mixed_names',package='globaltoolbox')
# get.iso(country=test_mixed_names)




# Get Standardized Name for City 
#' @name get.location.std
#' @title get.location.std
#' @description get a standardized name for a vector of Cities. This will identify most likely matched 
#' to the inputted names used a variety of tools and methods.
#' @param location vector of city names
#' @return vector of names for each location. These can then be used to identify other characteristics of the location
#' @export
get.city.std <- function(location){
  
  #location <- str_replace_all(location, "[[:punct:]]", "") # remove all punctuation
  location <- tolower(location)
  
  # just check the unique countries to speed it up
  location_all <- location
  location <- unique(location)
  location_indexes <- match(location_all, location)
  
  loc_stds <- as.character(lapply(X=location,
                                        FUN=function(X) as.character(match_city(X)) ))
  
  # If not completely successful, try using "match_locs_level2" function
  if (sum(is.na(ISO))!=0){
    locs_need_match <- is.na(loc_stds)    
    match_attempt2 <- as.character(lapply(X=location[locs_need_match],
                                          FUN=function(X) as.character(match_locs_level2(X)) ))
    # fill in new matches
    loc_stds[locs_need_match] <- match_attempt2
  }
  return(loc_stds[location_indexes])
}


# # Example
# data('test_city_names',package='globaltoolbox')
# get.city.std(location=test_city_names)






# Get Standardized Country Name from ISO3 
#' @name get.country.name.ISO3
#' @title get.country.name.ISO3
#' @description Standard country name from ISO3.
#' @param country vector of ISO3s
#' @return vector of country names matching each ISO. These can then be used to identify other characteristics of the country
#' @export
get.country.name.ISO3 <- function(ISO){
  return(as.character(country_codes$Country[match(toupper(ISO), country_codes$ISO3)]))
}


# Get Standardized Country Name from ISO2 
#' @name get.country.name.ISO2
#' @title get.country.name.ISO2
#' @description Standard country name from ISO2.
#' @param ISO2 vector of ISO2s
#' @return vector of country names matching ISO2. These can then be used to identify other characteristics of the country
#' @export
get.country.name.ISO2 <- function(ISO2){
  return(as.character(country_codes$Country[match(toupper(ISO2), country_codes$ISO2)]))
}



# Get ISO3 from ISO2 
#' @name get.ISO3.from.ISO2
#' @title get.ISO3.from.ISO2
#' @description ISO3 from ISO2.
#' @param ISO2 vector of ISO2s
#' @return vector of ISO3s matching each ISO2 These can then be used to identify other characteristics of the country
#' @export
get.ISO3.from.ISO2 <- function(ISO2){
  return(as.character(country_codes$ISO3[match(toupper(ISO2), country_codes$ISO2)]))
}



# Get ISO2 from ISO3 
#' @name get.ISO2.from.ISO3
#' @title get.ISO2.from.ISO3
#' @description ISO2 from ISO3.
#' @param ISO3 vector of ISO3s
#' @return vector of ISO2s matching each ISO3.
#' @export
get.ISO2.from.ISO3 <- function(ISO3){
  return(as.character(country_codes$ISO2[match(toupper(ISO3), country_codes$ISO3)]))
}



# Get UN code from Country 
#' @name get.UNcode
#' @title get.UNcode
#' @description UN code from country
#' @param country vector of country names
#' @return vector of UN codes matching each country. 
#' @export
get.UNcode <- function(country){
  iso <- get.iso(country)
  return(country_codes$UNcode[match(toupper(iso),country_codes$ISO3)])
}



# Get UN code from ISO3 
#' @name get.UNcode.from.ISO3
#' @title get.UNcode.from.ISO3
#' @description UN code from ISO3
#' @param ISO3 vector of ISO3
#' @return vector of UN codes matching each ISO3 
#' @export
get.UNcode.from.ISO3 <- function(ISO3){
  return(country_codes$UNcode[match(toupper(ISO3), country_codes$ISO3)])
}





# get.country.DHScode <- function(DHS_code){
#   return(as.character(dhs_countrydata$CountryName[match(toupper(DHS_code), toupper(dhs_countrydata$DHS_CountryCode))]))
# }



# Get various country codes from Country Name
#' @name get.country.code
#' @title get.country.code
#' @description various country codes from country names
#' @param country vector of countries
#' @param code vector of code types wanted (Includes: "FIFA", "IOC", "ISO2", "UNcode", "MARC", "GAUL", "FIPS", "WMO",
#'                 "ITU", "DS", "M49", "Continent", "TLD", "Geoname.ID", "EDGAR")
#' @return vector of codes matching each country 
#' @export
get.country.code <- function(country, 
                             code=c("FIFA", "IOC", "ISO2", "UNcode", "MARC", "GAUL", "FIPS", "WMO",
                                    "ITU", "DS", "M49", "Continent", "TLD", "Geoname.ID", "EDGAR")){
    iso3 <- get.iso(country)
    # Get code using ISO3
    code_columns <- match(toupper(code), toupper(colnames(country_codes)))
    return(as.character(country_codes[code_columns, match(toupper(iso3), country_codes$ISO3)]))
}




# Get ISO from nationality
# 
# # Get ISO3 from Country Name
# get.iso.from.nationality <- function(nationality, ISO.only=T){
#   
#   nationality <- tolower(nationality)
#   
# 
#   
#   if (length(iso.row.tmp)==1){
#     if (ISO.only){
#       return(as.character(iso_data$ISO3[iso.row.tmp]))
#     } else{
#       return(as.vector(iso_data$ISO3[iso.row.tmp]))
#     }
#   } else if (length(iso.row.tmp==0)){
#     #print('ISO Not Found')
#     return('ISO Not Found')
#   } else if (length(iso.row.tmp>1)){
#     #print(paste0(length(iso.row.tmp),' ISO matches found for ', country))
#     return(paste0(length(iso.row.tmp),' ISO matches found for ', country))
#   }
# }





# iso_match_fun <- function(country){
# 
#   iso.row.tmp <- which(country==tolower(iso_data$Country) | country==tolower(iso_data$Country2))
# 
#   if (length(iso.row.tmp)==0){
#     iso.row.tmp <- unique(c(grep(country, tolower(iso_data$Country)), grep(country, tolower(iso_data$Country2))))
#   }
# 
#   # Try matching individual words
#   country2 <- gsub(' {2,}',' ',country)
#   length.country <- length(strsplit(country2,' ')[[1]])
#   country2 <- gsub('\\(', '', country2) # Get rid of parentheses
#   country2 <- gsub('\\)', '', country2) # Get rid of parentheses
# 
#   if (length(iso.row.tmp)==0 & length.country>1){
#     country.words <- strsplit(country2,' ')[[1]]
#     matches <- lapply(country.words, grep, tolower(iso_data$Country)) # match each word of the country name with words in the iso data
#     match.row <- Reduce(intersect, matches)  # The match row is identified as the one for which matches of multiple word occurs (intersect)
# 
#     if (length(match.row)>0){
#       iso.row.tmp <- match.row
#     } else if (length(match.row)==0 & sum(unlist(matches))>0) {
#       iso.row.tmp <- as.integer(matches[which.min(lengths(matches))])  # Match row is the one with the least matches
#     }
#   }
# 
#   if (length(iso.row.tmp)==1){
#     if (ISO.only){
#       return(as.character(iso_data$ISO3[iso.row.tmp]))
#     } else{
#       return(as.vector(iso_data$ISO3[iso.row.tmp]))
#     }
#   } else if (length(iso.row.tmp==0)){
#     #print('ISO Not Found')
#     return('ISO Not Found')
#   } else if (length(iso.row.tmp>1)){
#     #print(paste0(length(iso.row.tmp),' ISO matches found for ', country))
#     return(paste0(length(iso.row.tmp),' ISO matches found for ', country))
#   }
# }


