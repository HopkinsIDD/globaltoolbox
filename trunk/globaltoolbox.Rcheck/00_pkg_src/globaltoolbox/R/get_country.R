# Get and Use ISO COdes


# source("R/country_data.R")
# source("R/region_data.R")


data('country_names',package='globaltoolbox')


#' @name match_country
#' @title match_country
#' @description use stringdist to get best match for country name
#' @param a country name to match
#' @param country_names country_names df loaded with the package
#' @param return_ISO TRUE/FALSE
#' @param return_country_name TRUE/FALSE
#' @param return_score TRUE/FALSE
#' @param return_score_matrix TRUE/FALSE
#' @return ISOs, country names, matching scores, full matching distance matrix
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @export
match_country <- function(a, country_names=country_names, 
                               return_ISO=TRUE, return_country_name=FALSE, 
                               return_score=FALSE, return_score_matrix=FALSE){
  a <- tolower(a)
  a <- str_replace_all(a, "[[:punct:]]", "") # remove all punctuation
  
  b <- tolower(country_names$name)
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
    country <- paste(country_names$country_std[best_], collapse=", ")
  } else {
    iso <- country_names$ISO3[best_]
    country <- country_names$country_std[best_]
  }
  
  # Return only ISO
  if (!return_country_name & !return_score){
    return(iso)
  # Return only Country Name
  } else if (return_country_name & !return_ISO & !return_score){
    return(country)
  # Return ISO and Score
  } else if (return_score & !return_country_name){
    return(list(iso=iso, score_sum=score_sum))
  # Return ISO, Score, Country Name
  } else if (return_score & return_country_name){
    return(list(iso=iso, country=country, score_sum=score_sum))
  # Return ISO and Country Name
  } else if (return_country_name & !return_score){
    return(list(iso=iso, country=country))
  }
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
    match_attempt2 <- as.character(sapply(X=country[isos_need_match], 
                                          FUN=match_country, 
                                          country_names=country_names))
    
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
  # Get ISO2 using ISO3
  return(as.character(country_codes$Country[match(toupper(iso3), country_codes$ISO3)]))
}

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


# 
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


