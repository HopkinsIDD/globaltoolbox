# Get and Use ISO COdes


source("R/country_data.R")
source("R/region_data.R")


###################################################
# Functions For Calling the codes



iso_match_fun <- function(country){
  
  iso.row.tmp <- which(country==tolower(iso_data$Country) | country==tolower(iso_data$Country2))
  
  if (length(iso.row.tmp)==0){
    iso.row.tmp <- unique(c(grep(country, tolower(iso_data$Country)), grep(country, tolower(iso_data$Country2))))
  }
  
  # Try matching individual words
  country2 <- gsub(' {2,}',' ',country)
  length.country <- length(strsplit(country2,' ')[[1]])
  country2 <- gsub('\\(', '', country2) # Get rid of parentheses
  country2 <- gsub('\\)', '', country2) # Get rid of parentheses
  
  if (length(iso.row.tmp)==0 & length.country>1){
    country.words <- strsplit(country2,' ')[[1]]
    matches <- lapply(country.words, grep, tolower(iso_data$Country)) # match each word of the country name with words in the iso data
    match.row <- Reduce(intersect, matches)  # The match row is identified as the one for which matches of multiple word occurs (intersect)
    
    if (length(match.row)>0){
      iso.row.tmp <- match.row
    } else if (length(match.row)==0 & sum(unlist(matches))>0) {
      iso.row.tmp <- as.integer(matches[which.min(lengths(matches))])  # Match row is the one with the least matches
    }
  }
  
  if (length(iso.row.tmp)==1){
    if (ISO.only){
      return(as.character(iso_data$ISO3[iso.row.tmp]))
    } else{
      return(as.vector(iso_data$ISO3[iso.row.tmp]))
    }
  } else if (length(iso.row.tmp==0)){
    #print('ISO Not Found')
    return('ISO Not Found')
  } else if (length(iso.row.tmp>1)){
    #print(paste0(length(iso.row.tmp),' ISO matches found for ', country))
    return(paste0(length(iso.row.tmp),' ISO matches found for ', country))
  }
}


best_match_country <- function(a, b=iso_data$Country, iso_list=iso_data$ISO3, 
                               return_country_name=FALSE, return_score=FALSE, return_score_matrix=FALSE){
  a <- tolower(a)
  b <- tolower(b)
  methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  dists <- as.data.frame(matrix(NA, nrow=length(b), ncol=length(methods), dimnames=list(b,methods)))
  for (j in 1:length(methods)){
    dists[,j]  <- stringdist(a, b, method=methods[j])
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
    iso <- paste(iso_list[best_], collapse=", ")
    country <- paste(b[best_], collapse=", ")
  } else {
    iso <- iso_list[best_]
    country <- b[best_]
  }
  
  if (!return_country_name & !return_score){
    return(iso)
  } else if (return_score & !return_country_name){
    return(list(iso=iso, score_sum=score_sum))
  } else if (return_score & return_country_name){
    return(list(iso=iso, country=country, score_sum=score_sum))
  } else if (return_country_name & !return_score){
    return(list(iso=iso, country=country))
  }
}



# Get ISO3 from Country Name
get.iso <- function(country, ISO.only=T){
  
  country <- tolower(country)
  country <- str_replace_all(country, "[[:punct:]]", "") # remove all punctuation
  
  # just do the unique countries to speed it up
  country_all <- country
  country <- unique(country)
  country_indexes <- match(country_all, country)
  
  # First try "countrycode" package
  ISO <- countrycode(country, 'country.name', 'iso3c')

  # if completely successful, move on
  if (sum(is.na(ISO))==0){
    return(ISO[country_indexes])
  } else {
  
    isos_need_match <- is.na(ISO)    
    match_attempt2 <- as.character(sapply(country[isos_need_match], iso_match_fun))
    match_attempt2[match_attempt2=="NULL"] <- NA
    
    # fill in new matches and get matches needed again
    ISO[isos_need_match][!is.na(match_attempt2)] <- match_attempt2[!is.na(match_attempt2)]
    
    # if completely successful, move on
    if (sum(is.na(ISO))==0){
      return(ISO[country_indexes])
    } else {
      
      isos_need_match <- is.na(ISO)    
      match_attempt3 <- as.character(sapply(country[isos_need_match], best_match_country, b=tolower(iso_data$Country)))
      
      # fill in new matches and get matches needed again
      ISO[isos_need_match][!is.na(match_attempt3)] <- match_attempt3[!is.na(match_attempt3)]

      # if completely successful, move on
      if (sum(is.na(ISO))==0){
        return(ISO[country_indexes])
      } else {
        
        isos_need_match <- is.na(ISO)    
        match_attempt4 <- as.character(sapply(country[isos_need_match], best_match_country, b=tolower(iso_data$Country2)))
        
        # fill in new matches and get matches needed again
        ISO[isos_need_match][!is.na(match_attempt4)] <- match_attempt4[!is.na(match_attempt4)]
        
      }
    }
  }
  return(ISO[country_indexes])
}




get.country.names.ISO3 <- function(ISO){
  return(as.character(iso_data$Country2[match(toupper(ISO), iso_data$ISO3)]))
}

get.country.names.ISO2 <- function(ISO2){
  return(as.character(iso_data$Country2[match(toupper(ISO2), iso_data$ISO2)]))
}

get.ISO3.from.ISO2 <- function(ISO2){
  return(as.character(iso_data$ISO3[match(toupper(ISO2), iso_data$ISO2)]))
}

get.ISO2.from.ISO3 <- function(ISO3){
  return(as.character(iso_data$ISO2[match(toupper(ISO3), iso_data$ISO3)]))
}

get.country.DHScode <- function(DHS_code){
  return(as.character(dhs_countrydata$CountryName[match(toupper(DHS_code), toupper(dhs_countrydata$DHS_CountryCode))]))
}

get.UNcode <- function(country){
  iso <- get.iso(country, ISO.only=T)
  return(iso_data$UNcode[match(toupper(iso),iso_data$ISO3)])
}

get.UNcode.from.ISO3 <- function(ISO3){
  return(iso_data$UNcode[match(toupper(ISO3), iso_data$ISO3)])
}




# Get ISO2 from Country Name
get.iso2 <- function(country){
  iso3 <- get.iso(country, ISO.only = T)
  # Get ISO2 using ISO3
  return(as.character(iso_data$ISO2[match(toupper(iso3), iso_data$ISO3)]))
}


# Get WHO Regions using ISO
get.who.region <- function(ISO){
  # Get WHO Regions using ISO
  return(as.character(who_regions$who.region[match(toupper(ISO), who_regions$iso)]))
}


# Get Other Regions using ISO
get.region <- function(ISO){
  # Get Regions using ISO
  return(as.character(iso_data$Region[match(toupper(ISO), iso_data$ISO3)]))
}
# Get Other Regions using ISO
get.subregion <- function(ISO){
  # Get Sub Regions using ISO
  return(as.character(iso_data$Sub.Region[match(toupper(ISO), iso_data$ISO3)]))
}






# Get ISO from nationality

# Get ISO3 from Country Name
get.iso.from.nationality <- function(nationality, ISO.only=T){
  
  nationality <- tolower(nationality)
  
  iso.row.tmp <- which(grep(nationality, tolower(nationality_data$Nationalities)) | nationality==tolower(nationality_data$Country))
  if (length(iso.row.tmp)==0){
    iso.row.tmp <- unique(c(grep(country, tolower(iso_data$Country)), grep(country, tolower(iso_data$Country2))))
  }
  
  # Try matching individual words
  country2 <- gsub(' {2,}',' ',country)
  length.country <- length(strsplit(country2,' ')[[1]])
  country2 <- gsub('\\(', '', country2) # Get rid of parentheses
  country2 <- gsub('\\)', '', country2) # Get rid of parentheses
  
  if (length(iso.row.tmp)==0 & length.country>1){
    country.words <- strsplit(country2,' ')[[1]]
    matches <- lapply(country.words, grep, tolower(iso_data$Country)) # match each word of the country name with words in the iso data
    match.row <- Reduce(intersect, matches)  # The match row is identified as the one for which matches of multiple word occurs (intersect)
    
    if (length(match.row)>0){
      iso.row.tmp <- match.row
    } else if (length(match.row)==0 & sum(unlist(matches))>0) {
      iso.row.tmp <- as.integer(matches[which.min(lengths(matches))])  # Match row is the one with the least matches
    }
  }
  
  if (length(iso.row.tmp)==1){
    if (ISO.only){
      return(as.character(iso_data$ISO3[iso.row.tmp]))
    } else{
      return(as.vector(iso_data$ISO3[iso.row.tmp]))
    }
  } else if (length(iso.row.tmp==0)){
    #print('ISO Not Found')
    return('ISO Not Found')
  } else if (length(iso.row.tmp>1)){
    #print(paste0(length(iso.row.tmp),' ISO matches found for ', country))
    return(paste0(length(iso.row.tmp),' ISO matches found for ', country))
  }
}

