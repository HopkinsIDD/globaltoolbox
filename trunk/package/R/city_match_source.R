# Get and Use ISO COdes


# Install and load needed packages
if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('RCurl')) install.packages('RCurl'); library(RCurl)
if(!require('tibble')) install.packages('tibble'); library(tibble)
if(!require('rstudioapi')) install.packages('rstudioapi'); library(rstudioapi)
if(!require('devtools')) install.packages('devtools'); library(devtools)
if(!require('roxygen2')) install.packages('roxygen2'); library(roxygen2)
if(!require('stringdist')) install.packages('stringdist'); library(stringdist)


# Get directory of current file (of ISO_code_source.R). We will use this as the directory to save files downloaded locally.
current.wd <- getwd()
print(paste0('Current working directory: ', getwd()))
# source.path <- file.path(rstudioapi::getActiveDocumentContext()$path)
# tmp <- as.integer(gregexpr("/", source.path)[[1]])
# source.path <- substr(source.path,1,tmp[length(tmp)]-1)

# If sourced from other file, this will give this file's path
source.path <- NULL
tryCatch({
  source.path <- dirname(sys.frame(1)$ofile)
}, error= function(x) print('Local Run'))


# If run directly from file, this will give this file's path
if (is.null(source.path)) {
  source.path <- dirname(rstudioapi::getSourceEditorContext()$path)
}

print(source.path)




#####################################################
# Import and Clean the Data

# First Check that you have internet connection and can access github. If not, import local data source
has_internet <- TRUE
tryCatch({
  is.character(getURL("https://raw.githubusercontent.com/datasets/country-codes/master/data/"))==TRUE &
    !is.null(curl::nslookup("www.github.com")) }, 
  error= function(x){
    has_internet <- FALSE
    print('No Internet Connection')
  }
)


# If you have internet and github is accessible, update the data
if (has_internet) {
  iso_data <- region_data <- NULL
  
  # source ISO code source
  source_url("https://raw.githubusercontent.com/shauntruelove/RandomDataAndCode/master/countries_and_regions/source/ISO_code_source.R")
  
  # Get City data
  tryCatch({
      city_data <- read.csv('https://raw.githubusercontent.com/shauntruelove/RandomDataAndCode/master/countries_and_regions/data/worldcities.csv', stringsAsFactors=FALSE)
  },
  error= function(x) print('No Internet Connection')
  )
  
  dir.create(file.path(source.path, 'iso_source'), showWarnings = FALSE)
  source.path.folder <- file.path(source.path, 'iso_source')
  print(paste0("Data will be save in ", file.path(source.path.folder, ".")))
  
  # Write these data to directory if they are available so they are updated
  write.csv(city_data, file.path(source.path.folder, 'city_data'), row.names = FALSE)
  
  # If not, use the local data
} else {
  source.path.folder <- file.path(source.path, 'iso_source')
  
  if (file.exists(file.path(source.path.folder, 'iso_data.csv'))) {
    city_data <- read.csv(file.path(source.path.folder, 'city_data.csv'),  header=TRUE, stringsAsFactors=FALSE)
  } else {
    print('No access to internet/github and local files could not be located')
    stop()
  }
} 


# Clean up the city data a little

city_data$city_clean <- iconv(city_data$city_ascii, from = 'UTF-8', to = 'ASCII//TRANSLIT')
city_data$city_clean <- str_replace_all(city_data$city_clean, "[[:punct:]]", "") # remove all punctuation






# Functions to match city names -------------------------------------------

a_all <- traveldata$citytownvillage
a <- a_all[1]
b <- city_data$city_clean


tmp <- 

best_match_city <- function(a, b=city_data$city_clean, data=city_data,
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
    city <- paste(b[best_], collapse=", ")
  } else {
    city <- b[best_]
  }
  
  if (!return_score){
    return(city)
  } else if (return_score){
    return(list(city=city, score_sum=score_sum))
  } 
}



get.city.clean <- function(city){
  
  # just do the unique cities to speed it up
  city_all <- city
  city <- tolower(unique(city))
  city_indexes <- match(city_all, city)
  
  # Try matching using stringdist
  match_attempt <- as.character(sapply(city, best_match_city, b=tolower(city_data$city_clean)))
  View(cbind(city, match_attempt))
  
  return(match_attempt)
}




# 
# 
# 
# # Extract needed vectors
# ISO2=as.vector(iso_data$ISO3166.1.Alpha.2)
# ISO3=as.vector(iso_data$ISO3166.1.Alpha.3)
# UNcode=as.vector(iso_data$ISO3166.1.numeric)
# 
# Country=as.character(iso_data$official_name_en)
# Country2=as.character(iso_data$CLDR.display.name)
# 
# # get rid of non-ASCII
# Country <- iconv(Country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
# Country2 <- iconv(Country2, from = 'UTF-8', to = 'ASCII//TRANSLIT')
# 
# Country[grep("Ivoire", Country)] <- "Cote d'Ivoire"
# Country2[grep("Ivoire", Country2)] <- "Ivory Coast"
# Country[grep("Micronesia (Federated States of)", Country)] <- "Micronesia, Federated States of"
# 
# Country[grep("Taiwan", Country2)] <- "Taiwan"
# Country2[grep("Saudi Arabia", Country)] <- "KSA"
# 
# region.row <- as.integer(sapply(X=ISO3, FUN=function(X) which(toupper(region_data$alpha.3)==toupper(X))))
# who.row <-as.integer(sapply(X=ISO3, FUN=function(X) which(toupper(who_regions$iso)==toupper(X))))
# iso_data_full <- iso_data
# iso_data <- tibble(ISO2, ISO3, UNcode, Country, Country2,
#                    region_data$region[region.row], region_data$sub.region[region.row], 
#                    who_regions$who.region[who.row], who_regions$region.mortality[who.row])
# colnames(iso_data) <- c('ISO2', 'ISO3', 'UNcode', 'Country', 'Country2', 'Region', 'Sub.Region', 'WHO.Region', 'WHO.Region.Mortality')
# 
# # Fill in missing data
# iso_data$Sub.Region[is.na(iso_data$Sub.Region)] <- iso_data_full$Intermediate.Region.Name[is.na(iso_data$Sub.Region)]
# rm(who.row, region.row, Country2, Country, ISO3, ISO2)
# 
# iso_data[iso_data$Country=='Namibia', 'ISO2'] <- 'NA'
# iso_data[iso_data$Country=='Georgia', 'Country2'] <- 'Republic of Georgia'
# 
# ###################################################
# # Functions For Calling the codes
# 
# get.country.names.ISO3 <- function(ISO){
#   return(as.character(iso_data$Country2[match(toupper(ISO), iso_data$ISO3)]))
# }
# 
# get.country.names.ISO2 <- function(ISO2){
#   return(as.character(iso_data$Country2[match(toupper(ISO2), iso_data$ISO2)]))
# }
# 
# get.ISO3.from.ISO2 <- function(ISO2){
#   return(as.character(iso_data$ISO3[match(toupper(ISO2), iso_data$ISO2)]))
# }
# 
# get.ISO2.from.ISO3 <- function(ISO3){
#   return(as.character(iso_data$ISO2[match(toupper(ISO3), iso_data$ISO3)]))
# }
# 
# get.country.DHScode <- function(DHS_code){
#   return(as.character(dhs_countrydata$CountryName[match(toupper(DHS_code), toupper(dhs_countrydata$DHS_CountryCode))]))
# }
# 
# get.UNcode <- function(country){
#   iso <- get.iso(country, ISO.only=T)
#   return(iso_data$UNcode[match(toupper(iso),iso_data$ISO3)])
# }
# 
# get.UNcode.from.ISO3 <- function(ISO3){
#   return(iso_data$UNcode[match(toupper(ISO3), iso_data$ISO3)])
# }
# 
# 
# iso_match_fun <- function(country){
#   
#   iso.row.tmp <- which(country==tolower(iso_data$Country) | country==tolower(iso_data$Country2))
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
# 
# 
# best_match_country <- function(a, b=iso_data$Country, iso_list=iso_data$ISO3, 
#                                return_country_name=FALSE, return_score=FALSE, return_score_matrix=FALSE){
#   a <- tolower(a)
#   b <- tolower(b)
#   methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
#   dists <- as.data.frame(matrix(NA, nrow=length(b), ncol=length(methods), dimnames=list(b,methods)))
#   for (j in 1:length(methods)){
#     dists[,j]  <- stringdist(a, b, method=methods[j])
#   }
#   dists$score_sums <- rowSums(dists)
#   dists$osa <- as.integer(dists$osa)
#   
#   
#   best_ <- NULL
#   # get best from results
#   if (any(dists$osa<=1)){
#     best_ <- which.min(dists$score_sums)
#   } else if (any(dists$jw<=.1)){
#     best_ <- which.min(dists$score_sums)
#   } else if (any(dists$osa<=3 & dists$jw<=0.31 & dists$soundex == 0)){
#     best_ <- which(dists$osa<=3 & dists$jw<=0.31 & dists$soundex==0)
#   } 
#   
#   if (length(best_)==0 & !return_score_matrix){
#     return(NA)
#   } else if (length(best_)==0 & return_score_matrix){
#     return(dists)
#   }
#   if (length(best_>1)){
#     iso <- paste(iso_list[best_], collapse=", ")
#     country <- paste(b[best_], collapse=", ")
#   } else {
#     iso <- iso_list[best_]
#     country <- b[best_]
#   }
#   
#   if (!return_country_name & !return_score){
#     return(iso)
#   } else if (return_score & !return_country_name){
#     return(list(iso=iso, score_sum=score_sum))
#   } else if (return_score & return_country_name){
#     return(list(iso=iso, country=country, score_sum=score_sum))
#   } else if (return_country_name & !return_score){
#     return(list(iso=iso, country=country))
#   }
# }
# 
# 
# 
# # Get ISO3 from Country Name
# get.iso <- function(country, ISO.only=T){
#   
#   country <- tolower(country)
#   country <- str_replace_all(country, "[[:punct:]]", "") # remove all punctuation
#   
#   # just do the unique countries to speed it up
#   country_all <- country
#   country <- unique(country)
#   country_indexes <- match(country_all, country)
#   
#   # First try "countrycode" package
#   ISO <- countrycode(country, 'country.name', 'iso3c')
# 
#   # if completely successful, move on
#   if (sum(is.na(ISO))==0){
#     return(ISO[country_indexes])
#   } else {
#   
#     isos_need_match <- is.na(ISO)    
#     match_attempt2 <- as.character(sapply(country[isos_need_match], iso_match_fun))
#     match_attempt2[match_attempt2=="NULL"] <- NA
#     
#     # fill in new matches and get matches needed again
#     ISO[isos_need_match][!is.na(match_attempt2)] <- match_attempt2[!is.na(match_attempt2)]
#     
#     # if completely successful, move on
#     if (sum(is.na(ISO))==0){
#       return(ISO[country_indexes])
#     } else {
#       
#       isos_need_match <- is.na(ISO)    
#       match_attempt3 <- as.character(sapply(country[isos_need_match], best_match_country, b=tolower(iso_data$Country)))
#       
#       # fill in new matches and get matches needed again
#       ISO[isos_need_match][!is.na(match_attempt3)] <- match_attempt3[!is.na(match_attempt3)]
# 
#       # if completely successful, move on
#       if (sum(is.na(ISO))==0){
#         return(ISO[country_indexes])
#       } else {
#         
#         isos_need_match <- is.na(ISO)    
#         match_attempt4 <- as.character(sapply(country[isos_need_match], best_match_country, b=tolower(iso_data$Country2)))
#         
#         # fill in new matches and get matches needed again
#         ISO[isos_need_match][!is.na(match_attempt4)] <- match_attempt4[!is.na(match_attempt4)]
#         
#       }
#     }
#   }
#   return(ISO[country_indexes])
# }
# 
# 
# 
# # Get ISO2 from Country Name
# get.iso2 <- function(country){
#   iso3 <- get.iso(country, ISO.only = T)
#   # Get ISO2 using ISO3
#   return(as.character(iso_data$ISO2[match(toupper(iso3), iso_data$ISO3)]))
# }
# 
# 
# # Get WHO Regions using ISO
# get.who.region <- function(ISO){
#   # Get WHO Regions using ISO
#   return(as.character(who_regions$who.region[match(toupper(ISO), who_regions$iso)]))
# }
# 
# 
# # Get Other Regions using ISO
# get.region <- function(ISO){
#   # Get Regions using ISO
#   return(as.character(iso_data$Region[match(toupper(ISO), iso_data$ISO3)]))
# }
# # Get Other Regions using ISO
# get.subregion <- function(ISO){
#   # Get Sub Regions using ISO
#   return(as.character(iso_data$Sub.Region[match(toupper(ISO), iso_data$ISO3)]))
# }
# 
# 
# 
# 
# 
# 
# # Get ISO from nationality
# 
# # Get ISO3 from Country Name
# get.iso.from.nationality <- function(nationality, ISO.only=T){
#   
#   nationality <- tolower(nationality)
#   
#   iso.row.tmp <- which(grep(nationality, tolower(nationality_data$Nationalities)) | nationality==tolower(nationality_data$Country))
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
# 
