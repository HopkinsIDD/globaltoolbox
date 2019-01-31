##' Source code to build Country-based data source
##' 



# Install and load needed packages
if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('RCurl')) install.packages('RCurl'); library(RCurl)
if(!require('tibble')) install.packages('tibble'); library(tibble)
if(!require('countrycode')) install.packages('countrycode'); library(countrycode)
if(!require('rstudioapi')) install.packages('rstudioapi'); library(rstudioapi)
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
  
  # Get Country Codes, Regions, 
  tryCatch({
    iso_data         <- read.csv("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv", stringsAsFactors=F)
    region_data      <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv", stringsAsFactors=F)
    who_regions      <- read.csv("data-raw/who_regions.csv", stringsAsFactors=FALSE)
    dhs_countrydata  <- read.csv("data-raw/DHS_countrydata.csv", stringsAsFactors=FALSE)
    nationality_data <- read.csv("data-raw/nationalities_and_languages.csv", stringsAsFactors=FALSE)
  },
  error= function(x) print('No Internet Connection')
  )
  
  dir.create(file.path(source.path, 'iso_source'), showWarnings = FALSE)
  source.path.folder <- file.path(source.path, 'iso_source')
  print(paste0("Data will be save in ", file.path(source.path.folder, ".")))
  
  # Write these data to directory if they are available so they are updated
  write.csv(iso_data, file.path(source.path.folder, 'iso_data.csv'), row.names = FALSE)
  write.csv(region_data, file.path(source.path.folder, 'region_data.csv'), row.names = FALSE)    
  write.csv(who_regions, file.path(source.path.folder, 'who_regions.csv'), row.names = FALSE)
  write.csv(dhs_countrydata, file.path(source.path.folder, 'dhs_countrydata.csv'), row.names = FALSE)
  write.csv(nationality_data, file.path(source.path.folder, 'nationalities_and_languages.csv'), row.names = FALSE)
  
  # If not, use the local data
} else {
  source.path.folder <- file.path(source.path, 'iso_source')
  
  if (file.exists(file.path(source.path.folder, 'iso_data.csv'))) {
    iso_data <- read.csv(file.path(source.path.folder, 'iso_data.csv'), header=TRUE, stringsAsFactors=FALSE)
    region_data <- read.csv(file.path(source.path.folder, 'region_data.csv'),  header=TRUE, stringsAsFactors=FALSE)
    who_regions <- read.csv(file.path(source.path.folder, 'who_regions.csv'),  header=TRUE, stringsAsFactors=FALSE)
    dhs_countrydata <- read.csv(file.path(source.path.folder, 'dhs_countrydata.csv'),  header=TRUE, stringsAsFactors=FALSE)
    nationality_data <- read.csv(file.path(source.path.folder, 'nationalities_and_languages.csv'),  header=TRUE, stringsAsFactors=FALSE)
    
  } else {
    print('No access to internet/github and local files could not be located')
    stop()
  }
} 




# Extract needed vectors
ISO2=as.vector(iso_data$ISO3166.1.Alpha.2)
ISO3=as.vector(iso_data$ISO3166.1.Alpha.3)
UNcode=as.vector(iso_data$ISO3166.1.numeric)

Country=as.character(iso_data$official_name_en)
Country2=as.character(iso_data$CLDR.display.name)

# get rid of non-ASCII
Country <- iconv(Country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
Country2 <- iconv(Country2, from = 'UTF-8', to = 'ASCII//TRANSLIT')

Country[grep("Ivoire", Country)] <- "Cote d'Ivoire"
Country2[grep("Ivoire", Country2)] <- "Ivory Coast"
Country[grep("Micronesia (Federated States of)", Country)] <- "Micronesia, Federated States of"

Country[grep("Taiwan", Country2)] <- "Taiwan"
Country2[grep("Saudi Arabia", Country)] <- "KSA"

region.row <- as.integer(sapply(X=ISO3, FUN=function(X) which(toupper(region_data$alpha.3)==toupper(X))))
who.row <-as.integer(sapply(X=ISO3, FUN=function(X) which(toupper(who_regions$iso)==toupper(X))))
iso_data_full <- iso_data
iso_data <- tibble(ISO2, ISO3, UNcode, Country, Country2,
                   region_data$region[region.row], region_data$sub.region[region.row], 
                   who_regions$who.region[who.row], who_regions$region.mortality[who.row])
colnames(iso_data) <- c('ISO2', 'ISO3', 'UNcode', 'Country', 'Country2', 'Region', 'Sub.Region', 'WHO.Region', 'WHO.Region.Mortality')

# Fill in missing data
iso_data$Sub.Region[is.na(iso_data$Sub.Region)] <- iso_data_full$Intermediate.Region.Name[is.na(iso_data$Sub.Region)]
rm(who.row, region.row, Country2, Country, ISO3, ISO2)

iso_data[iso_data$Country=='Namibia', 'ISO2'] <- 'NA'
iso_data[iso_data$Country=='Georgia', 'Country2'] <- 'Republic of Georgia'





devtools::use_data(x)


