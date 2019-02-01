##' Source code to build Country-based data source
##' 



# Install and load needed packages
if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('RCurl')) install.packages('RCurl'); library(RCurl)
if(!require('tibble')) install.packages('tibble'); library(tibble)
  if(!require('countrycode')) install.packages('countrycode'); library(countrycode)
#if(!require('rstudioapi')) install.packages('rstudioapi'); library(rstudioapi)
#if(!require('stringdist')) install.packages('stringdist'); library(stringdist)



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
    alt_country_names <- rworldmap::countrySynonyms
    region_data_2    <- rworldmap::countryRegions
  },
  error= function(x) print('No Internet Connection')
  )
  
  # dir.create(file.path(source.path, 'iso_source'), showWarnings = FALSE)
  # source.path.folder <- file.path(source.path, 'iso_source')
  # print(paste0("Data will be save in ", file.path(source.path.folder, ".")))
  # 
  # Write these data to directory if they are available so they are updated
  write.csv(iso_data, 'data-raw/iso_data.csv', row.names = FALSE)
  write.csv(region_data, 'data-raw/region_data.csv', row.names = FALSE)    
  write.csv(who_regions, 'data-raw/who_regions.csv', row.names = FALSE)
  write.csv(dhs_countrydata, 'data-raw/dhs_countrydata.csv', row.names = FALSE)
  write.csv(nationality_data, 'data-raw/nationalities_and_languages.csv', row.names = FALSE)
  write.csv(alt_country_names, 'data-raw/alt_country_names.csv', row.names = FALSE)
  write.csv(region_data_2, 'data-raw/region_data_2.csv', row.names = FALSE)
  
  # If not, use the local data
} else {
  source.path.folder <- file.path(source.path, 'iso_source')
  
  if (file.exists('data-raw/iso_data.csv')) {
    iso_data <- read.csv('data-raw/iso_data.csv', header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
    region_data <- read.csv('data-raw/region_data.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
    who_regions <- read.csv('data-raw/who_regions.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
    dhs_countrydata <- read.csv('data-raw/dhs_countrydata.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
    nationality_data <- read.csv('data-raw/nationalities_and_languages.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
    alt_country_names <- read.csv('data-raw/alt_country_names.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
    region_data_2 <- read.csv('data-raw/region_data_2.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
    
  } else {
    print('No access to internet/github and local files could not be located')
    stop()
  }
} 





# Build Single Region Dataset ---------------------------------------------
#  - this will be linked using country codes

# Check for missing ISOs
sum(is.na(region_data$iso))
sum(is.na(region_data_2$iso))
sum(is.na(who_regions$iso))

region_data$name <- iconv(region_data$name, from = 'UTF-8', to = 'ASCII//TRANSLIT')

region_data <- full_join(region_data %>% select(iso=alpha.3, country=name, region:intermediate.region.code),
                         who_regions %>% select(iso, country2=country, who.region, who.region.mortality=region.mortality), 
                         by=c("iso"="iso")) %>% as.data.frame()
region_data <- full_join(region_data,
                         region_data_2 %>% select(ISO3, country3=ADMIN, continent, REGION, GEO3major, GEO3, GLOCAF, Stern, SRESmajor, SRES, GBD),
                         by=c("iso"="ISO3")) %>% as.data.frame()

# Combine added country names
region_data$country[is.na(region_data$country) & !is.na(region_data$country2)] <- 
  region_data$country2[is.na(region_data$country) & !is.na(region_data$country2)]
region_data$country[is.na(region_data$country) & !is.na(region_data$country3)] <- 
  region_data$country3[is.na(region_data$country) & !is.na(region_data$country3)]

region_data <- region_data %>% select(-country2, -country3)

#write.csv(region_data, 'data/region_data.csv', row.names = FALSE)
usethis::use_data(region_data, overwrite = TRUE)







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


