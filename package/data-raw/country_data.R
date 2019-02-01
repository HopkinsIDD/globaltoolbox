##' Source code to build Country-based data source
##' 



# Install and load needed packages
if(!require('dplyr')) install.packages('dplyr'); library(dplyr)
if(!require('readr')) install.packages('readr'); library(readr)
# if(!require('RCurl')) install.packages('RCurl'); library(RCurl)
# if(!require('tibble')) install.packages('tibble'); library(tibble)
if(!require('curl')) install.packages('curl'); library(curl)




#####################################################
# Import and Clean the Data

# # First Check that you have internet connection and can access github. If not, import local data source
# has_internet <- TRUE
# tryCatch({
#   is.character(getURL("https://raw.githubusercontent.com/datasets/country-codes/master/data/"))==TRUE &
#     !is.null(curl::nslookup("www.github.com")) }, 
#   error= function(x){
#     has_internet <- FALSE
#     print('No Internet Connection')
#   }
# )
# 
#
# # If you have internet and github is accessible, update the data
# if (has_internet) {
#   iso_data <- NULL
#   
#   # Get Country Codes, Regions, 
#   tryCatch({
    iso_data         <- read.csv("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
    dhs_countrydata  <- read.csv("data-raw/DHS_countrydata.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
    nationality_data <- read.csv("data-raw/nationalities_and_languages.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
    alt_country_names <- rworldmap::countrySynonyms
  # },
  # error= function(x) print('No Internet Connection')
  # )

  # Write these data to directory if they are available so they are updated
  write.csv(iso_data, './data-raw/iso_data.csv', row.names = FALSE)
  #write.csv(dhs_countrydata, 'data-raw/dhs_countrydata.csv', row.names = FALSE)
  #write.csv(nationality_data, 'data-raw/nationalities_and_languages.csv', row.names = FALSE)
  write.csv(alt_country_names, './data-raw/alt_country_names.csv', row.names = FALSE)

#   # If not, use the local data
# } else {
#   source.path.folder <- file.path(source.path, 'iso_source')
#   
#   if (file.exists('data-raw/iso_data.csv')) {
#     iso_data <- read.csv('data-raw/iso_data.csv', header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
#     dhs_countrydata <- read.csv('data-raw/dhs_countrydata.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
#     nationality_data <- read.csv('data-raw/nationalities_and_languages.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
#     alt_country_names <- read.csv('data-raw/alt_country_names.csv',  header=TRUE, stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
# 
#   } else {
#     print('No access to internet/github and local files could not be located')
#     stop()
#   }
# } 





# Build Single Country Dataset ---------------------------------------------
#  - this will be linked to using country codes

# Check for missing ISOs
sum(is.na(iso_data$ISO3166.1.Alpha.3))
sum(is.na(region_data_2$iso))
sum(is.na(who_regions$iso))


country_data <- iso_data %>% select(ISO3=ISO3166.1.Alpha.3,
                                    ISO2=ISO3166.1.Alpha.2,
                                    Country=official_name_en,
                                    Country2=CLDR.display.name,
                                    UNcode=ISO3166.1.numeric, everything())

# get rid of non-ASCII
country_data$Country = iconv(country_data$Country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
country_data$Country2 = iconv(country_data$Country2, from = 'UTF-8', to = 'ASCII//TRANSLIT')

# Fix missing ISOs (temporary fix)        
country_data$ISO3[is.na(country_data$ISO3)] <- country_data$Country[is.na(country_data$ISO3)] 
# Fix missing Country
country_data$Country[is.na(country_data$Country)] <- country_data$Country2[is.na(country_data$Country)] 



# COUNTRY NAMES -----------------------------------------------------------

# Separate to country name database, country codes, and other details

country_names_df <- country_data %>% select(ISO3, Country, Country2, FIFA, IOC,
                                         contains("official_name"), 
                                         contains("UNTERM"), ISO4217.currency_country_name) %>%
            mutate(UNTERM.Spanish.Short = iconv(UNTERM.Spanish.Short, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   UNTERM.Spanish.Formal = iconv(UNTERM.Spanish.Formal, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   UNTERM.French.Formal = iconv(UNTERM.French.Formal, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   UNTERM.French.Short = iconv(UNTERM.French.Short, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   UNTERM.English.Short = iconv(UNTERM.English.Short, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   UNTERM.English.Formal = iconv(UNTERM.English.Formal, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   official_name_es = iconv(official_name_es, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   official_name_fr = iconv(official_name_fr, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   IOC = iconv(IOC, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
                   FIFA = iconv(FIFA, from = 'UTF-8', to = 'ASCII//TRANSLIT')
                   
                   )

# Fix IOC
country_names_df$IOC[country_names_df$IOC=="" | country_names_df$IOC==" "] <- NA
country_names_df$FIFA[country_names_df$FIFA=="" | country_names_df$FIFA==" "] <- NA


# Merge with alt_country names
country_names_df <- full_join(country_names_df, alt_country_names %>% mutate(ISO3=toupper(ISO3)), 
                           by=c("ISO3"="ISO3")) %>%
                  select(ISO3, ID, everything())
country_names_df$ISO3[country_names_df$ISO3=="" | country_names_df$ISO3==" "] <- NA

# Check for unmatched duplicates
dups <- match(tolower((country_names_df %>% filter(is.na(ISO3)))$name1), 
      tolower(country_names_df$Country))
dups <- tolower((country_names_df %>% filter(is.na(ISO3)))$name1)[!is.na(dups)]
dupped.a <- match(dups, tolower(country_names_df$name1))
dupped.b <- match(dups, tolower(country_names_df$Country))

country_names_df[dupped.b,c(2,(which(colnames(country_names_df)=="name1")):ncol(country_names_df))] <- 
  country_names_df[dupped.a,c(2,(which(colnames(country_names_df)=="name1")):ncol(country_names_df))]
country_names_df <- country_names_df[-dupped.a,]

# Fix missing country and ISO
country_names_df$Country[is.na(country_names_df$Country)] <- country_names_df$name1[is.na(country_names_df$Country)] 
country_names_df$ISOtrue <- country_names_df$ISO3 
country_names_df$ISO3[is.na(country_names_df$ISO3)] <- country_names_df$Country[is.na(country_names_df$ISO3)] 

# Remove West Bank (already there with palestine)
country_names_df <- country_names_df[country_names_df$ID!=276,]
row.names(country_names_df) <- country_names_df$ISO3

# # Convert to list for easy manipulation
# country_name_list <- lapply(split(country_names_df, row.names(country_names_df)), unlist)
# # TEST
# country_name_list$USA


# Convert to long -- easier to search
country_names <- country_names_df %>% mutate(country_std=Country) %>% gather(key="name_type", value="name", -ISO3, -country_std, -ID)
country_names <- country_names %>% filter(!is.na(name) & name!="" & name!=" ")

# Add "America" to USA
country_names <- rbind(country_names, as.vector(c(country_names[which(country_names$ISO3=="USA")[1],1:3], 
                                                  name_type="added1", name="America")))

# Get rid of duplicates 
name_types <- unique(country_names$name_type)
country_names <- country_names %>% mutate(name_type=factor(name_type, levels=name_types, labels = name_types)) %>%
                     arrange(ISO3, name_type) %>%
                     mutate(duplic_name=duplicated(toupper(name)))
country_names <- country_names %>% filter(duplic_name==FALSE) %>% select(-duplic_name)

country_iso <- country_names_df %>% select(ISO=ISO3, Country)

# Save names data
usethis::use_data(country_iso, overwrite = TRUE)
usethis::use_data(country_names, overwrite = TRUE)
#usethis::use_data(country_name_list, overwrite = TRUE)



# COUNTRY CODES -----------------------------------------------------------


country_codes <- country_data %>% select(ISO3, Country, FIFA, IOC, everything())
country_codes <- country_codes %>% select(-contains("official_name"), 
                                        -contains("UNTERM"),
                                        -Dial, -is_independent,
                                        -Developed...Developing.Countries,
                                        -contains("ISO4217.currency"),
                                        -Small.Island.Developing.States..SIDS.,
                                        -contains("region"),
                                        -contains("Region"),
                                        -Country2, -Capital, -Languages,
                                        -Least.Developed.Countries..LDC.,
                                        -Global.Name, -Global.Code,
                                        -Land.Locked.Developing.Countries..LLDC.)
# Save codes data
usethis::use_data(country_codes, overwrite = TRUE)



# ALL OTHER DATA ----------------------------------------------------------





# TEST AND EXAMPLE DATA ---------------------------------------------------

# Example of country names
test_country_names <- c("NEWZEALAND", "Sudan", "THAILLAND", "Denmark", "ITALY", "LEBANON", 
                "THAILAND", "KSA", "BAHARAIN", "UAE", "INDONEISIA", "Spain", 
                "U.S.A", "UK", "MALDIVE", "Oman", "PORTUGAL", "SAUDIARAB", 
                "SRILANKA", "SOUDIARABIA", "Italy", "TAIWAN", "Honkong", 
                "K.S.A", "Nepal", "LIBIYA", "Indoesia", "Chine", "America")
usethis::use_data(test_country_names, overwrite = TRUE)




