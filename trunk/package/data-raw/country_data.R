##' Source code to build Country-based data source
##' 



# Install and load needed packages
if(!require('dplyr')) install.packages('dplyr'); library(dplyr)
if(!require('readr')) install.packages('readr'); library(readr)
# if(!require('RCurl')) install.packages('RCurl'); library(RCurl)
# if(!require('tibble')) install.packages('tibble'); library(tibble)
if(!require('curl')) install.packages('curl'); library(curl)
if(!require('ISOcodes')) install.packages('ISOcodes'); library(ISOcodes)




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
country_data     <- read.csv("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
dhs_countrydata  <- read.csv("data-raw/DHS_countrydata.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
nationality_data <- read.csv("data-raw/nationalities_and_languages.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
alt_country_names <- rworldmap::countrySynonyms
ISOcodes_level1 <- ISOcodes::ISO_3166_1
ISOcodes_level2 <- ISOcodes::ISO_3166_2
ISOcodes_level3 <- ISOcodes::ISO_3166_3

# },
# error= function(x) print('No Internet Connection')
# )

# Write these data to directory if they are available so they are updated
write.csv(country_data, 'data-raw/iso_data.csv', row.names = FALSE)
#write.csv(dhs_countrydata, 'data-raw/dhs_countrydata.csv', row.names = FALSE)
#write.csv(nationality_data, 'data-raw/nationalities_and_languages.csv', row.names = FALSE)
write.csv(alt_country_names, 'data-raw/alt_country_names.csv', row.names = FALSE)

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



  


# Build Country Reference Dataset  ---------------------------------------------
#  - this will be linked to using country codes

country_df <- ISOcodes_level1 %>% mutate(ISO_level=1, Type="Country") %>% 
                  select(ISO3=Alpha_3, ISO2=Alpha_2, Numeric, ISO_level, everything()) %>% as.data.frame()
country_df$Name[!is.na(country_df$Common_name)] <- country_df$Common_name[!is.na(country_df$Common_name)] # replace name with common name if available
country_df <- country_df %>% select(-Common_name)

# Save for the package
usethis::use_data(country_df, overwrite = TRUE, compress = 'xz')




# Sub-Country Locations ---------------------------------------------------
# -- Includes England, Wales, etc.

ISOcodes_level2 <- ISOcodes_level2 %>% mutate(ISO2=substr(Code,1,2)) %>% select(ISO2, everything())
ISOcodes_level2$ISO3 <- country_df$ISO3[match(ISOcodes_level2$ISO2,country_df$ISO2)]  
ISOcodes_level2$Parent <- country_df$Name[match(ISOcodes_level2$ISO2,country_df$ISO2)]  
ISOcodes_level2 <- ISOcodes_level2 %>% mutate(ISO_level=2) %>% select(ISO3, ISO2, everything())
ISOcodes_level2$Name[ISOcodes_level2$Code=="AE-AZ"] <- "Abu Dhabi"
ISOcodes_level2$Name[ISOcodes_level2$Code=="AE-AJ"] <- "Ajman"
ISOcodes_level2$Name[ISOcodes_level2$Code=="AE-DU"] <- "Dubai"
locations_lvl2 <- ISOcodes_level2

usethis::use_data(locations_lvl2, overwrite = TRUE, compress = 'xz')







# BUILD COUNTRY NAME DATA -------------------------------------------------

# Check for missing ISOs
sum(is.na(country_data$ISO3166.1.Alpha.3))

country_data <- country_data %>% select(ISO3=ISO3166.1.Alpha.3,
                                    ISO2=ISO3166.1.Alpha.2,
                                    Country=official_name_en,
                                    Country2=CLDR.display.name,
                                    UNcode=ISO3166.1.numeric, everything()) %>%
                                  filter(!is.na(ISO3))

# get rid of non-ASCII
country_data$Country = iconv(country_data$Country, from = 'UTF-8', to = 'ASCII//TRANSLIT')
country_data$Country2 = iconv(country_data$Country2, from = 'UTF-8', to = 'ASCII//TRANSLIT')



# Check for differences with codes from ISOcodes package
not_matched <- country_data[which(is.na(match(country_data$ISO3, country_df$ISO3))),]$Country
grep(not_matched, country_df$Name)
grep(not_matched, ISOcodes_level2$Name)
# --> currently includes only "Sark" which is part of the UK

# Fix missing ISOs (temporary fix)        
#country_data$ISO3[is.na(country_data$ISO3)] <- country_data$Country[is.na(country_data$ISO3)] 
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

# Fix IOC & FIFA
country_names_df$IOC[country_names_df$IOC=="" | country_names_df$IOC==" "] <- NA
country_names_df$FIFA[country_names_df$FIFA=="" | country_names_df$FIFA==" "] <- NA


# Merge Reference data with names data
country_names_df <- full_join(country_df %>% select(-ISO2), country_names_df %>% mutate(ISO3=toupper(ISO3)), 
                              by=c("ISO3"="ISO3")) %>%
  select(ISO3, Numeric, Name, everything())
country_names_df$ISO3[country_names_df$ISO3=="" | country_names_df$ISO3==" "] <- NA


# Check for unmatched duplicates
dups <- match(tolower((country_names_df %>% filter(is.na(ISO3)))$Country), 
              tolower(country_names_df$Name))
dups <- tolower((country_names_df %>% filter(is.na(ISO3)))$name1)[!is.na(dups)]

if (length(dups)>0){
  dupped.a <- match(dups, tolower(country_names_df$name1))
  dupped.b <- match(dups, tolower(country_names_df$Name))
  
  country_names_df[dupped.b,(which(colnames(country_names_df)=="Country")):ncol(country_names_df)] <- 
    country_names_df[dupped.a,(which(colnames(country_names_df)=="Country")):ncol(country_names_df)]
  country_names_df <- country_names_df[-dupped.a,]
}
country_names_df$Name[is.na(country_names_df$Name)] <- country_names_df$Country[is.na(country_names_df$Name)] 



# Merge with alt_country names
country_names_df <- full_join(country_names_df, alt_country_names %>% mutate(ISO3=toupper(ISO3)), 
                           by=c("ISO3"="ISO3")) %>%
                  select(ISO3, Numeric, Name, everything())
country_names_df$ISO3[country_names_df$ISO3=="" | country_names_df$ISO3==" "] <- NA

# Check for unmatched duplicates
dups <- match(tolower((country_names_df %>% filter(is.na(ISO3)))$name1), 
      tolower(country_names_df$Name))
dups <- tolower((country_names_df %>% filter(is.na(ISO3)))$name1)[!is.na(dups)]; print(dups)
if (length(dups)>0){
  dupped.a <- match(dups, tolower(country_names_df$name1))
  dupped.b <- match(dups, tolower(country_names_df$Name))
  country_names_df[dupped.b,(which(colnames(country_names_df)=="name1")):ncol(country_names_df)] <- 
    country_names_df[dupped.a,(which(colnames(country_names_df)=="name1")):ncol(country_names_df)]
  country_names_df <- country_names_df[-dupped.a,]
}

# Fix missing Name
country_names_df$Name[is.na(country_names_df$Name)] <- country_names_df$name1[is.na(country_names_df$Name)] 


# Check ISO Level-2 data
dups <- match(tolower((country_names_df %>% filter(is.na(Numeric)))$Name), 
              tolower(locations_lvl2$Name))
dups <- tolower((country_names_df %>% filter(is.na(Numeric)))$Name)[!is.na(dups)]; print(dups)
if (length(dups)>0){
  dupped.a <- match(dups, tolower(country_names_df$Name))
  dupped.b <- match(dups, tolower(locations_lvl2$Name))
  
  country_names_df[dupped.a, c("Name", "ISO3", "ISO_level")] <- locations_lvl2[dupped.b,c("Name", "Code", "ISO_level")]
}


# Fix missing ISO
country_names_df$ISO3true <- country_names_df$ISO3 
country_names_df$ISO3[is.na(country_names_df$ISO3)] <- country_names_df$Name[is.na(country_names_df$ISO3)] 

# Remove West Bank (already there with palestine)
country_names_df <- country_names_df[country_names_df$ID!=276,]
country_names_df <- country_names_df %>% filter(!is.na(ISO3))
row.names(country_names_df) <- country_names_df$ISO3

# # Convert to list for easy manipulation
# country_name_list <- lapply(split(country_names_df, row.names(country_names_df)), unlist)
# # TEST
# country_name_list$USA


# Convert to long -- easier to search
country_names <- country_names_df %>% select(-ID, -Numeric, -ISOtrue) %>% gather(key="name_type", value="names", -ISO3, -Name, -ISO_level)
country_names <- country_names %>% filter(!is.na(names) & names!="" & names!=" ")

# Add "America" to USA
country_names <- rbind(country_names, as.vector(c(country_names[which(country_names$ISO3=="USA")[1],1:3], 
                                                  name_type="added1", names="America")))

# Get rid of duplicates 
name_types <- unique(country_names$name_type)
country_names <- country_names %>% mutate(name_type=factor(name_type, levels=name_types, labels = name_types)) %>%
                     arrange(ISO3, name_type) %>%
                     mutate(duplic_name=duplicated(toupper(names)))
country_names <- country_names %>% filter(duplic_name==FALSE) %>% select(-duplic_name)
country_iso <- country_names_df %>% select(ISO3, Name)

# Save names data
usethis::use_data(country_iso, overwrite = TRUE, compress = 'xz')
usethis::use_data(country_names, overwrite = TRUE, compress = 'xz')
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
#write_csv(country_codes, "data-raw/country_codes.csv")
usethis::use_data(country_codes, overwrite = TRUE, compress = 'xz')





# ALL OTHER DATA ----------------------------------------------------------

# CITY DATA #

# Need to cite the source for this !!!!
city_data <- read_csv("data-raw/worldcities.csv") %>% as.data.frame()
city_data <- city_data %>% rename(Name=city, ISO3=iso3, ISO2=iso2)

#check unique ISOs
length(unique(city_data$iso3)) # should be 249 or less

usethis::use_data(city_data, overwrite = TRUE, compress = 'xz')









# TEST AND EXAMPLE DATA ---------------------------------------------------

# Example of country names
test_country_names <- c("NEWZEALAND", "Sudan", "THAILLAND", "Denmark", "ITALY", "LEBANON", 
                "THAILAND", "KSA", "BAHARAIN", "UAE", "INDONEISIA", "Spain", 
                "U.S.A", "UK", "MALDIVE", "Oman", "PORTUGAL", "SAUDIARAB", 
                "SRILANKA", "SOUDIARABIA", "Italy", "TAIWAN", "Honkong", 
                "K.S.A", "Nepal", "LIBIYA", "Indoesia", "Chine", "America")
usethis::use_data(test_country_names, overwrite = TRUE, compress = 'xz')


# Example of country names mixed with sub-country
test_mixed_names <- c("NEWZEALAND", "Sudan", "THAILLAND", "Denmark", "ITALY", "LEBANON", 
                        "THAILAND", "KSA", "BAHARAIN", "UAE", "INDONEISIA", "Spain", 
                        "U.S.A", "UK", "MALDIVE", "Oman", "PORTUGAL", "SAUDIARAB", 
                        "SRILANKA", "SOUDIARABIA", "Italy", "TAIWAN", "Honkong", 
                        "K.S.A", "Nepal", "LIBIYA", "Indoesia", "Chine", "America",
                        "England", "Dubai", "Dubaaay", "Wales")
usethis::use_data(test_mixed_names, overwrite = TRUE, compress = 'xz')


# Example of country names mixed with sub-country
test_city_names <- c("Chikago", "Newyork", "Dohaah", "Hong Kong", "Honkong", 
                      "Cayro", "Shanghay", "Dubai", "Dubaay", "Bankok", "Sanfrancisco", "Seaatel")
usethis::use_data(test_city_names, overwrite = TRUE, compress = 'xz')
