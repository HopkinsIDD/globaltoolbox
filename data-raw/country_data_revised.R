##' Source code to build Country-based data source


# Install and load needed packages
if(!require('dplyr')) install.packages('dplyr'); library(dplyr)
if(!require('readr')) install.packages('readr'); library(readr)
if(!require('tidyr')) install.packages('tidyr'); library(tidyr)

# if(!require('RCurl')) install.packages('RCurl'); library(RCurl)
# if(!require('tibble')) install.packages('tibble'); library(tibble)
# if(!require('curl')) install.packages('curl'); library(curl)




# START WITH UN STATISTICS DIVISION DATA (M49) ----------------------------------
# - See https://unstats.un.org/unsd/methodology/m49/overview/ for data source.
# - Last data update: 08 April 2019

undataEN <- read_csv('data-raw/UN_M49_EN.csv')
colnames(undataEN)

undataEN <- undataEN %>% rename(ISO3=`ISO-alpha3 Code`) %>% mutate(std_name=ISO3, type='country') %>%
  select(std_name,
         name=`Country or Area`, 
         type,
         ISO3,
         UN_M49_code=`M49 Code`,
         region_name=`Region Name`, region_code=`Region Code`, 
         subregion_name=`Sub-region Name`, subregion_code=`Sub-region Code`,
         intermed_region_name=`Intermediate Region Name`, 
         intermed_region_code=`Intermediate Region Code`,
         development=`Developed / Developing Countries`, 
         LLDC=`Land Locked Developing Countries (LLDC)`, 
         LDC=`Least Developed Countries (LDC)`)


# ADD DATA FROM `ISOcodes` PACKAGE -------------------------------------------

#if(!require('ISOcodes')) install.packages('ISOcodes'); library(ISOcodes)
ISOcodes_level1 <- ISOcodes::ISO_3166_1 %>% rename(ISO3=Alpha_3) %>%
  mutate(std_name=ISO3, ISO_level=1, type="country") %>%
  select(std_name, name=Name, type, 
         ISO3, 
         ISO2=Alpha_2, 
         UN_M49_code=Numeric, 
         ISO_level, everything())
ISOcodes_level1$name[!is.na(ISOcodes_level1$Common_name)] <- ISOcodes_level1$Common_name[!is.na(ISOcodes_level1$Common_name)] # replace name with common name if available
ISOcodes_level1 <- ISOcodes_level1 %>% select(-Common_name)


# Sub-Country Locations ---------------------------------------------------
# -- Includes England, Wales, etc., and districts, parishes, etc.

ISOcodes_level2 <- ISOcodes::ISO_3166_2 %>% mutate(ISO2=substr(Code,1,2)) %>% 
  select(ISO2, name=Name, everything(), -Parent)
ISOcodes_level2$ISO3 <- ISOcodes_level1$ISO3[match(ISOcodes_level2$ISO2,ISOcodes_level1$ISO2)]  
ISOcodes_level2$parent_name <- ISOcodes_level1$name[match(ISOcodes_level2$ISO2,ISOcodes_level1$ISO2)]  
ISOcodes_level2$name[ISOcodes_level2$Code=="AE-AZ"] <- "Abu Dhabi"
ISOcodes_level2$name[ISOcodes_level2$Code=="AE-AJ"] <- "Ajman"
ISOcodes_level2$name[ISOcodes_level2$Code=="AE-DU"] <- "Dubai"
ISOcodes_level2$subcode <- substr(ISOcodes_level2$Code, 4,5)
ISOcodes_level2$std_name <- paste(ISOcodes_level2$ISO3, ISOcodes_level2$subcode, sep="::")
ISOcodes_level2 <- ISOcodes_level2 %>% mutate(ISO_level=2) %>% 
  select(std_name, name, type=Type, ISO3, ISO2, everything())


# Merge levels 1 & 2
ISOcodes_data <- full_join(ISOcodes_level1, ISOcodes_level2)

# Merge with UN data
location_db <- full_join(undataEN, ISOcodes_data)


# Save this
usethis::use_data(location_db, overwrite = TRUE, compress = 'xz')





# Use this for the alias database
alias_data <- country_df %>% select(ISO3, Name, Official_name) %>% 
  tidyr::gather(key="alias_type", value="alias", -ISO3) %>%
  mutate(type="country") %>%
  filter(!is.na(alias))











# IMPORT CITIES DATA FROM simplemaps.com ----------------------------------
# -- Need to cite the source for this !!!!
# -- From https://simplemaps.com/data/world-cities

city_data1 <- read_csv("data-raw/worldcities.csv")
city_data1 <- city_data1 %>% rename(name=city, name_ascii=city_ascii, ISO3=iso3, ISO2=iso2) %>% mutate(type="city")




# IMPORT CITIES FROM DATAHUB ----------------------------------------------


city_data <- read_csv("data-raw/worldcities.csv")
city_data <- city_data %>% rename(name=city, name_ascii=city_ascii, ISO3=iso3, ISO2=iso2) %>% mutate(type="city")


world-cities_datahub.csv



usethis::use_data(city_data, overwrite = TRUE, compress = 'xz')
















#####################################################
# Import and Clean the Data
country_data     <- read.csv("https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
dhs_countrydata  <- read.csv("data-raw/DHS_countrydata.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
nationality_data <- read.csv("data-raw/nationalities_and_languages.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
alt_country_names <- rworldmap::countrySynonyms
ISOcodes_level1 <- ISOcodes::ISO_3166_1
ISOcodes_level2 <- ISOcodes::ISO_3166_2
ISOcodes_level3 <- ISOcodes::ISO_3166_3


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

country_df <- ISOcodes_level1 %>% mutate(ISO_level=1, type="country") %>% 
                  select(ISO3=Alpha_3, ISO2=Alpha_2, Numeric, ISO_level, everything()) %>% as.data.frame()
country_df$Name[!is.na(country_df$Common_name)] <- country_df$Common_name[!is.na(country_df$Common_name)] # replace name with common name if available
country_df <- country_df %>% select(-Common_name)


# # Save for the package
# usethis::use_data(country_df, overwrite = TRUE, compress = 'xz')


# Use this for the alias database
alias_data <- country_df %>% select(ISO3, Name, Official_name) %>% 
  tidyr::gather(key="alias_type", value="alias", -ISO3) %>%
  mutate(type="country") %>%
  filter(!is.na(alias))











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
