##' Source code to build Country-based data source
##' 


if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)



#####################################################
# Import and Clean the Data

region_data <- NULL

# Get Country Codes, Regions, 
tryCatch({
  region_data      <- read.csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
  who_regions      <- read.csv("data-raw/who_regions.csv", stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
  region_data_2    <- rworldmap::countryRegions
},
error= function(x) print('No Internet Connection')
)

# Write these data to directory if they are available so they are updated
write.csv(region_data, 'data-raw/region_data.csv', row.names = FALSE)     
write.csv(who_regions, 'data-raw/who_regions.csv', row.names = FALSE)
write.csv(region_data_2, 'data-raw/region_data_2.csv', row.names = FALSE)




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

region_data <- region_data %>% select(-country2, -country3) %>% rename(ISO3=iso)

#write.csv(region_data, 'data/region_data.csv', row.names = FALSE)
usethis::use_data(region_data, overwrite = TRUE)
