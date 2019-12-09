# Testing datasets

library(tidyverse)

# Dengue in Southeast Asia

dengue_se_asia <- read_csv("data-raw/dengue_se_asia.csv")
dengue_locs <- as.character(unlist(dengue_se_asia[2,-c(1:2)]))

# first split off country
dengue_locs_df <- str_split_fixed(dengue_locs, ", ", 2)
dengue_locs <- data.frame(orig = dengue_locs, city=dengue_locs_df[,1], country=dengue_locs_df[,2], stringsAsFactors = FALSE) 



# REMOVE INVALID CHARACTERS -----------------------------------------------

# should never have "_" (underscores) in names, encoded or not
dengue_locs <- dengue_locs %>% mutate(city = gsub("_","", city),
                                      country = gsub("_","", country))


# CONVERT ENCODING --------------------------------------------------

# Function to convert to ASCII

convert_to_ascii <- function(x){
  encoding <- ifelse(Encoding(x)=="unknown", "ASCII", Encoding(x))
  iconv(x, encoding, "ASCII//TRANSLIT", "") 
}
convert_to_utf8 <- function(x){
  encoding <- ifelse(Encoding(x)=="unknown", "ASCII", Encoding(x))
  iconv(x, encoding, "UTF-8", "") 
}

dengue_locs$city_ascii <- sapply(dengue_locs$city, convert_to_ascii)
dengue_locs$city_utf8 <- sapply(dengue_locs$city, convert_to_utf8)
# Remove ?
dengue_locs$city_ascii <- gsub([:punct:],"", dengue_locs$city_ascii)



enc2native(dengue_locs$city)



# test current function
tmp1 <- standardize_string(dengue_locs$city)
tmp2 <- standardize_location_strings(dengue_locs$city)
View(data.frame(orig=dengue_locs, unlist(tmp1)))










###########
