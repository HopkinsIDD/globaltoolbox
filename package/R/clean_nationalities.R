
# Load nationalities and languages
nationalities <- read_csv("C:/Users/shaun/Documents/GitHub/RandomDataAndCode/countries_and_regions/data/nationalities_and_languages.csv")
# source: https://www.vocabulary.cl/Basic/Nationalities.htm

nationalities2 <- read_csv("C:/Users/shaun/Documents/GitHub/RandomDataAndCode/countries_and_regions/data/countries_and_demonyms.csv")
# source: https://www.worldatlas.com/articles/what-is-a-demonym-a-list-of-nationalities.html


# Add ISOs
nationalities$ISO3 <- get.iso(nationalities$Country)
nationalities2$ISO3 <- get.iso(nationalities2$Country)


# Split nationalities into multiple columns
nationalities2 <- nationalities2 %>% separate(Demonym, 
                                              c("Nationality1", "Nationality2", "Nationality3", "Nationality4", "Nationality5"),
                                              "; ", remove = FALSE)
nationalities <- nationalities %>% select(ISO3, Country, NationalityA=Nationality, NationalityB=Nationality2, Language)
nationalities_ <- left_join(nationalities, nationalities2, by=c("ISO3"="ISO3")) 

# Combine nationalities and get rid of duplicates
nationalities_ <- nationalities_ %>% unite(nationalities, 
                                           NationalityA, NationalityB, Nationality1, Nationality2, Nationality3, Nationality4, Nationality5, 
                                           sep=", ", remove=TRUE)

for (i in 1:nrow(nationalities_)){
  tmp <- unique(unlist(strsplit(nationalities_$nationalities[i], split=", ")))
  tmp <- tmp[tmp!="NA"]
  tmp <- tmp[!is.na(tmp)]
  nationalities_$nationalities[i] <- paste(tmp, collapse = ", ")
}

nationalities_ <- nationalities_ %>% rename(Country=Country.x, Nationalities=nationalities) %>% select(-Country.y, -Demonym) %>% as.data.frame()

# Save it
write_csv(nationalities_, "C:/Users/shaun/Documents/GitHub/RandomDataAndCode/countries_and_regions/data/nationalities_and_languages.csv")





