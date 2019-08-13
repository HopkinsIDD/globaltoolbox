
# Airport Survey from Bangladesh



# SETUP -------------------------------------------------------------------

library(tidyverse)

bgd_survey_locs <- read_csv("data-raw/testingdata_bgdairportsurvey.csv")

# Destinations
bgd_dests <- bgd_survey_locs %>% select(dest_country, dest_citytownvillage, dest_arealocality, airports)
# Bangladesh locations
bgd_locs_visited <- bgd_survey_locs %>% select(bgd_district, bgd_thana, bgd_areas_visited)



# CHECK FUNCTIONS ---------------------------------------------------------


# Countries
bgd_dests <- bgd_dests %>% mutate(country_std = standardize_location_strings(dest_country))

