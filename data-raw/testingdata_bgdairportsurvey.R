## code to prepare `testingdata_bgdairportsurvey` dataset goes here

testingdata_bgdairportsurvey <- read.csv("data-raw/testingdata_bgdairportsurvey.csv", stringsAsFactors = FALSE)

usethis::use_data(testingdata_bgdairportsurvey)
