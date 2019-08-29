##' Testing and Troubleshooting globaltoolbox
##' 




# SETUP -------------------------------------------------------------------

library(globaltoolbox)
library(tidyverse)


dbname <- "../gaul_gadm.sqlite"


# Load DB as data.frame ---------------------------------------------------

db_df <- get_location_metadata(dbname=dbname)
write_csv(db_df, "../gaul_gadm_sqlite.csv")


nrow(db_df)








# EXPLORE ISSUES ----------------------------------------------------------




# ~ Issue: Multiple numeric alias for U.S. counties -----------------------

# Example: 	"::unitedstatesofamerica::indiana::madison"
sum(db_df$name=="::unitedstatesofamerica::indiana::madison")
db_df$alias[db_df$name=="::unitedstatesofamerica::indiana::madison"]


# ---> The issue is not restricted to U.S. counties, its the whole database

db_df$alias_numeric <- as.integer(db_df$alias)
sum(!is.na(db_df$alias_numeric))
sum(!is.na(db_df$alias_numeric))/nrow(db_df)

# how many come from each source
table(!is.na(db_df$alias_numeric), db_df$source_name)

# GADM
db_df %>% filter(!is.na(alias_numeric) & source_name=="GADM")
db_df %>% filter(name=="::kenya::baringo::805")

# globaltoolbox (alias df)
db_df %>% filter(!is.na(alias_numeric) & source_name=="globaltoolbox")
# --> seem to all be previous ID numbers (but they are off by 1-2)
db_df %>% filter(name=="::afghanistan")



# ~ NAs in alias ----------------------------------------------------------
#    --> These should be removed from DB

sum(is.na(db_df$alias))
sum(is.na(db_df$alias))/nrow(db_df)

# how many come from each source
table(is.na(db_df$alias), db_df$source_name)





# ~ Redundant variables ---------------------------------------------------

# `id` and `location_id` and `descendent_id`
all.equal(db_df$id, db_df$location_id)  # --> completely redundant
all.equal(db_df$id, db_df$descendent_id)  # --> completely redundant


# `depth_from_source` and `level`
all.equal(as.integer(db_df$depth_from_source), as.integer(db_df$level))  # NAs not present in depth_from_source
all.equal(as.integer(db_df$depth_from_source)[!is.na(db_df$level)], as.integer(db_df$level)[!is.na(db_df$level)])  # --> completely redundant, except 1354 NAs





# Locations with same readable_name --------------------------------------------------------

View(db_df %>% filter(readable_name=="togo"))


db_samename <- db_df %>% filter(is.na(alias_numeric) & !is.na(alias)) %>% 
  mutate(id_name=paste0(id, "-", readable_name)) %>%
  filter(!duplicated(id_name)) %>%
  arrange(readable_name, id)

dup_readable_name <- db_samename$readable_name[duplicated(db_samename$readable_name)]

db_samename <- db_samename[db_samename$readable_name %in% dup_readable_name, ]





