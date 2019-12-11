##' Testing and Troubleshooting globaltoolbox
##' 




# SETUP -------------------------------------------------------------------

library(globaltoolbox)
library(tidyverse)


#dbname <- "../gaul_gadm.sqlite"
dbname <- "../eth.sqlite"        # Ethiopia test database
dbname <- "../KEN.sqlite"        # Kenya test database

# Load DB as data.frame ---------------------------------------------------

# db_df <- get_location_metadata(dbname=dbname)
# write_csv(db_df, "../gaul_gadm_sqlite.csv")
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
View(db_df %>% filter(!is.na(alias_numeric) & source_name=="globaltoolbox"))
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




# look at "shkria" and "ashsharqiyah"

View(db_df %>% filter(readable_name=="shrkia"))
View(db_df %>% filter(readable_name=="ashsharqiyah"))






#..............................................................................
# Fix standardize_name.R --------------------------------------------------



standardize_name(location = "homabaytown", scope="", dbname=dbname, 
                        strict_scope=TRUE, standardize_location=TRUE, standardize_db=FALSE)


# ~ Break it down to fix code ---------------------------------------------

source('R/user_database_interface.R') # source to get functions that are not exported by package

location = "homabaytown"; scope=""; dbname=dbname; 
strict_scope=FALSE; standardize_location=TRUE; standardize_db=FALSE
depth=NA

original_location <- location
location <- standardize_location_strings(location)

  rc <- sapply(
    location,
    database_standardize_name,
    source = scope,
    dbname = dbname
  )
  if(length(rc) == length(location)){
    print(stats::setNames(rc, original_location))
  }


## limit database and alias_database to scope and metadata
db_scoped <- get_all_aliases(
  source = scope,
  dbname = dbname,
  depth = depth,
  strict_scope = strict_scope
)

if (standardize_db){
  db_scoped$name_unstandard <- tolower(db_scoped$name)
  db_scoped$name <- standardize_location_strings(db_scoped$name)
  db_scoped$readable_name <- standardize_location_strings(db_scoped$readable_name)
  db_scoped$alias <- standardize_location_strings(db_scoped$alias)
}

## Clean Locations and aliases to match

matches_ <- rep(NA, length(location))

  names_b_data <- dplyr::select(
    db_scoped,
    .data$id,
    name = .data$readable_name,
    depth = .data$depth_from_source
  )
  names_b_data <- dplyr::filter(names_b_data, !is.na(name) & name!="")
  names_b_data <- names_b_data %>% mutate(id_tmp = paste(id, name, sep="-")) %>% dplyr::filter(!duplicated(id_tmp))
  
  # matches_[is.na(matches_) & !is.na(location)] <- sapply(location[is.na(matches_) & !is.na(location)],
  #                                                        match_names,
  #                                                        names_b_data = names_b_data,
  #                                                        return_match_scores = FALSE)



# ~ Run match_names function -------------------------------------------------------
  
  names_ <- names_b_data$name
  tmp <- names_[nchar(names_)<=3]
  
  location_orig <- location

name_a <- location
name_a <- "tot"# "Comoe" #"Herat"
names_b <- as.character(names_b_data$name)
    
# First check for an exact match  .............................................................
# ---> skip this part of the function for testing 

# If no exact match, do the string matching algorithms

## string matching methods
## Consider passing in methods as an argument?
##   lv      : Levenshtein distance - Minimal number of insertions, deletions and replacements needed for transforming string a into string b.
##   dl      : Damerau-Levenshtein distance - Like Levenshtein distance, but transposition of adjacent symbols is allowed.
##   osa     : Optimal String Alignment - Like (full) Damerau-Levenshtein distance but each substring may only be edited once.
##   lcs     : Longest Common Substring distance - Minimum number of symbols that have to be removed in both strings until resulting substrings are identical.
##   qgram   : q-gram distance - Sum of absolute differences between N-gram vectors of both strings.
##   cosine  : Cosine distance - 1 minus the cosine similarity of both N-gram vectors.
##   jaccard : Jaccard distance - 1 minues the quotient of shared N-grams and all observed N-grams.
##   jw      : Jaro-Winkler distance - This distance is a formula of 5 parameters determined by the two compared strings (A,B,m,t,l) and p chosen from [0, 0.25].
##   soundex :
methods <- c("osa", "jw", "soundex")
#methods <- c('osa','lv','dl','lcs','qgram','cosine','jaccard','jw','soundex')
dists <- as.data.frame(matrix(
  NA,
  nrow = length(names_b),
  ncol = length(methods),
  dimnames = list(names_b, methods)
))
for (j in 1:length(methods)){
  dists[, j]  <- suppressWarnings(
    stringdist::stringdist(name_a, names_b, method = methods[j], q=2, p=.1)
  )
}
dists$soundex <- dists$soundex*3 # use soundex to weed out poor matches
#dists$lcs <- dists$lcs*2 # use longest common substring to weed out poor matches

dists <- data.frame(names_b_data,
                    names_clean = names_b,
                    dists,
                    score_sums = rowSums(dists))
      
      

# ~~ Exploring Scores -----------------------------------------------------

# scores_ <- c("osa", "qgram", "jw", "cosine","jaccard", "soundex") # 0 is the lowest for each of these
# par(mfrow=c(2,3))
# for(i in 1:length(scores_)){
#   hist(dists[,scores_[i]], breaks=50, main = scores_[i])
# }
# 
# # Calc probability of each being a match for each score compared to the full dataset
# 
# dists <- dists %>% mutate(osa_prob = ((osa - 0) / sum(osa)))
# 
# jw <- dists$jw
# jw <- jw[jw<.3]
# mean(jw)


# Check for numbers in name_a and require numbers in names_b
if (grepl("[0-9]", name_a)){
  names_b_numeric <- grepl("[0-9]", names_b)
  dists$score_sums <- dists$score_sums + ((!names_b_numeric) * 20)
}

if(!all(is.na(dists))){
  dists$score_sums_normalized <- 1 - (dists$score_sums / max(dists$score_sums))
} else {
  dists$score_sums_normalized <- dists$score_sums
}
dists$osa <- as.integer(dists$osa)


## get best match from results
best_ <- NULL
if (any(dists$osa <= 1)){
  
  ## OSA less than 1 is highly likely a match
  best_ <- which(dists$osa <= 1 & dists$score_sums == min(dists$score_sums))
} else if (any(dists$jw <= .1)){
  best_ <- which(dists$jw <= .1 & dists$score_sums == min(dists$score_sums))
  
  # OSA <=3 indicates good match
} else if (any(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)){  
  best_ <- which(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)
}



standardize_name(location = "homabaytown", scope="", dbname=dbname, 
                 strict_scope=TRUE, standardize_location=TRUE, standardize_db=FALSE)

standardize_name(location = "tak", scope="", dbname=dbname, 
                 strict_scope=TRUE, standardize_location=TRUE, standardize_db=FALSE)

standardize_name(location = "lib", scope="", dbname=dbname, 
                 strict_scope=TRUE, standardize_location=TRUE, standardize_db=FALSE)





  
      
      
      
  
