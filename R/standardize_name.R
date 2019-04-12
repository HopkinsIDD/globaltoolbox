#' @include database_management.R get_location_data.R
## data('country_names',package='globaltoolbox')
## data('country_codes',package='globaltoolbox')
## 

#' @name standardize_name
#' @title standardize_name
#' @description Using various methods, matches the inputted location name to a location and returns a standardized code
#' for that location
#' @param location location name to match
#' @param scope a known standardized location scope, if available. For a city, this would be the code for the country 
#' where it is located
#' @param metadata Additional data that may be useful to identify the location name
#' @param database database to pull location information from. If NULL, it will pull from the database included in the package.
#' @param return_match_score logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
#' @return standardized database code which can be used to identify other data
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @import dplyr
#' @export
standardize_name <- function(location, scope, metadata, dbname=NULL, ...){
  
  ## # TEMPORARY -- draw from country_names and country_codes data
  ## data('country_names',package='globaltoolbox')
  ## alias_database <- country_names %>% mutate(id = ISO3)
  ## # data('country_codes',package='globaltoolbox')
  ## # database_tmp <- country_codes
  ## 
  ## Set database name if default
  if (is.null(dbname)){
    dbname <- default_database_filename()
  }
  ## limit database and alias_database to scope and metadata
  db_scoped <- get_location_metadata(source=scope, dbname=dbname, aliases=TRUE)

  try({
    rc <- database_standardize_name(location,scope,dbname=dbname)
    if(length(rc) == 1){return(rc)}
  })
  
  
  
  ## Clean Locations and aliases to match
  ## - cleaning here will be faster than for each match (maybe?)
  location_clean <- tolower(iconv(location, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
  location_clean <- str_replace_all(location_clean, "[[:punct:]]", "") # remove all punctuation
  names_b_data = db_scoped %>% select(id, name=name)
  names_b_data$name = tolower(iconv(names_b_data$name, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
  names_b_data$name <- str_replace_all(names_b_data$name, "[[:punct:]]", "") # remove all punctuation

  ## Run the "match_names" function to match. 
  ##  - This returns a row index pertaining to names_b_data.
  ##  - Any location not finding a best match will return NA
  ##  - Any location finding multible different best matches will return NA
  matches_ <- sapply(location_clean, 
                     match_names, 
                     names_b_data = names_b_data, 
                     return_match_score = FALSE)
  
  ## NOTE: As currently implemented, we are not restricting the scope or metadata for each location.
  
  return(db_scoped$id[matches_])
}




#' @name match_names
#' @title match_names
#' @description Using the stringdist package, matches the inputted location name to a location and returns an index
#' referring to the inputted data
#' @param name_a location name to match
#' @param names_b_data A database of aliases with columns `id` and `name`.
#' @param return_match_scores Logical. Whether to return a the matching scores for each alias.
#' @param clean_a Logical; whether to clean and standardize `name_a`.
#' @param clean_b Logical; whether to clean and standardize the names from `names_b_data`.
#' @return an index of the best match of name_a from names_b_data.
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @export
match_names <- function(name_a, names_b_data, 
                        return_match_scores=FALSE,
                        clean_a=FALSE, clean_b=FALSE){
  
  names_b <- names_b_data$name
  
  ## Clean name if not already done
  if (clean_a){
    name_a = tolower(iconv(name_a, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
    name_a <- str_replace_all(name_a, "[[:punct:]]", "") # remove all punctuation
  }
  
  ## Clean aliases if not done
  if (clean_b){
    names_b = tolower(iconv(names_b, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
    names_b <- str_replace_all(names_b, "[[:punct:]]", "") # remove all punctuation
  }
  
  ## string matching methods
  ##methods <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
  methods <- c("osa", "jw", "soundex")
  dists <- as.data.frame(matrix(NA, nrow=length(names_b), ncol=length(methods), 
                                dimnames=list(names_b, methods)))
  for (j in 1:length(methods)){
    dists[,j]  <- suppressWarnings(stringdist(name_a, names_b, method=methods[j]))
  }
  dists <- data.frame(names_b_data, 
                      names_clean=names_b,
                      dists,
                      score_sums = rowSums(dists))
  dists$score_sums_normalized <- 1 - (dists$score_sums / max(dists$score_sums))
  dists$osa <- as.integer(dists$osa)

  
  ## get best match from results
  
  best_ <- NULL
  if (any(dists$osa<=1)){  # OSA less than 1 is highly likely a match
    best_ <- which(dists$score_sums==min(dists$score_sums))
  } else if (any(dists$jw<=.1)){
    best_ <- which(dists$score_sums==min(dists$score_sums))
  } else if (any(dists$osa<=3 & dists$jw<=0.31 & dists$soundex==0)){
    best_ <- which(dists$osa<=3 & dists$jw<=0.31 & dists$soundex==0)
  } 
  
  ## If no good match was found, return either nothing, or the score matrix
  if (length(best_)==0 & !return_match_scores){
    return(NA)
  } else if (length(best_)==0 & return_match_scores){
    return(dists[order(dists$score_sums),])
  }
  
  ## If more than 1 meeting the criteria for best match, first check if they are the same,
  ## otherwise, return all
  if (length(best_)>1){
    dists$alias_ind <- 1:nrow(dists)
    dists_best <- dists[best_,] 
    dists_best <- dists_best[order(dists_best$score_sums),] # Order by score sum
    dists_best <- dists_best[!duplicated(dists_best$id),] # remove duplicate ids
    
    ## if still more than 1 best match, return NA or the distance matrix
    if (nrow(dists_best)>1){
      if (return_match_scores){
        return(dists_best)
      }
      return(NA)
    
    ## if only 1 best, replace best_ with index
    } else {
      best_ <- as.integer(dists_best$alias_ind)
    }
  }
  return(best_)
}

#' @name create_standardized_name
#' @title create_standardized_name
#' @description Using the stringdist package, matches the inputted location name to a location and returns an index
#' referring to the inputted data
#' @param name Non-standardized name to create a standard name for.
#' @param parent standard or non-standard parent name of one level higher than name.
#' @param check_aliases Logical; whether to check aliases of name.
#' @param dbname name of database in which name is trying to match.
#' @return an index of the best match of name_a from names_b_data.
#' @importFrom stringr str_replace_all str_to_title
#' @export
create_standardized_name <- function(name, parent=NA, check_aliases=FALSE, dbname=NULL){
  
  ## Do a couple of checks
  if(is.null(parent)){
    warning("This function does not currently handle creating standardized country names")
    return(name)
  }
  if (is.na(parent)){
    return("Need to specify parent")
  }
  
  ## Load database
  ## Set database name if default
  if (is.null(dbname)){
    dbname <- default_database_filename()
  }
  ## db <- get_location_metadata(dbname=dbname)

  
  ## Standardize the location name
  name <- tolower(iconv(name, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
  name <- str_replace_all(name, "[[:punct:]]", "") # remove all punctuation
  name <- str_to_title(name)
  name <- str_replace_all(name, " ", "-") # Replace space with dash
  
  ## Check for parent in database (check aliases if appropriate)
  std_parent_name = database_standardize_name(
    name=parent,
    dbname=dbname,
    standard=TRUE,
    source=NULL
  )

  ## If we didn't find the parent, take more drastic measures
  if(length(std_parent_name) == 0){
    ## Check if parent is standardized, and if so, look up in database
    if (grepl("::", parent)){
      
      parent_match <- which(tolower(db$name)==tolower(parent))
      
      if (length(parent_match)==1){
        std_parent_name <- db$name[tolower(db$name)==tolower(parent)]
        
        ## if no matches, need to specify parent better
      } else if (length(parent_match)>1){
        return("Multiple parent matches. Please provide more specific parent.")
        
        ## if no match, need to check for incomplete parent name
      } else if (length(parent_match)==0){
        parent_possible_matches <- grep(tolower(parent), tolower(db$name))
        if (length(parent_possible_matches==1)){
          std_parent_name <- db$name[parent_possible_matches]
          
          ## If no match found, try to take only the last chunk after the final ::
        } else if (length(parent_possible_matches==0)){
          parent_ <- unlist(strsplit(parent, "::"))
          parent_ <- parent_[length(parent_)]
          std_parent_name <- standardize_name(location=parent_, dbname=dbname)
          if (length(std_parent_name>1)){
            return("Multiple parent matches. Please provide more specific parent.")
          }
          
        } else if (length(parent_possible_matches>1)){
          return("Multiple parent matches. Please provide more specific parent.")
        }
      }
      ## If parent not standarized with ::, look up parent
    } else {
      std_parent_name <- standardize_name(location=parent, dbname=dbname)
    }
  }
  
  ## Check for name in aliases, if specified. return alias standard name if already exists.
  if (check_aliases){
    std_name <- standardize_name(location=name, scope=std_parent_name, dbname=dbname)
    if (!is.na(std_name)){
      return(std_name)
    } 
  }
  
  ## Deal with the ISO-3166-2 case here
  if(!grepl("::",parent)){
    warning("This function does not properly standardize ISO-3166-2 names")
  }
  
  ## If parent was identified, and name not found in aliases, return new name
  if (!is.na(std_parent_name)){
    return(paste(std_parent_name, name, sep="::"))
  } else {
    return("Could not identify the parent. Please recheck.")
  }
}

## 
## 
## parent <- "TZA::06"
## test_parent <- c("AFR::TZA::05", "AFR::TZA", "AFR::TZA::06")










#' @name standardize_name_local
#' @title standardize_name_local
#' @description Using various methods, matches the inputted location name to a location and returns a standardized code
#' for that location
#' @param location location name to match
#' @param scope a known standardized location scope, if available. For a city, this would be the code for the country 
#' where it is located
#' @param metadata Additional data that may be useful to identify the location name
#' @param database database to pull location information from. If NULL, it will pull from the database included in the package.
#' @param return_match_score logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
#' @return standardized database code which can be used to identify other data
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @import dplyr
#' @export
standardize_name_local <- function(location, scope=NULL, metadata, 
                                   return_match_scores=FALSE,
                                   ...){
  
  ## # TEMPORARY -- draw from country_names and country_codes data
  ## data('country_names',package='globaltoolbox')
  ## alias_database <- country_names %>% mutate(id = ISO3)
  ## # data('country_codes',package='globaltoolbox')
  ## # database_tmp <- country_codes

  ## limit database and alias_database to scope and metadata
  db_scoped <- metadata
  if (!is.null(scope)){
    db_scoped <- db_scoped %>% filter(grepl(scope, id) & id!=scope)
  }
  if (exists("type")){
    db_scoped <- db_scoped[db_scoped$type==type,]
  }
  
  # try({
  #   rc <- database_standardize_name(location,scope,dbname=dbname)
  #   if(length(rc) == 1){return(rc)}
  # })
  
  ## Clean Locations and aliases to match
  ## - cleaning here will be faster than for each match (maybe?)
  location_clean <- tolower(iconv(location, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
  location_clean <- str_replace_all(location_clean, "[[:punct:]]", "") # remove all punctuation
  names_b_data = db_scoped %>% select(id, name)
  names_b_data$name = tolower(iconv(names_b_data$name, from = 'UTF-8', to = 'ASCII//TRANSLIT'))
  names_b_data$name <- str_replace_all(names_b_data$name, "[[:punct:]]", "") # remove all punctuation
  
  ## Run the "match_names" function to match. 
  ##  - This returns a row index pertaining to names_b_data.
  ##  - Any location not finding a best match will return NA
  ##  - Any location finding multible different best matches will return NA
  matches_ <- sapply(location_clean, 
                     match_names, 
                     names_b_data = names_b_data,
                     return_match_scores=return_match_scores,
                     clean_a=FALSE, clean_b=FALSE)
  
  ## NOTE: As currently implemented, we are not restricting the scope or metadata for each location.
  
  return(db_scoped$id[matches_])
}



#' @name get_common_name_local
#' @title get_common_name_local
#' @description Wrapper function for `standardize_name` returns a standardized common name
#' for a location
#' @param location location name to match
#' @param scope a known standardized location scope, if available. For a city, this would be the code for the country 
#' where it is located
#' @param metadata Additional data that may be useful to identify the location name
#' @param database database to pull location information from. If NULL, it will pull from the database included in the package.
#' @param return_match_score logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
#' @return standardized database code which can be used to identify other data
#' @importFrom stringdist stringdist
#' @importFrom stringr str_replace_all
#' @import dplyr
#' @export
get_common_name_local <- function(location, scope=NULL, metadata, 
                                  return_match_scores=FALSE, ...){
  
  loc_ids <- standardize_name_local(location=location, scope=scope, metadata=metadata, 
                         return_match_scores=return_match_scores, ...)
  
  return(metadata$name[match(loc_ids, metadata$id)])

}


# 
# 
# metadata <- read_csv("data-raw/bangladesh_locations.csv") # From GADM
# # standardize_name_local(location, scope="BGD::Barisal", metadata, type="district")
# # get_common_name_local(location, scope="BGD::Barisal", metadata, type="district")
# standardize_name_local(location="Barisal", scope="BGD", metadata, type="division")
# get_common_name_local(location="Barrrisl", scope="BGD", metadata, type="division")


