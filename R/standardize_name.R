#' @include database_management.R get_location_data.R string_manipulation.R


#' @title standardize_name
#' @description Using various methods, matches the inputted location name to a location and returns a standardized code
#' for that location
#' @param location location name to match
#' @param scope a known standardized location scope, if available. For a city, this would be the code for the country
#' where it is located
#' @param metadata Additional data that may be useful to identify the location name
#' @param dbname database name to pull location information from. Default pulls from the database included in the package.
#' @param db_scoped inputted scoped database. This speeds it up when doing multiple matches.
#' @param strict_scope Logical, whether scope is strict. (what does "strict" even mean. we need to make this clearer.)
#' @param depth Depth in the tree to search, with country-level as depth=1.
#' @param standardize_location Logical, whether location strings should be standardized. This is already done in `telescoping_standardize()`. Default is FALSE.
#' @param return_match_scores logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
#' @return standardized database code which can be used to identify other data
#' @import dplyr
#' @export
standardize_name <- function(
  location,
  scope="",
  metadata,
  dbname=default_database_filename(),
  strict_scope=TRUE,
  depth=NA,
  standardize_location=FALSE,
  standardize_db=FALSE,
  ...
){
  original_location <- location
  if(length(scope) > 1){
    stop(paste(
      "This function takes a single scope.",
      "For multiple scopes, see telescoping_standardize"
    ))
  }
  if (standardize_location){
    location <- standardize_location_strings(location)
  }

  suppressWarnings(try({
      rc <- sapply(
          location,
          database_standardize_name,
          source = scope,
          dbname = dbname
      )
    if(length(rc) == length(location)){
        return(stats::setNames(rc, original_location))
    }
  },
  silent = TRUE))

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

  # First check if the location has already been standardized
  if(any(grepl("::", location))){

    ## -- cleaning here will be faster than for each match maybe
    names_b_data <- dplyr::select(
      db_scoped,
      .data$id,
      name = .data$name,
      depth = .data$depth_from_source)
    names_b_data <- dplyr::filter(names_b_data, !is.na(name) & name!="")     # Make sure no NAs or blanks
    names_b_data <- names_b_data %>% mutate(id_tmp = paste(id, name, sep="-")) %>% dplyr::filter(!duplicated(id_tmp))

    ## Run the "match_names" function to match.
    ##  This returns a row index pertaining to names_b_data.
    ##  Any location not finding a best match will return NA
    ##  Any location finding multible different best matches will return NA
    matches_ <- sapply(location,
                       match_names,
                       names_b_data = names_b_data,
                       return_match_scores = FALSE)

    if (any(is.na(matches_))){
      ## Unstandardize things without a match
      tmp_scope <- gsub("::[^:]*$", "", g)
      tmp_location <- gsub(".*:$", "", g)
      if (tmp_scope != scope){
        stop(paste(
          "The scopes do not agree, the location provides",
          tmp_scope,
          "but you provided",
          scope)
        )
      }
      ## For the rest of matching use the simplified version
      scope <- tmp_scope
      location <- tmp_location
    }
  }

  # If no matches, try the readable_name
  if(sum(is.na(matches_)) > 0){
    names_b_data <- dplyr::select(
                                db_scoped,
                                .data$id,
                                name = .data$readable_name,
                                depth = .data$depth_from_source
                          )
    names_b_data <- dplyr::filter(names_b_data, !is.na(name) & name!="")
    names_b_data <- names_b_data %>% mutate(id_tmp = paste(id, name, sep="-")) %>% dplyr::filter(!duplicated(id_tmp))

    matches_[is.na(matches_) & !is.na(location)] <- sapply(location[is.na(matches_) & !is.na(location)],
                                         match_names,
                                         names_b_data = names_b_data,
                                         return_match_scores = FALSE)
  }

  # If no matches, try the Aliases
  if (sum(is.na(matches_)) > 0){
      names_b_data <- dplyr::select(
                                 db_scoped,
                                 .data$id,
                                 name = .data$alias,
                                 depth = .data$depth_from_source
                             )
      names_b_data <- dplyr::filter(names_b_data, !is.na(name) & name!="")
      names_b_data <- names_b_data %>% mutate(id_tmp = paste(id, name, sep="-")) %>% dplyr::filter(!duplicated(id_tmp))

      matches_[is.na(matches_) & !is.na(location)] <- sapply(location[is.na(matches_) & !is.na(location)],
                                          match_names,
                                          names_b_data = names_b_data,
                                          return_match_scores = FALSE)
  }


  # If any matches, return the matched name, with original, as a named vector
  matched_locs <- stats::setNames(db_scoped$name[match(matches_, db_scoped$id)], original_location)

  # If any with non-NA errors, put those into the returned value
  matched_locs[is.na(matched_locs)] <- stats::setNames(matches_[is.na(matched_locs)], original_location[is.na(matched_locs)])

  return(matched_locs)
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
#' @export
match_names <- function(name_a, names_b_data,
                        return_match_scores=FALSE,
                        clean_a=FALSE, clean_b=FALSE){

  # Check for error (NA in name_a)
  if (is.na(name_a)){
    return(stats::setNames(as.integer(NA), name_a))
  }

  # If location ids, limit to unique ones
  # if (!is.null(names_b_data$id)){
  #   names_b_data <- names_b_data[!duplicated(names_b_data$id),]
  # }
  names_b <- as.character(names_b_data$name)

  ## Clean name if not already done
  if (clean_a){
    name_a <- standardize_location_strings(name_a)
  }

  ## Clean aliases if not done
  if (clean_b){
    names_b <- standardize_location_strings(names_b)
  }


  # First check for an exact match  .............................................................
  exact_match <- (names_b %in% name_a)

  if (sum(exact_match)>=1){
    ## Ensure that only the lowest depth matches are allowed
    exact_match[which(exact_match)] <- names_b_data$depth[exact_match] == min(names_b_data$depth[exact_match])
    # If 1 exact match
    if (sum(exact_match)==1){

      if (return_match_scores){
        return(list(best = setNames(names_b_data$id[exact_match], rep(name_a,times=nrow(names_b_data))),
                    match_scores = NA))
      } else {
        return(setNames(names_b_data$id[exact_match], name_a))
      }

    ## if  more than 1 best match, return the number of matches and the description data
    } else {
      message(paste(name_a,"has",sum(exact_match),"exact matches.  Ignoring them."))
      if (return_match_scores){
        return(list(best = setNames(as.integer(NA), name_a),
                    match_scores = names_b_data[exact_match,]))
      } else {
        return(setNames(as.integer(NA), name_a))
      }
    }


  # If no exact match, do the string matching algorithms
  } else {

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
    } else if (any(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)){
      best_ <- which(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)
    }

    # ## Modified return if name has 5 or fewer characters 
    # ## --> can generate spurious matches [NEED TO FIX THIS]
    # if ( (nchar(name_a) <= 5) & !return_match_scores){
    #   return(setNames(as.integer(NA),name_a))
    # } else if ( (nchar(name_a) <= 5) & return_match_scores){
    #   return(list(best = setNames(as.integer(NA, name_a)),
    #               match_scores = rep(as.numeric(NA), times = min(20, nrow(dists)))
    #   ))
    # }
    
    ## If no good match was found, return either nothing, or the score matrix (up to 20 long)
    if (length(best_) == 0 & !return_match_scores){
      return(setNames(as.integer(NA), name_a))
    } else if (length(best_) == 0 & return_match_scores){
      return(list(best = setNames(as.integer(NA), name_a),
                  match_scores = dists[order(dists$score_sums), ][1:min(20, nrow(dists)),]))
    }

    ## If more than 1 meeting the criteria for best match
    ## -- first check if they are the same, otherwise, return all
    ## if only 1 best, replace best_ with ID
    if (length(best_)==1){
      best_match <- dists$id[best_]
      best_match <- stats::setNames(best_match, name_a)

      if (!return_match_scores){
        return(best_match)
      } else {
        return(list(best = best_match,
                    match_scores = dists[order(dists$score_sums), ][1:min(20, nrow(dists)),]))
      }

    } else if (length(best_)>1){
      dists_best <- dists[best_, ]
      ## Order by score sum
      dists_best <- dists_best[order(dists_best$score_sums), ]
      ## Remove duplicate ids
      dists_best <- dists_best[!duplicated(dists_best$id), ]
      ## Take the score of minimal depth
      if (!return_match_scores){
        dists_best <- dists_best[dists_best$depth == min(dists_best$depth), ]
      }

      ## if still more than 1 best match, return the number of matches or the distance matrix
      if (nrow(dists_best) > 1){
        message(paste(name_a,"has",nrow(dists_best),"best matches.  Ignoring them."))
        if (return_match_scores){
          return(list(best = setNames(as.integer(NA), name_a),
                      match_scores = names_b_data[exact_match,]))
        } else {
          return(setNames(as.integer(NA), name_a))
        }
      } else {
        best_match <- dists_best$id
        best_match <- stats::setNames(best_match, name_a)

        if (!return_match_scores){
          return(best_match)
        } else {
          return(list(best = best_match,
                      match_scores = dists[order(dists$score_sums), ][1:min(20, nrow(dists)),]))
        }
      }
    }
  }
}






#' @title get_match_distances
#' @description Using various methods, matches the inputted location name to a location and returns list of string distance matrices
#' @param location location name to match
#' @param scope a known standardized location scope, if available. For a city, this would be the code for the country
#' where it is located
#' @param metadata Additional data that may be useful to identify the location name
#' @param dbname database name to pull location information from. Default pulls from the database included in the package.
#' @param db_scoped inputted scoped database. This speeds it up when doing multiple matches.
#' @param strict_scope Logical, whether scope is strict.
#' @param depth Depth in the tree to search, with country-level as depth=1.
#' @param standardize_location Logical, whether location strings should be standardized. This is already done in `telescoping_standardize()`. Default is FALSE.
#' @param return_match_scores logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
#' @return standardized database code which can be used to identify other data
#' @import dplyr
#' @export
get_match_distances <- function(
  location,
  scope="",
  metadata,
  dbname=default_database_filename(),
  strict_scope=TRUE,
  depth=NA,
  standardize_location=FALSE,
  standardize_db=FALSE,
  ...
){
  original_location <- location
  if(length(scope) > 1){
    stop(paste(
      "This function takes a single scope.",
      "For multiple scopes, see telescoping_standardize"
    ))
  }
  if (standardize_location){
    location <- standardize_location_strings(location)
  }

  ## limit database and alias_database to scope and metadata
  db_scoped <- get_all_aliases(
    source = scope,
    dbname = dbname,
    depth = depth,
    strict_scope = strict_scope
  )

  if (standardize_db){
    db_scoped$name <- standardize_location_strings(db_scoped$name)
    db_scoped$readable_name <- standardize_location_strings(db_scoped$readable_name)
    db_scoped$alias <- standardize_location_strings(db_scoped$alias)
  }


  # Get list of distance matrices
   names_b_data <- dplyr::select(
      db_scoped,
      .data$id,
      name = .data$readable_name,
      depth = .data$depth_from_source
    )
    # Make sure no NAs or blanks
    names_b_data <- dplyr::filter(names_b_data, !is.na(name) & name!="")
    names_b_data <- names_b_data %>% mutate(id_tmp = paste(id, name, sep="-")) %>% dplyr::filter(!duplicated(id_tmp))

    match_dists <- list()
    for (m in 1:length(location)){
      match_dists[[location[m]]] <- dplyr::right_join(db_scoped %>% dplyr::select(name_full=name, id) %>% filter(!duplicated(id)),
                                                      match_names(location[m], names_b_data = names_b_data, return_match_scores = TRUE),
                                                      by=c("id"))
    }
    return(match_dists)

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
#' @export
create_standardized_name <- function(name,
  parent=NA,
  check_aliases=FALSE,
  dbname=default_database_filename(),
  verbose=FALSE
){
  ## Do a couple of checks
  if(is.null(parent)){
    if(verbose){
      warning(paste(
        "This function does not currently handle",
        "creating standardized country names"
      ))
    }
    return(standardize_location_strings(name))
  }
  if (length(parent) == 1 && is.na(parent)){
    ## JK: Turned these into stops someone not looking at the results
    ##     might not notice the function failed
    stop("Need to specify parent")
  }

  ## Load database
  ## Standardize the location name
  name <- standardize_location_strings(name)

  std_parent_name <- sapply(
    parent,
    function(x){
      tryCatch(
        database_standardize_name(
          name = x,
          dbname = dbname,
          aliases = FALSE,
          source = ""
        ),
        error = function(err){
          NA
        }
      )
    }
  )
  parents_nonstd <- is.na(std_parent_name)

  ## Check for parent in database (check aliases if appropriate)
  if (sum(parents_nonstd) > 0){
    std_parent_name[parents_nonstd] <-
      sapply(
        parent[parents_nonstd],
        function(x){
          tryCatch(
            database_standardize_name(
              name = x,
              dbname = dbname,
              aliases = TRUE,
              source = ""
            ),
            error = function(err){
              NA
            }
          )
        }
      )
    parents_nonstd <- is.na(std_parent_name)
  }

  ## Check for name in aliases, if specified.
  ## Throw an error if one is found.
  if (check_aliases){
    std_name <- standardize_name(
      location = name,
      scope = ifelse(
        is.na(std_parent_name),
        NULL,
        std_parent_name
      ),
      dbname = dbname,
      strict_scope = TRUE
    )
    if(length(name) > 1){
      stop("This is not vectorized")
    }
    if(!is.na(std_name)){
      stop(paste(
        "The location",
        name,
        "already has a standardized entry in the database"
      ))
    }
  }

  ## Deal with the ISO 3166 2 case here
  if(any(!grepl("::", parent))){
    if(verbose){
      warning("This function does not properly standardize ISO-3166-2 names")
    }
  }

  ## If parent was identified, and name not found in aliases, return new name
  if(any(!is.na(std_parent_name))){
    return(standardize_location_strings(
      paste(std_parent_name, name, sep = "::")
    ))
  } else {
    ## JK: Turned these into stops someone not looking at the results
    ##     might not notice the function failed
    stop("Could not identify the parent. Please recheck.")
  }
}


#' @name standardize_name_local
#' @title standardize_name_local
#' @description Using various methods, matches the inputted location name to a location and returns a standardized code
#' for that location
#' @param location location name to match
#' @param scope a known standardized location scope, if available. For a city, this would be the code for the country
#' where it is located
#' @param metadata Additional data that may be useful to identify the location name
#' @param database database to pull location information from. If NULL, it will pull from the database included in the package.
#' @param return_match_scores logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
#' @return standardized database code which can be used to identify other data
#' @import dplyr
#' @export
standardize_name_local <- function(
  location,
  scope="",
  metadata,
  return_match_scores=FALSE,
  ...,
  type = NULL
){

  db_scoped <- metadata
  if (!is.null(scope)){
    db_scoped <- dplyr::filter(
      db_scoped,
      grepl(.data$scope, .data$id) & .data$id != .data$scope
    )
  }
  ## JK : This seems better
  if(!is.null(type)){
    db_scoped <- db_scoped[type == db_scoped[["type"]], ]
  }

  ## Clean Locations and aliases to match
  ##   cleaning here will be faster than for each match (maybe?)
  location <- standardize_location_strings(location)
  names_b_data <- db_scoped[, c('id', 'name')]
  names_b_data$name <-
    standardize_location_strings(names_b_data$name)

  ## Run the "match_names" function to match.
  ##    This returns a row index pertaining to names_b_data.
  ##    Any location not finding a best match will return NA
  ##    Any location finding multible different best matches will return NA
  matches_ <- sapply(
    location,
    match_names,
    names_b_data = names_b_data,
    return_match_scores = return_match_scores,
    clean_a = FALSE,
    clean_b = FALSE
  )
  ## NOTE: As currently implemented, we are using a single scope and metadata
  ##       See telescoping_standardize for an alternative
  return(db_scoped$name[matches_])
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
#' @param return_match_scores logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
#' @return standardized database code which can be used to identify other data
#' @export
get_common_name_local <- function(
  location,
  scope="",
  metadata,
  return_match_scores=FALSE,
  ...
){
  loc_ids <- standardize_name_local(
    location = location,
    scope = scope,
    metadata = metadata,
    return_match_scores = return_match_scores,
    ...
  )

  return(metadata$name[match(loc_ids, metadata$id)])

}
