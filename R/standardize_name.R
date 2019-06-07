#' @include database_management.R get_location_data.R string_manipulation.R


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
#' @import dplyr
#' @export
standardize_name <- function(
  location,
  scope="",
  metadata,
  dbname=default_database_filename(),
  strict_scope=TRUE,
  depth=NA,
  ...
){
  original_location <- location
  if(length(scope) > 1){
    stop(paste(
      "This function takes a single scope.",
      "For multiple scopes, see telescoping_standardize"
    ))
  }

  location_clean <- standardize_location_strings(location)

  try({
      rc <- sapply(
          location_clean,
          database_standardize_name,
          source = scope,
          dbname = dbname
      )
    if(length(rc) == length(location)){
        return(stats::setNames(rc, original_location))
    }
  },
  silent = T)
  ## limit database and alias_database to scope and metadata
  db_scoped <- get_all_aliases(
    source = scope,
    dbname = dbname,
    aliases = TRUE,
    depth = depth,
    strict_scope = strict_scope
  )

  ## Clean Locations and aliases to match
  ## cleaning here will be faster than for each match maybe
  names_b_data <- dplyr::select(
    db_scoped,
    .data$id,
    name = .data$name,
    depth = .data$depth_from_source
  )
  names_b_data$name <-
    standardize_location_strings(names_b_data$name)

  ## Run the "match_names" function to match.
  ##  This returns a row index pertaining to names_b_data.
  ##  Any location not finding a best match will return NA
  ##  Any location finding multible different best matches will return NA
  matches_ <- sapply(location_clean,
                     match_names,
                     names_b_data = names_b_data,
                     return_match_score = FALSE)

  # If no matches, try the readable_name
  if(sum(is.na(matches_)) > 0){
      names_b_data <- dplyr::select(
                                db_scoped,
                                .data$id,
                                name = .data$readable_name,
                                depth = .data$depth_from_source
                            )
    names_b_data$name <-
      standardize_location_strings(names_b_data$name)

    matches_[is.na(matches_)] <- sapply(location_clean[is.na(matches_)],
                                         match_names,
                                         names_b_data = names_b_data,
                                         return_match_score = FALSE)
                    }
  # If no matches, try the Aliases
  if (sum(is.na(matches_)) > 0){
      names_b_data <- dplyr::select(
                                 db_scoped,
                                 .data$id,
                                 name = .data$alias,
                                 depth = .data$depth_from_source
                             )
    names_b_data$name <-
      standardize_location_strings(names_b_data$name)

    matches_[is.na(matches_)] <- sapply(location_clean[is.na(matches_)],
                                        match_names,
                                        names_b_data = names_b_data,
                                        return_match_score = FALSE)
  }

  if(all(is.na(matches_))){
    return(stats::setNames(matches_, original_location))
  }
  return(stats::setNames(db_scoped$name[matches_], original_location))
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

  names_b <- names_b_data$name

  ## Clean name if not already done
  if (clean_a){
    name_a <- standardize_location_strings(name_a)
  }

  ## Clean aliases if not done
  if (clean_b){
    names_b <- standardize_location_strings(names_b)
  }

  ## string matching methods
  ## Consider passing in methods as an argument?
  ##   osa     :
  ##   lv      :
  ##   dl      :
  ##   lcs     :
  ##   qgram   :
  ##   cosine  :
  ##   jaccard :
  ##   jw      :
  ##   soundex :
  methods <- c("osa", "jw", "soundex")
  dists <- as.data.frame(matrix(
    NA,
    nrow = length(names_b),
    ncol = length(methods),
    dimnames = list(names_b, methods)
  ))
  for (j in 1:length(methods)){
    dists[, j]  <- suppressWarnings(
      stringdist::stringdist(name_a, names_b, method = methods[j])
    )
  }
  dists <- data.frame(names_b_data,
                      names_clean = names_b,
                      dists,
                      score_sums = rowSums(dists))
  if(!all(is.na(dists))){
    dists$score_sums_normalized <-
      1 - (dists$score_sums / max(dists$score_sums))
  } else {
    dists$score_sums_normalized <- dists$score_sums
  }
  dists$osa <- as.integer(dists$osa)


  ## get best match from results

  best_ <- NULL
  if (any(dists$osa <= 1)){
    ## OSA less than 1 is highly likely a match
    best_ <- which(dists$score_sums == min(dists$score_sums))
  } else if (any(dists$jw <= .1)){
    best_ <- which(dists$ score_sums == min(dists$score_sums))
  } else if (any(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)){
    best_ <- which(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)
  }

  ## If no good match was found, return either nothing, or the score matrix
  if (length(best_) == 0 & !return_match_scores){
    return(NA)
  } else if (length(best_) == 0 & return_match_scores){
    return(dists[order(dists$score_sums), ])
  }

  ## If more than 1 meeting the criteria for best match
  ## first check if they are the same,
  ## otherwise, return all
  if (length(best_) > 1){
    dists$alias_ind <- 1:nrow(dists)
    dists_best <- dists[best_, ]
    ## Order by score sum
    dists_best <- dists_best[order(dists_best$score_sums), ]
    ## Remove duplicate ids
    dists_best <- dists_best[!duplicated(dists_best$id), ]
    ## Take the score of minimal depth
    dists_best <- dists_best[dists_best$depth == min(dists_best$depth), ]

    ## if still more than 1 best match, return NA or the distance matrix
    if (nrow(dists_best) > 1){
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
#' @param return_match_score logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
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
  ## JK: I think this shouldn't be part of the local version
  # try({
  #   rc <- sapply(
  #     location,
  #     database_standardize_name,
  #     source = scope,
  #     dbname = dbname
  #   )
  #   if(length(rc) == length(location)){
  #     return(rc)
  #   }
  # },
  # silent = T)
  ## limit database and alias_database to scope and metadata
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
  location_clean <- standardize_location_strings(location)
  names_b_data <- db_scoped[, c('id', 'name')]
  names_b_data$name <-
    standardize_location_strings(names_b_data$name)

  ## Run the "match_names" function to match.
  ##    This returns a row index pertaining to names_b_data.
  ##    Any location not finding a best match will return NA
  ##    Any location finding multible different best matches will return NA
  matches_ <- sapply(
    location_clean,
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
#' @param return_match_score logical, whether to return the matching score. Score reported on 0-1 scale, with 1 being a perfect match.
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
