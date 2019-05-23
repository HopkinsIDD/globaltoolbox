
#' @name create_location_sf
#' @title match_names
#' @description ???
#' @param location_name Location name to generate shapefiles for.
#' @param thorough Logical, whether to include all levels of the location...
#' @return Array of location shapefiles???
#' @import dplyr
#' @export
create_location_sf <- function(location_name, thorough=FALSE){
  original_location_name <- location_name
  location_name <- globaltoolbox::standardize_location_strings(location_name)
  location_array <- strsplit(location_name, split = '::')
  if(thorough){
    location_array <- lapply(
      location_array,
      function(x){
        lapply(
          1:length(x),
          function(i){
            x[1:i]
          }
        )
      }
    )
  } else {
    location_array <- lapply(
      location_array,
      function(x){
        list(x)
      }
    )
  }
  location_array <- lapply(
    location_array,
    function(y){
      lapply(
        y,
        function(x){
          strsplit(x, split = '|', fixed = TRUE)
        }
      )
    }
  )

  ## We currently have the following list levels
  ## 1 Location in args list
  ### 2 If thorough all shapefiles that matter
  #### 3 ISO_level
  ##### 4 location_within that arg

  ## Build an array to store things in for easier access
  old_location_array <- location_array
  nargs <- length(old_location_array)
  nthr <- max(sapply(old_location_array, length))
  niso <- max(sapply(
    old_location_array,
    function(x){
      max(sapply(x, length))
    }
  ))
  npipe <- max(sapply(
    old_location_array,
    function(x){
      max(sapply(
        x,
        function(x){
          max(sapply(x, length))
        }
      ))
    }
  ))
  location_array <- array(as.character(NA), c(niso, nargs, npipe, nthr))

  ## Populate the array
  for(arg_idx in 1:nargs){
    ## Calculate the number of pipes for each ISO region for this arg.
    this_pipe_lengths <- lapply(
      old_location_array[[arg_idx]],
      function(y){
        sapply(y, length)
      }
    )
    ## Assuming the length is the single non 1 value of this_pipe_lengths
    thr_depth <- length(old_location_array[[arg_idx]])
    pipe_length <- unique(this_pipe_lengths[[thr_depth]])
    pipe_length <- pipe_length[pipe_length > 1]
    ## The length might actually be 1...
    if(length(pipe_length) == 0){
      pipe_length <- 1
    }
    ## Checking just to be sure
    if(length(pipe_length) > 1){
      ## This function is meant to not fail easily
      ## the pipe_length is set to NA to indicate a failed state
      pipe_length <- NA
    }
    ISO_depth <- sapply(old_location_array[[arg_idx]], length)
    if(!is.na(pipe_length)){
      for(pipe_idx in 1:pipe_length){
        for(thr_idx in 1:thr_depth){
          for(ISO_idx in 1:ISO_depth[thr_idx]){
            if(pipe_idx <= this_pipe_lengths[[thr_idx]][ISO_idx]){
              location_array[ISO_idx, arg_idx, pipe_idx, thr_idx] <-
                old_location_array[[arg_idx]][[thr_idx]][[ISO_idx]][[pipe_idx]]
            } else {
              location_array[ISO_idx, arg_idx, pipe_idx, thr_idx] <-
                old_location_array[[arg_idx]][[thr_idx]][[ISO_idx]][[1]]
            }
          }
        }
      }
    }
  }
  ## We will use this for finding the shapefiles.
  ## They will eventually be part of this object
  ungrouped_shapefiles <-
    dplyr::filter(reshape2::melt(location_array), !is.na(value))
  ungrouped_shapefiles$value <-
    as.character(ungrouped_shapefiles$value)
  names(ungrouped_shapefiles)[1:4] <-
    c('ISO_level', 'arg_idx', 'pipe_idx', 'thr_idx')
  ## JK : There should be a better way to do this
  ungrouped_shapefiles <- ungrouped_shapefiles %>%
    dplyr::group_by(arg_idx, pipe_idx, thr_idx) %>%
    do({
      tmp <- .
      tmp$ISO_A2_level <- max(tmp$ISO_level)
      tmp
    })

  ungrouped_shapefiles <- tidyr::spread(
    ungrouped_shapefiles,
    key = ISO_level,
    value = value,
    drop = T
  )
  names(ungrouped_shapefiles)[5] <- 'ISO_A1'
  if(ncol(ungrouped_shapefiles) > 5){
    names(ungrouped_shapefiles)[6:ncol(ungrouped_shapefiles)] <-
      paste(
        'ISO_A2_L',
        as.numeric(names(ungrouped_shapefiles)[6:ncol(ungrouped_shapefiles)]),
        sep = ''
      )
  }
  if(thorough){
    ungrouped_shapefiles <- ungrouped_shapefiles %>%
      dplyr::filter(thr_idx > 0)
  }
  ungrouped_shapefiles$location_name <- paste0(
    apply(
      ungrouped_shapefiles[, 5:ncol(ungrouped_shapefiles)],
      1,
      function(x){
        paste(na.omit(x), collapse = '::')
      }
    )
  )
  ungrouped_shapefiles$idx <- 1:nrow(ungrouped_shapefiles)
  ungrouped_shapefiles$location <-
    original_location_name[ungrouped_shapefiles$arg_idx]
  ungrouped_shapefiles$source <-
      gsub(':?:?[^:]*$', '', ungrouped_shapefiles$location_name)
  ungrouped_shapefiles$source[ungrouped_shapefiles$source == ''] <- NA
  return(ungrouped_shapefiles)
}



# ADD ARGUMENTS


#' @name telescoping_standardize
#' @title telescoping_standardize
#' @description ???
#' @param location_name Location name to generate shapefiles for.
#' @param dbname Filename of the database. Default will use the package-defined name.
#' @return Location standized name???
#' @import dplyr
#' @export
telescoping_standardize <- function(
  location_name,
  dbname=default_database_filename()
){
  location_name <- globaltoolbox::standardize_location_strings(location_name)
  location_sf <-
    globaltoolbox::create_location_sf(location_name, thorough = TRUE)
  all_names <- list()
  for(level in sort(unique(location_sf$ISO_A2_level))){
      # tmp_location_sf <- dplyr::filter(location_sf, ISO_A2_level == level)
      tmp_location_sf <- location_sf[location_sf[["ISO_A2_level"]] == level, ]
    counter <- 1
    tmp_location_sf$standardized_source <- as.character(NA)
    while(
      (level > counter) &
      (any(is.na(tmp_location_sf$standardized_source)))
    ){
      tmp_location_sf_idx <- is.na(tmp_location_sf$standardized_source)
      tmp_location_sf$standardized_source[tmp_location_sf_idx] <-
        all_names[[level - counter]][
          tmp_location_sf$source[tmp_location_sf_idx]
        ]
      counter <- counter + 1
    }
    for(scope in unique(tmp_location_sf$standardized_source)){
      if(is.na(scope)){
        tmp_location_sf_idx <- is.na(tmp_location_sf$standardized_source)
        nonstandard_names <- unique(
          tmp_location_sf$location_name[tmp_location_sf_idx]
        )
        standard_names <- standardize_name(
          gsub('.*:', '', nonstandard_names),
          scope = NULL,
          dbname = dbname
        )
     } else {
        nonstandard_names <- unique(tmp_location_sf$location_name[
          (!is.na(tmp_location_sf$standardized_source)) &
          (tmp_location_sf$standardized_source == scope)
        ])
        standard_names <- standardize_name(
          gsub('.*:', '', nonstandard_names),
          scope = scope,
          dbname = dbname
        )
      }
      names <- stats::setNames(standard_names, nonstandard_names)
      if(level <= length(all_names)){
        all_names[[level]] <- c(all_names[[level]], names)
      } else {
        all_names[[level]] <- names
      }
    }
  }
  all_names <- unlist(all_names)
  location_sf$standardized_name <- all_names[location_sf$location_name]
  location_sf <- dplyr::group_by(location_sf, arg_idx, pipe_idx)
  location_sf <- dplyr::summarize(
    location_sf,
    standardized_name = ifelse(
      any(is.na(standardized_name)),
      as.character(NA),
      standardized_name[which.max(thr_idx)]
    )
  )
  location_sf <- dplyr::ungroup(location_sf)
  location_sf <- dplyr::group_by(location_sf, arg_idx)
  location_sf <- dplyr::summarize(
    location_sf,
    standardized_name = ifelse(
        any(is.na(standardized_name)),
        as.character(NA),
        paste(standardized_name, collapse = '|')
    )
  )
  location_sf <- dplyr::ungroup(location_sf)
  return(location_sf$standardized_name)
}
