#' @name load_gadm
#' @title load_gadm
#' @description Build database of locations from online GADM data repository
#' @param countries The set of countries to load gadm information into the database for
#' @param dbname The name of the database file (there is a default database maintained by the package)
#' @return standardized database built from GADM data
#' @import dplyr
#' @export
load_gadm <- function(
  countries = NULL,
  max_depth = Inf,
  dbname = default_database_filename()
){
  country_aliases.csv <- system.file(
    "extdata",
    "country_aliases.csv",
    package = "globaltoolbox"
  )
  country_aliases <- read.csv(country_aliases.csv)
  country_aliases <- country_aliases[
    (country_aliases[, 1] == countries ) |
    (country_aliases[, 2] == countries ),
  ]

  countries <- unique(country_aliases$country_code)

  error_messages <- c('')

  alias_ISO_columns <- c(
    "HASC",
    "VARNAME",
    "ISO",
    "NAME",
    "ID",
    "NL_NAME"
  )

  for(ISO_A1 in countries){
    print(ISO_A1)
    destination <- tempfile(fileext = '.rds')

    # Download GADM for the country
    try({
      ISO_level <- 0
      website <- paste(
        "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
        ISO_A1,
        "_adm",
        ISO_level,
        ".rds",
        sep = ''
      )
      download.file(website, destination, mode = 'wb')
    },
    silent = T)

    # if GADM file downloaded correctly process it
      if(file.exists(destination) & (file.size(destination) > 0)){
      country_data <- sf::st_as_sf(readRDS(destination))
      country_data$type <- 'ISO_A1'
      country_data$depth <- 0
      metadata_frame <- as.data.frame(country_data)
      metadata_frame <- metadata_frame[, colnames(metadata_frame) != 'geometry']
      metadata_frame <-
        metadata_frame[, !grepl("NAME", colnames(metadata_frame))]
      metadata_frame <-
        metadata_frame[, !(
          colnames(metadata_frame) %in% c("VALIDFR", "VALIDTO")
        )]
      descendent_id <- database_add_descendent(
        dbname = dbname,
        metadata = metadata_frame,
        standardized_name = "",
        readable_descendent_name = country_data$ISO
      )
      for(alias_idx in
        c(
          which(colnames(country_data) == 'ISO'),
          which(grepl("NAME", colnames(country_data)))
        )
      ){
        alias <- country_data[[alias_idx]][1]
        if(grepl("^\n \r$", alias)){
          next
        }
        location_id <- descendent_id
        tryCatch({
          database_add_location_alias(
            dbname = dbname,
            location_id = descendent_id,
            alias = alias
          )
        },
        error = function(e){
          if(!(
            grepl("UNIQUE constraint failed", e$message)
          )){
            stop(paste(
              "The only way creating an alias should fail is",
              "if the alias is already in the database,",
              "but it failed with message:",
              e$message
            ))
          }
        })
      }
      tryCatch({
        geometry <- country_data$geometry
        time_left <- "1800-01-01"
        time_right <- "2020-01-01"
        database_add_location_geometry(
          location_id = location_id,
          time_left = time_left,
          time_right = time_right,
          geometry = geometry,
          dbname = dbname
        )
      },
      error = function(e){
      },
      silent = T)
      unlink(destination)
    }

    ## Lower level regions:
    toggle <- TRUE
    ISO_level <- 0
    while(toggle){
        if(length(error_messages) > 11){
            warning("Over 10 errors")
        }
        if(length(error_messages) > 101){
            warning("Over 100 errors")
        }
        if(length(error_messages) > 1001){
            warning("Over 1000 errors")
        }
        if(length(error_messages) > 10001){
            warning("Over 10000 errors")
        }
        if(length(error_messages) > 100001){
            warning("Over 100000 errors")
        }
      try({
        ISO_level <- ISO_level + 1
        website <- paste(
          "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
          ISO_A1,
          "_adm",
          ISO_level,
          ".rds",
          sep = ''
        )
        download.file(website, destination, mode = 'wb')
      },
      silent = T)
      if(file.exists(destination) & (file.size(destination) > 0)){
        country_data <- sf::st_as_sf(readRDS(destination))
        country_data$type <- paste0('ISO_A2_L', ISO_level)
        country_data$depth <- ISO_level
        parent_name <- c()
        for(i in 0:(ISO_level - 1)){
          ## Remove data about containing countries
            parent_name <- paste(
                parent_name,
                country_data[[paste('NAME', i, sep = '_')]],
                sep = '::')
        }
        parent_name <- gsub('^::', '', parent_name)
        country_data$standardized_parent_name <-
          telescoping_standardize(parent_name, dbname = dbname)
        for(row in 1:nrow(country_data)){
          print(paste(
            ISO_A1,
            "level",
            ISO_level,
            "location",
            row,
            "/",
            nrow(country_data)
          ))
          tmp_data <- country_data[row, ]
          metadata_frame <- as.data.frame(tmp_data)
          metadata_frame <- metadata_frame[, !(
            colnames(metadata_frame) %in%
            paste(alias_ISO_columns, ISO_level, sep = '_')
          )]
          metadata_frame <- metadata_frame[, (
            colnames(metadata_frame) != 'geometry'
          )]
          metadata_frame <- metadata_frame[, (
            colnames(metadata_frame) != 'ISO'
          )]
          for(i in 0:(ISO_level - 1)){
            ## Remove data about containing countries
              metadata_frame <- metadata_frame[, !(
                  grepl(paste0('_', i, '$'), colnames(metadata_frame))
                  )]
            colnames(metadata_frame) <-
                gsub(paste0('_', i, '$'), '', colnames(metadata_frame))
          }
          tryCatch({
            descendent_id <- database_add_descendent(
              dbname = dbname,
              metadata = metadata_frame,
              standardized_name = tmp_data$standardized_parent_name,
              readable_descendent_name =
                tmp_data[[paste('NAME', ISO_level, sep = '_')]]
            )
          },
          error = function(e){
            error_messages <<- c(
              error_messages,
              paste(
                ISO_A1,
                "level",
                ISO_level,
                "location",
                row,
                "/",
                nrow(country_data),
                e$message
              )
            )
          })
          tmp_alias_ISO_columns <- paste(alias_ISO_columns[
            paste(alias_ISO_columns, ISO_level, sep = '_') %in%
            colnames(country_data)
          ],
          ISO_level,
          sep = '_'
          )
          for(alias_name in tmp_alias_ISO_columns){
            alias <- tmp_data[[alias_name]][1]
            if(is.na(alias)){
              next
            }
            if(grepl("^\n \r$", alias)){
              next
            }
            if(grepl("|", alias, fixed = T)){
              alias <- strsplit(alias, '|', fixed = T)[[1]]
            }
            for(this_alias in alias){
              location_id <- descendent_id
              tryCatch({
                database_add_location_alias(
                  dbname = dbname,
                  location_id = descendent_id,
                  alias = this_alias
                )
              },
              error = function(e){
                if(!(
                  grepl("UNIQUE constraint failed", e$message)
                )){
                  stop(
                    "The only way creating an alias should fail is if",
                    "the alias is already in the database"
                  )
                }
              })
            }
          }
          geometry <- tmp_data$geometry
          time_left <- "1800-01-01"
          time_right <- "2020-01-01"
          tryCatch({
            geometry <- country_data$geometry
            time_left <- "1800-01-01"
            time_right <- "2020-01-01"
            database_add_location_geometry(
              location_id = location_id,
              time_left = time_left,
              time_right = time_right,
              geometry = geometry,
              dbname = dbname
            )
          },
          error = function(e){
          },
          silent = T)
          unlink(destination)
        }
      } else {
        toggle <- FALSE
      }
    }
  }
  for(msg in error_messages){
    message(msg)
  }
  return()
}



#' @name standardize_gadm_lhs_time
#' @title standardize_gadm_lhs_time
#' @param x Time to standardize.
#' @return Standardized time.
#' @export
standardize_gadm_lhs_time <- function(x){
    if(x == 'Present'){
        return(lubridate::now())
    }
    if(x == 'Unknown'){
        return(lubridate::ymd('1900-01-01'))
    }
  x <- gsub('[^1234567890]', '', x)
  if(nchar(x) == 4){
    return(lubridate::ymd(paste(x, '01', '01')))
  }
  if(nchar(x) == 6){
    return(lubridate::ymd(paste(substr(x, 1, 4), substr(x, 5, 6), '01')))
  }
  if(nchar(x) == 8){
    return(
      lubridate::ymd(paste(substr(x, 1, 4), substr(x, 5, 6), substr(x, 7, 8)))
    )
  }
  return(NA)
}



#' @name standardize_gadm_rhs_time
#' @title standardize_gadm_rhs_time
#' @param x Time to standardize.
#' @return Standardized time.
#' @export
standardize_gadm_rhs_time <- function(x){
    if(x == 'Present'){
        return(lubridate::now() + lubridate::years(1))
    }
    if(x == 'Unknown'){
        return(lubridate::now() + lubridate::years(1))
    }
  x <- gsub('[^1234567890]', '', x)
  if(nchar(x) == 4){
    return(lubridate::ymd(paste(x, '12', '31')))
  }
  if(nchar(x) == 6){
    return(lubridate::ymd(paste(substr(x, 1, 4), substr(x, 5, 6), '31')))
  }
  if(nchar(x) == 8){
    return(
      lubridate::ymd(paste(substr(x, 1, 4), substr(x, 5, 6), substr(x, 7, 8)))
    )
  }
  return(NA)
}

load_sf <- function(
  sf_object,
  source,
  name_columns,
  alias_column = c(),
  geometry_columns = "geometry",
  match_threshold = 1e-6
){

  error_messages <- c('')

  sf_object$depth <- NA
  sf_object$source <- as.character(NA)
  sf_object$exact_match <- FALSE
  while(is.na(any(sf_object$depth))){
    sf_object$source_updated <- FALSE
    ## Search for a containing location at depth whatever
    for(source in unique(sf_object$source)){

      if(is.na(source)){
        tmp_sf <- sf_object[is.na(sf_object$source),]
        source <- NULL
        stop("This case is not yet working")
      } else {
        tmp_sf <- sf_object[sf_object$source == source,]
      }
      all_geometries <- get_location_geometry(
        source = source,
        depth = 1,
        strict_scope = TRUE
      )
      intersection_matrix <- sf::st_intersects(all_geometries, tmp_sf)

      ## We only need the geometries from that this source that intersect
      intersection_indices <- sapply(intersection_matrix, length) > 0
      all_geometries <- all_geometries[intersection_indices, ]
      intersection_matrix <- intersection_matrix[intersection_indices]

      ## Now we find containment relationships

      for(intersection in 1:length(intersection_matrix)){
        index <- intersection_indices[intersection]
        this_intersection <- st_intersectiion(
          all_geometries$geometry[index],
          tmp_sf$geometry[intersection_matrix[intersection] ]
        )

        this_area <- st_area(this_intersection)
        container_area <- st_area(all_geometries$geometry[index])
        contained_area <- st_area(
          tmp_sf$geometry[intersection_matrix[intersection] ]
        )
        ## Also set depth

        ## If containment, we set the source.
        area_difference_1 <-
          (this_area - contained_area) / contained_area
        partial_i <- which(as.numeric(area_difference_1) < match_threshold)
        if(length(partial_i) > 0){
          tmp_sf$source[
            (intersection_matrix[intersection])[exact_i]
          ] <- all_geometries$name[index]
        }

        ## If we have an exact match, then mark for merger
        area_difference_2 <-
          (this_area - container_area) / container_area
        exact_i <- which(as.numeric(area_difference_2) < match_threshold)
        if(length(exact_i) > 0){
          tmp_sf$exact_match[
            (intersection_matrix[intersection])[exact_i]
          ] <- TRUE
        }
      }
    }
    ## Anything that hasn't been updated by now didn't have a match
    ## Source stays how it was before.


  }
  ## Every location should have a source and a depth now.
  ## Create a new location/locat

      metadata_frame <- as.data.frame(sf_object)
      metadata_frame <- metadata_frame[, colnames(metadata_frame) != "geometry"]
      metadata_frame <-
        metadata_frame[, !grepl("NAME", colnames(metadata_frame))]
      metadata_frame <-
        metadata_frame[, !(
          colnames(metadata_frame) %in% c("VALIDFR", "VALIDTO")
        )]
      descendent_id <- database_add_descendent(
        dbname = dbname,
        metadata = metadata_frame,
        standardized_name = NULL,
        readable_descendent_name = sf_object$ISO
      )
      for(alias_idx in
        c(
          which(colnames(sf_object) == 'ISO'),
          which(grepl("NAME", colnames(sf_object)))
        )
      ){
        alias <- sf_object[[alias_idx]][1]
        if(grepl("^\n \r$", alias)){
          next
        }
        location_id <- descendent_id
        tryCatch({
          database_add_location_alias(
            dbname = dbname,
            location_id = descendent_id,
            alias = alias
          )
        },
        error = function(e){
          if(!(
            grepl("UNIQUE constraint failed", e$message)
          )){
            stop(paste(
              "The only way creating an alias should fail is",
              "if the alias is already in the database,",
              "but it failed with message:",
              e$message
            ))
          }
        })
      }
      tryCatch({
        geometry <- sf_object$geometry
        time_left <- "1800-01-01"
        time_right <- "2020-01-01"
        database_add_location_geometry(
          location_id = location_id,
          time_left = time_left,
          time_right = time_right,
          geometry = geometry,
          dbname = dbname
        )
      },
      error = function(e){
      },
      silent = T)
      unlink(destination)

  for(msg in error_messages){
    message(msg)
  }
}

find_geometry_source <- function(
  sf_object,
  source = NULL,
  match_threshold = 1e-6
){

  error_messages <- c('')

  sf_object$depth <- NA
  sf_object$source <- as.character(NA)
  sf_object$exact_match <- FALSE
  sf_object$finished <- FALSE
  sf_object$source_updated <- FALSE

  while(!all(sf_object$finished)){
    sf_object$source_updated <- FALSE

    for(source in unique(sf_object$source)){

      if(is.na(source)){
        tmp_sf <- sf_object[is.na(sf_object$source),]
        source <- NULL
        stop("This case is not yet working")
      } else {
        tmp_sf <- sf_object[sf_object$source == source,]
      }

      all_geometries <- get_location_geometry(
        source = source,
        depth = 1,
        strict_scope = TRUE
      )
      intersection_matrix <- sf::st_intersects(all_geometries, tmp_sf)

      ## We only need the geometries from that this source that intersect
      intersection_indices <- sapply(intersection_matrix, length) > 0
      all_geometries <- all_geometries[intersection_indices, ]
      intersection_matrix <- intersection_matrix[intersection_indices]

      ## Now we find containment relationships

      for(intersection in 1:length(intersection_matrix)){
        index <- intersection_indices[intersection]
        this_intersection <- st_intersectiion(
          all_geometries$geometry[index],
          tmp_sf$geometry[intersection_matrix[intersection] ]
        )

        this_area <- st_area(this_intersection)
        container_area <- st_area(all_geometries$geometry[index])
        contained_area <- st_area(
          tmp_sf$geometry[intersection_matrix[intersection] ]
        )
        ## Also set depth

        ## If containment, we set the source.
        area_difference_1 <-
          (this_area - contained_area) / contained_area
        partial_i <- which(as.numeric(area_difference_1) < match_threshold)
        if(length(partial_i) > 0){
          tmp_sf$source[
            (intersection_matrix[intersection])[exact_i]
          ] <- all_geometries$name[index]
        }

        ## If we have an exact match, then mark for merger
        area_difference_2 <-
          (this_area - container_area) / container_area
        exact_i <- which(as.numeric(area_difference_2) < match_threshold)
        if(length(exact_i) > 0){
          tmp_sf$exact_match[
            (intersection_matrix[intersection])[exact_i]
          ] <- TRUE
        }
      }
    }
  }

  for(msg in error_messages){
    message(msg)
  }
}
