#' @include string_manipulation.R

#' @name load_country_aliases
#' @title load_country_aliases
#' @description create entries for countries and their aliases in a database
#' @param dbname The name of the database.
#' @export
load_country_aliases <- function(dbname = default_database_filename()){
  filename <- system.file(
    "extdata",
    "alt_country_names.csv",
    package = "globaltoolbox"
  )
  countries <- read.csv(filename, stringsAsFactors = FALSE)[, -1]
  countries <- countries[,c(2:ncol(countries), 1)]
  for (row in 1:nrow(countries)){
    cat(paste0("\r",row,'/',nrow(countries)))
    non_na_column_found <- FALSE
    id <- NA
    ## Check to see if the country is in the database:
    country_names <- countries[row, ]
    country_names <- country_names[!is.na(country_names)]
    country_names <- country_names[country_names != ""]
    country_names <- unique(unlist(setNames(
      standardize_string(country_names),
      country_names)))
    # country_names <- database_standardize_name(country_names, dbname = dbname)
    # if(any(!is.na(country_names))){
    #   country_names <- country_names[!is.na(country_names)]
    #   stop(paste(names(country_names), "matches", country_names))
    # }
    # country_names <- names(country_names)
    for (name in country_names){
      if (non_na_column_found){
        database_add_location_alias(location_id = id, alias = name, dbname = dbname)
      } else {
        found <- FALSE
        suppressWarnings(try({
          tmp_name <- database_standardize_name(name, "", TRUE, TRUE, 1, dbname)
          found <- TRUE
        },
        silent = TRUE
        ))
        if(found){
          stop(paste(name,"matches",tmp_name))
        }
        id <- database_add_descendant(
          readable_descendant_name = name,
          standardized_parent_name = "",
            metadata = list(
              import_type = "load_country_aliases",
              level = 0,
              source_name = "globaltoolbox"
            ),
          dbname = dbname
        )
        non_na_column_found <- TRUE
      }
    }
  }
  cat('\n')

  filename <- system.file(
    "extdata",
    "iso31662_codes.csv",
    package = "globaltoolbox"
  )
  countries <- read.csv(filename, stringsAsFactors = FALSE)
  countries <- countries[,c("ISO3", "Name", "Code")]
  countries$country_name <- telescoping_standardize(countries$ISO3,dbname=dbname,max_jump_depth=1)
  for(i in 1:nrow(countries)){
    cat(paste0("\r",i,'/',nrow(countries)))
    this_loc = countries[i,]
    found <- FALSE
    suppressWarnings(try({
      tmp_name <- database_standardize_name(standardize_string(this_loc$Name)[[1]], this_loc$country_name, TRUE, TRUE, 1, dbname)
      found <- TRUE
    },
    silent = TRUE
    ))
    if(found){
      id <- get_database_id_from_name(tmp_name,dbname=dbname)
      message(paste(this_loc$Name,"matches",tmp_name))
    } else {
      id <- database_add_descendant(
        readable_descendant_name = this_loc$Name,
        standardized_parent_name = this_loc$country_name,
          metadata = list(
            import_type = "load_country_aliases",
            level = 0,
            source_name = "globaltoolbox"
          ),
        dbname = dbname
      )
    }
    database_add_location_alias(location_id = id, alias = this_loc$Code, dbname = dbname)
  }
  cat('\n')
}



#' @name standardize_gadm_lhs_time
#' @title standardize_gadm_lhs_time
#' @description Turn gadm start times into Date objects
#' @param x Time to standardize.
#' @return Standardized time.
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
#' @description Turn gadm end times into Date objects
#' @param x Time to standardize.
#' @return Standardized time.
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



#' @name load_hierarchical_sf
#' @title load_hierarchical_sf
#' @description Build database of locations from an sf object with built in hierarchy
#' @param filename File to pull locations from
#' @param time_left First time the shapefiles are valid from
#' @param time_right Last time the shapefiles are valid until
#' @param hierarchy_column_names character vector containing the names of columns which have the location names (order determines hierarchy).
#' @param alias_column_names list of character vectors containing the names of columns which contain aliases for each level (in order by hierarchy).
#' @param source_name An identifier for this source
#' @param dbname Name of the database to load into. Defaults to a standard location.
#' @export
load_hierarchical_sf <- function(
  filename,
  time_left,
  time_right,
  hierarchy_column_names,
  alias_column_names,
  source_name,
  max_depth = NA,
  geometry = TRUE,
  geometry_union = FALSE,
  log_file = as.character(NA),
  dbname = default_database_filename()
){
  error_messages <- c("")
  shp_files <- sf::st_read(filename, stringsAsFactors = FALSE)
  n_levels <- min(max_depth, length(hierarchy_column_names), na.rm = TRUE)
  if(!is.na(log_file)){
    message(paste("Logging to file",log_file))
  }
  for (level in 1:n_levels){
    alias_columns <- alias_column_names[[level]]
    alias_columns <- alias_columns[alias_columns %in% names(shp_files)]
    all_names <- tibble::as_tibble(shp_files)
    all_aliases <- all_names[, c(
      hierarchy_column_names[1:level],
      alias_columns
    )]
    all_names <- all_names[, hierarchy_column_names[1:level] ]
    all_string_names <- apply(all_names, 1, paste, collapse = "::")
    unique_names <- unique(all_names)
    if (nrow(unique(all_aliases)) == nrow(unique_names)){
      unique_aliases <- unique(all_aliases)[, alias_columns]
    } else {
      unique_aliases <- summarize(
        group_by(
          unique(all_aliases),
          !!!rlang::syms(hierarchy_column_names[1:level])
        ),
        !!alias_columns := paste(!!!rlang::syms(alias_columns), collapse = "|")
      )[, alias_columns]
    }
    unique_string_names <- apply(unique_names, 1, paste, collapse = "::")
    last_level <- level - 1
    shp_name <- unique_names[[level]]
    shp_name <- standardize_location_strings(shp_name)
    missing_idx <- is.na(shp_name) |
        (shp_name == "administrativeunitnotavailable") |
        (shp_name == "nameunknown") |
        (shp_name == "")
    unique_names <- unique_names[!missing_idx, ]
    unique_aliases <- unique_aliases[!missing_idx, ]
    shp_name <- shp_name[!missing_idx]
    if(length(shp_name) == 0){
      next
    }
    shp_source <- rep("", nrow(unique_names))
    if (last_level > 0){
      shp_source <- unique_names[, 1:last_level]
      shp_source <- apply(shp_source, 1, paste, collapse = "::")
      shp_source <- telescoping_standardize(shp_source, dbname = dbname)
    }
    for (i in 1:nrow(unique_names)){
      cat(paste0("\rlevel ", level, ": ", i, "/", nrow(unique_names)))
      id <- NA
      tmp_name <- NA
      suppressWarnings(try({
        tmp_name <- database_standardize_name(shp_name[[i]], source = shp_source[[i]], dbname = dbname)
      },
      silent = T
      ))
      ## only check aliases if you couldn't find the original name
      if(is.na(tmp_name)){
        for(alias in unique_aliases[i, ]){

          tmp_tmp_name <- NA
          suppressWarnings(try({
            tmp_tmp_name <- database_standardize_name(alias, source = shp_source[[i]], dbname = dbname)
          },
          silent = T
          ))

          if(!is.na(tmp_tmp_name)){
            ## if no tmp_name, set it now
            if(is.na(tmp_name)){
              tmp_name <- tmp_tmp_name
            } else if(isTRUE(tmp_name != tmp_tmp_name)){
              error_messages <- c(error_messages, paste(paste(shp_source[[i]], shp_name[[i]], sep = "::"),"matches two locations",tmp_name,"and",tmp_tmp_name))
            }
          }
        }
      }
      if(is.na(tmp_name)){
        tmp_name <- paste(shp_source[[i]], shp_name[[i]], sep = "::")
      } else {
        tryCatch({
          id <- globaltoolbox:::get_database_id_from_name(
            tmp_name,
            dbname = dbname
          )
        },
        error = function(e){
          stop(paste("Could not find a database id for",tmp_name,"after standardizing"))
        })
      }
      if(is.na(id)){
        tryCatch({
          id <- database_add_descendant(
            standardized_parent_name = shp_source[[i]],
            readable_descendant_name = shp_name[[i]],
            metadata = list(
              import_type = "load_hierarchical_sf",
              level = level - 1,
              source_name = source_name
            ),
            dbname = dbname
          )
        },
        error = function(e){
          if((!grepl('UNIQUE constraint failed', e$message))){
            error_messages <<- c(
              error_messages,
              paste(
                shp_source[[i]],
                "had an error entering",
                shp_name[[i]],
                "located at",
                i,
                "/",
                nrow(unique_names),
               "with error",
                e$message
              )
            )
          }
        })
      }
      for(alias in unique_aliases[i, ]){
        if(is.na(alias)){next}
        if(alias == ""){next}
        tryCatch({
          database_add_location_alias(
            dbname = dbname,
            location_id = id,
            alias = alias
          )
        },
        error = function(e){
          if(!is.na(id) && (!grepl('UNIQUE constraint failed', e$message))){
            error_messages <<- c(
              error_messages,
              paste(
                shp_source[[i]],
                "had an error entering an alias",
                alias,
                "for",
                paste0(shp_source[[i]], "::", shp_name[[i]]),
                "with error",
                e$message
              )
            )
          }
        })
      }
      if(geometry & (geometry_union | (level == length(hierarchy_column_names)))){
        tryCatch({
            this_geometry <- shp_files[
                all_string_names == unique_string_names[[i]],
                ]$geometry
          if(length(this_geometry) > 1){
            this_geometry <- st_union(this_geometry)
          }
          database_add_location_geometry(
            location_id = id,
            time_left = time_left,
            time_right = time_right,
            geometry = this_geometry,
            dbname = dbname
          )
        },
        error = function(e){
          if(!is.na(id)){
            error_messages <<- c(
              error_messages,
              paste(
                shp_source[[i]],
                "had an error entering geometry for",
                shp_name[[i]],
                "located at",
                i,
                "/",
                nrow(unique_names),
                "with error",
                e$message
              )
            )
          }
        })
      }
    }
    cat("\n")
    if(!is.na(log_file)){
      sink(log_file,append=TRUE)
      for(msg in error_messages){
          print(msg)
      }
      sink()
    }
    for(msg in error_messages){
        message(msg)
    }
    error_messages <<- c()
  }
}


#' @name load_sf
#' @title load_sf
#' @description Add locations and shapefiles to the database from a shapefile.
#' @param sf_object The shapefile to add to the database as an sf object
#' @param sf_object_origin Where you got the shapefile as a character
#' @param name_column Column name of sf_object containing the name of each shapefile
#' @param source A standardized location name containing this shapefile
#' @param alias_columns Any column names of sf_object which contain alternative names for the location
#' @param geometry_columns The column name of sf_object which contains the geometry
#' @param match_threshold How similar two geomtries need to be to be considered identical.  This is a percentage of non-overlapping area.
#' @param dbname Name of the database to load into. Defaults to a standard location.
#' @export
load_sf <- function(
  sf_object,
  sf_object_origin,
  name_column,
  source = '',
  alias_columns = c(),
  metadata_columns = c(),
  geometry_columns = "geometry",
  match_threshold = 1e-6,
  dbname = default_database_filename()
){
  con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname)

  ## Find non overlapping regions by extent


  if(!(source == '')){
    warning("This is not currently implemented")
  }
  add_query <- "INSERT INTO location_hierarchy
    (parent_id, descendant_id,depth)
    VALUES ({parent_id},{descendant_id},{depth})"
  update_query <- "UPDATE OR IGNORE location_hierarchy
    SET
      depth = depth + 1
    WHERE
      descendant_id IN (
      SELECT
        descendant_id
      FROM
        location_hierarchy
      WHERE
        parent_id = {descendant_id} AND
        depth > 0
      )
    AND
      parent_id IN (
      SELECT
        parent_id
      FROM
        location_hierarchy
      WHERE
        descendant_id = {descendant_id} AND
        depth > 0
      )"

  error_messages <- c('')

  sf_object$origin <- sf_object_origin
  if(
    'origin' %in%
      c(metadata_columns, name_column, alias_columns, geometry_columns)
  ){
    stop(
      "origin is a reserved name,",
      "and cannot be a column name.  Please rename"
    )
  }
  metadata_columns <- c(metadata_columns, "origin")

  sources <- find_geometry_source(
    sf_object,
    source,
    match_threshold
  )
  ## create metadata frame
  metadata_frame <- tibble::as_tibble(sf_object)[, metadata_columns]
  for(idx in 1:nrow(sf_object)){
    print(paste(idx, " / ", nrow(sf_object)))
    tmp_sources <- sources[idx, ]

    if(tmp_sources$exact_match){
      stop("This code is not yet implemented")
      warning("This implementation is fragile")

      ## try({
      ## descendant_id <- database_add_descendant(
      ## dbname = dbname,
      ## metadata = metadata_frame[idx,],
      ## standardized_parent_name = tmp_sources$source,
      ## readable_descendant_name = sf_object[[name_column]][idx]
      ## )
      ##
      ## database_merge_locations(
      ## descendant_id,
      ## get_database_id_from_name(sources$source[idx]),
      ## dbname=dbname
      ## )})
    } else {

      ## create standardized name
      warning("Not using check_aliases because it is currently too sensitive")
      standardized_name <- create_standardized_name(
        dbname = dbname,
        name = tmp_sources$name,
        parent = tmp_sources$source,
        check_aliases = FALSE
      )
        try({
      descendant_id <- database_add_descendant(
        dbname = dbname,
        metadata = metadata_frame[idx,],
        standardized_parent_name = tmp_sources$source,
        readable_descendant_name = sf_object[[name_column]][idx]
      )
      potential_children <- get_location_geometry(source=tmp_sources$source)
      if(nrow(potential_children) == 0){
        sf::st_crs(potential_children) <- sf::st_crs(sf_object)
      }
      partial_children <- sf::st_intersects(
        sf_object[idx,],
        potential_children
      )[[1]]
      potential_children <- potential_children[partial_children,]
      children_intersections <- sf::st_intersection(
        sf_object[idx,],
        potential_children
      )
      ci_area <- sf::st_area(children_intersections)
      cf_area <- sf::st_area(potential_children)
      actual_children_idx <- which(as.numeric(abs(ci_area - cf_area)/cf_area) < match_threshold)
      actual_children <- potential_children[actual_children_idx, ]
      if(nrow(actual_children) > 0){
        all_children <- actual_children$location_id
        ## Add hierarchy to each actual child.
        ## The depth is the depth from the parent to that child - 1
        parent_id <- descendant_id
        descendant_id <- all_children
        depth <- actual_children$depth_from_source

        parent_id = parent_id[[1]]
        tryCatch({
          DBI::dbClearResult(DBI::dbSendQuery(
            con,
            glue::glue_sql(.con = con, add_query)
          ))
          DBI::dbClearResult(DBI::dbSendQuery(
            con,
            glue::glue_sql(.con = con, update_query)
          ))
        },
        error = function(e){
            RSQLite::dbDisconnect(con)
            stop(e$message)
        })

        ## Increase depth of connections from parent(s) of source to child
        ## Increase should be by one
          warning("Not yet connecting children of entered locations")
      }
      })
    }
  }
}



#' @name find_geometry_source
#' @title find_geometry_source
#' @description Get the smallest geometry already in the database which contains sf_object
#' @param sf_object An sf object whose geometry should be contained
#' @param source A standardized location name representing a location known to contain the object
#' @param match_threshold How similar two geomtries need to be to be considered identical.  This is a percentage of non-overlapping area.
#' @param dbname Name of the database to load into. Defaults to a standard location.
#' @export
find_geometry_source <- function(
  sf_object,
  source = "",
  match_threshold = 1e-6,
  dbname = default_dtabase_filename()
){
  error_messages <- c('')

  sf_object$depth <- NA
  sf_object$source <- ""
  sf_object$exact_match <- FALSE
  sf_object$finished <- FALSE
  sf_object$source_updated <- FALSE

  while(!all(sf_object$finished)){
    sf_object$source_updated <- FALSE

    for(source in unique(sf_object$source)){

      tmp_idx <- which(sf_object$source == source)

      all_geometries <- get_location_geometry(
        source = source,
        depth = 1,
        strict_scope = TRUE,
        dbname = dbname
      )

      if(nrow(all_geometries) == 0){
        next
      }
      if(sf::st_crs(all_geometries) != sf::st_crs(sf_object)){
        if(is.na(sf::st_crs(all_geometries))){
          sf::st_crs(all_geometries) <- sf::st_crs(sf_object)
        }
        if(is.na(sf::st_crs(sf_object))){
          sf::st_crs(sf_object) <- sf::st_crs(all_geometries)
        }
      }
      intersection_matrix <-
        sf::st_intersects(all_geometries, sf_object[tmp_idx, ])

      ## We only need the geometries from that this source that intersect
      intersection_indices <- sapply(intersection_matrix, length) > 0
      all_geometries <- all_geometries[intersection_indices, ]
      intersection_matrix <- intersection_matrix[intersection_indices]

      ## Now we find containment relationships

      for(intersection in 1:length(intersection_matrix)){
        index <- intersection_matrix[[intersection]]

        this_intersection <- sf::st_intersection(
          all_geometries$geometry[intersection],
          sf_object[tmp_idx, ]$geometry[index]
        )

        this_area <- sf::st_area(this_intersection)
        container_area <- sf::st_area(all_geometries$geometry[intersection])
        contained_area <- sf::st_area(sf_object[tmp_idx, ]$geometry[index])
        ## Also set depth

        ## If containment, we set the source.
        area_difference_1 <-
          abs(this_area - contained_area) / contained_area
        partial_i <- which(as.numeric(area_difference_1) < match_threshold)
        if(length(partial_i) > 0){
          tmp_source <- sf_object$source
          tmp_source[tmp_idx][(index)[partial_i] ] <- all_geometries$name[intersection]
          sf_object$source <- tmp_source
          sf_object[tmp_idx, ]$source_updated[(index)[partial_i]] <- TRUE
        }

        ## If we have an exact match, then mark for merger
        area_difference_2 <-
          abs(this_area - container_area) / container_area
        exact_i <- which(as.numeric(area_difference_2) < match_threshold)
        if(length(exact_i) > 0){
          sf_object[tmp_idx, ]$exact_match[
          (index)[exact_i]
          ] <- TRUE
        }
      }
    }
    sf_object$finished <- sf_object$finished | (!sf_object$source_updated)
  }

  for(msg in error_messages){
    message(msg)
  }
  return(sf_object)
}



#' @name load_gadm
#' @title load_gadm
#' @description Build database of locations from online GADM data repository
#' @param countries Vector of countries for which to load gadm information into the database.
#' if countries="all", the whole GADM data repository will be loaded; this will take approximately XX hours
#' and produce a database of ~XX Gb.
#' @param max_depth The maximum depth from the country to pull information for.
#' @param dbname Name of the database to load into. Defaults to a standard location.
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
  country_aliases <- utils::read.csv(country_aliases.csv)

  if(isTRUE(is.null(countries))){
    stop("Please include at least one country")
  } else if(countries == "all"){
    ## No need to do anything here
  } else {
    unmatched_countries <- countries[!(countries %in% c(as.character(country_aliases[,1]),as.character(country_aliases[,2])))]
    if(length(unmatched_countries) > 0){
      stop(paste0("Error: No countries matching [", paste(unmatched_countries, collapse = ", "),
                    "] could be identified. Please check spelling or re-specify."))
    }
    country_aliases <- country_aliases[
      (country_aliases[, 1] == countries ) |
        (country_aliases[, 2] == countries ),
    ]

  }

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

    ## Download GADM for the country
    suppressWarnings(try({
      ISO_level <- 0
      website <- paste(
        "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
        ISO_A1,
        "_adm",
        ISO_level,
        ".rds",
        sep = ''
      )
      utils::download.file(website, destination, mode = 'wb')
    },
    silent = T))

    ## if GADM file downloaded correctly process it
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
      descendant_id <- database_add_descendant(
        dbname = dbname,
        metadata = metadata_frame,
        standardized_parent_name = "",
        readable_descendant_name = country_data$ISO
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
        location_id <- descendant_id
        tryCatch({
          database_add_location_alias(
            dbname = dbname,
            location_id = descendant_id,
            alias = alias
          )
        },
        error = function(e){
          if(!(
            grepl("duplicate key value violates unique constraint", e$message)
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
      if(length(error_messages) > 10){
        warning("Over 10 errors")
      } else if(length(error_messages) > 100){
        warning("Over 100 errors")
      } else if(length(error_messages) > 1000){
        warning("Over 1,000 errors")
      } else if(length(error_messages) > 10000){
        warning("Over 10,000 errors")
      } else if(length(error_messages) > 100000){
        warning("Over 100,000 errors")
      }
      destination <- tempfile(fileext = ".rds")
      suppressWarnings(try({
        ISO_level <- ISO_level + 1
        website <- paste(
          "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
          ISO_A1,
          "_adm",
          ISO_level,
          ".rds",
          sep = ''
        )
        utils::download.file(website, destination, mode = 'wb')
      },
      silent = T))
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
            descendant_id <- database_add_descendant(
              dbname = dbname,
              metadata = metadata_frame,
              standardized_parent_name = tmp_data$standardized_parent_name,
              readable_descendant_name =
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
              location_id <- descendant_id
              tryCatch({
                database_add_location_alias(
                  dbname = dbname,
                  location_id = descendant_id,
                  alias = this_alias
                )
              },
              error = function(e){
                if(!(
                  grepl("duplicate key value violates unique constraint", e$message)
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
          }
          tryCatch({
            geometry <- tmp_data$geometry
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
        }
        unlink(destination)
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
