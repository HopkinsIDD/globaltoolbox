#' @export
group_location_sf <- function(location_sf){
  #' @import sf
  library(sf)
  # valid_locations = st_is_valid(location_sf)
  # if(!all(valid_locations)){
  #   location_sf$geometry[!valid_locations] = st_make_valid(location_sf$geometry[!valid_locations])
  # }
  if(any(location_sf$thr_idx > 1)){
    location_list = split(location_sf,as.factor(paste(location_sf$arg_idx,location_sf$pipe_idx)))
    # location_list = head(location_list)
    for(location_idx in 1:length(location_list)){
      if(nrow(location_list[[location_idx]]) > 1){
        orig_tmp_location_sf = location_list[[location_idx]]
        tmp_location_sf = orig_tmp_location_sf
        print(paste(location_idx,"/",length(location_list)))
        tmp_area = min(st_area(tmp_location_sf))
        tmp_shapefile_source = paste("'",paste(arrange(tmp_location_sf,-as.numeric(ISO_A2_level))$shapefile_source,collapse="'+'"),"'",sep='')
        tmp_location_sf = arrange(tmp_location_sf,-as.numeric(ISO_A2_level))
        tmp_location_sf = st_intersection(tmp_location_sf[1,],tmp_location_sf[-1,])
        tmp_location_sf = tmp_location_sf[,!grepl('.1$',names(tmp_location_sf))]
        if(nrow(tmp_location_sf) > 1){
          tmp_location_sf$shapefile_source = tmp_shapefile_source
          if(any(st_area(tmp_location_sf) < .9*tmp_area)){
            tmp_location_sf$geometry = st_sfc(st_point(c(NaN,NaN)))
          }
          tmp_location_sf = tmp_location_sf[1,]
        } else {
          tmp_location_sf = orig_tmp_location_sf
          tmp_location_sf = arrange(tmp_location_sf,-as.numeric(ISO_A2_level))
          tmp_location_sf = tmp_location_sf[1,]
          tmp_location_sf$geometry = st_sfc(st_point(c(NaN,NaN)))
          tmp_location_sf$shapefile_source = tmp_shapefile_source
        }
        if(nrow(tmp_location_sf) > 1){
          stop("This should not happen")
        }
        location_list[[location_idx]] = tmp_location_sf
      }
    }
    for(location_idx in 1:length(location_list)){
      if(nrow(location_list[[location_idx]]) > 1){
        x = location_list[[location_idx]]
        tmp_x = x %>% arrange(-as.numeric(ISO_A2_level)) %>% st_intersection() %>% filter(n.overlaps == nrow(x))
        tmp_x$shapefile_source = paste("'",paste(arrange(x,-as.numeric(ISO_A2_level))$shapefile_source,collapse="'+'"),"'",sep='')
        if(st_area(tmp_x) < .9 * min(st_area(x))){
          tmp_x$geometry = st_sfc(st_point(c(NaN,NaN)))
        }
        location_list[[location_idx]] = tmp_x
      }
    }
    location_sf = reduce_sf_vector(location_list)
  }
  if(any(location_sf$pipe_idx > 1)){
    #' @importFrom dplyr filter
    location_sf_piped = location_sf %>% filter(pipe_idx > 1)
    #' @importFrom dplyr filter
    location_sf_piped = location_sf %>% filter(arg_idx %in% unique(location_sf_piped$arg_idx))
    #' @importFrom dplyr filter
    location_sf_unpiped = location_sf %>% filter(!(arg_idx %in% unique(location_sf_piped$arg_idx)))
    piped_list = location_sf_piped %>%
      split(as.factor(location_sf_piped$arg_idx)) %>%
      lapply(function(x){
        if(nrow(x) > 1){
          if(any(st_dimension(x) < 2)){
            x$geometry = st_sfc(st_point(c(NaN,NaN)))
          }
          tmp_x = x %>%
            arrange(-as.numeric(ISO_A2_level)) %>%
            st_union()
          tmp2_x = as.data.frame(x) %>%
            select(-geometry) %>%
            summarize_all(funs(if(length(unique(.))==1){unique(.)} else {paste(.,collapse='|')})) %>%
            cbind(.,tmp_x) %>%
            st_as_sf()
          x = tmp2_x
        }
        return(x)
      })
    location_sf_piped = reduce_sf_vector(piped_list)
    if(is.na(st_crs(location_sf_piped))){
      st_crs(location_sf_piped) = st_crs(location_sf_unpiped)
    }
    #' @importFrom dplyr arrange
    location_sf = rbind(location_sf_unpiped,location_sf_piped)
  }
  return(location_sf)
}

#' @export
create_location_sf <- function(location_name,thorough=FALSE){
  original_location_name = location_name
  location_name = standardize_locations(location_name)
  location_array = strsplit(location_name,split='::')
  if(thorough){
    location_array = lapply(location_array,function(x){lapply(1:length(x),function(i){x[1:i]})})
  } else {
    location_array = lapply(location_array,function(x){list(x)})
  }
  location_array = lapply(location_array,function(y){lapply(y,function(x){strsplit(x,split='|',fixed=TRUE)})})

  ## We currently have the following list levels
  ## 1 Location in args list
  ### 2 If thorough all shapefiles that matter
  #### 3 ISO_level
  ##### 4 location_within that arg
  
  ## Build an array to store things in for easier access
  old_location_array = location_array
  nargs = length(old_location_array)
  nthr = max(sapply(old_location_array,length))
  niso = max(sapply(old_location_array,function(x){max(sapply(x,length))}))
  npipe = max(sapply(old_location_array,function(x){max(sapply(x,function(x){max(sapply(x,length))}))}))
  location_array = array(as.character(NA),c(niso,nargs,npipe,nthr))
  
  ## Populate the array
  for(arg_idx in 1:nargs){
    ## Calculate the number of pipes for each ISO region for this arg.
    this_pipe_lengths = lapply(old_location_array[[arg_idx]],function(y){sapply(y,length)})
    ## We are assuming that the length is the single non-1 value of this_pipe_lengths
    thr_depth = length(old_location_array[[arg_idx]])
    pipe_length = unique(this_pipe_lengths[[thr_depth]])
    pipe_length = pipe_length[pipe_length > 1]
    ## The length might actually be 1...
    if(length(pipe_length) == 0){
      pipe_length = 1
    }
    ## Checking just to be sure
    if(length(pipe_length) > 1){
      ## This function is meant to not fail easily, so the pipe_length is set to 0 to indicate a failed state
      pipe_length = NA
    }
    ISO_depth = sapply(old_location_array[[arg_idx]],length)
    if(!is.na(pipe_length)){
      for(pipe_idx in 1:pipe_length){
        for(thr_idx in 1:thr_depth){
          for(ISO_idx in 1:ISO_depth[thr_idx]){
            if(pipe_idx <= this_pipe_lengths[[thr_idx]][ISO_idx]){
              location_array[ISO_idx, arg_idx, pipe_idx,thr_idx] <- old_location_array[[arg_idx]][[thr_idx]][[ISO_idx]][[pipe_idx]]
            } else {
              location_array[ISO_idx, arg_idx, pipe_idx,thr_idx] <- old_location_array[[arg_idx]][[thr_idx]][[ISO_idx]][[1]]
            }
          }
        }
      }
    }
  }
  ## We will use this for finding the shapefiles.  They will eventually be part of this object
  #' @importFrom dplyr filter
  #' @importFrom reshape2 melt
  ungrouped_shapefiles <- dplyr::filter(melt(location_array),!is.na(value))
  ungrouped_shapefiles$value = as.character(ungrouped_shapefiles$value)
  names(ungrouped_shapefiles)[1:4] <- c('ISO_level','arg_idx','pipe_idx','thr_idx')
  ungrouped_shapefiles <- ungrouped_shapefiles %>%
    group_by(arg_idx,pipe_idx,thr_idx) %>%
    do({
      tmp = .
      tmp$ISO_A2_level = max(tmp$ISO_level)
      tmp
    })
  
  #' @importFrom tidyr spread
  ungrouped_shapefiles <- spread(ungrouped_shapefiles,key=ISO_level,value=value,drop=T)
  names(ungrouped_shapefiles)[5] <- 'ISO_A1'
  if(ncol(ungrouped_shapefiles) > 5){
      names(ungrouped_shapefiles)[6:ncol(ungrouped_shapefiles)] <- paste('ISO_A2_L',as.numeric(names(ungrouped_shapefiles)[6:ncol(ungrouped_shapefiles)]),sep='')
  }
  if(thorough){
    ungrouped_shapefiles = ungrouped_shapefiles %>% filter(thr_idx > 0)
  }
  ungrouped_shapefiles$location_name = paste0(apply(ungrouped_shapefiles[,5:ncol(ungrouped_shapefiles)],1,function(x){paste(na.omit(x),collapse='::')}))
  # ungrouped_shapefiles$location_full = location_name[ungrouped_shapefiles$arg_idx]
  ungrouped_shapefiles$idx = 1:nrow(ungrouped_shapefiles)
  ungrouped_shapefiles$location = original_location_name[ungrouped_shapefiles$arg_idx]
  ungrouped_shapefiles$source = gsub(':?:?[^:]*$','',ungrouped_shapefiles$location_name)
  ungrouped_shapefiles$source[ungrouped_shapefiles$source == ''] = NA
  return(ungrouped_shapefiles)
}

#' @export
telescoping_standardize <- function(location_name){
  location_name = standardize_location_strings(location_name)
  location_sf = create_location_sf(location_name,thorough=TRUE)
  all_names = list()
  for(level in sort(unique(location_sf$ISO_A2_level))){
    tmp_location_sf = dplyr::filter(location_sf,ISO_A2_level==level)
    counter = 1
    tmp_location_sf$standardized_source = as.character(NA)
    while((level > counter) & (any(is.na(tmp_location_sf$standardized_source)))){
      tmp_location_sf$standardized_source[is.na(tmp_location_sf$standardized_source)] =
        all_names[[level-counter]][tmp_location_sf$source[is.na(tmp_location_sf$standardized_source)]]
      counter = counter + 1
    }
    for(scope in unique(tmp_location_sf$standardized_source)){
      if(is.na(scope)){
        nonstandard_names = unique(tmp_location_sf$location_name[is.na(tmp_location_sf$standardized_source)])
        standard_names = standardize_name(gsub('.*:','',nonstandard_names),scope=NULL)
     } else {
        nonstandard_names = unique(tmp_location_sf$location_name[
          (!is.na(tmp_location_sf$standardized_source)) &
          (tmp_location_sf$standardized_source == scope)
        ])
        standard_names = standardize_name(gsub('.*:','',nonstandard_names),scope=scope)
      }
      names = setNames(standard_names,nonstandard_names)
      if(level <= length(all_names)){
        all_names[[level]] = c(all_names[[level]],names)
      } else {
        all_names[[level]] = names
      }
    }
  }
  all_names = unlist(all_names)
  location_sf$standardized_name = all_names[location_sf$location_name]
  location_sf = group_by(location_sf,arg_idx,pipe_idx)
  location_sf = summarize(location_sf,standardized_name=ifelse(any(is.na(standardized_name)),as.character(NA),standardized_name[which.max(thr_idx)]))
  location_sf = ungroup(location_sf)
  location_sf = group_by(location_sf,arg_idx)
  location_sf = summarize(location_sf,standardized_name=ifelse(any(is.na(standardized_name)),as.character(NA),paste(standardized_name,collapse='|')))
  location_sf = ungroup(location_sf)
  return(location_sf$standardized_name)
}

get_shape_file <- function(location_name,taxonomy_dir = 'taxonomy-verified', verbose=FALSE,method='name',date=now(),thorough = TRUE){

  location_sf = create_location_sf(location_name,thorough=TRUE)
  all_names = list()
  for(level in sort(unique(location_sf$ISO_A2_level))){
    tmp_location_sf = dplyr::filter(location_sf,ISO_A2_level==level)
    all_names[[level]] = unique(tmp_location_sf$location_name)
    standardize_name(all_names[[level]])
  }
  
  
  ungrouped_shapefiles <- create_location_sf(location_name,date,thorough)
  ungrouped_shapefiles <- left_join(ungrouped_shapefiles,all_location_files,by='location_name')
  
  max_ISO_A2_level = max(ungrouped_shapefiles$ISO_A2_level)
  if(is.na(max_ISO_A2_level)){
    stop("Something went wrong, please contact package maintainer")
  }
  
  
  ## This section is not working
  ## This section populates the sections of the 
  tmp_shapefiles <- dplyr::filter(ungrouped_shapefiles,ISO_A2_level < 0)
  if(nrow(tmp_shapefiles) > 0){
    unique_who_regions = unique(tmp_shapefiles$who_region)
    unique_who_regions = unique_who_regions[!is.na(unique_who_regions)]
    #' @importFrom sf st_point
    ungrouped_shapefiles$geometry[tmp_shapefiles$idx] = st_sfc(st_point(c(NaN,NaN)))
    ## Get WHO region shapefiles here...
    warning(paste("We do not yet support getting shape files for WHO regions.  The following locations were affected:",paste(unique_who_regions,collapse=',')))
  }
  
  ## If we only have WHO_region level data we are finished.
  if(max_ISO_A2_level <= -1){
    return(group_location_sf(ungrouped_shapefiles))
  }
  
  ## Check the location files:
  location_files = unique(ungrouped_shapefiles$location_file)
  ## Warn if any locations are missing.
  if(method=='center'){
    if(any(is.na(location_files))){
      warning(paste(
        "Could not find location files for the following locations",
        paste(unique(ungrouped_shapefiles$location_name[
          is.na(ungrouped_shapefiles$location_file)
          ]),collapse=', ')
      ))
    }
    location_files = location_files[!is.na(location_files)]
  }
  
  for(location in unique(location_files)){
    if((method =='name') & (is.na(location))){
      idx = which(is.na(ungrouped_shapefiles$location_file))
    } else {
      idx = which(ungrouped_shapefiles$location_file == location)
    }
    if(length(idx) == 0){
      stop("This should never happen.")
    }
    if((method =='name') & is.na(location)){
      loc_file = list()
    } else {
      loc_file = read_location_csv(
        paste(taxonomy_dir,'Location',location,sep='/')
      )
      for(this_idx in idx){
        if('cent_lat' %in% names(loc_file)){
          ungrouped_shapefiles$geometry[[this_idx]][[2]] = loc_file$cent_lat
        }
        if('cent_lat' %in% names(loc_file)){
          ungrouped_shapefiles$geometry[[this_idx]][[1]] = loc_file$cent_lon
        }
      }
      # Update bounding box
      ungrouped_shapefiles = st_update_bounding_box(ungrouped_shapefiles)
    }
    if(
      (!('gis_file' %in% names(loc_file)))
    ){
      if(verbose){
        # warning("Not including a gis_file field is depricated for location ",location)
      }
      ## Using GADM shapefiles
      ungrouped_shapefiles$shapefile_source[idx] = paste0(
        'GADM/',
        fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
        '_adm',
        (ungrouped_shapefiles$ISO_A2_level[idx]),
        '.rds'
      )
    } else {
      if(is.na(loc_file$gis_file) | (loc_file$gis_file == 'GADM')){
        ## Still using GADM shapefiles
        ungrouped_shapefiles$shapefile_source[idx] = paste0(
          'GADM/',
          fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
          '_adm',
          (ungrouped_shapefiles$ISO_A2_level[idx]),
          '.rds'
        )
      } else {
        ## Using custom files
        ## Need to check years:
        nshapefile = sum(grepl('gis_file',names(loc_file)))
        if(nshapefile > 1){
          for(shape_idx in 1:nshapefile){
            this_source = loc_file[[paste('gis_file',shape_idx,sep='::')]]
            if(shape_idx == 1){
              this_source = loc_file[['gis_file']]
            }
            #' @importFrom lubridate ymd
            this_start = ymd(loc_file[[paste('gis_start',shape_idx,sep='::')]])
            if(is.na(this_start)){
              #' @importFrom lubridate ymd
              this_start = ymd("0000-01-01")
            }
            #' @importFrom lubridate ymd
            this_end = ymd(loc_file[[paste('gis_end',shape_idx,sep='::')]])
            if(is.na(this_end)){
              this_end = lubridate::now()
            }
            this_idx = idx[(
              (this_start < ungrouped_shapefiles$date[idx]) &
                (this_end > ungrouped_shapefiles$date[idx])
            )]
            if(length(this_idx) > 0){
              ungrouped_shapefiles$shapefile_source[this_idx] = this_source
            }
          }
          if(any(is.na(ungrouped_shapefiles$shapefile_source[idx]))){
            this_idx = idx[which(is.na(
              ungrouped_shapefiles$shapefile_source[idx]
            ))]
            ungrouped_shapefiles$shapefile_source[this_idx] <- paste0(
              'GADM/',
              fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
              '_adm',
              (ungrouped_shapefiles$ISO_A2_level[idx]),
              '.rds'
            )
          }
        } else {
          if((length(loc_file) > 0) & isTRUE(!(is.na(loc_file$gis_file)))){
            ungrouped_shapefiles$shapefile_source[idx] = loc_file$gis_file
          } else {
            ungrouped_shapefiles$shapefile_source[idx] <- paste0(
              'GADM/',
              fix_country_name(ungrouped_shapefiles$ISO_A1[idx]),
              '_adm',
              (ungrouped_shapefiles$ISO_A2_level[idx]),
              '.rds'
            )
          }
        }
      }
    }
  }
  
  ## Check the shape files
  missing_shapefiles = unique(ungrouped_shapefiles$shapefile_source[
    !file.exists(paste(
      taxonomy_dir,
      'ShapeFiles',
      ungrouped_shapefiles$shapefile_source,
      sep='/'
    ))])
  missing_gadm_links = substr(
    missing_shapefiles,
    start=6,
    stop=nchar(missing_shapefiles)
  )
  for(shapefile_idx in which(startsWith(missing_shapefiles,'GADM'))){
    destination = paste(
      taxonomy_dir,
      'ShapeFiles',
      missing_shapefiles[shapefile_idx],
      sep='/'
    )
    website = paste(
      "http://biogeo.ucdavis.edu/data/gadm2.8/rds/",
      missing_gadm_links[shapefile_idx],
      sep=''
    )
    tryCatch({
      download.file(website,destination,mode='wb')
      #' @importFrom sf st_as_sf
      tmp_shapefile = st_as_sf(readRDS(destination))
    },
    error = function(e){
      warning("Could not download shape file",missing_shapefiles[idx])
      if(file.exists(destination)){
        file.remove(destination)
      }
    })
  }
  missing_shapefiles = unique(ungrouped_shapefiles$shapefile_source[
    !file.exists(paste(
      taxonomy_dir,
      'ShapeFiles',
      ungrouped_shapefiles$shapefile_source,
      sep='/'
    ))])
  missing_shapefiles = missing_shapefiles[!is.na(missing_shapefiles)]
  if(length(missing_shapefiles) > 0){
    warning(
      "Missing the following shapefiles ",
      paste(missing_shapefiles,collapse=', ')
    )
  }
  #' @importFrom sf st_as_sf
  ungrouped_shapefiles <- st_as_sf(ungrouped_shapefiles)
  
  invalid_idx = which(!st_is_valid(ungrouped_shapefiles))
  for(shp_src in unique(ungrouped_shapefiles$shapefile_source)){
    if(shp_src %in% missing_shapefiles){next;}
    if(is.na(shp_src)){next;}
    idx = which(ungrouped_shapefiles$shapefile_source == shp_src)
    idx = idx[!(idx %in% invalid_idx)]
    if(length(idx) == 0){next;}
    file = paste(taxonomy_dir,'ShapeFiles',shp_src,sep='/')
    file_ending = substr(file,nchar(file)- 2,nchar(file))
    if(file_ending == 'rds'){
      #' @importFrom sf st_as_sf
      shapefile = st_as_sf(readRDS(file))
    } else {
      #' @importFrom sf st_read
      shapefile = st_read(file,quiet=TRUE)
    }
    if(method == 'center'){
      shapefile = shapefile$geometry
    }
      #' @importFrom sf st_crs
      if(is.na(st_crs(ungrouped_shapefiles))){
        #' @importFrom sf st_crs st_crs<-
        st_crs(ungrouped_shapefiles) = st_crs(shapefile)
      } else if(is.na(st_crs(shapefile))) {
        warning("Shapefile with no crs")
        st_crs(shapefile) = st_crs(ungrouped_shapefiles)
      }else {
        #' @importFrom sf st_crs st_transform
        shapefile = st_transform(shapefile,st_crs(ungrouped_shapefiles))
      }
    if(method == 'center'){
      #' @importFrom sf st_intersects
      intersections = st_intersects(ungrouped_shapefiles[idx,],shapefile)
      tmp_idx = idx[sapply(intersections,length) != 1]
      if(length(tmp_idx) > 0 ){
        warning(paste("This should not happen.  A shapefile was found, but did not intersect the point given in the location file for location(s)", paste(ungrouped_shapefiles$location[tmp_idx], collapse = ', ')))
      }
      ungrouped_shapefiles$geometry[tmp_idx] = st_point()
      tmp_idx = idx[sapply(intersections,length) == 1]
      ungrouped_shapefiles$geometry[tmp_idx] = shapefile[unlist(intersections[1:length(tmp_idx)]) ]
    } else if(method == 'name'){
      ISO_level = unique(ungrouped_shapefiles$ISO_A2_level[idx])
      if(length(ISO_level) > 1){stop("Not sure how to deal with a single shapefile source containing multiple ISO levels")}
      ISO_name = paste0("ISO_A2_L",ISO_level)
      if(ISO_level == 0){
        ISO_name = "ISO_A1"
      }
      if(ISO_level < 0){stop("We do not currently support shapefiles for ISO levels < 0")}
      currently_set_to = 1:length(idx) * NA
      field_names = names(shapefile)
      if(ISO_level > 0){
        field_names = names(shapefile)[endsWith(names(shapefile),suffix = as.character(ISO_level))]
      }
      for(field in field_names){
        std_string = standardize_string(shapefile[[field]])
        for(row_idx in 1:nrow(shapefile)){
	  if(any((!is.na(std_string[[row_idx]])) & (ungrouped_shapefiles[[ISO_name]][idx] == std_string[[row_idx]]))){
            tmp_idx = idx[which(ungrouped_shapefiles[[ISO_name]][idx] == std_string[row_idx])]
	    if(any(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx))){
	      tmp_idx2 = tmp_idx[which(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx))]
	      currently_set_to[tmp_idx2] = row_idx
	      ungrouped_shapefiles$geometry[tmp_idx2] = shapefile$geometry[row_idx]
	    }
	    if(any(!(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx)))){
	      tmp_idx2 = tmp_idx[which(!(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx)))]
	      currently_set_to[tmp_idx2] = 0
	      #' @importFrom sf st_point
	      ungrouped_shapefiles$geometry[tmp_idx2] = st_point(1:2* NA)
	    }
	      which(is.na(currently_set_to[tmp_idx]) | (currently_set_to[tmp_idx] == row_idx))
	  }
	}
      }
      
    }
  }
  # Update bounding box
  ungrouped_shapefiles = st_update_bounding_box(ungrouped_shapefiles)
  # ungrouped_shapefiles %>% group_by(shapefile_source) %>% do({tmp = .; tmp$shapefile = st_read(tmp$shapefile_source)$geometry})
###   if(method=='center'){
###     
###   } else {
###     warning("This method is not functional")
###     unique_ISO_A1s = unique(ungrouped_shapefiles$ISO_A1)
###     unique_ISO_A1s = unique_ISO_A1s[!is.na(unique_ISO_A1s)]
###     ## Since the shape files are stored separately by country, we look at the countries separately.
###     for(this_ISO_A1 in unique_ISO_A1s){
###       tmp_shapefiles = dplyr::filter(ungrouped_shapefiles,ISO_A1==this_ISO_A1)
###       unique_ISO_levels = max(unique(tmp_shapefiles$ISO_A2_level)):0
###       
###       ## Loop over each ISO level and check to make sure it matches.  If its the right level then put in the shapefile
###       for(ISO_level in unique_ISO_levels){
###         tmp_shapefiles = dplyr::filter(ungrouped_shapefiles,ISO_A1 == this_ISO_A1,ISO_A2_level >= ISO_level)
###         
###         ISO_val = tmp_shapefiles[[paste('ISO_A2_L',ISO_level,sep='')]]
###         if(ISO_level> 3){
###           if(verbose){
###             message("GADM does not normally have ISO levels this deep")
###           }
###         }
###         destination = paste(taxonomy_dir,'/ShapeFiles/',this_ISO_A1,'_adm',ISO_level,'.rds',sep='')
###         if(!file.exists(destination)){
###           if(verbose){
###             message(paste("Could not find shape file at ISO_A2 level",ISO_level))
###           }
###           #' @importFrom sf st_sf
###           #' @importFrom sf st_sfc
###           all_shape_files = st_sf(st_sfc())
###         } else {
###           #' @importFrom sf st_as_sf
###           all_shape_files = st_as_sf(readRDS(destination))
###           #' @importFrom sf st_is_valid
###           if(!all(st_is_valid(all_shape_files))){
###             #' @importFrom lwgeom st_make_valid
###             ## Switch to tryCatch instead of suppressWarnings here in case there are actual warnings (other than the default for fixing things)
###             all_shape_files <- suppressWarnings(st_make_valid(all_shape_files))
###             saveRDS(object=all_shape_files,file=destination)
###           }
###         }
###         #' @importFrom sf st_is_valid
###         if(!all(st_is_valid(all_shape_files))){
###           file.remove(destination)
###           stop("This should be dealt with by now.")
###         }
###         for(field in names(all_shape_files)[endsWith(names(all_shape_files),suffix = as.character(ISO_level))]){
###           tmp_field = paste(field,'tmp',sep='_')
###           all_shape_files[[tmp_field]] = all_shape_files[[field]]
###           if(mode(field)==mode(character(0))){
###             all_shape_files[[tmp_field]] = standardize_string(all_shape_files[[tmp_field]])
###           }
###         }
###         this_shape_file = all_shape_files
###         if(ISO_level > 0){
###           for(index_idx in 1:nrow(tmp_shapefiles)){
###             match_indices = c()
###             for(field in names(all_shape_files)[endsWith(names(all_shape_files),suffix='tmp')]){
###               match_indices = union(match_indices,which(sapply(all_shape_files[[field]],function(x){any(ISO_val[index_idx] %in% x)})))
###             }
###             matching_ISO_regions = all_shape_files[match_indices,]
###             if(nrow(matching_ISO_regions)==0){
###               #' @importFrom sf st_point
###               ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###             } else {
###               if((tmp_shapefiles$ISO_A2_level[index_idx] == ISO_level) & (nrow(matching_ISO_regions) == 1)){
###                 #' @importFrom sf st_crs st_crs<-
###                 if(is.na(st_crs(ungrouped_shapefiles))){
###                   st_crs(ungrouped_shapefiles) <- st_crs(matching_ISO_regions)
###                 }
###                 ## This is the ISO level to pull the geometry from
###                 ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ] = matching_ISO_regions$geometry
###                 if(st_crs(ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ]) != st_crs(matching_ISO_regions$geometry)){
###                   ## st_transform(ungrouped_shapefiles,st_crs(matching_ISO_regions$geometry))
###                   warning("Lost the crs")
###                 }
###                 ## It matches a real region, just not one containing our point.
###                 #' @importFrom sf st_intersects
###                 if(!st_intersects(
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ],
###                   matching_ISO_regions,
###                   sparse=FALSE
###                 )){
###                   #' @importFrom sf st_point
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###                 }
###               } else {
###                 if(is.na(st_crs(ungrouped_shapefiles))){
###                   st_crs(ungrouped_shapefiles) <- st_crs(matching_ISO_regions)
###                 }
###                 ## Make sure the geometries intersect
###                 #' @importFrom sf st_intersects
###                 if(!all(st_intersects(
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ],
###                   matching_ISO_regions,
###                   sparse=FALSE
###                 ))){
###                   #' @importFrom sf st_point
###                   ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###                 }
###               }
###               if(ISO_level == 1){
###                 ungrouped_shapefiles[[paste("ISO_A2_L",ISO_level,sep='')]][tmp_shapefiles$idx[index_idx]] = gsub('.','-',matching_ISO_regions$HASC_1,fixed=T)
###                 ## ungrouped_shapefiles$WHO_region = lookup_WHO_region(ungrouped_shapefiles$ISO_A1)
###               }
###               if(ISO_level > 1){
###                 ungrouped_shapefiles[[paste("ISO_A2_L",ISO_level,sep='')]][tmp_shapefiles$idx[index_idx]] = matching_ISO_regions[[paste('NAME_',ISO_level,sep='')]]
###               }
###             }
###           }
###         } else {
###           this_country_name = fix_country_name(this_shape_file$NAME_ENGLISH,verbose==FALSE)
###           this_WHO_region = lookup_WHO_region(this_country_name)
###           for(index_idx in 1:nrow(tmp_shapefiles)){
###             if(tmp_shapefiles$ISO_A2_level[index_idx] == ISO_level){
###               ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = this_shape_file$geometry
###             } else {
###               #' @importFrom sf st_crs st_crs<-
###               if(is.na(st_crs(ungrouped_shapefiles))){
###                 st_crs(ungrouped_shapefiles) <- st_crs(this_shape_file)
###               }
###               #' @importFrom sf st_intersects
###               if(!all(st_intersects(
###                 ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx] ],
###                 this_shape_file,
###                 sparse=FALSE
###               ))){
###                 #' @importFrom sf st_point
###                 ungrouped_shapefiles$geometry[tmp_shapefiles$idx[index_idx]] = st_point(c(NaN,NaN))
###               }
###             }
###           }
###           if(!is.na(this_country_name)){
###             ungrouped_shapefiles$ISO_A1[tmp_shapefiles$idx] = this_country_name
###             if(!is.na(this_WHO_region)){
###               ungrouped_shapefiles$who_region[tmp_shapefiles$idx] = this_WHO_region
###             }
###           }
###         }
###         print(paste("ISO level",ISO_level))
###       }
###     }
###   }
  
  return(group_location_sf(ungrouped_shapefiles))
}

#' @export
standardize_locations <- function(location_name){
  location_tmp = location_name
  location_tmp = gsub('|','VERTCHARACTER',location_tmp,fixed=TRUE)
  location_tmp = gsub('-','DASHCHARACTER',location_tmp,fixed=TRUE)
  location_tmp = gsub('::','DOUBLECOLONCHARACTER',location_tmp,fixed=TRUE)
  location_tmp = standardize_string(location_tmp)
  location_tmp = gsub('DOUBLECOLONCHARACTER','::',location_tmp,fixed=TRUE)
  location_tmp = gsub('DASHCHARACTER','-',location_tmp,fixed=TRUE)
  location_tmp = gsub('VERTCHARACTER','|',location_tmp,fixed=TRUE)
  while(any(
    grepl(pattern = '::$',location_tmp) ||
    grepl(pattern = '::NA$',location_tmp) ||
    grepl(pattern = '::::',location_tmp)
    )){
    location_tmp = gsub(':::',':',location_tmp,fixed=TRUE)
    location_tmp = gsub('::NA$','',location_tmp)
    location_tmp = gsub('::$','',location_tmp)
  }
  return(location_tmp)
}

#' @export
ranked_encodings = c('UTF-8','LATIN1')
standardize_string <- function(string){
  # if(is.na(string)){return(NA)}
  string = as.character(string)
  string = strsplit(string,'|',fixed=TRUE)
  string = lapply(string,function(x){gsub(' ','',x)})
  string = lapply(string,function(x){
    if(length(x) == 0){
      return(x)
    }
    for(i in 1:length(x)){
      if(Encoding(x[i]) != 'unknown'){
        x[i] = iconv(from=Encoding(x[i]),to='ASCII//TRANSLIT',x[i])
      } else {
          for(encoding in ranked_encodings){
              if(!is.na(x[i])){next}
              y = iconv(from=encoding,to='ASCII//TRANSLIT',x[i])
              if(!is.na(y)){
                  x[i] = y
              }
          }
      }
    }
    return(x)
  })
  string = lapply(string,function(x){gsub('[[:punct:]]','',x)})
  string = lapply(string,function(x){toupper(x)})
  string[sapply(string,length) == 0] = ''
  return(string)
}
