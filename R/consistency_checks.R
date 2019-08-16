
detect_subtree_match <- function(lhs,rhs,dbname=default_database_filename()){
  lhs <- standardize_name(lhs,dbname=dbname)
  rhs <- standardize_name(rhs,dbname=dbname)

  if(lhs == rhs){
    return(NA)
  }
  
  lhs_source <- gsub('::[^:]*','',lhs)
  rhs_source <- gsub('::[^:]*','',rhs)
  
  if(lhs_source != rhs_source){
    return(NaN)
  }
  
  lhs_sub_name <- gsub(".*::", "", get_location_metadata(source = lhs, aliases = FALSE, depth = 1, dbname = dbname)$name)
  rhs_sub_name <- gsub(".*::", "", get_location_metadata(source = rhs, aliases = FALSE, depth = 1, dbname = dbname)$name)

  # if(mean(c(length(lhs_sub_name),length(rhs_sub_name))) <= 10){
  #   return(NaN)
  # }
  
  lhs_match <- standardize_name(lhs_sub_name, scope = rhs, dbname = dbname)
  rhs_match <- standardize_name(rhs_sub_name, scope = lhs, dbname = dbname)
  percent_match <- mean(c(!is.na(lhs_match), !is.na(rhs_match)))
  
  ## Potentially recur
  
  return(percent_match)
}

detect_all_subtree_matches <- function(source = "", dbname = default_database_filename(),verbose=FALSE){
  all_locations <- get_location_metadata(source = source, aliases = FALSE, depth = 1, dbname = dbname)$name
  matches <- data_frame(
    lhs = character(0),
    rhs = character(0),
    match_rate = numeric(0)
  )
  all_reses <- c()
  for(i in 1:length(all_locations)){
    for(j in (i+1):length(all_locations)){
      if(j > length(all_locations)){
        break
      }
      res <- detect_subtree_match(all_locations[i], all_locations[j],dbname=dbname)
      all_reses <- c(res,all_reses)
      # print(paste(floor(100 * res) , ":",  all_locations[i], "matches", all_locations[j]))
      if((!is.na(res)) & res > .2){
        matches <- rbind(matches,data.frame(lhs=all_locations[i], rhs = all_locations[j], match_rate = res))
        if(verbose){
          print(matches[nrow(matches), ])
        }
      }
    }
  }
  return(list(
    matches = matches,
    match_distribution = all_reses
  ))
}
