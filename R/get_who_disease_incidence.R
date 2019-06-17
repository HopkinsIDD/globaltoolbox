

#' @name who_disease_incidence_month
#' @title who_disease_incidence_month
#' @description Load monthly incidence data reported to the World Health Organization
#' @param diseaes Disease of interest. Options currently include "measles" and "rubella"
#' @param save_dir Where to save the xls file, if interested. If NULL, will not save.
#' @param long_data Logical, whether long form is desired (default).
#' @return 
#' @export


who_disease_incidence_month <- function(disease=c("measles", "rubella"), save_dir=NULL, long_data=TRUE) {
  
  if (is.na(disease)) {
    return("Please specify a disease")
  } else if (!(disease %in% c("measles", "rubella"))) {
    return("This disease is not currently available or part of this function.")
  } else if (length(disease)>1) {
    return("Please specify one disease at a time.")
  }
  
  url_data <- paste0("https://www.who.int/immunization/monitoring_surveillance/burden/vpd/", disease, "casesbycountrybymonth.xls?ua=1")

  # Load xls from temporary file
  httr::GET(url_data, httr::write_disk(tf <- tempfile(fileext = ".xls")))
  incid_data <- readxl::read_excel(tf, sheet="WEB", na=c("NA", "na", ""," "))
  unlink(tf)
  incid_data <- dplyr::mutate_each(incid_data, list(as.integer), 4:16)

  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
    httr::GET(url_data, httr::write_disk(file.path(save_dir, paste0(disease,"casesbycountrybymonth.xls")), overwrite=TRUE))
  }
  
  if (long_data) {
    # Convert to long data
    incid_long <- incid_data %>% tidyr::gather("Month", "Cases", -Region, -ISO3, -Country, -Year)
    years_ <- sort(unique(incid_long$Year))
    month_yr_who <- paste0(rep(substr(month.name,1,3), length(years_)),"-",sort(rep(years_, 12)))
    incid_long <- incid_long %>% dplyr::mutate(Month=substr(Month,1,3)) %>% 
      dplyr::mutate(month_yr=factor(paste0(Month,"-",Year), levels=month_yr_who), source="WHO")
    incid_long <- incid_long %>% arrange(ISO3, month_yr)
    colnames(incid_long) <- tolower(colnames(incid_long))
    
    return(incid_long)
    
  } else {
    return(incid_data)
  }
  
}






#' @name who_disease_incidence_year
#' @title who_disease_incidence_year
#' @description Load monthly incidence data reported to the World Health Organization
#' @param diseaes Disease of interest. Options currently include "CRS","Diphtheria","JapEnc","Measles","Mumps","Ntetanus","Pertussis","Polio","Rubella","Ttetanus",and "Yfever".
#' @param save_dir Where to save the xls file, if interested. If NULL, will not save.
#' @param long_data Logical, whether long form is desired (default).
#' @return 
#' @export


who_disease_incidence_year <- function(disease=c("CRS","Diphtheria","JapEnc","Measles","Mumps","Ntetanus","Pertussis","Polio","Rubella","Ttetanus","Yfever"),
                                       save_dir=NULL, long_data=TRUE) {
  
  disease_list <- c("CRS","Diphtheria","JapEnc","Measles","Mumps","Ntetanus","Pertussis","Polio","Rubella","Ttetanus","Yfever")
  # bypass any capitalization issues
  disease <- disease_list[match(tolower(disease), tolower(disease_list))]
  
  if (is.na(disease)) {
    return("Please specify a disease")
  } else if (!(disease %in% disease_list)) {
    return(paste0(disease, " is not currently available or part of this function."))
  } else if (length(disease)>1) {
    return("Please specify one disease at a time.")
  }
  

  # URL
  url_data <- "https://www.who.int/immunization/monitoring_surveillance/data/incidence_series.xls"
  
  # Load xls from temporary file
  httr::GET(url_data, httr::write_disk(tf <- tempfile(fileext = ".xls")))
  incid_data <- readxl::read_excel(tf, sheet=disease, na=c("NA", "na", ""," "))
  unlink(tf)
  incid_data <- dplyr::mutate_each(incid_data, list(as.integer), 5:ncol(incid_data))
  
  if (!is.null(save_dir)) {
    dir.create(save_dir, recursive = TRUE)
    httr::GET(url_data, httr::write_disk(file.path(save_dir, paste0(disease,"casesbycountrybymonth.xls")), overwrite=TRUE))
  }
  
  if (long_data) {
    # Convert to long data
    incid_long <- incid_data %>% tidyr::gather("Year", "Cases", -WHO_REGION, -ISO_code, -Cname, -Disease)
    colnames(incid_long) <- tolower(colnames(incid_long))
    incid_long <- incid_long %>% dplyr::rename(iso3 = iso_code, country=cname) %>% mutate(year = as.integer(year))
    incid_long <- incid_long %>% arrange(iso3, year)
    return(incid_long)
    
  } else {
    return(incid_data)
  }
  
}







