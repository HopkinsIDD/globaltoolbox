get_iso_country_codes <- function(country){
  if(length(country) > 1){
    stop("country should be a single country")
  }
  if(mode(country) != 'character'){
    country = as.character(country)
  }
  if(nchar(country) != 2){
    stop("country should be the iso alpha-2 code")
  }
  weburl = paste0("https://www.iso.org/obp/ui/#iso:code:3166:",country)
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = port,
    browserName = "firefox"
  )
  if(!remDr$getStatus()$ready){
    stop("The web browser did not initialize successfully")
  }
  remDr$open()
  remDr$navigate(url = weburl)
  webElem <- remDr$findElement(using = "xpath", '//*[(@id = "subdivision")]//td')
  # webElem$describeElement()
  websrc <- webElem$getPageSource()[[1]] %>%
    read_html()
  websrc %>% 
    xml2::xml_children() %>%
    .[[2]] %>%
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[2]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[2]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>%
    xml2::xml_children() %>%
    .[[2]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[2]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[2]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[1]] %>% 
    xml2::xml_children() %>%
    .[[5]] %>% 
    xml2::xml_children() %>%
    .[[7]] %>% 
    rvest::html_table() ->
    rc
  remDr$close()
  return(rc)
}
