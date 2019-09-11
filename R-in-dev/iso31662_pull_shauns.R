get_iso_country_codes <- function(country, port=4567L, chromever = "latest"){
  
  rm(driver)
  gc() 
  
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

  eCaps <- list(chromeOptions = list(args = c('--no-sandbox','--headless', '--disable-gpu', '--window-size=1280,800')))
  driver <- RSelenium::rsDriver(browser=browser, port=port, chromever = chromever, extraCapabilities=eCaps)
  Sys.sleep(1) # Sleep to give it a chance to work
  remDr <- driver[["client"]]
  Sys.sleep(1)

  remDr$open()
  Sys.sleep(1)
  
  if(!remDr$getStatus()$ready){
    stop("The web browser did not initialize successfully")
  }

  remDr$navigate(url = weburl)
  Sys.sleep(1)
  webElem <- remDr$findElement(using = "xpath", '//*[(@id = "subdivision")]//td')
  Sys.sleep(1)
  websrc <- webElem$getPageSource()[[1]] %>%
    xml2::read_html()
    
  rc <- websrc %>% rvest::html_table()
  rc <- rc[[3]] 
  remDr$close()
  rm(driver) # make sure port is not in use next time
  gc()
  return(rc)
}
