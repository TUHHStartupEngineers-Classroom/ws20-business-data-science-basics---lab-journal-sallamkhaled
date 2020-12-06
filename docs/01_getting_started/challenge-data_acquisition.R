# Challenge 1 - API ----
library(httr)
library(jsonlite)
library(dplyr)
library(RSQLite)
library(dplyr)
library(tibble)
library(rvest)

#Covid-19 Germany statistics

url <-  "https://api.covid19api.com/dayone/country/germany/status/confirmed"
resp <- GET(url)

list <- resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON() %>%
  head(10)

list

#Challenge 2 - Web Scraping

bike_webscraping <- function(url) {
  
  bike_html <- read_html(url)
  bike_url_tbl  <- bike_html %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text()%>%
    enframe(name = "No.", value = "Bike.Name")
  bike_database_tbl<-bike_url_tbl%>% 
    mutate(price= bike_html%>%
             html_nodes(css =".catalog-category-bikes__price-title")%>% html_text())
  
}
url= "https://www.rosebikes.de/fahrräder/gravel"
bike_prices<-bike_webscraping(url)
saveRDS(bike_prices,"challenge-data_acquistion.rds")
bike_prices
