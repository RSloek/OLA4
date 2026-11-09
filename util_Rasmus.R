library(httr)
library(leaflet)
library(ggplot2)
library(httr)
library(rvest)
user_name <- "rasmussloek@gmail.com"
clientId = "rasmussloek@gmail.com-api-client"
clientSecret = "IcJKt1lPJBoiIXoHnj5uaxUBdzm7gpcU"


# Request an access token

getToken <- function() {
  token=NULL
  response <- POST(
    url = "https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
    body = list(
      grant_type = "client_credentials",
      client_id = clientId,
      client_secret = clientSecret
    ),
    encode = "form"
  )
  
  # Parse the JSON response
  response$status_code
  if (response$status_code == 200) {
    content <- content(response, as = "parsed", type = "application/json")
    token <- content$access_token
  }
  return(token)
}

# lav dette til en function i util - getRawFromUrl(url)
getRawFromUrl <- function(url, token) {
  
  raw = GET(
    url = url,
    add_headers(Authorization = paste("Bearer", token, sep = " "))
  )
  
  print(raw$status_code)
  return(raw)
}

myMapPlot <- function(df) {
  leaflet(df) %>% 
    addTiles() %>% 
    addPolylines(lat = ~latitude, lng = ~longitude)
}

myEDAPlots <- function(df) {
  par(mfcol=c(1,3), ann=F)
  plot(df$latitude,df$longitude)
  plot(df$true_track,df$time)
  plot(df$baro_altitude,df$time)
}

statesMakeDF <- function(raw, tabs) {
  # make raw to df
  content <- content(raw, as="text")
  rawJSONlist <- fromJSON(content)
  df <- as.data.frame(rawJSONlist$states)
  
  # scrape variable colnames
  vars <- tabs[[5]]
  names=unlist(vars$Property)
  names=names[1:(length(names)-1)]
  colnames(df)=names
  return(df)
}

tracksMakeDF <- function(raw, tabs) {
  # make raw to df
  rawJSONlist <- fromJSON(content(raw, as="text"))
  df <- as.data.frame(rawJSONlist)
  
  # scrape variable colnames
  vars1 <- tabs[[17]]
  vars2 <- tabs[[18]]
  names1 <- unlist(vars1$Property[1:length(vars1$Property)-1])
  names2 <- unlist(vars2$Property)
  namesdf <- c(names1, names2)
  colnames(df)=namesdf
  return(df)
}







