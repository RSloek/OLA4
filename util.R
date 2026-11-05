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






# Gemmer to ggplot2-plots af én flyvning som PNG i en mappe:
#   <navn>_eda.png  – samme tre plots som myEDAPlots (position, kurs, højde)
#   <navn>_map.png  – ruten på et kort, ligesom myMapPlot
#
# df     : data frame med kolonnerne icao24, time, latitude, longitude,
#          baro_altitude, true_track (som i loopdf/statedf)
# mappe  : mappen plots gemmes i (oprettes automatisk)
# navn   : start på filnavnet. Standard er icao24 + tidspunkt for første waypoint
# titel  : skriv en konkluderende overskrift til rapporten, fx
#          "Flyet cirkler over samme område i 40 minutter"
# Returnerer stierne til de gemte filer (usynligt).
gemPlots <- function(df, mappe = "plots", navn = NULL,
                     titelEDA = NULL, titelMap = NULL,
                     bredde = 10, hoejde = 6, dpi = 150) {
  
  dir.create(mappe, showWarnings = FALSE, recursive = TRUE)
  
  # path-kolonnerne kan komme ud som tekst – gør dem til tal
  for (k in c("time", "latitude", "longitude", "baro_altitude", "true_track")) {
    df[[k]] <- as.numeric(df[[k]])
  }
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  if (nrow(df) < 2) {
    warning("For få waypoints til at plotte - intet gemt")
    return(invisible(NULL))
  }
  
  icao <- as.character(df$icao24[1])
  tid  <- as.POSIXct(df$time, origin = "1970-01-01", tz = "UTC")
  if (is.null(navn)) navn <- paste0(icao, "_", format(min(tid), "%Y%m%d_%H%M%S"))
  
  # objektiv undertitel: hvad grafen viser
  undertitel <- paste0("icao24 ", icao, " · ", nrow(df), " waypoints · ",
                       format(min(tid), "%d-%m-%Y %H:%M"), "–",
                       format(max(tid), "%H:%M"), " UTC")
  
  if (is.null(titelEDA)) titelEDA <- paste("Flyvning", icao, "- position, kurs og højde")
  if (is.null(titelMap)) titelMap <- paste("Flyvning", icao, "- rute")
  
  # ---- EDA: tre paneler i ét billede (svarer til par(mfcol = c(1,3))) ----
  minutter <- (df$time - min(df$time)) / 60
  eda <- rbind(
    data.frame(panel = "Position (længde / bredde)",   x = df$longitude, y = df$latitude),
    data.frame(panel = "Kurs i grader (min. fra start)", x = minutter,   y = df$true_track),
    data.frame(panel = "Højde i meter (min. fra start)", x = minutter,   y = df$baro_altitude)
  )
  eda$panel <- factor(eda$panel, levels = unique(eda$panel))
  
  pEDA <- ggplot(eda, aes(x = x, y = y)) +
    geom_point(size = 0.8, colour = "steelblue") +
    facet_wrap(~panel, scales = "free") +
    labs(title = titelEDA, subtitle = undertitel, x = NULL, y = NULL) +
    theme_minimal()
  
  # ---- Kort: ruten med landegrænser (kræver pakken "maps") ----
  margen <- 0.5
  xlim <- range(df$longitude) + c(-margen, margen)
  ylim <- range(df$latitude)  + c(-margen, margen)
  
  pMap <- ggplot(df, aes(x = longitude, y = latitude)) +
    borders("world", colour = "grey60", fill = "grey93") +
    geom_path(colour = "steelblue", linewidth = 0.8) +
    geom_point(data = df[1, ],       colour = "darkgreen", size = 2.5) +  # start
    geom_point(data = df[nrow(df), ], colour = "red",       size = 2.5) +  # slut
    coord_quickmap(xlim = xlim, ylim = ylim) +
    labs(title = titelMap,
         subtitle = paste0(undertitel, " · grøn = start, rød = slut"),
         x = "Længdegrad", y = "Breddegrad") +
    theme_minimal()
  
  # ---- Gem ----
  filEDA <- file.path(mappe, paste0(navn, "_eda.png"))
  filMap <- file.path(mappe, paste0(navn, "_map.png"))
  ggsave(filEDA, pEDA, width = bredde, height = hoejde, dpi = dpi, bg = "white")
  ggsave(filMap, pMap, width = bredde, height = hoejde, dpi = dpi, bg = "white")
  
  message("Gemt: ", filEDA, " og ", filMap)
  invisible(c(eda = filEDA, map = filMap))
}





