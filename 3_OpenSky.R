library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
source("util.R")
library(logr)


#=============================================================================.
# Lav url'er==================================================================
#=============================================================================.

baseurl <- "https://opensky-network.org/api"

endpointS <- "/states/all"
endpointT <- "/tracks/all"

queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)

### NORTH SEA ###
lamin= c(58.786395, 57.906395, 55.397831, 53.804621, 52.196834)
lomin= c(-6.437988, -2.505898, -1.305898, 0.152305, 1.936963)

lamax= c(61.856149, 58.786395, 57.906395, 55.397831, 53.804621)
lomax= c(4.608945, 5.207227, 8.007227, 8.201172, 4.174805)

#loop for at få alle url'er
urlList <- c()
for (i in 1:length(lomax)) {
  urlList=c(urlList, paste0(queryS, "?lamin=",lamin[i],"&lomin=",lomin[i],"&lamax=",lamax[i],"&lomax=",lomax[i]))
}
urlList


#=============================================================================.
# "tabs" variable til scrabe funktoiner=======================================
#=============================================================================.
page <-  read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs <-  page |> html_elements("table") |> html_table()

#=============================================================================.
# Load training data==========================================================
#=============================================================================.
fl=list.files(path = "./RDS_fly",pattern = "*rds", full.names = T)
RDSlist=lapply(fl,readRDS)

#scrape names from api doc
vars1 <- tabs[[17]]
vars2 <- tabs[[18]]
names1 <- unlist(vars1$Property[1:length(vars1$Property)-1])
names2 <- unlist(vars2$Property)
namesdf <- c(names1, names2)
for (i in 1:length(RDSlist)) {
  names(RDSlist[[i]])=namesdf
  names(RDSlist)[i] <- RDSlist[[i]]$icao24[1]
}

#=============================================================================.
# Hvor er flyene fra?=========================================================
#=============================================================================.

# find alle norht sea fly
# statedf <- data.frame()
# for (url in urlList) {
#   token=getToken()
#   raw <- getRawFromUrl(url, token)
#   urldf <- statesMakeDF(raw, tabs)
#   statedf <- rbind(statedf, urldf)
#   Sys.sleep(0.1)
# }

statedf <- data.frame()
for (url in urlList) {
  tryCatch({ #trycatch for hvis der ikke er nogle fly i vores boks
    #hent rå data========================.
    token=getToken()
    raw <- getRawFromUrl(url, token)
    
    #lav df og læg til===================.
    urldf <- statesMakeDF(raw, tabs)
    statedf <- rbind(statedf, urldf)
    log_print(paste0("[", match(url, urlList), "/", length(urlList), "] boks - ", nrow(urldf), " fly.\nstatus: ", raw$status_code, "."))
  }, error = function(e) {
    #hvis fejl=========================.
    log_print(paste0("[", match(url, urlList), "/", length(urlList), "] boks - FEJL: ", conditionMessage(e)))
  })  
  Sys.sleep(0.1)
}

# df over hvilket land fly kommer fra
flytable <- table(statedf$origin_country)
flydf <- as.data.frame(flytable)
flydf <- flydf[order(flydf$Freq, decreasing = T),]

#=============================================================================.
# Enkelte fly=================================================================
#=============================================================================.


# alle icaos i en vector
icaov = statedf$icao24

# one test icao
token=getToken()
url=paste0(queryT,"?icao24=",icaov[[1]] , "&time=0")
raw <- getRawFromUrl(url, token)
trackicaovdf <- tracksMakeDF(raw, tabs)

#test data frame
RDSdf <- RDSlist[[1]]

#plots
myEDAPlots(trackicaovdf)
myMapPlot(trackicaovdf)

myEDAPlots(RDSdf)
myMapPlot(RDSdf)

#=============================================================================.
# Flere fly===================================================================
#=============================================================================.

tracks <- list()
for (i in 1:length(icaov)) {
  token=getToken()
  url=paste0(queryT,"?icao24=",icaov[i], "&time=0")
  raw <- getRawFromUrl(url, token)
  if (raw$status == 200) {
    loopdf <- tracksMakeDF(raw, tabs)
    loopdf$sd <- sd(loopdf$true_track)
    tracks[[icaov[i]]] <- loopdf
    Sys.sleep(0.2)
  } else {
    print(raw$status_code)
    print(icaov[i])
  }
}

# test plot
myEDAPlots(tracks[[1]])
myMapPlot(tracks[[1]])



for (i in 1:length(tracks)) {
  if (tracks[[i]]$sd[1] > 100) {
    print(tracks[[i]]$icao24[1])
    print(tracks[[i]]$sd[1])
  }
}

# myEDAPlots(tracks$`43c6f4`)
# myMapPlot(tracks$`43c6f4`)


#=============================================================================.
# Egen algo===================================================================
#=============================================================================.

alleFly <- c(tracks, RDSlist)

cirkelFlyIcao <- c()
for (fly in alleFly) {
  if (antalOmgange(fly) >= 2) {
    cirkelFlyIcao <- c(cirkelFlyIcao, fly$icao24[1])
  }
}
cirkelFly <- alleFly[cirkelFlyIcao]

resultdf <- data.frame(
  icao  = c(names(tracks), names(RDSlist)),
  isOff = c(rep(0, length(tracks)), rep(1, length(RDSlist)))
)
resultdf$myOwnAlg <- ifelse(resultdf$icao %in% cirkelFlyIcao, 1, 0)

View(resultdf)

for (fly in RDSlist) {
  print(myMapPlot(fly))
}

antalOmgange(cirkelFly[[1]])
#=============================================================================.
# Hvor er flyene fra?=========================================================
#=============================================================================.

# Nøgletal til overskrift og undertitel
n        <- sum(flydf$Freq)
nLande   <- nrow(flydf)
topLand  <- as.character(flydf$Var1[1])
andelTop <- round(100 * flydf$Freq[1] / n)
hentet   <- format(Sys.time(), "%d-%m-%Y kl. %H:%M")   # flyt gerne op lige efter states-loopet

# Andel pr. land
flydf$andel <- flydf$Freq / n

# Fremhæv kun det største land, resten i grå
flydf$fremhaev <- flydf$Var1 == topLand

ggplot(flydf, aes(x = andel, y = reorder(Var1, andel), fill = fremhaev)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::percent(andel, accuracy = 1)), hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(`TRUE` = "#1f4e79", `FALSE` = "grey70"), guide = "none") +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    # Konkluderende overskrift – skrevet til dette øjebliksbillede
    title    = paste0("Tyskland har flest fly over Nordsøen – mere end hvert femte fly er tysk (", andelTop, " %)"),
    # Objektiv undertitel – hvad grafen viser
    subtitle = paste0("Andel af fly pr. registreringsland over Nordsøen, øjebliksbillede ",
                      hentet, " (n = ", n, " fly fra ", nLande, " lande)"),
    x        = "Andel af flyene",
    y        = NULL,
    caption  = "Kilde: OpenSky Network REST API"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title          = element_text(face = "bold"),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank()
  )

