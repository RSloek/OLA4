library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
source("util_Rasmus.R")
library(logr)


#=============================================================================.
# lav url'er==================================================================
#=============================================================================.

baseurl <- "https://opensky-network.org/api"

endpointS <- "/states/all"
endpointT <- "/tracks/all"

queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)

#### NORTH SEA ####
lamin= c(51.896834, 53.804621, 55.397831, 58.286395)
lomin= c(1.636963, -0.252305, -1.695898, -6.437988)

lamax= c(53.804621, 55.397831, 58.286395, 61.856149)
lomax= c(4.174805, 8.201172, 8.007227, 5.108945)

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
tabs <- page |> html_elements("table") |> html_table()

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
statedf <- data.frame()
for (url in urlList) {
  token=getToken()
  raw <- getRawFromUrl(url, token)
  urldf <- statesMakeDF(raw, tabs)
  statedf <- rbind(statedf, urldf)
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
  } else if (raw$status == 404){
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
n         <- sum(flydf$Freq)
nLande    <- nrow(flydf)
top3      <- as.character(flydf$Var1[1:3])
andelTop3 <- round(100 * sum(flydf$Freq[1:3]) / n)
hentet    <- format(Sys.time(), "%d-%m-%Y kl. %H:%M")   # flyt gerne op lige efter states-loopet

# Andel pr. land
flydf$andel <- flydf$Freq / n

# Fremhæv de tre største lande, resten i grå
flydf$fremhaev <- flydf$Var1 %in% top3

ggplot(flydf, aes(x = andel, y = reorder(Var1, andel), fill = fremhaev)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = scales::percent(andel, accuracy = 1)), hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(`TRUE` = "#1f4e79", `FALSE` = "grey70"), guide = "none") +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    # Konkluderende overskrift
    title    = paste0(andelTop3, " % af flyene over Nordsøen er registreret i blot tre lande"),
    # Objektiv undertitel – hvad grafen viser
    subtitle = paste0("Andel af fly pr. registreringsland i fem bokse over Nordsøen, øjebliksbillede ",
                      hentet, " (n = ", n, " fly fra ", nLande, " lande). Fremhævet: de tre største lande"),
    x        = "Andel af flyene",
    y        = NULL,
    caption  = "Kilde: OpenSky Network REST API (/states/all), variablen origin_country"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    plot.title          = element_text(face = "bold"),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank()
  )

