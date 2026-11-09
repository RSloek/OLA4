library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
source("util_Rasmus.R")
library(logr)


#=============================================================================.
#lav url'er===================================================================
#=============================================================================.

baseurl <- "https://opensky-network.org/api"

endpointS <- "/states/all"
endpointT <- "/tracks/all"

queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)

#### NORTH SEA ####
lamin= c(53.304621, 55.397831, 58.286395, 51.896834)
lomin= c(-0.252305, -3.295898, -6.437988, 1.636963)

lamax= c(55.397831, 58.286395, 61.856149, 53.804621)
lomax= c(8.701172, 8.657227, 5.108945, 4.174805)

#loop for at få alle url'er (3 url'er)
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
View(statedf)

# df over hvilket land fly kommer fra
flytable <- table(statedf$origin_country)
flydf <- as.data.frame(flytable)
flydf <- flydf[order(flydf$Freq, decreasing = T),]


#=============================================================================.
# Enkelte fly==================================================================
#=============================================================================.


# alle icaos i en vector
icaov = statedf$icao24

# one test icao
ictest=icaov[[2]]
ictest

token=getToken()
url=paste0(queryT,"?icao24=",ictest, "&time=0")
raw <- getRawFromUrl(url, token)
trackdf <- tracksMakeDF(raw, tabs)

#plots
myEDAPlots(trackdf)
myMapPlot(trackdf)








tracks <- list()
for (i in 1:length(icaov)) {
  token=getToken()
  url=paste0(queryT,"?icao24=",icaov[i], "&time=0")
  raw <- getRawFromUrl(url, token)
  if (raw$status == 200) {
    loopdf <- tracksMakeDF(raw, tabs)
    tracks[[icaov[i]]] <- loopdf
    Sys.sleep(0.1)
  } else if (raw$status == 404){
    print(icaov[i])
  }
}


myEDAPlots(tracks$`4bc8c4`)
myMapPlot(tracks$`4bc8c4`)


# load training-data
fl=list.files(path = "./RDS_fly",pattern = "*rds", full.names = T)
collist=lapply(fl,readRDS)
testdf=collist[[1]]
testdf=readRDS("RDS_fly/circMT.rds")
names(testdf) <- namesdf

# plots to explore data
myEDAPlots(testdf)
# map-plot to verify flight
myMapPlot(testdf)
par(mfrow=c(1,1))
plot(testdf$lng,testdf$lat)
plot(testdf$time,testdf$crs)

dlatv=diff(testdf$lat)
dlnv=diff(testdf$lng)
dlatv=c(0,dlatv)
dlnv=c(0,dlnv)
testdf$dlat=dlatv
testdf$dlng=dlnv

ggplot(testdf,aes(x=time,y=lat))+geom_line()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_continuous(breaks = scales::pretty_breaks(n=35))


# apply circmetrics
# lm
cmodel=lm(lat ~ lng+crs+alt, data=testdf)
nmodel=lm(lat ~ lng+crs+alt, data=statedf)
summary(cmodel)
summary(nmodel)
# sd
csd=sd(testdf$crs)
nsd=sd(statedf$crs)

# myOwnDetectionAlg
# use diff to track change of fortegn








#=============================================================================.
# Hvor er flyene fra?=========================================================
#=============================================================================.

# Nøgletal til overskrift og undertitel
n       <- sum(flydf$Freq)
topLand <- as.character(flydf$Var1[1])
andel   <- round(100 * max(flydf$Freq) / n)
nLande  <- nrow(flydf)
hentet  <- "06-11-2026 kl. 12:30"   # <- skriv tidspunktet for jeres hentning

# Fremhæv det største land, resten i grå
flydf$fremhaev <- flydf$Var1 == topLand

ggplot(flydf, aes(x = Freq, y = reorder(Var1, Freq), fill = fremhaev)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = Freq), hjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c(`TRUE` = "#1f4e79", `FALSE` = "grey70"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    # Konkluderende overskrift – ret teksten, hvis jeres friske data viser noget andet
    title    = paste0(topLand, " dominerer luftrummet over Nordsøen – ",
                      andel, " % af flyene er registreret dér"),
    # Objektiv undertitel – hvad grafen viser
    subtitle = paste0("Antal fly pr. registreringsland i Nordsø-bounding-boxen, øjebliksbillede ",
                      hentet, " (n = ", n, " fly fra ", nLande, " lande)"),
    x        = "Antal fly",
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

