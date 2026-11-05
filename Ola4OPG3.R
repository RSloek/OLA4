library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
source("util.R")
library(logr)

baseurl <- "https://opensky-network.org/api"

#
endpointS <- "/states/all"
endpointT <- "/tracks/all"

#
queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)


#### NORTH SEA ####
lamin=52.055129
lamax=56.196869
lomin=-6.065140
lomax=4.305954

# variable scrabe til funktoiner
page <-  read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs <- page |> html_elements("table") |> html_table()


fullurl=paste0(queryS, "?lamin=",lamin,"&lomin=",lomin,"&lamax=",lamax,"&lomax=",lomax)

# one take over NS (skateboard)
token=getToken()
raw <- getRawFromUrl(fullurl, token)
statedf <- statesMakeDF(raw, tabs)

# find all icaos and put in vector
icaov = statedf$icao24
# find all tracks for each normal icao
# start with one test icao
ictest=icaov[[50]]
ictest
token=getToken()
url=paste0(queryT,"?icao24=",ictest, "&time=0")
raw <- getRawFromUrl(url, token)
statedf <- tracksMakeDF(raw, tabs)


#plots
myEDAPlots(statedf)
myMapPlot(statedf)


tracks <- list()
for (i in 1:length(icaov)) {
  token=getToken()
  url=paste0(queryT,"?icao24=",icaov[i], "&time=0")
  raw <- getRawFromUrl(url, token)
  if (raw$status == 200) {
    loopdf <- tracksMakeDF(raw, tabs)
    tracks[[icaov[i]]] <- loopdf
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



