library(jsonlite)
library(httr)
library(stringr)
library(ggplot2)
source("util_Rasmus.R")
library(logr)

#=============================================================================.
# åben log====================================================================
#=============================================================================.

#åben log med "openSky_2026.11.12_09:33:17.log" som navn
log_open(file_name = paste0("openSky_", format(Sys.time(), "%Y.%m.%d_%H-%M-%S"), ".log"))
log_print("Script startet.")

#=============================================================================.
# lav url'er==================================================================
#=============================================================================.

# endpoints============================.
baseurl <- "https://opensky-network.org/api"

endpointS <- "/states/all"
endpointT <- "/tracks/all"

queryS <- paste0(baseurl,endpointS)
queryT <- paste0(baseurl,endpointT)

# Nordsø-bokse=========================.
lamin= c(58.786395, 57.906395, 55.397831, 53.804621, 52.196834)
lomin= c(-6.437988, -2.505898, -1.305898, 0.152305, 1.936963)

lamax= c(61.856149, 58.786395, 57.906395, 55.397831, 53.804621)
lomax= c(4.608945, 5.207227, 8.007227, 8.201172, 4.174805)

#til bbox
#1.936963,52.196834,4.174805,53.804621
#0.152305,53.804621,8.201172,55.397831
#-1.305898,55.397831,8.007227,57.906395
#-2.505898,57.906395,5.207227,58.786395
#-6.437988,58.786395,4.608945,61.856149


# url for hver boks====================.
urlList <- c()
for (i in 1:length(lomax)) {
  urlList=c(urlList, paste0(queryS, "?lamin=",lamin[i],"&lomin=",lomin[i],"&lamax=",lamax[i],"&lomax=",lomax[i]))
}
urlList
log_print(paste0(length(urlList), " url'er lavet til Nordsø-boksene."))

#=============================================================================.
# "tabs" variable til scrabe funktoiner=======================================
#=============================================================================.
page <-  read_html("https://openskynetwork.github.io/opensky-api/rest.html")
tabs <- page |> html_elements("table") |> html_table()
log_print(paste0("API-dokumentation hentet - ", length(tabs), " tabeller."))

#=============================================================================.
# Hvor er flyene fra?=========================================================
#=============================================================================.

# find alle norht sea fly==============.
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


# df over hvilket land fly kommer fra==.
flytable <- table(statedf$origin_country)
flydf <- as.data.frame(flytable)
flydf <- flydf[order(flydf$Freq, decreasing = T),]


#=============================================================================.
# Flere fly===================================================================
#=============================================================================.

# alle icaos i en vector===============.
icaov = statedf$icao24


#start loop============================.
tracks <- list()
log_print(paste("Starter loop over", length(icaov), "fly"))
for (i in 1:length(icaov)) {
  #hent rå data========================.
  token=getToken()
  url=paste0(queryT,"?icao24=",icaov[i], "&time=0")
  raw <- getRawFromUrl(url, token)
  if (raw$status == 200) {
    #hvis god==========================.
    tryCatch({ #trycatch for hvis der ingen track er
      loopdf <- tracksMakeDF(raw, tabs)
      log_print(paste0("[", i, "/", length(icaov), "] ", icaov[i], " - henter track.\nok - ", nrow(loopdf), " waypoints.\nstatus: ", raw$status, "."))
      loopdf$sd <- sd(loopdf$true_track)
      tracks[[icaov[i]]] <- loopdf
    }, error = function(e) {
      #hvis fejl=========================.
      log_print(paste0("[", i, "/", length(icaov), "] ", icaov[i], " - FEJL: ", conditionMessage(e)))
    })
    Sys.sleep(0.2)
  } else{
    #hvis dårlig=======================.
    log_print(paste0(icaov[i], " har ingen track(", raw$status_code, ")" ))
  }
}
log_print(paste0("Loop færdigt - ", length(tracks), " af ", length(icaov), " fly har et track."))

# myEDAPlots(tracks$`43c6f4`)
# myMapPlot(tracks$`43c6f4`)


#=============================================================================.
# Egen algo===================================================================
#=============================================================================.

# find cirklende fly===================.
log_print(paste0("Kører egen algoritme på ", length(tracks), " fly."))

cirkelFlyIcao <- c()
for (fly in tracks) {
  #er det et cirkelfly?================.
  if (antalOmgange(fly) >= 2) {
    cirkelFlyIcao <- c(cirkelFlyIcao, fly$icao24[1])
  }
}

cirkelFly <- tracks[cirkelFlyIcao]
log_print(paste0(length(cirkelFlyIcao), " cirklende fly fundet: ", paste(cirkelFlyIcao, collapse = ", "), "."))

# resultat-df==========================.
resultdf <- data.frame(icao = names(tracks))
resultdf$myOwnAlg <- ifelse(resultdf$icao %in% cirkelFlyIcao, 1, 0)


#=============================================================================.
# Gem fundne fly==============================================================
#=============================================================================.

fundfil <- "cirkelfly_fundet.csv"
nu <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# hent tidligere fund==================.
if (file.exists(fundfil)) {
  alleFund <- read.csv(fundfil, colClasses = "character") #colonerne skal være character, hvis 798432 er fly kode bliver det læst som forkert data type
} else {
  alleFund <- data.frame(icao = character(), foersteGang = character(),
                         sidstSet = character(), antalGange = character())
}

#hvilke fly er helt nye for os
#nyefly <- cirkelFlyIcao[!(cirkelFlyIcao %in% alleFund$icao)]

# opdater eller tilføj=================.
nyefly <- c()
antalNye <- 0
for (icao in cirkelFlyIcao) {
  if (icao %in% alleFund$icao) {
    #kendt fly - opdater===============.
    r <- which(alleFund$icao == icao)
    alleFund$sidstSet[r]   <- nu
    alleFund$antalGange[r] <- as.numeric(alleFund$antalGange[r]) + 1
  } else {
    #nyt fly - tilføj==================.
    alleFund <- rbind(alleFund, data.frame(icao = icao, foersteGang = nu,
                                           sidstSet = nu, antalGange = 1))
    antalNye <- antalNye + 1
    nyefly <- c(nyefly, icao)
  }
}

# gem filen============================.
write.csv(alleFund, fundfil, row.names = FALSE) #row.names for at undgå kolone med rownames 1. 2. osv... på næste read.csv
log_print(paste0(length(cirkelFlyIcao), " cirkelfly fundet - heraf ", antalNye, " nye.\nI alt ", nrow(alleFund), " forskellige fly i ", fundfil, "."))

#=============================================================================.
# Gem tracks==================================================================
#=============================================================================.

# hent tidligere tracks================.
trackfil <- "cirkelfly_tracks_gemt.rds"
if (file.exists(trackfil)) {
  gemteTracks <- readRDS(trackfil)
} else {
  gemteTracks <- list()
}
log_print(paste0(length(gemteTracks), " flyveture hentet fra ", trackfil, "."))

antalNyeTure   <- 0
antalOpdateret <- 0

# gennemgå nye tracks==================.
for (icao in names(cirkelFly)) {
  ny      <- tracks[[icao]]
  nyStart <- min(as.numeric(ny$time))
  nySlut  <- max(as.numeric(ny$time))
  
  #find samme flyvetur=================.
  sammeTur <- NULL
  for (id in names(gemteTracks)) {
    gammel <- gemteTracks[[id]]
    if (gammel$icao24[1] == icao &&
        nyStart <= max(as.numeric(gammel$time)) &&
        nySlut  >= min(as.numeric(gammel$time))) {
      sammeTur <- id
    }
  }
  
  if (is.null(sammeTur)) {
    #ny flyvetur=======================.
    id <- paste0(icao, "_", nyStart)
    gemteTracks[[id]] <- ny
    antalNyeTure <- antalNyeTure + 1
    log_print(paste0(id, " - ny flyvetur gemt (", nrow(ny), " waypoints)."))
  } else {
    #opdater flyvetur==================.
    samlet <- rbind(gemteTracks[[sammeTur]], ny)
    samlet <- samlet[!duplicated(samlet$time), ]
    samlet <- samlet[order(as.numeric(samlet$time)), ]
    samlet$sd <- sd(samlet$true_track)
    log_print(paste0(sammeTur, " - opdateret fra ", nrow(gemteTracks[[sammeTur]]), " til ", nrow(samlet), " waypoints."))
    gemteTracks[[sammeTur]] <- samlet
    antalOpdateret <- antalOpdateret + 1
  }
}

# gem filen============================.
saveRDS(gemteTracks, trackfil)
log_print(paste0(antalNyeTure, " nye flyveture, ", antalOpdateret, " opdateret.\nI alt ", length(gemteTracks), " flyveture i ", trackfil, "."))

#=============================================================================.
# Send mail===================================================================
#=============================================================================.
library(emayili)

if (length(nyefly) > 0) {
  tryCatch({
    #lav mailen==========================.
    mail <- envelope(
      from      = Sys.getenv("GMAIL_USER"),
      to        = Sys.getenv("MAIL_TIL"),
      subject   = paste0("Mistænkelige fly over Nordsøen - ", format(Sys.time(), "%d-%m-%Y %H:%M")),
      text      = paste0("Kørslen fandt ", length(nyefly), " cirklende fly:\n\n", paste(nyefly, collapse = "\n"))
    )
    
    #send via gmail======================.
    smtp <- server(host = "smtp.gmail.com", port = 465,
                   username = Sys.getenv("GMAIL_USER"),
                   password = Sys.getenv("GMAIL_APP_PW"))
    smtp(mail)
    log_print(paste0("Mail sendt til ", Sys.getenv("MAIL_TIL"), " med ", length(cirkelFlyIcao), " fly."))
  }, error = function(e) {
    #hvis fejl=========================.
    log_print(paste0("Mail kunne ikke sendes: ", conditionMessage(e)))
  })
} else {
  log_print("Ingen cirkelfly - ingen mail sendt.")
}

# luk log==============================.
log_print("Script færdigt.")
log_close()

for (fly in cirkelfly_tracks_gemt) {
  print(myMapPlot(fly))
}

