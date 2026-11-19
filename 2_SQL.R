# Opgave 2 SQL
library(dplyr)
library(RMariaDB)
library(DBI)

#connect to emp
wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = Sys.getenv("LOCAL_PASSWORD"),
                db = "Bilbasen",
                opert = "3306"
)


# dbWriteTable(wc, "bbscrape", Bilbasedf_clean, overwrite=TRUE)

#=============================================================================.
# lav ny scrape data==========================================================
#=============================================================================.
kørsel2 <- Bilbasedf_clean

#to nye rækker
toNye <- data.frame(Bil_id = c("3758493", "1789048"), 
                 Bilmodel = c("Mercedes EQA250+ AMG Premium 5d", "Mercedes EQS450+ AMG Line 5d") , 
                 Pris = c(394800, 400000), 
                 Registrerings_år = c(2025, 2018), 
                 Kørte_km = c(20000, 230000), 
                 Rækkevidde = c(530, "NA"), 
                 Drivmiddel = c("El","Diesel"), 
                 Forhandler_id = c("c1f5d6e2-f576-4def-9d2c-3a410587bf8e", "ca0cc050-ae9b-4444-ab09-46e653a18bb9"),
                 Lokation = c("Middelfart, Fyn", "Silkeborg, Østjylland"), 
                 Link = c("https://www.bilbasen.dk/brugt/bil/mercedes/cla250/electric-progressive-advance-plus-4d/7004965", "https://www.bilbasen.dk/brugt/bil/mercedes/cla250/electric-progressive-advance-plus-4d/7004565"), 
                 Bilmodel_kort = c("Mercedes EQA250", "Mercedes EQS450"))
kørsel2 <- rbind(kørsel2, toNye)

#ændre pris på 3
kørsel2[c(89, 983, 219),"Pris"] <- c(829900, 200000, 380000)

#fjern 5 rækker
kørsel2 <- kørsel2[-c(78, 1782, 1389, 1222, 2),]




nyDato <- as.Date("2026-11-12")

#=============================================================================.
# 1. Hent forrige kørsel fra databasen========================================
#=============================================================================.
# nyeste pris for hver bil, der ikke er solgt


query <- "
SELECT b.Bil_id, p.pris AS Pris
FROM bil b
JOIN pris p ON p.bil_id = b.Bil_id
WHERE b.sold = 0
  AND p.scrapedate = (SELECT MAX(p2.scrapedate)
                      FROM pris p2
                      WHERE p2.bil_id = b.Bil_id)"

kørsel1 <- dbGetQuery(wc, query)

#=============================================================================.
# 2. Find forskellene=========================================================
#=============================================================================.
# nye biler
nyeBiler <- kørsel2[!(kørsel2$Bil_id %in% kørsel1$Bil_id), ]

# ændrede priser
faelles  <- merge(kørsel1, kørsel2[, c("Bil_id", "Pris")], by = "Bil_id", suffixes = c("_gl", "_ny"))

aendrede <- faelles[faelles$Pris_gl != faelles$Pris_ny, ]

# solgte biler
solgte <- kørsel1[!(kørsel1$Bil_id %in% kørsel2$Bil_id), ]

# nye forhandlere
query <- "SELECT forhandler_id FROM forhandler"
glForh <- dbGetQuery(wc, query)


nyeBilerForh <- nyeBiler[!(nyeBiler$Forhandler_id %in% glForh$forhandler_id),]
nyeBilerForh <- nyeBilerForh[, c("Forhandler_id", "Lokation")]
nyeForh <- unique(nyeBilerForh)

#=============================================================================.
# 3. Opdater databasen========================================================
#=============================================================================.

#nye forhandler i "forhandler"
dbWriteTable(wc, "forhandler", nyeForh, append = TRUE)


# nye biler i "bil"
bilNy <- data.frame(
  Bil_id             = nyeBiler$Bil_id,
  forhandler_id      = nyeBiler$Forhandler_id,
  model              = nyeBiler$Bilmodel_kort,
  `registrerings_år` = nyeBiler$Registrerings_år,
  `kørte_km`         = nyeBiler$Kørte_km,
  drivmiddel         = nyeBiler$Drivmiddel,
  `rækkevidde`       = as.numeric(nyeBiler$Rækkevidde),
  link               = nyeBiler$Link,
  sold               = 0)

dbWriteTable(wc, "bil", bilNy, append = TRUE)

# nye priser i "pris" (nye biler og ændrede priser)
prisNy <- data.frame(
  bil_id     = c(nyeBiler$Bil_id, aendrede$Bil_id),
  pris       = c(nyeBiler$Pris,   aendrede$Pris_ny),
  scrapedate = nyDato)

dbWriteTable(wc, "pris", prisNy, append = TRUE)

# solgte biler markeres
query <- paste0("UPDATE bil SET sold = 1 WHERE Bil_id IN ('", paste(solgte$Bil_id, collapse = "', '"), "');")
dbExecute(wc, query)




















