# Opgave 2 SQL

library(dplyr)
install.packages("RMariaDB")
library(RMariaDB)
install.packages("DBI")
library(DBI)

#connect to world
wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = "Svante123",
                db = "world",
                opert = "3306"
)

baseQuery <- "select c.name, c.population, ctr.name from city c
join country ctr on ctr.code=c.CountryCode
where ctr.continent='"
endQuery <- "' order by ctr.name;"

Kontinent_Query <- function(kontinent) {
  total_Query <- paste0(baseQuery, kontinent, endQuery)
  kontinentdf <- dbGetQuery(wc, total_Query)
  return(kontinentdf)
}


Kontinent_Query("Europa")


connect_func <- function(query){
  wc <- dbConnect(MariaDB(),
                  host = "localhost",
                  user = "root",
                  password = "Svante123",
                  db = "world",
                  opert = "3306"
  ) 
  resdf <- dbGetQuery(wc, query)
  return(resdf)
}

query= "select c.name, c.population, ctr.name from city c
join country ctr on ctr.code=c.CountryCode
where ctr.continent='Europe'
order by ctr.name;"

resdf <- dbGetQuery(wc, query)
View(resdf)

udlejningdf <- data.frame(RID = NA, EMPNO = 7369, Bil_id = 6966520, FD=Sys.Date()-1, BD=Sys.Date(), Beskr = "Kurt")

dbWriteTable(wc, "Udlejning", udlejningdf, overwrite=TRUE)

udlejningdf <- data.frame(EMPNO = 7369, Bil_id = 6966520, FD=Sys.Date()-1, BD=Sys.Date(), Beskr = "Kurt")

dbWriteTable(wc, "Udlejning", udlejningdf, append = TRUE)
