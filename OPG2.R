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


dbWriteTable(wc, "bbscrape", Bilbasedf_clean, overwrite=TRUE)


udlejningdf <- data.frame(RID = NA, EMPNO = 7369, Bil_id = 6966520, FD=Sys.Date()-1, BD=Sys.Date(), Beskr = "Kurt")

dbWriteTable(wc, "Udlejning", udlejningdf, overwrite=TRUE)

udlejningdf <- data.frame(EMPNO = 7369, Bil_id = 6966520, FD=Sys.Date()-1, BD=Sys.Date(), Beskr = "Kurt")

dbWriteTable(wc, "Udlejning", udlejningdf, append = TRUE)
