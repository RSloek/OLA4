# Opgave 2 SQL
library(dplyr)
library(RMariaDB)
library(DBI)

#connect to emp
wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = Sys.getenv("LOCAL_PASSWORD"),
                db = "emp",
                opert = "3306"
)


udlejningdf <- data.frame(RID = NA, EMPNO = 7369, Bil_id = 6966520, FD=Sys.Date()-1, BD=Sys.Date(), Beskr = "Kurt")

dbWriteTable(wc, "Udlejning", udlejningdf, overwrite=TRUE)

udlejningdf <- data.frame(EMPNO = 7369, Bil_id = 6966520, FD=Sys.Date()-1, BD=Sys.Date(), Beskr = "Kurt")

dbWriteTable(wc, "Udlejning", udlejningdf, append = TRUE)
