## ---- sec-5.1 ----

## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- scan-twice ----
colnam <- scan("molclock1.txt", nlines=1, what="")
molclock <- scan("molclock1.txt", skip=1,
                 what=c(list(""), rep(list(1),5)))
molclock <- data.frame(molclock, row.names=1)
  # Column 1 supplies row names
names(molclock) <- colnam

## ---- download-bitr ----
webdir <- "http://bitre.gov.au/publications/ongoing/files/"
url <- paste0(webdir,
              "domestic_airline_activity_TopRoutesJuly2004July2015.xls")
download.file(url, destfile="auAirTraffic.xls", mode="wb")

## ---- sec-5.2 ----

## ---- get-por ----
library(memisc)
## Unzip; return path to "NES1948.POR"
path2file <- unzip(system.file("anes/NES1948.ZIP",package="memisc"),
                     "NES1948.POR",exdir=tempfile())

## ---- importer ----
# Get information about the columns in the file
nes1948imp <- spss.portable.file(path2file)
show(nes1948imp)

## ---- details ----
## Get details about the columns (here, columns 4 to 9 only)
description(nes1948imp)[4:9]

## ---- codebook ----
## Get codebook information for column 5
codebook(nes1948imp[, 5])

## ---- import-subset ----
vote.socdem.48 <- subset(nes1948imp,
              select=c(
                  v480018,
                  v480029,
                  v480030,
                  v480045
                  ))

## ---- import-all ----
socdem.48 <- as.data.set(nes1948imp)

## ---- help-importers ----
## Go to help page for 'importers'
help(spss.portable.file)

## ---- vignette-anes48 ----
vignette("anes48")

## ---- sec-5.3 ----

## ---- eaus-rain ----
webroot <- "http://www.bom.gov.au/web01/ncc/www/cli_chg/timeseries/"
rpath <- paste0(webroot, "rain/0112/eaus/", "latest.txt")
totrain <- read.table(rpath)

## ---- getbom ----
getbom <-
function(suffix=c("AVt","Rain"), loc="eaus"){
        webroot <- "http://www.bom.gov.au/web01/ncc/www/cli_chg/timeseries/"
        midfix <- switch(suffix[1], AVt="tmean/0112/", Rain="rain/0112/")
        webpage <- paste(webroot, midfix, loc, "/latest.txt", sep="")
        print(webpage)
        read.table(webpage)$V2
        }
##
## Example of use
offt = c(seaus=14.7, saus=18.6, eaus=20.5,  naus=24.7, swaus=16.3,
         qld=23.2, nsw=17.3, nt=25.2,sa=19.5, tas=10.4, vic=14.1,
         wa=22.5, mdb=17.7, aus=21.8)
z <- list()
for(loc in names(offt))z[[loc]] <- getbom(suffix="Rain", loc=loc)
bomRain <- as.data.frame(z)

## ---- WDI-pkg ----
library(WDI)
WDIsearch('co2')[1:4,]

## ---- WDI-fun ----
library(WDI)
inds <- c('SP.DYN.TFRT.IN','SP.DYN.LE00.IN', 'SP.POP.TOTL',
 'NY.GDP.PCAP.CD', 'SE.ADT.1524.LT.FE.ZS')
indnams <- c("fertility.rate", "life.expectancy", "population",
             "GDP.per.capita.Current.USD", "15.to.25.yr.female.literacy")
names(inds) <- indnams
wdiData <- WDI(country="all",indicator=inds, start=1960, end=2013, extra=TRUE)
colnum <- match(inds, names(wdiData))
names(wdiData)[colnum] <- indnams
## Drop unwanted "region"
WorldBank <- droplevels(subset(wdiData, !region %in% "Aggregates"))

## ---- quakes-data ----
## Input data from internet
from <-
  paste(c("http://wfs-beta.geonet.org.nz/",
          "geoserver/geonet/ows?service=WFS",
          "&version=1.0.0",
          "&request=GetFeature",
          "&typeName=geonet:quake",
          "&outputFormat=csv",
          "&cql_filter=origintime>='2009-08-01'",
          "+AND+magnitude>4.5"),
        collapse="")
quakes <- read.csv(from)
z <- strsplit(as.character(quakes$origintime),
              split="T")
quakes$Date <- as.Date(sapply(z, function(x)x[1]))
quakes$Time <- sapply(z, function(x)x[2])

## ---- load-XML ----
library(XML)

## ---- input-1920 ----
url <- "http://www.planecrashinfo.com/1920/1920.htm"
to1920 <- readHTMLTable(url, header=TRUE)
to1920 <- as.data.frame(to1920)

## ---- input-from2010 ----
url <- paste0("http://www.planecrashinfo.com/",
              2010:2014, "/", 2010:2014, ".htm")
tab <- sapply(url, function(x)readHTMLTable(x, header=TRUE))

## ---- input-longwinded ----
## The following less efficent alternative code spells the steps out in more detail
## tab <- vector('list', 5)
## k <- 0
## for(yr in 2010:2014){
##  k <- k+1
##  url <- paste0("http://www.planecrashinfo.com/", yr, "/", yr, ".htm")
##  tab[[k]] <- as.data.frame(readHTMLTable(url, header=TRUE))
## }

## ---- combine-years ----
## Now combine the 95 separate tables into one
airAccs <- do.call('rbind', tab)
names(airAccs) <- c("Date", "Location/Operator",
                     "AircraftType/Registration", "Fatalities")
airAccs$Date <- as.Date(airAccs$Date, format="%d %b %Y")

## ---- sec-5.4 ----

## ---- DB-create ----
library(DAAG)
library(RSQLite)
driveLite <- dbDriver("SQLite")
con <- dbConnect(driveLite, dbname="hillracesDB")
dbWriteTable(con, "hills2000", hills2000,
             overwrite=TRUE)
dbWriteTable(con, "nihills", nihills,
             overwrite=TRUE)
dbListTables(con)

## ---- DB-access ----
## Get rows 16 to 20 from the nihills DB
dbGetQuery(con,
  "select * from nihills limit 5 offset 15")
dbDisconnect(con)

## ---- sec-5.5 ----

## ---- gzip ----
gzip -9 coral55?.spot
