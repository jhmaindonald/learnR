## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- sec-8.1 ----

## ---- sec-8.2 ----

## ---- help-UCB ----
help(UCBAdmissions)     # Get details of the data
example(UCBAdmissions)

## ---- str-UCB ----
str(UCBAdmissions)

## ---- tab-UCB ----
UCBdf <- as.data.frame.table(UCBAdmissions)
head(UCBdf, 5)

## ---- UCBdf-plyr ----
library(plyr)
UCBdf <- adply(.data=UCBAdmissions,
               .margins=1:3,
               .fun=identity)
names(UCBdf)[4] <- "Freq"

## ---- overall-rate ----
library(dplyr)
gpUCBgender <- dplyr::group_by(UCBdf, Gender)
AdmitRate <- dplyr::summarise(gpUCBgender,
                              Accept=sum(Freq[Admit=="Admitted"]),
                              Total=sum(Freq),
                              pcAccept=100*Accept/Total)
AdmitRate

## ---- byDept ----
gpUCBgd <- dplyr::group_by(UCBdf, Gender, Dept)
rateDept <- dplyr::summarise(gpUCBgd,
    Total=sum(Freq),
    pcAccept=100*sum(Freq[Admit=="Admitted"])/Total)

## ---- acceptByDept ----
xtabs(pcAccept~Gender+Dept, data=rateDept)

## ---- totsByDept ----
xtabs(Total~Gender+Dept, data=rateDept)


## ---- margin21 ----
## Tabulate by Admit (margin 2) & Gender (margin 1)
(margin21 <- margin.table(UCBAdmissions,
                          margin=2:1))

## ---- props-by-row ----
prop.table(margin21, margin=1)

## ---- props ----
props <- function(x, elem=1)sum(x[elem])/sum(x)

## ---- cut-bronchit ----
library(DAAGviz)
catcig <- with(bronchit,
               cut(cig, breaks=c(0,1,10,30),
                   include.lowest=TRUE))
tab <- with(bronchit, table(r, catcig))
round(apply(tab, 2, props, elem=2), 3)

## ---- matrix-manip ----
X + Y             # Elementwise addition
X * Y             # Elementwise multiplication
X %*% B           # Matrix multiplication
solve(X, Y)       # Solve X B = Y for B
svd(X)            # Singular value decomposition
qr(X)             # QR decomposition
t(X)              # Transpose of X

## ---- dream-enter ----
movesArray <-
   matrix(c(5,3,17,85), ncol=2,
          dimnames=list("Dreamer"=c("Yes","No"),
                        "Object"=c("Yes","No")))
movesTab <- as.table(movesArray)
movesTab

## ---- tab2df ----
as.data.frame(movesTab)

## ---- mat2df ----
as.data.frame(movesArray)

## ---- sec-8.3 ----

## ---- adplyDream ----
detach("package:dplyr")
library(plyr)
dreamMoves <-
   matrix(c(5,3,17,85), ncol=2,
          dimnames=list("Dreamer"=c("Yes","No"),
                        "Object"=c("Yes","No")))
(dfdream <- plyr::adply(dreamMoves, 1:2,
                        .fun=identity))

## ---- daply ----
plyr::daply(dfdream, 1:2, function(df)df[,3])

## ---- aaply ----
plyr::aaply(UCBAdmissions, 1:2, sum)

## ---- numEq0by ----
library(DAAG, quietly=TRUE)
plyr::daply(nswdemo, .(trt),
      function(df)sum(df[,"re74"]==0, na.rm=TRUE))

## ---- numEq0cols ----
options(digits=3)
plyr::daply(nswdemo, .(trt, black),
      function(df)sum(df[,"re75"]==0)/nrow(df))

## ---- numEq0colw ----
plyr::ddply(nswdemo, .(trt, black),
      colwise(function(x)sum(x==0)/length(x),
              .cols=.(re75, re78)))

## ---- lefrt ----
library(DAAG)
detach("package:plyr")
library(dplyr)
names(cricketer)[1] <- "hand"
gpByYear <- group_by(cricketer, year)
lefrt <- dplyr::summarise(gpByYear,
                          left=sum(hand=='left'),
                          right=sum(hand=='right'))
## Check first few rows
lefrt[1:4, ]

## ---- ww1 ----
## Use subset() from base R
ww1kia <- subset(cricketer,
                 kia==1 & (year+life)%in% 1914:1918)
range(ww1kia$year)

## ---- ww1-dplyr ----
ww1kia <- filter(cricketer,
                 kia==1, (year+life)%in% 1914:1918)

## ---- kia-prop ----
## Use filter(), group_by() and summarise() from dplyr
crickChoose <- filter(cricketer,
                      year%in%(1869:1896), ((kia==1)|(year+life)>1918))
gpByYearKIA <- group_by(crickChoose, year)
crickKIAyrs <- dplyr::summarise(gpByYearKIA,
                                kia=sum(kia), all=length(year), prop=kia/all)
crickKIAyrs[1:4, ]

## ---- dplyr-intro ----
vignette("introduction", package="dplyr")

## ---- Crimean ----
## Create dataset Crimean, for use in later calculations
library(HistData)   # Nightingale is from this package
library(reshape2)   # Has the function melt()
Crimean <- melt(Nightingale[,c(1,8:10)], "Date")
names(Crimean) <- c("Date", "Cause", "Deaths")
Crimean$Cause <- factor(sub("\\.rate", "", Crimean$Cause))
Crimean$Regime <- ordered(rep(c(rep('Before', 12), rep('After', 12)), 3),
                          levels=c('Before', 'After'))
formdat <- format.Date(sort(unique(Crimean$Date)), format="%d %b %y")
Crimean$Date <- ordered(format.Date(Crimean$Date,
                        format="%b %y"), levels=formdat)

## ---- wdiEx ----
options(width=84)
## DAAGviz must be installed, need not be loaded
path2file <- system.file("datasets/wdiEx.csv", package="DAAGviz")
wdiEx <- read.csv(path2file)
print(wdiEx, row.names=FALSE)
options(width=54)

## ---- wdiLong ----
library(reshape2)
wdiLong <- melt(wdiEx, id.vars=c("Country.Code",
                "Indicator.Name"),
                measure.vars=c("X2000", "X2010"))
## More simply: wdiLong <- melt(wdiEx[, -c(2,4)])
wdiLong

## ---- fac2var ----
names(wdiLong)[3] <- "Year"
wdiData <- dcast(wdiLong,
                 Country.Code+Year ~ Indicator.Name,
                 value.var="value")
wdiData

## ---- fix-Year ----
wdiData <- within(wdiData, {
   levels(Year) <- substring(levels(Year),2)
   Year <- as.numeric(as.character(Year))
})
wdiData

## ---- sec-8.4 ----

## ---- scan-twice ----
colnam <- scan("molclock1.txt", nlines=1, what="")
molclock <- scan("molclock1.txt", skip=1,
                 what=c(list(""), rep(list(1),5)))
molclock <- data.frame(molclock, row.names=1)
  # Column 1 supplies row names
names(molclock) <- colnam

## ---- sec-8.5 ----

## ---- getbom ----
`getbom` <-
  function(suffix=c("AVt","Rain"), loc="eaus"){
    webroot <- "http://www.bom.gov.au/web01/ncc/www/cli_chg/timeseries/"
    midfix <- switch(suffix[1], AVt="tmean/0112/", Rain="rain/0112/")
    webpage <- paste0(webroot, midfix, loc, "/latest.txt")
    print(webpage)
    nam <- switch(loc, seaus="se", saus="south", eaus="east", naus="north",
                  swaus="sw", qld="QLD", nsw="NSW", nt="NT",sa="SA",
                  tas="TAS", vic="VIC", wa="WA")
    x <- read.table(webpage)$V2
    if(suffix[1]%in%c("avt","AVt")){
      off <- switch(loc, seaus=14.72, saus=18.58, eaus=20.51,  naus=24.71,
                    swaus=16.3, qld=23.23, nsw=17.33, nt=25.17,sa=19.45,
                    tas=10.35, vic=14.10, wa=22.47)
      x <- c(rep(NA,10), x+off)
    }
    x
  }
##
## Example of use
seRain <- getbom(suffix="Rain", loc="seaus")

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
}
## Create a motion chart
WorldBank <- droplevels(subset(wdiData, !region %in% "Aggregates"))

## ---- getWBD-fun ----
getWorldBankData <- function(id='SP.POP.TOTL', date='1960:2010',
                             value="value", per.page=12000){
  require(RJSONIO)
  url <- paste0("http://api.worldbank.org/countries/all/indicators/",
                id, "?date=", date, "&format=json&per_page=",
                per.page)

  wbData <- fromJSON(url)[[2]]
  ## Data is accessed in Javascript Object Notation (JSON)

  wbData = data.frame(
    year = as.numeric(sapply(wbData, "[[", "date")),
    value = as.numeric(sapply(wbData, function(x)
      ifelse(is.null(x[["value"]]),NA, x[["value"]]))),
    country.name = sapply(wbData, function(x) x[["country"]]['value']),
    country.id = sapply(wbData, function(x) x[["country"]]['id'])
    )

  names(wbData)[2] <- value

  return(wbData)
}
## Obtain data for total population (SP.POP.TOTL)
wbData <- getWorldBankData(id="SP.POP.TOTL")

## ---- webpage-tab ----
library(XML)
u = "http://en.wikipedia.org/wiki/List_of_countries_by_population"
tables <- readHTMLTable(u)
sapply(tables, length)

## ---- webpage-tab2 ----
names(tables[[1]])

## ---- sec-8.6 ----

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

## ---- sec-8.7 ----

## ---- dump-vol-wt ----
volume <- c(351, 955, 662, 1203, 557, 460)
weight <- c(250, 840, 550, 1360, 640, 420)
dump(c("volume", "weight"), file="books.R")

## ---- source-bks ----
source("books.R")

## ---- sec-8.8 ----

## ---- big-mat ----
xy <- matrix(rnorm(5*10^7), ncol=100)
dim(xy)

## ---- sys-time ----
system.time(xy+1)
xy.df <- data.frame(xy)
system.time(xy.df+1)

## ---- vignette ----
vignette("datatable-intro", package="data.table")

## ---- gzip ----
gzip -9 coral55?.spot
