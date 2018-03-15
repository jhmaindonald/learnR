## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)


## ---- sec-4.1 ----

## ---- vecs ----
c(2,3,5,2,7,1)
3:10 # The numbers 3, 4,.., 10
c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
c("fig","mango","apple","prune")

## ---- mode ----
x <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
mode(x)

## ---- subscripts ----
x <- c(3,11,8,15,12)   # Assign to x the values
x[c(2,4)]              # Extract elements 2 and 4

## ---- negative-subs ----
x <- c(3,11,8,15,12)
x[-c(2,3)]

## ---- logical ----
x>10   # Values are logical (TRUE or FALSE)
x[x > 10]
"John" %in% c("Jeff", "Alan", "John")

## ---- named-els ----
altitude <- c(Cambarville=800, Bellbird=300,
              "Allyn River"=300,
              "Whian Whian"=400,
              Byrangery=200, Conondale=400,
              Bulburin=600)
##
## Names can be used to extract elements
altitude[c("Cambarville", "Bellbird")]

## ---- subset-fun ----
subset(altitude, altitude>400)

## ---- fruit ----
fruit <- c("fig","mango","apple","plum", "fig")

## ---- fruitfac ----
fruitfac <- factor(fruit)

## ---- glycInd ----
glycInd <- c(apple=40, fig=35, mango=55, plum=25)
## Take levels in order of stated glycInd index
fruitfac <- factor(fruit,
                   levels=names(sort(glycInd)))
levels(fruitfac)
unclass(fruitfac)  # Examine stored values

## ---- badlev ----
trt <- c("A","A","Control")
trtfac <- factor(trt,
  levels=c("control","A"))
table(trtfac)

## ---- faclev ----
fruit <- c("fig","mango","apple", "plum","fig")
fruitfac <- factor(fruit)
fruitfac == "fig"

## ---- ordered ----
windowTint <- ordered(rep(c("lo","med","hi"), 2),
                      levels=c("lo","med","hi"))
windowTint
sum(windowTint > "lo")

## ---- fruitfac2 ----
fruitfac <- factor(c("fig","mango","apple","plum", "fig"))

## ---- rem-bylevels ----
ff2 <- fruitfac[!fruitfac %in% c("fig","plum")]
ff2
table(ff2)

## ---- droplev ----
droplevels(ff2)

## ---- isna ----
is.na(c(1, NA, 3, 0, NA))

## ---- eqna ----
c(1, NA, 3, 0, NA) == NA

## ---- sec-4.2 ----

## ---- place-travelbooks ----
## Place the file in the working directory
library(DAAG, quietly=TRUE)
datafile("travelbooks")
travelbooks <- read.table('travelbooks.txt')

## ---- dat2mat ----
travelmat <- as.matrix(travelbooks[, 1:4])
  # From data frame to matrix
newtravelbooks <- as.data.frame(travelmat)
  # From matrix to data frame

## ---- matrix-dim ----
travelmat <- as.matrix(travelbooks[, 1:4])
dim(travelmat)

## ---- matrix-dim-attr ----
attr(travelmat, "dim")

## ---- lgth-tbooks ----
c(dframelgth=length(travelbooks),
  matlgth=length(travelmat))

#### ---- matrix-xtract-rows ----
travelmat[, 4]
travelmat[, "weight"]
travelmat[, 1:3]
travelmat[2,]

## ---- add2df ----
travelbooks$area <- with(travelbooks, width*height)
travelbooks$density <- with(travelbooks,
                            weight/volume)
names(travelbooks)   # Check column names

## ---- ais-sport ----
with(ais, table(sport))

## ---- rowswim ----
rowswim <- with(ais, sport %in% c("Row", "Swim"))
aisRS <- droplevels(subset(ais, rowswim))
xtabs(~sport, data=aisRS)

## ---- rowswim-nodrop ----
xtabs(~sport, data=subset(ais, rowswim))

## ---- na-rows ----
test.df <- data.frame(x=c(1:2,NA), y=1:3)
test.df
## complete.cases()
complete.cases(test.df)
## na.omit()
na.omit(test.df)

## ---- ss 6.2.7 ----
## ---- list-cbr ----
rCBR <- list(society="ssai", branch="Canberra",
             presenter="John",
             tutors=c("Emma", "Chris", "Frank"))

## ---- list-details ----
length(rCBR)      # rCBR has 4 elements
names(rCBR)

## ---- list-el ----
rCBR[4]           # Also a list,  name is 'tutors'

## ---- list-el-contents ----
rCBR[[4]]         # Contents of 4th list element
rCBR$tutors       # Equivalent to rCBR[["tutors"]]


## ---- sec-4.3 ----

## ---- rem-dim ----
travelvec <-  as.matrix(travelbooks[, 1:4])
dim(travelvec) <- NULL  # Columns of travelmat are stacked into one
                        # long vector
travelvec
  # as(travelmat, "vector") is however preferable

## ---- table ----
library(DAAG)        # possum is from DAAG
with(possum, table(Pop, sex))

## ---- tab-na ----
library(DAAG)
table(nswdemo$re74==0, exclude=NULL)

## ---- xtabs ----
xtabs(~ Pop+sex, data=possum)

## ---- UCBdf ----
UCBdf <- as.data.frame.table(UCBAdmissions)
head(UCBdf, n=3)

## ---- admit ----
xtabs(Freq ~  Admit+Dept, data=UCBdf)

## ---- sec-4.4 ----

## ---- mean-na-rm ----
mean(c(1, NA, 3, 0, NA), na.rm=T)

## ---- str-again ----
library(DAAG)
str(possumsites)

## ---- mean-sd ----
mean.and.sd <- function(x){
    av <- mean(x)
    sdev <- sd(x)
    c(mean=av, sd = sdev)   # return value
}

## ---- hetero ----
hetero <- c(.43,.25,.53,.47,.81,.42,.61)
mean.and.sd(hetero)

## ---- mean-sd2 ----
mean.and.sd <- function(x = rnorm(20))
                 c(mean=mean(x), sd=sd(x))
mean.and.sd()
mean.and.sd()

## ---- molclock1-in ----
library(DAAG)
datafile("molclock1")
molclock <-
  read.table("molclock1.txt")

## ---- apply ----
apply(molclock, 2, range)

## ---- apply-UCBAdmit ----
apply(UCBAdmissions, c(1,2), sum)

## ---- sapply ----
sapply(molclock, range)

## ---- sapply-NA ----
sapply(molclock, range,
       na.rm=TRUE)

## ---- countNA ----
countNA <- function(x)sum(is.na(x))

## ---- sapply-countNA ----
library(MASS)
sapply(Pima.tr2[, 1:5], countNA)

## ---- sapply-Pima ----
sapply(Pima.tr2[, 1:5],
       function(x)sum(is.na(x)))

## ---- cuckoos-specs ----
(spec <- levels(cuckoos$species))

## ---- slashdot ----
specnam <- sub("\\.",
               " ", spec)

## ---- spec-sub ----
(specnam <- sub(".", " ", spec, fixed=TRUE))

## ---- elec-dates ----
# Electricity Billing Dates
dat <- c("2003-08-24","2003-11-23","2004-02-22",
         "2004-05-03")
dd <- as.Date(dat)

## ---- date-sub ----
as.Date("1960-12-1") - as.Date("1960-1-1")

## ---- date-diffs ----
dd <- as.Date(c("2003-08-24","2003-11-23",
                "2004-02-22", "2004-05-03"))
diff(dd)

## ---- unclass-diff ----
unclass(diff(dd))

## ---- format-Date ----
dec1 <- as.Date("2004-12-1")
format(dec1, format="%b %d %Y")
format(dec1, format="%a %b %d %Y")

## ---- date-labs ----
## Labeling of graph: data frame jobs (DAAG)
library(DAAG); library(lattice)
fromdate <- as.Date("1Jan1995", format="%d%b%Y")
startofmonth <- seq(from=fromdate, by="1 month",
                    length=24)
atdates <- seq(from=fromdate, by="6 month",
               length=4)
xyplot(BC ~ startofmonth, data=jobs,
       scale=list(x=list(at=atdates,
                         labels=format(atdates,
                                       "%b%y"))))

## ----- julian ----
dates <- as.Date(c("1908-09-17", "1912-07-12"))
julian(dates)
julian(dates, origin=as.Date("1908-01-01"))

## ---- wkdays ----
dates <- as.Date(c("1908-09-17", "1912-07-12"))
weekdays(dates)
months(dates)
quarters(dates)

## ---- intervalCounts ----
intervalCounts <- function(date, from=NULL, to=NULL, interval="1 month"){
  if(is.null(from))from <- min(date)
  if(is.null(to))to <- max(date)
  dateBreaks <- seq(from=from, to=to, by=interval)
  dateBreaks <- c(dateBreaks, max(dateBreaks)+diff(dateBreaks[1:2]))
  cutDates <- cut(date, dateBreaks, right=FALSE)
  countDF <- data.frame(Date=dateBreaks[-length(dateBreaks)],
                        num=as.vector(table(cutDates)))
  countDF
}

## ---- event-dates ----
dates <- c("1908-09-17", "1912-07-12", "1913-08-06", "1913-09-09", "1913-10-17")
dates <- as.Date(dates)
(byYear <- intervalCounts(dates, from=as.Date("1908-01-01"), interval='1 year'))

## ---- countzeros ----
## Define a function that counts zeros
countzeros <- function(x)sum(!is.na(x) & x==0)
aggregate(nswdemo[, c("re74", "re75", "re78")],
          by=list(group=nswdemo$trt),
          FUN=countzeros)

## ---- countprop ----
## countprop() counts proportion of zero values
countprop <- function(x){
    sum(!is.na(x) & x==0)/length(na.omit(x))}
prop0 <-
  aggregate(nswdemo[, c("re74","re75","re78")],
            by=list(group=nswdemo$trt),
            FUN=countprop)
## Now improve the labeling
rownames(prop0) <- c("Control", "Treated")
round(prop0,2)

## ---- prop0 ----
prop0 <-
  sapply(split(nswdemo[, c("re74","re75","re78")],
               nswdemo$trt),
         FUN=function(z)sapply(z, countprop))
round(t(prop0), 2)

## ---- sec-4.5 ----

## ---- spatPointsDF ----
library(sp)
data(meuse)
class(meuse)
coordinates(meuse) <- ~ x + y
class(meuse)

## ---- bubble ----
bubble(meuse, zcol="zinc", scales=list(tck=0.5),
       maxsize=2, xlab="Easting", ylab="Northing")

## ---- slotNames ----
slotNames(meuse)

## ---- sec-4.8 ----

## ---- naINassign ----
y <- c(1, NA, 3, 0, NA)
y[y > 0]
y[y > 0] <- c(11, 12)

