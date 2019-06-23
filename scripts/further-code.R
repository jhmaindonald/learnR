## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- loadDAAG ----
library(DAAG)

## ---- loadlattice ----
library(lattice)

## ---- sec-9.1 ----

## ---- ant111b ----
ant111b <- DAAG::ant111b
Site <- with(ant111b, reorder(site, harvwt,
                              FUN=mean))
lattice::stripplot(Site ~ harvwt, data=ant111b,
          scales=list(tck=0.5),
          xlab="Harvest weight of corn")

## ---- sec-9.2 ----

## ---- ss-9.2.1 ----

## ---- getErie ----
Erie <- DAAG::greatLakes[,"Erie"]

## ---- Erie1 ----
plot(Erie, xlab="", fg="gray",
     ylab="Level (m)")

## ---- ar-Erie ----
ar(Erie)

## ---- ar1-Erie ----
ar(Erie, order.max=1)

## ---- lagErie ----
## Panel A
lag.plot(Erie, lags=3,
         do.lines=FALSE,
         layout=c(1,3), fg="gray",
         main="  ")
mtext(side=3, line=2, adj=0,
      "A: Lag plots, at lags of 1, 2 and 3")

## ---- acfErie ----
## Panel B
acf(Erie, main="", fg="gray")
mtext(side=3, line=1, adj=0, cex=1.25,
      "B: Autocorrelation function")

## ---- gamErie ----
## Code
library(mgcv)
df <-  data.frame(
  height=as.vector(Erie),
  year=time(Erie))
obj <- gam(height ~ s(year),
           data=df)
plot(obj, fg="gray",
     shift=mean(df$height),
     residuals=TRUE, pch=1,
     xlab="",
     ylab="Height of lake")

## ---- ss-9.2.3 ----

## ---- arima-sim ----
for (i in 1:6){
ysim <-
  arima.sim(list(ar=0.85),
            n=200)
df <- data.frame(x=1:200,
                 y=ysim)
df.gam <- gam(y ~ s(x),
              data=df)
plot(df.gam, fg="gray",
     ylab=paste("Sim", i),
     residuals=TRUE)
}

## ---- Erie-fcast ----
erie.ar <- arima(Erie,
            order=c(1,0,0))
library(forecast)
fc <- forecast(erie.ar,
               h=15)
plot(fc, main="", fg="gray",
     ylab="Lake level (m)")
  # 15 time points ahead

## ---- ss-9.2.4 ----

## ---- mdb-gam ----
## Code
bomregions <- DAAG::bomregions2018
mdbRain.gam <- gam(mdbRain ~ s(Year) + s(SOI),
                   data=bomregions)
plot(mdbRain.gam, residuals=TRUE, se=2, fg="gray",
     pch=1, select=1, cex=1.35, ylab="Partial, Year")
mtext(side=3, line=0.75, "A: Effect of Year", adj=0)
plot(mdbRain.gam, residuals=TRUE, se=2, fg="gray",
     pch=1, select=2, cex=1.35, ylab="Partial, SOI")
mtext(side=3, line=0.75, "B: Effect of SOI", adj=0)

## ---- ar1sims ----
opar <- par(mar=c(2.1, 3.1, 0.25, 1.1), mgp=c(2.25,0.5,0))
mdbRain.gam <- gam(mdbRain ~ s(Year) + s(SOI),
                   data=bomregions)
n <-  dim(bomregions)[1]
acf(resid(mdbRain.gam), ylab="MDB series")
for(i in 1:5)acf(rnorm(n), ylab=paste("Sim",i),
                 fg="gray", col="gray40")
par(opar)

## ---- ss-9.2.6 ----

## ---- use-eventCounts ----
## Code
airAccs <- gamclass::airAccs
fromDate <- as.Date("2006-01-01")
dfWeek06 <- gamclass::eventCounts(airAccs, dateCol="Date",
                                  from=fromDate,
                                by="1 week", prefix="num")
dfWeek06$day <- julian(dfWeek06$Date, origin=fromDate)

## ---- plotGAM-byWk ----
## Code
library(mgcv)
year <- seq(from=fromDate, to=max(dfWeek06$Date), by="1 year")
at6 <- julian(seq(from=fromDate, to=max(dfWeek06$Date), by="6 months"), origin=fromDate)
atyear <- julian(year, origin=fromDate)
dfWeek06.gam <- gam(num~s(day, k=200), data=dfWeek06, family=quasipoisson)
avWk <- mean(predict(dfWeek06.gam))
plot(dfWeek06.gam, xaxt="n", shift=avWk, trans=exp, rug=FALSE,
     xlab="", ylab="Estimated rate per week", fg="gray")
axis(1, at=atyear, labels=format(year, "%Y"), lwd=0, lwd.ticks=1)
abline(h=0.5+(1:4)*0.5, v=at6, col="gray", lty=3, lwd=0.5)
# mtext(side=3, line=0.75, "A: Events per week, vs date", adj=0)

## ---- plotGAM-byDay ----
dfDay06 <- gamclass::eventCounts(airAccs, dateCol="Date", from= fromDate,
                                 by="1 day", prefix="num")
dfDay06$day <- julian(dfDay06$Date, origin=fromDate)
year <- seq(from=fromDate, to=max(dfDay06$Date), by="1 year")
dfDay06.gam <- gam(formula = num ~ s(day, k=200), family = quasipoisson,
                   data = dfDay06)
avDay <- mean(predict(dfDay06.gam))
plot(dfDay06.gam, xaxt="n", shift=avDay, trans=exp, rug=FALSE,
     fg="gray", xlab="", ylab="Estimated rate per day")
axis(1, at=atyear, labels=format(year, "%Y"))
# mtext(side=3, line=0.75, "B: Events per day, vs date", adj=0)

## ---- sec-9.3 ----

## ---- loadMASS ----
library(MASS, quietly=TRUE)

## ---- fgl-ldaCV ----
fglCV.lda <- lda(type ~ ., data=fgl, CV=TRUE)
tab <- table(fgl$type, fglCV.lda$class)
## Confusion matrix
print(round(apply(tab, 1, function(x)x/sum(x)),
            digits=3))

## ---- confusion ----
library(DAAG)
confusion(fgl$type, fglCV.lda$class)

## ---- fgl-lda ----
fgl.lda <- lda(type ~ ., data=fgl)

## ---- fgl-scores2D ----
## Code
library(lattice)
scores <- predict(fgl.lda)$x
xyplot(scores[,2] ~ scores[,1], groups=fgl$type,
       xlab="Discriminant 1",
       ylab="Discriminant 2",
       aspect=1, scales=list(tck=0.4),
       auto.key=list(space="right"),
       par.settings=simpleTheme(alpha=0.6, pch=1:6))

## ---- sec-9.4 ----

## ---- load-rpart ----
library(rpart)

## ---- bronchit-first3 ----
bronchit <- DAAGviz::bronchit
head(bronchit, 3)

## ---- bronchit-rfac ----
## Now make the outcome variable a factor
bronchit <-
  within(bronchit,
         rfac <- factor(r, labels=c("abs","pres")))

## ---- bronchit-rp ----
set.seed(47)   # Reproduce tree shown
b.rpart <- rpart(rfac ~ cig+poll, data=bronchit,
                 method="class")

## ---- treefig ----
opar <- par(mar=rep(1.1,4))
plot(b.rpart)
text(b.rpart, xpd=TRUE)
par(opar)

## ---- ss-9.4.1 ----

## ---- rf-x-bronchit ----
opar <- par(mar=rep(2.1,4))
set.seed(31)   # Reproduce the trees shown
num <- 1:nrow(bronchit)
for(i in 1:12){
  useobs <- sample(num, replace=TRUE)
  dset <- bronchit[useobs, ]
  b.rpart <- rpart(rfac ~ cig+poll, data=dset,
                   control=rpart.control(maxdepth=2))
  plot(b.rpart, uniform=TRUE)
  text(b.rpart, xpd=TRUE, cex=1.2)
}
par(opar)

## ---- loadrf ----
library(randomForest, quietly=TRUE)

## ---- rf-bronchit ----
(bronchit.rf <- randomForest(rfac ~ cig+poll,
                             data=bronchit))

## ---- proximity-plot ----
parset <- simpleTheme(pch=1:2)
bronchit.rf <- randomForest(rfac ~ cig+poll,
                            proximity=TRUE,
                            data=bronchit)
points <- cmdscale(1-bronchit.rf$proximity)
xyplot(points[,2] ~ points[,1],
       groups=bronchit$rfac,
       xlab="Axis 1", ylab="Axis 2",
       par.settings=parset, aspect=1,
       auto.key=list(columns=2))

## ---- fgl-rf ----
(fgl.rf <- randomForest(type ~ ., data=fgl))

## ---- sec-9.5 ----

## ---- ss-9.5.1 ----

## ---- aupoints ----
audists <- DAAG::audists
aupts <- cmdscale(audists)
plot(aupts, axes=FALSE, ann=FALSE, fg="gray",
     frame.plot=TRUE)
city <- rownames(aupts)
pos <- rep(1,length(city))
pos[city=="Melbourne"]<- 3
pos[city=="Canberra"] <- 4
par(xpd=TRUE)
text(aupts, labels=city, pos=pos)
par(xpd=FALSE)

## ---- cfPhysical ----
align2D <- function(lat=aulatlong$latitude,
                    long=aulatlong$longitude,
                    x1=aupts[,1], x2 = aupts[,2],
                    wts=NULL){
    ## Get best fit in space of (latitude, longitude)
    if(is.null(wts))wts <- rep(1,length(x1))
    fitlat <- predict(lm(lat ~ x1+x2, weights=wts))
    fitlong <- predict(lm(long ~ x1+x2, weights=wts))
    list(fitlat = fitlat, fitlong=fitlong, lat=lat, long=long)
}

## ---- au-overlay ----
oz::oz()
aulatlong <- DAAG::aulatlong
fitcoords <- align2D(lat=aulatlong$latitude,
                      long=aulatlong$longitude,
                      x1=aupts[,1], x2 = aupts[,2],
                      wts=NULL)
x <-with(fitcoords,
         as.vector(rbind(lat, fitlat, rep(NA,length(lat)))))
y <-with(fitcoords,
         as.vector(rbind(long, fitlong, rep(NA,length(long)))))
points(aulatlong, col="red", pch=16, cex=1.5)
lines(x, y, col="gray40", lwd=3)

## ---- au-sammon ----
oz::oz()
aupts.sam <- sammon(audists, trace=FALSE)$points
wt <- apply(as.matrix(audists), 1,function(x)sum(1/x[x>0]))
fitcoords <- align2D(lat=aulatlong$latitude,
                     long=aulatlong$longitude,
                     x1=aupts.sam[,1], x2 = aupts.sam[,2],
                     wts=wt)
x <-with(fitcoords,
         as.vector(rbind(lat, fitlat, rep(NA,length(lat)))))
y <-with(fitcoords,
         as.vector(rbind(long, fitlong, rep(NA,length(long)))))
points(aulatlong, col="red", pch=16, cex=1.5)
lines(x, y, col="gray40", lwd=3)

## ---- au-mds ----
oz::oz()
points(aulatlong, col="red", pch=16, cex=1.5)
wt <- apply(as.matrix(audists), 1,function(x)sum(1/x[x>0]^2))
aupoints.mds <- isoMDS(audists, as.matrix(aulatlong))
align2D(x1=aupoints.mds$points[,1],
        x2 = aupoints.mds$points[,2], wts=wt)


