## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- ss-12.1.1 ----

## ---- loadDAAG ----
library(DAAG)

## ---- smooth-ohms ----
## Plot points
plot(ohms ~ juice, data=fruitohms)
## Add smooth curve, using default
## smoothing window
with(fruitohms,
     lines(lowess(ohms ~ juice), col="gray", lwd=2))

## ---- plotVIS ----
## Code
library(DAAGviz)
library(mgcv)
eyeAmpM.gam <- gam(amp ~ s(x,y), data=subset(eyeAmp, Sex=="m"))
eyeAmpF.gam <- gam(amp ~ s(x,y), data=subset(eyeAmp, Sex=="f"))
lims <- range(c(predict(eyeAmpF.gam), predict(eyeAmpM.gam)))
vis.gam(eyeAmpM.gam, plot.type='contour', color="cm", zlim=lims, main="")
mtext(side=3, line=0.5, adj=0, "A: Response amplitudes, Males")
vis.gam(eyeAmpF.gam, plot.type='contour', color="cm", zlim=lims, main="")
mtext(side=3, line=0.5, adj=0, "B: Response amplitudes, Females")

## ---- sec-12.2 ----

## ---- ant111b ----
# ant111b is in DAAG
Site <- with(ant111b, reorder(site, harvwt,
                              FUN=mean))
stripplot(Site ~ harvwt, data=ant111b,
          scales=list(tck=0.5),
          xlab="Harvest weight of corn")

## ---- sec-12.3 ----

## ---- ss-12.3.1 ----

## ---- getErie ----
Erie <- greatLakes[,"Erie"]

## ---- Erie1 ----
## Code
plot(Erie, xlab="",
     ylab="Level (m)")

## ---- ar-Erie ----
ar(Erie)

## ---- ar1-Erie ----
ar(Erie, order.max=1)

## ---- lagErie ----
## Panel A
lag.plot(Erie, lags=3,
         do.lines=FALSE,
         layout=c(1,3),
         main="  ")
mtext(side=3, line=2, adj=0,
      "A: Lag plots, at lags of 1, 2 and 3")

## ---- acfErie ----
## Panel B
acf(Erie, main="")
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
plot(obj,
     shift=mean(df$height),
     residuals=TRUE, pch=1,
     xlab="",
     ylab="Height of lake")

## ---- ss-12.3.2 ----

## ---- arima-sim ----
for (i in 1:6){
ysim <-
  arima.sim(list(ar=0.85),
            n=200)
df <- data.frame(x=1:200,
                 y=ysim)
df.gam <- gam(y ~ s(x),
              data=df)
plot(df.gam,
     ylab=paste("Sim", i),
     residuals=TRUE)
}

## ---- Erie-fcast ----
erie.ar <- arima(Erie,
            order=c(1,0,0))
library(forecast)
fc <- forecast(erie.ar,
               h=15)
plot(fc, main="",
     ylab="Lake level (m)")
  # 15 time points ahead

## ---- ss-12.3.3 ----

## ---- mdb-gam ----
## Code
mdbRain.gam <- gam(mdbRain ~ s(Year) + s(SOI),
                   data=bomregions)
plot(mdbRain.gam, residuals=TRUE, se=2, pch=1,
     select=1, cex=1.35, ylab="Partial of Year")
mtext(side=3, line=0.75, "A: Effect of Year", adj=0)
plot(mdbRain.gam, residuals=TRUE, se=2, pch=1,
     select=2, cex=1.35, ylab="Partial of SOI")
mtext(side=3, line=0.75, "B: Effect of SOI", adj=0)

## ---- ar1sims ----
opar <- par(mar=c(2.1, 3.1, 0.25, 1.1), mgp=c(2.25,0.5,0))
mdbRain.gam <- gam(mdbRain ~ s(Year) + s(SOI),
                   data=bomregions)
n <-  dim(bomregions)[1]
acf(resid(mdbRain.gam), ylab="MDB series")
for(i in 1:5)acf(rnorm(n), ylab=paste("Sim",i),
                 col="gray40")
par(opar)

## ---- use-eventCounts ----
## Code
airAccs <- gamclass::airAccs
fromDate <- as.Date("2006-01-01")
dfDay06 <- gamclass::eventCounts(airAccs, dateCol="Date", from= fromDate,
                                 by="1 day", prefix="num")
dfDay06$day <- julian(dfDay06$Date, origin=fromDate)
dfWeek06 <- gamclass::eventCounts(airAccs, dateCol="Date", from=fromDate,
                                  by="1 week", prefix="num")
dfWeek06$day <- julian(dfWeek06$Date, origin=fromDate)

## ---- plotGAM ----
library(mgcv)
year <- seq(from=fromDate, to=max(dfDay06$Date), by="1 year")
atyear <- julian(year, origin=fromDate)
dfDay06.gam <- gam(formula = num ~ s(day, k=200), family = quasipoisson,
                   data = dfDay06)
av <- mean(predict(dfDay06.gam))
plot(dfDay06.gam, xaxt="n", shift=av, trans=exp, rug=FALSE, xlab="",
     ylab="Estimated rate per day")
axis(1, at=atyear, labels=format(year, "%Y"))
mtext(side=3, line=0.75, "A: Events per day, vs date", adj=0)
dfWeek06.gam <- gam(num~s(day, k=200), data=dfWeek06, family=quasipoisson)
av <- mean(predict(dfWeek06.gam))
plot(dfWeek06.gam, xaxt="n", shift=av, trans=exp, rug=FALSE, xlab="",
      ylab="Estimated rate per week")
axis(1, at=atyear, labels=format(year, "%Y"))
mtext(side=3, line=0.75, "B: Events per week, vs date", adj=0)

## ---- sec-12.4 ----

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
library(lattice)
scores <- predict(fgl.lda)$x
xyplot(scores[,2] ~ scores[,1], groups=fgl$type,
       xlab="Discriminant 1",
       ylab="Discriminant 2",
       aspect=1, scales=list(tck=0.4),
       auto.key=list(space="right"),
       par.settings=simpleTheme(alpha=0.6, pch=1:6))

## ---- sec-12.5 ----

## ---- load-rpart ----
library(rpart)

## ---- bronchit-first3 ----
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

## ---- ss-12.5.1 ----

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

## ---- sec-12.6 ----

## ---- ss-12.7.1 ----

## ---- aupoints ----
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

## ---- load-oz ----
library(oz, quietly=TRUE)

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
oz()
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
oz()
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
oz()
points(aulatlong, col="red", pch=16, cex=1.5)
wt <- apply(as.matrix(audists), 1,function(x)sum(1/x[x>0]^2))
aupoints.mds <- isoMDS(audists, as.matrix(aulatlong))
align2D(x1=aupoints.mds$points[,1],
        x2 = aupoints.mds$points[,2], wts=wt)

