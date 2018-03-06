## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- sec-5.1 ----

## ---- load-MASS ----
## Below, the dataset MASS::mammals will be required
library(MASS, quietly=TRUE)

## ---- brain-bodyA ----
plot(brain ~ body, data=mammals)
mtext(side=3, line=0.5, adj=0, "A: Unlogged data")

## ---- brain-bodyB ----
plot(brain ~ body, data=mammals, log="xy")
mtext(side=3, line=0.5, adj=0,
      "B: Log scales (both axes)")

## ---- plot-trees ----
## Code used for the plot
plot(trees, cex.labels=1.5)
  # Calls pairs(trees)

## ---- sec-5.2

## ---- load-DAAG ----
library(DAAG, quietly=TRUE)

## ---- str ----
str(worldRecords, vec.len=3)

## ---- trackVSroad ----
## Code
library(lattice)
xyplot(Time ~ Distance, scales=list(tck=0.5),
       groups=roadORtrack, data=worldRecords,
       auto.key=list(columns=2), aspect=1)
## On a a colour device the default is to use
## different colours, not different symbols,
## to distinguish groups.

## ---- wrTimesA ----
## Code for Left panel
parset <- list(layout.widths=list(left.padding=2.5,
                                  right.padding=2.5))
xyplot(log(Time) ~ log(Distance),
       groups=roadORtrack, data=worldRecords,
       scales=list(tck=0.5),
       par.settings=parset,
       auto.key=list(columns=2), aspect=1)

## ---- wrTimesA-nopad ----
## Code for Left panel
xyplot(log(Time) ~ log(Distance),
       groups=roadORtrack, data=worldRecords,
       scales=list(tck=0.5),
       auto.key=list(columns=2), aspect=1)

## ---- wrTimesB ----
## Right panel
xyplot(Time ~ Distance, groups=roadORtrack,
       data=worldRecords,
       scales=list(log=10, tck=0.5),
       auto.key=list(columns=2), aspect=1)

## ---- logTimeDist ----
worldrec.lm <- lm(log(Time) ~ log(Distance),
                  data=worldRecords)

## ---- worldrec-lm-coef ----
coef(worldrec.lm)

## ---- timevsDist ----
plot(log(Time) ~ log(Distance),
     data = worldRecords)
abline(worldrec.lm)

## ---- diag12 ----
## Code
plot(worldrec.lm, which=c(1,5),
     sub.caption=rep("",2))

## ---- row40 ----
worldRecords["40", ]

## ---- worldrec-lminfo ----
print(worldrec.lm)    # Alternatively, type worldrec.lm
summary(worldrec.lm)

## ---- objnames ----
names(worldrec.lm)

## ---- obj-call ----
worldrec.lm$call


## ---- str-nihills ----
str(nihills)

## ---- showcorr ----
showcorr <- function(x,y,...){
    panel.xyplot(x,y,...)
    xy <- current.panel.limits()
    rho <- paste(round(cor(x,y),3))
    eps <- 0.035*diff(range(y))
    panel.text(max(x), min(y)+eps, rho,
               pos=2, offset=-0.2)
}

## ---- nihills-spmA ----
## Scatterplot matrix; unlogged data
library(lattice)
splom(~nihills,  xlab="",
       main=list("A: Untransformed data", x=0,
       just="left", fontface="plain"))

## ---- nihills-spmB ----
lognihills <- log(nihills)
names(lognihills) <- paste0("l", names(nihills))
## Scatterplot matrix; log scales
splom(~ lognihills, lower.panel=showcorr, xlab="",
       main=list("B: Log transformed data", x=0,
       just="left", fontface="plain"))

## ---- newnames ----
newnames <- c("ldist", "lclimb", "ltime", "ltimef")
names(lognihills) <- newnames

## ---- cor-ni ----
round(cor(nihills),3)

## ---- cor-logni ----
round(cor(lognihills),3)

## ---- regress2 ----
nihills.lm <- lm(ltime ~ lclimb + ldist,
                 data=lognihills)

## ---- sec-5.4 ----

## ---- gg-tomato ----
## Code
library(ggplot2)
tomato <- within(tomato, trt <- relevel(trt, ref="water only"))
quickplot(weight, trt, data=tomato, xlab="Weight (g)", ylab="")

## ---- aov-tomato ----
tomato.aov <- aov(weight ~ trt, data=tomato)
round(coef(summary.lm(tomato.aov)), 3)

## ---- do-interact ----
## Code
library(DAAG)
with(rice, interaction.plot(x.factor=fert,
                            trace.factor=variety,
                            ShootDryMass,
                            cex.lab=1.4))

## ---- sec-5.5 ----

## ---- MDBrainfall ----
## Code
library(DAAG)
plot(mdbRain ~ Year, data=bomregions2012)
## Calculate and plot curve showing long-term trend
with(bomregions2012, lines(lowess(mdbRain ~ Year, f=2/3), lty=2))
## Calculate and plot curve of short-term trends
with(bomregions2012, lines(lowess(mdbRain ~ Year, f=0.1),
                           lty=1, col="gray45"))

## ---- sec-5.6 ----

## ---- Huron-ex ----
Year=as(time(LakeHuron), "vector")
huron <- data.frame(year=Year, mean.height=LakeHuron)

## ---- lag1 ----
lag.plot(huron$mean.height)
