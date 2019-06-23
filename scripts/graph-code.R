## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- load-DAAG ----
## DAAG has several datasets that will be required
library(DAAG, quietly=TRUE)

## ---- sec-7.1 ----

## ---- ss-7.1.1 ----

## ---- demo-baseg ----
demo(graphics)

## ---- with-women ----
## Older syntax:
with(women,
     plot(height, weight))

## ---- form-women ----
## Graphics formula:
plot(weight ~ height,
     data=women)

## ---- formxy ----
plot(y ~ x)   # Use a formula to specify the graph
plot(x, y)    # Horizontal ordinate, then vertical

## ---- gph-type ----
plot(distance ~ stretch, data=elasticband)
plot(ACT ~ year, data=austpop, type="l")
plot(ACT ~ year, data=austpop, type="b")

## ---- with ----
with(austpop, plot(spline(year, ACT), type="l"))
  # Fit smooth curve through points

## ---- primates ----
## Data (1st 2 lines)
head(primates, 2)

## ---- brain-body ----
plot(Brainwt ~ Bodywt, xlim=c(0, 300),
     ylim=c(0,1500), data=primates, fg="gray",
     xlab="Body weight (kg)",
     ylab="Brain weight (g)")
# Specify xlim to allow room for the labels
with(primates,
     text(Brainwt ~ Bodywt, cex=0.8,
          labels=rownames(primates), pos=4))
# pos: pos=1 (below), 2 (left), 3 (above)

## ---- ss-7.1.2 ----

## ---- setpar ----
oldpar <- par(cex=1.25)
  # par(oldpar) to restore

## ---- ss-7.1.3 ----

## ---- load-Brewer ----
## Load to run code for Supplementary Figure 1
library(RColorBrewer)   # Required for Set1 and Dark2 RColorBrewer palettes

## ---- col-plots ----
opt <- options(width=75)
colpal <- rev(list(
    "Default palette" = palette()[1:8],  cm.colors = cm.colors(12),
    terrain.colors = terrain.colors(12), heat.colors = heat.colors(12),
    blueRamp = colorRampPalette(c(blues9, "white"))(12),
    "Brewer-Set1" = brewer.pal(8, "Set1"),
    "Brewer-Dark2" = brewer.pal(8, "Dark2")))
palnam <- names(colpal)
plot(1, 1, xlim=c(0.5,12.5), ylim=c(0,length(palnam)+0.5), type="n",
     axes=FALSE, xlab="", ylab="")
for(i in 1:length(palnam)){
    len <- length(colpal[[i]])
    points(1:len, rep(i,len), pch=15, col=colpal[[i]], cex=5.5)
    legend(1, i+0.025, palnam[i], adj=0, box.col="white", bg="white",
           x.intersp=0, y.intersp=0, yjust=0)
}
options(opt)

## ---- brewer-all ----
display.brewer.all()

## ---- brewer-qual ----
Set1 <- brewer.pal(8, "Set1")[-6]
## Check out the palette
plot(1:7, pch=16, cex=2, col=Set1)

## ---- colrampBlue ----
blueRamp = colorRampPalette(c("white", blues9))

## ---- alpha-ex ----
## Sample from the 15992 rows
dfsamp <- cps1[sample(nrow(cps1), 3000), ]
plot(re78 ~ re75, data=dfsamp, pch=20, cex=0.5,
     col="black", las=0, fg="gray")
mtext(side=3, line=0.5, "A: 100% opacity", adj=0)
plot(re78 ~ re75, data=dfsamp, pch=20, cex=0.5, las=0,
     col=adjustcolor("black", alpha=0.4), fg="gray")
mtext(side=3, line=0.5, "B: 40% opacity", adj=0)
blueRamp <- colorRampPalette(c("white", blues9))
with(dfsamp, smoothScatter(re75~re74, , fg="gray",
                           las=0, colramp=blueRamp))
mtext(side=3, line=0.5, "C: Color density plot",
      adj=0)

## ---- ss-7.1.4 ----

## ---- aspect ----
plot((1:30)*0.92, sin((1:30)*0.92),
     xlab="", ylab="")

## ---- ss-7.1.5 ----

## ---- Animals-body-brain ----
## Supplementary figure 9.2
library(MASS)
oldpar <- par(pch=16, pty="s", mfrow=c(2,2))
with(Animals, {      # bracket several R statements
  plot(body, brain)
  plot(sqrt(body), sqrt(brain))
  plot(body^0.1, brain^0.1)
  plot(log(body), log(brain))
})                   # close both sets of brackets
par(oldpar)          # Restore former settings

## ---- par-fig ----
par(fig = c(0, 1, 0.38, 1), mgp=c(3,0.5,0))
          # xleft, xright, ybottom, ytop
## Panel A
par(fig = c(0, 1, 0, 0.38), new=TRUE)
## Plot graph B
par(fig = c(0, 1, 0, 1))    # Restore settings

## ---- ss-7.1.6 ----

## ---- poss-hist ----
ftotlen <- subset(possum, sex=="f")[, "totlngth"]
## Left panel: breaks at 72.5, 77.5,..
hist(ftotlen, breaks = 72.5 + (0:5)*5, freq=FALSE,
     xlab="Total length", ylim=c(0,0.11),
     main ="A: Breaks at 72.5, 77.5,...")
## Now superimpose a density curve, as in Fig. 7.3
lines(density(ftotlen))
##
## Panel B: breaks at 75, 80, ...
hist(ftotlen, breaks = 75 + (0:5)*5, freq=FALSE,
     xlab="Total length", ylim=c(0,0.11),
     main="B: Breaks at 75, 80, ...")

## ---- fposs-density ----
## Supplementary figure 9.3
with(subset(possum, sex=="f"),
     plot(density(totlngth), type="l"))

## ---- boxplot ----
## Code
with(subset(possum, sex=="f"),
     {boxplot(totlngth, horizontal=TRUE)
      rug(totlngth)} )

## ---- possum-qqn ----
ftotlen <- subset(possum, sex == "f")[, "totlngth"]
yl <- expression(bold("Data"))
xy <- qqnorm(ftotlen, xlab="", ylab=yl, fg="gray")
usr <- par()$usr
fillcol <- adjustcolor("gray60", alpha=0.2)
rect(usr[1], usr[3],
     usr[2], usr[4], col=fillcol)
for(i in 1:7){
  qqnorm(rnorm(43), xlab="", xaxt="n",
         ylab="Simulated",
         main="Q-Q: Simulated")
  axis(1, at=-2:2, col="gray", col.ticks="black")
}
# rnorm() generates random samples from a normal
# distribution with, by default, mean 0 and SD=1.

## ---- qqn-simple ----
## Q-Q plot for the data (top left panel)
ftotlen <- subset(possum, sex == "f")[, "totlngth"]
qqnorm(ftotlen, xlab="",
       ylab=expression(bold("Data")))
## Code for a plot with random normal data
qqnorm(rnorm(43), xlab="", ylab="Simulated")

## ---- ss-7.1.7 ----

## ---- plot-expr ----
yl <- expression("Area = " * pi * r^~2)
plot(1:5, pi*(1:5)^2, xlab="Radius (r)", ylab=yl)

## ---- sec-7.2 ----

## ---- load-latticeExtra ----
library(latticeExtra, quietly=TRUE)

## ---- ss-7.2.1 ----

## ---- demo-lattice ----
demo(lattice)

## ---- cline-gph ----
## On the command line: Create and print object
xyplot(Brainwt ~ Bodywt, data=primates)

## ---- saveAsObj ----
## Save the result as a trellis graphics object
# [For plot(), this is not possible.]
## Create trellis object
gph <- xyplot(Brainwt ~ Bodywt, data=primates)
## Print graph; a graphics device must now be open
print(gph)

## ---- updateObj ----
gph <- xyplot(Brainwt ~ Bodywt, data=primates)
gph2 <- update(gph, xlab="Body wt (kg)",
               ylab="Brain wt (g)")
print(gph2)  # Or it is enough to type 'gph2'

## ---- ACTgph ----
print(xyplot(ACT ~ year, data=austpop))

## ---- ss-7.2.2 ----

## ---- sexSport ----
options(width=84)
with(ais, table(sex,sport))
options(width=54)

## ---- rowSwim ----
xyplot(ht ~ wt | sport, groups=sex, data=ais,
       par.settings=simpleTheme(pch=c(4,1)),
       scales=list(tck=0.5),
       auto.key=list(space="right"),
       subset=sport%in%c("Row","Swim"))

## ---- ----
dev.off()            # Close device
trellis.device()     # Start new device,
# by default with color=TRUE

## ---- ss-7.2.4 ----

## ---- grog-simple ----
## Simple version of plot
grogplot <- xyplot(
              Beer+Spirit+Wine ~ Year | Country,
              data=grog, outer=FALSE,
              auto.key=list(columns=3))

## ---- grog-update ----
## Update trellis object, then print
ylab <- "Amount consumed (per person)"
parset <- simpleTheme(pch=c(1,3,4))
finalplot <- update(grogplot, ylim=c(0,5.5),
                     xlab="", ylab=ylab,
                     par.settings=parset)
print(finalplot)

## ---- BSW ----
xyplot(Beer+Spirit+Wine ~ Year,
       groups=Country, outer=TRUE,
       data=grog, auto.key=list(columns=2) )

## ---- ss-7.2.5 ----

## ---- strip-grob ----
plotnam <- "Stripplot of cuckoo data"
stripplot(species ~ length, xlab="", data=cuckoos,
  legend=list(top=list(fun=grid::textGrob,
                       args=list(label=plotnam,
                                 x=0))))
# x=0 is equivalent to x=unit(0,"npc")
# npc units are on a scale from 0 to 1

## ---- ss-7.2.6 ----

## ---- nam-t-p-g ----
names(trellis.par.get())

## ---- trellis-parset ----
trellis.par.set(fontsize = list(text = 7,
                                points = 4))

## ---- trellis-settings ----
trellis.device(color=FALSE)
show.settings()
trellis.device(color=TRUE)
show.settings()

## ---- jobs-basic ----
## 1. Create a basic version of the graphics object
jobsB.xyplot <-
  xyplot(Ontario+Quebec+BC+Alberta+Prairies+Atlantic ~ Date,
         data=jobs, type="b", layout=c(3,2), outer=TRUE,
         ylab="Number of jobs",
         scales=list(y=list(relation="sliced", log=TRUE)))

## ---- jobs-enhance ----
## 2. Code for the enhancements to jobsB.xyplot
ylabpos <- exp(pretty(log(unlist(jobs[,-7])), 100))
ylabels <- paste0(round(ylabpos),"\n(", log(ylabpos), ")")
## Create a date object 'startofmonth'; use instead of 'Date'
atdates <- seq(from=95, by=0.5, length=5)
datelabs <- format(seq(from=as.Date("1Jan1995", format="%d%b%Y"),
                       by="6 month", length=5), "%b%y")
update(jobsB.xyplot, xlab="", between=list(x=0.5, y=0.5),
       scales=list(x=list(at=atdates, labels=datelabs),
                   y=list(at=ylabpos, labels=ylabels), tck=0.6) )

## ---- ss-7.2.7 ----

## ---- improved ----
levels(cuckoos$species) <-
 sub(".", " ",
  levels(cuckoos$species),
  fixed=TRUE)

## ---- strip-bw ----
stripplot(species ~ length, data=cuckoos,
          xlab="Cuckoo egg length (mm)")
bwplot(species ~ length, data=cuckoos,
       xlab="Cuckoo egg length (mm)")

## ---- lattice-density ----
## Code
colset <- c("gray","black")
densityplot(~ earconch | sex, groups=Pop,
            data=possum,
            par.settings=simpleTheme(col=colset),
            auto.key=list(columns=2))

## ---- ss-7.2.8 ----

## ---- gph-equiv ----
xyplot(species ~ length, xlab="", data=cuckoos)
xyplot(species ~ length, xlab="", data=cuckoos,
       panel=panel.xyplot)

## ---- gph-from ----
gph <- xyplot(Brainwt ~ Bodywt,  data=primates,
              xlim=c(0,300))

## ---- my-panel ----
my.panel <- function(x,y){
  panel.xyplot(x,y)
  panel.text(x,y, labels=rownames(primates),
             cex=0.65, pos=4)
}
update(gph, panel=my.panel,
       scales=list(tck=0.6))

## ---- supply-panel ----
xyplot(Brainwt ~ Bodywt, data=primates,
       xlim=c(0,300), panel=my.panel)

## ---- ss-7.2.9 ----

## ---- gph-again ----
gph <- xyplot(Brainwt ~ Bodywt,  data=primates,
              xlim=c(0,300))

## ---- label-layer ----
gph + latticeExtra::layer(panel.text(x,y,
                       labels=rownames(primates),
                       pos=4))

## ---- sec-7.3 ----

## ---- load-ggplot2 ----
library(ggplot2)

## ---- ggplot2-brain ----
library(MASS)
quickplot(body, brain, data=mammals, log="xy") +
  coord_fixed()

## ---- ggplot2-brain-lm ----
quickplot(body, brain, data=mammals, log="xy") +
  coord_fixed() +
  geom_smooth(method=lm)

## ---- ggplot2-brain-full ----
ggplot(mammals, aes(body, brain)) +
  geom_point() +
  scale_x_continuous(trans="log") +
  scale_y_continuous(trans="log")

## ---- qplot-smooth ----
library(DAAG)
library(ggplot2)
## Default loess smooth, with SE bands added.
quickplot(Year, mdbRain, data=bomregions2018,
          geom=c("point","smooth"), xlab="",
          ylab="Av. rainfall, M-D basin")

## ---- bom2018 ----
ggplot(bomregions2018, aes(x=Year, y=mdbRain)) +
  geom_point() +                      # Scatterplot
  geom_smooth(span=0.5, se=TRUE) +    # Add smooth
  xlab("") +             # Blank out x-axis label
  ylab("Av. rainfall, M-D basin")
## NB: aes() has supplied x- and y-axis variables

## ---- qplotADDgg ----
qplot(Year, mdbRain, data=bomregions2018,
      geom="point",
      xlab="", ylab="Av. rainfall, M-D basin") +
  geom_smooth(span=0.5, se=TRUE)

## ---- load-quantreg ----
library(quantreg)

## ---- load-splines ----
library(splines)

## ---- twenty5080 ----
## Supplementary figure 4
quickplot(Year, mdbRain, data=bomregions2018) +
          geom_quantile(formula = y ~ ns(x,5),
          quantiles=c(0.2,0.5,0.8) )

## ---- overlay-dens-simple ----
## Overlay with boxplots and density contours
quickplot(wt, ht, data=ais,
          geom=c("boxplot", "point", "density2d"),
          facets = . ~ sex)

## ---- overlay-dens-frills ----
quickplot(wt, ht, xlab="Weight (kg)",
          ylab="Height (cm)", data=ais,
          facets = . ~ sex) +
  geom_boxplot(aes(group=sex),
               outlier.size=1.75,
               outlier.colour="gray",
               color="gray") +
  geom_point(shape=2, size=1) +
  geom_density2d(color="gray")

## ---- aisRS ----
## Extract from ais data for rowers and swimmers
aisRS <- subset(ais, sport %in% c("Row","Swim"))
aisRS$sport <- droplevels(aisRS$sport)

## ---- quick-rs1 ----
quickplot(wt, ht,
          data=aisRS,
          geom="point",
          size=I(2),
          colour=sex,
          shape=sport)

## ---- quick-rs2 ----
ggplot(aisRS) +
  geom_point(aes(wt, ht,
               color=sex,
               shape=sport),
             size=2)

## ---- quick-further ----
## Identify sex by color, sport by shape (1 panel)
quickplot(wt, ht, data=aisRS, geom="point",
           color=sex, shape=sport, size=I(2.5))
## Identify sex by color, sport by size (1 panel)
quickplot(wt, ht, data=aisRS, geom="point",
          color=sex, size=sport)

## ---- mult-aes ----
## Distinguish sex by color & shape
## Different sports have different panels
quickplot(wt, ht, data=aisRS, geom="point",
          size=I(2.5), color=sex, shape=sex,
          facets = . ~ sport)

## ---- lat-position ----
cuckoos.strip <- stripplot(species ~ length, xlab="", data=cuckoos)
print(cuckoos.strip, position=c(0,0.5,1,1))
                   # xleft, ybottom, xright, ytop
cuckoos.bw <- bwplot(species ~ length, xlab="", data=cuckoos)
print(cuckoos.bw, position=c(0,0,1,0.5), newpage=FALSE)

## ---- sec-7.4 ----

## ---- ss-7.4.1 ----

## ---- figlim ----
par(fig = c(0, 1, 0.38, 1))
          # xleft, xright, ylow, yhigh
## Plot graph A
par(fig = c(0, 1, 0, 0.38), new=TRUE)
## Plot graph B
par(fig = c(0, 1, 0, 1))     # Resets to default

## ---- base-lattice ----
plot(0:1, 0:1, type="n", bty="n", axes=FALSE,
     xlab="", ylab="")
lab <- "Lattice bwplot (i.e., boxplot)"
mtext(side=3, line=3, lab)
cuckoos.bw <- bwplot(species~length, data=cuckoos)
print(cuckoos.bw, newpage=FALSE)


## ---- sec-7.5 ----

## ---- ss7.5.1 ----

## ---- load-carANDrgl ----
## The car and rgl packages must be installed
library(rgl, quietly=TRUE)
library(car, quietly=TRUE)
rgl::setupKnitr()
knit_hooks$set(rgl=hook_rgl)

## ---- rgl-demo ----
library(DAAG, quietly=TRUE)
open3d()            # Precedes the call to par3d()
par3d(cex=0.75)     # Optional
                    # Other params: see help(par3d)
with(nihills, scatter3d(x=log(dist), y=log(climb),
                        z=log(time),
                        grid=FALSE,
                        surface=FALSE,
                        point.col="black",
                        axis.scales=FALSE))
## NB: Use middle or right mouse button to drag a
## rectangle around a point that is to be labeled.

## ---- source-rggobi ----
source("http://www.ggobi.org/downloads/install.r")

## ---- label-points ----
with(nihills, identify3d(x=log(dist), y=log(climb),
                         z=log(time),
                         labels=row.names(nihills),
                         col="gray"))

## ---- ss-7.5.2 ----

## ---- grogVis ----
library(googleVis)
M <- gvisMotionChart(grog, id="Country", timevar="Year")
## This next line requires a live internet connection,
## and Adobe Flash must be installed.
plot(M)

## ---- demo-WorldBank ----
demo(WorldBank)

## ---- gvisMotionChart ----
M <- gvisMotionChart(WorldBank, idvar="country",
          timevar="year",
          xvar="life.expectancy",
          yvar="fertility.rate",
          colorvar="region", sizevar="population",
          options=list(width=700, height=600))
## Now display the motion chart
plot(M)

## ---- ElecVis ----
xnam <- "Electric power consumption (kWh per capita)"
ynam <- "Mobile cellular subscriptions (per 100 people)"
M <- gvisMotionChart(wdiSel, idvar="Country.Name", timevar="Year",
                     xvar=xnam, yvar=ynam,
                     colorvar="region", sizevar="Population, total",
                     options=list(width=600, height=500),
                     chartid="wbMotionChartSel")
plot(M)



## ---- sec-7.7-Exercises ----

## ---- hotspots ----
## Exercise 3
plot(age ~ distance, data=hotspots)
with(hotspots, identify(age ~ distance, labels=name))

## ---- anymiss ----
## Exercise 7
missIND <- complete.cases(Pima.tr2)
Pima.tr2$anymiss <- c("miss","nomiss")[missIND+1]

## ---- Pima-plus ----
## Exercise 7a
library(lattice)
stripplot(anymiss ~ npreg + glu | type, data=Pima.tr2, outer=TRUE,
          scales=list(relation="free"), xlab="Measure")

## ---- density-Pima ----
## Exercise 7b
library(lattice)
## npreg & glu side by side (add other variables, as convenient)
densityplot( ~ npreg + glu | type, groups=anymiss, data=Pima.tr2,
            auto.key=list(columns=2), scales=list(relation="free"))
















