## ---- sec-8.1 ----

## ---- plt-roller ----
library(DAAG)
plot(depression ~ weight, data=roller, fg="gray")

## ---- pltWline ----
plot(depression ~ weight, data=roller, fg="gray")
roller.lm <- lm(depression ~ weight, data=roller)
# For a line through the origin, specify
# depression ~ 0 + weight
abline(roller.lm)

## ---- ss-8.1.1 ----

## ---- ss-8.1.2 ----

## ---- lm-obj ----
lm(depression ~ weight, data=roller)

## ---- nam-lm-obj ----
roller.lm <- lm(depression ~ weight, data=roller)
names(roller.lm)

## ---- ss-8.1.3 ----

## ---- sec-8.2 ----

## ---- fVSmTimeAB ----
def.par <- par(no.readonly = TRUE)
layout(matrix(c(1,3,0,0,2,4), 2, 3), respect = TRUE,
       widths=c(7.5,1,7.5), heights=c(7.15,2))
xlim <- range(DAAG::nihills$time)+c(-0.1,0.1)
par(mar=c(0.2,3,2,0), bty="o", cex.axis=1.1)
plot(timef~time, data=DAAG::nihills, xlim=xlim, xaxt='n',
     ylab="", fg="gray", main=" ")
abline(lm(timef~time, data=DAAG::nihills), col=2)
mtext(side=2,line=2.25,"Female times", las=0)
mtext(side=3,line=0.5,"A: Untransformed scales", adj=0)
plot(timef~time, data=DAAG::nihills, xlim=xlim, xaxt='n',
     log='xy', fg="gray", ylab="", main=" ")
abline(lm(log10(timef)~log10(time), data=DAAG::nihills), col=2)
mtext(side=2,line=2.25,"Female times (log scale)", las=0)
mtext(side=3,line=0.5,"B: Logarithmic scales", adj=0)
par(mar=c(3,2.75,0,0), bty="n")
pars = list(boxwex = 4.0, staplewex = 0.5, outwex = 0.5)
boxplot(DAAG::nihills$time, horizontal=T, xlim=xlim,
        pars=pars, at=2, axes=0)
axis(1, lwd = 0, lwd.ticks = 1)
mtext(side=1,line=2,"Male times")
boxplot(DAAG::nihills$time, horizontal=T, xlim=xlim,
        log='x', pars=pars, at=2, axes=0)
axis(1, lwd = 0, lwd.ticks = 1)
mtext(side=1,line=2,"Male times (log scale)")
par(def.par)

## ---- mftime-lm ----
mftime.lm <- lm(timef ~ time, data=nihills)

## ---- rec-logmf ----
xyplot(timef~time, data=nihills,
       scales=list(log=10),
       aspect=1, type=c("p","r"))

## ---- diag-mf ----
opar <- par(pty="s")
mftime.lm <- lm(timef ~ time, data=nihills)
plot(mftime.lm, cex.caption=0.8)
par(opar)

## ---- coef-logmf ----
mflogtime.lm <- lm(log(timef) ~ log(time),
                   data=nihills)
round(coef(mflogtime.lm), 3)

## ---- tohoriz-mf ----
## Panel A
plot(resid(mftime.lm)~time, data=nihills)
mtext(side=3, line=0.5, "A: Residuals, unlogged data")
## Panel B
plot(resid(mflogtime.lm) ~ log(time), data=nihills)
mtext(side=3, line=0.5, "B: Residuals, logged data")

## ---- diag-logmf ----
opar <- par(pty="s")
plot(mflogtime.lm, cex.caption=0.8)
par(opar)

## ---- omit-77s ----
round(coef(mflogtime.lm), 4)
omitrow <- rownames(nihills)!="Seven Sevens"
update(mflogtime.lm, data=subset(nihills, omitrow))

## ---- sec-8.3 ----

## ---- plt-mftime ----
plot(mftime.lm, which=1)

## ---- simscat ----
## Code
gph <- plotSimScat(obj=mftime.lm, show="residuals",
                   type=c("p","smooth"),
                   layout=c(4,1))
update(gph, xlab="Time (h) for males",
      ylab="Residuals")

## ---- which2 ----
plot(mftime.lm, which=2)

## ---- simdiag2 ----
plotSimDiags(obj=mftime.lm, which=2, layout=c(4,1))

## ---- which3 ----
plot(mftime.lm, which=3)

## ---- simdiag3 ----
plotSimDiags(obj=mftime.lm, which=3, layout=c(4,1))

## ---- which5 ----
plot(mftime.lm, which=5)

## ---- simdiag5 ----
plotSimDiags(obj=mftime.lm, which=5, layout=c(4,1))

## ---- ss-8.3.2 ----

## ---- mftime-sims ----
gph <- plotSimScat(mftime.lm, layout=c(4,1))
update(gph, xlab="Male record times (h)",
       ylab="Female record times (h)")

## ---- sec-8.5 ----

## ---- lev-tomato ----
lev <- c("Water", "A", "B", "C")
tomato[, "trt"] <- factor(rep(lev, rep(6,4)),
                          levels=lev)

## ---- ss-8.5.1 ----

## ---- tomato-aov ----
## Analysis of variance: tomato data (from DAAG)
tomato.aov <- aov(weight ~ trt, data=tomato)

## ---- termplot-aov-wt ----
## Panel A: Use weight as outcome variable
tomato.aov <- aov(weight ~ trt, data=tomato)
termplot(tomato.aov, xlab="Treatment",
         ylab="Partial for treatment",
         partial.resid=TRUE, se=TRUE, pch=16)
mtext(side=3, line=0.5, "A: weight", adj=0, cex=1.2)

## ---- termplot-aov-logwt ----
## Panel B: Use log(weight) as outcome variable
logtomato.aov <- aov(log(weight) ~ trt, data=tomato)
termplot(logtomato.aov, xlab="Treatment",
         ylab="Partial for treatment",
         partial.resid=TRUE, se=TRUE, pch=16)
mtext(side=3, line=0.5, "B: log(weight)", adj=0,
      cex=1.2)

## ---- aov-coef ----
round(coef(summary.lm(tomato.aov)),3)

## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- modmat ----
model.matrix(tomato.aov)

## ---- ss-8.5.2 ----

## ---- noConst ----
## Omit constant term from fit;
## force parameters to estimate treatment means
tomatoM.aov <- aov(weight ~ 0 + trt, data=tomato)

## ---- noConst-mm ----
mmat <- model.matrix(tomatoM.aov)
mmat[1:9, ]
## ...   ...    ...    ...

## ---- contr-sum ----
oldoptions <- options(contrasts=c("contr.sum",
                                  "contr.poly"))
tomatoS.aov <- aov(weight ~ trt, data=tomato)
round(coef(summary.lm(tomatoS.aov)),3)
options(oldoptions)  # Restore default contrasts

## ---- cuckoos-cf ----
options(width=80)
cuckoos.lm <- lm(breadth ~ species + length, data=cuckoos)
cuckoosI.lm <- lm(breadth ~ species + length + species:length, data=cuckoos)
print(anova(cuckoos.lm, cuckoosI.lm), digits=3)
options(width=54)

## ---- cuckoos-ests ----
options(width=80)
print(coef(summary(cuckoos.lm)), digits=2)
options(width=54)

## ---- anova-Cp ----
anova(cuckoos.lm, cuckoosI.lm, test="Cp")

## ---- alt-write ----
cuckoosI.lm <- lm(breadth ~ species + length +
                  species:length, data=cuckoos)

## ---- sec-11.6 ----

## ---- str-nihills ----
str(nihills)

## ---- scatter-ni ----
## Unlogged data
library(lattice)
## Scatterplot matrix; unlogged data
splom(~nihills)

## ---- scatter-logni ----
## Logged data
lognihills <- log(nihills)
names(lognihills) <- paste0("l", names(nihills))
## Scatterplot matrix; log scales
splom(~ lognihills)

## ---- ss-8.6.1 ----

## ---- nireg-climb ----
lognihills <- log(nihills)
lognam <- paste0("l", names(nihills))
names(lognihills) <- lognam
lognihills.lm <- lm(ltime ~ ldist + lclimb,
                    data=lognihills)
round(coef(lognihills.lm),3)

## ---- nireg-slope ----
nihills$gradient <- with(nihills, climb/dist)
lognihills <- log(nihills)
lognam <- paste0("l", names(nihills))
names(lognihills) <- lognam
lognigrad.lm <- lm(ltime ~ ldist + lgradient,
                   data=lognihills)
round(coef(lognigrad.lm),3)

## ---- tplot-ni ----
## Plot the terms in the model
termplot(lognigrad.lm, col.term="gray", partial=TRUE,
         col.res="black", smooth=panel.smooth)

## ---- sec-8.7 ----

## ---- ss-8.7.1 ----

## ---- varselect-sim ----
bestsetNoise(m=100, n=40, nvmax=3)
bestsetNoise(m=100, n=40, method="backward",
             nvmax=3)

## ---- bsnVary ----
library(DAAG)
library(quantreg)
library(splines)
set.seed(37)   # Use to reproduce graph shown
bsnVaryNvar(m=100, nvar=3:50, nvmax=3, fg="gray")

## ---- sec-8.8 ----

## ---- Elec-spm ----
library(car)
library(Ecdat)
data(Electricity)
spm(Electricity, smooth=TRUE, regLine=FALSE,
    col=adjustcolor(rep("black",3), alpha.f=0.3))

## ---- ss-8.8.1 ----

## ---- spm-cost-q ----
varlabs <- c("log(cost)", "log(q)")
spm(log(Electricity[,1:2]), var.labels=varlabs,
    smooth=TRUE, regLine=FALSE,
    col=adjustcolor(rep("black",3), alpha.f=0.5))

## ---- elec-me ----
elec.lm <- lm(log(cost) ~ log(q)+pl+sl+pk+sk+pf+sf,
              data=Electricity)

## ---- elec-me-tplot ----
termplot(elec.lm, partial=T, smooth=panel.smooth,
         transform.x=TRUE)

## ---- elec-me-coef ----
round(coef(summary(elec.lm)),5)

## ---- drop-pk-sk-sf ----
elec2.lm <- lm(log(cost) ~ log(q)+pl+sl+pf,
               data=Electricity)
round(coef(summary(elec2.lm)),5)

## ---- check-x ----
elec2x.lm <- lm(log(cost) ~ (log(q)+pl+sl+pf)^2,
                data=Electricity)
anova(elec2.lm, elec2x.lm)

## ---- coef-elecx-sum ----
round(coef(summary(elec2x.lm)),5)

## ---- elec2xx-coef ----
elec2xx.lm <- lm(log(cost) ~ log(q)+pl+sl+pf+
                 log(q):pl+log(q):sl,
                 data=Electricity)
round(coef(summary(elec2xx.lm)),5)

## ---- add1-2xx ----
add1(elec2xx.lm, scope=~(log(q)+pl+sl+pk+sk+pf+sf)^2, test="F")

## ---- sec-8.9 ----

## ---- load-DAAGviz-KernSmooth ----
library(DAAGviz, quietly=TRUE)
library(KernSmooth, quietly=TRUE)

## ---- bronchit-ylim ----
ylim <- range(bronchit$poll)+c(0,2.5)

## ---- bronchitA ----
## Code for Panel A
colr <- adjustcolor(c("red","blue"), alpha=0.5)
plot(poll ~ cig,
     xlab="# cigarettes per day", ylab="Pollution",
     col=colr[r+1], pch=(3:2)[r+1], data=bronchit,
     ylim=ylim)
legend(x="topright",
       legend=c("Non-sufferer","Sufferer"),
       ncol=2, pch=c(3,2), col=c(2,4), cex=0.8)
mtext(side=3, line=1.0,
      expression("A: Untransformed "*italic(x)*"-scale"), adj=0)

## ---- bronchitB ----
## Code for Panel B
plot(poll ~ log(cig+1), col=c(2,4)[r+1], pch=(3:2)[r+1],
     xlab="log(# cigarettes per day + 1)", ylab="", data=bronchit, ylim=ylim)
xy1 <- with(subset(bronchit, r==0), cbind(x=log(cig+1), y=poll))
xy2 <- with(subset(bronchit, r==1), cbind(x=log(cig+1), y=poll))
est1 <- bkde2D(xy1, bandwidth=c(0.7, 3))
est2 <- bkde2D(xy2, bandwidth=c(0.7, 3))
lev <- pretty(c(est1$fhat, est2$fhat),4)
contour(est1$x1, est1$x2, est1$fhat, levels=lev, add=TRUE, col=2)
contour(est2$x1, est2$x2, est2$fhat, levels=lev, add=TRUE, col=4, lty=2)
legend(x="topright", legend=c("Non-sufferer","Sufferer"), ncol=2, lty=1:2,
       col=c(2,4), cex=0.8)
mtext(side=3, line=1.0, expression("B: Log transformed "*italic(x)*"-scale"),
      adj=0)

## ---- cig2-glm ----
cig2.glm <- glm(r ~ log(cig+1) + poll, family=binomial, data=bronchit)
summary(cig2.glm)

## ---- cig2-tplot ----
termplot(cig2.glm)

## ---- sec-8.10 ----

## ---- oddbooks ----
volume <- apply(oddbooks[, 1:3], 1, prod)
area <- apply(oddbooks[, 2:3], 1, prod)
lob1.lm <- lm(log(weight) ~ log(volume), data=oddbooks)
lob2.lm <- lm(log(weight) ~ log(thick)+log(area), data=oddbooks)
lob3.lm <- lm(log(weight) ~ log(thick)+log(breadth)+log(height),
              data=oddbooks)

## ---- sec-8.11 ----

## ---- smooth-ohms ----
## Plot points
plot(ohms ~ juice, data=fruitohms, fg="gray")
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
