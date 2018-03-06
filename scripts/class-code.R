## ---- fgl-ldaCV ----
library(MASS, quietly=TRUE)
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

## ---- fgl-scores1-2 ----
library(lattice)
scores <- predict(fgl.lda)$x
xyplot(scores[,2] ~ scores[,1], groups=fgl$type,
       xlab="Discriminant 1",
       ylab="Discriminant 2",
       aspect=1, scales=list(tck=0.4),
       auto.key=list(columns=3),
       par.settings=simpleTheme(alpha=0.6,
                                pch=1:6))

## ---- bronchit-first3 ----
library(SMIR); data(bronchit)
head(bronchit, 3)
## Now make the outcome variable a factor
bronchit <-
  within(bronchit,
         rfac <- factor(r, labels=c("abs","pres")))

## ---- bronchit-rp ----
library(rpart)
set.seed(47)   # Reproduce tree shown
b.rpart <- rpart(rfac ~ cig+poll, data=bronchit)

## ---- treefig ----
opar <- par(mar=rep(1.1,4))
plot(b.rpart)
text(b.rpart, xpd=TRUE)
par(opar)

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

## ---- aupoints ----
opar <- par(mar=c(1.1, 1.6, 0.6, 3.6))
library(DAAG)
aupts <- cmdscale(audists)
plot(aupts, axes=FALSE, 
     ann=FALSE, fg="gray",
     frame.plot=TRUE)
city <- rownames(aupts)
pos <- rep(1,length(city))
pos[city=="Melbourne"]<- 3
pos[city=="Canberra"] <- 4
text(aupts, labels=city,
     pos=pos, xpd=TRUE)
par(opar)

## ---- identify ----
identify(aupoints, labels=rownames(aupoints))

## ---- audistfits----
audistfits <- as.matrix(dist(aupoints))
misfit <- as.matrix(dist(aupoints)) -
  as.matrix(audists)
for (j in 1:9)for (i in (j+1):10){
  lines(aupoints[c(i,j), 1], aupoints[c(i,j), 2],
        col="gray")
  midx <- mean(aupoints[c(i,j), 1])
  midy <- mean(aupoints[c(i,j), 2])
  text(midx, midy, paste(round(misfit[i,j])))
}
colnames(misfit) <- abbreviate(colnames(misfit),6)
print(round(misfit))


