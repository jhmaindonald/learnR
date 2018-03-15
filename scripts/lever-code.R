## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- sec-12.1 ----

## ---- ss-12.1.1 ----

## ---- mgpVSdisp ----
plot(mpg ~ disp, data=mtcars)

## ---- mgpVSdisp-equiv ----
yvar <- "mpg"
xvar <- "disp"
form <- as.formula(paste(yvar, "~", xvar))
plot(form, data=mtcars)

## ---- fun-args ----
plot.mtcars <- function(xvar="disp", yvar="mpg"){
    form <- as.formula(paste(yvar, "~", xvar))
    plot(form, data=mtcars)
}

## ---- mtcars-nam ----
names(mtcars)

## ---- mtcars-gph ----
plot.mtcars(xvar="hp", yvar="mpg", data=mtcars)

## ---- ss-12.1.2 ----

## ---- all-vars ----
all.vars(mpg ~ disp)

## ---- mtcars-gph-plus ----
plot.mtcars <- function(form = mpg ~ disp){
   yvar <- all.vars(form)[1]
   xvar <- all.vars(form)[2]
   ## Include information that allows a meaningful label
   mtcars.info <-
     c(mpg= "Miles/(US) gallon",        cyl= "Number of cylinders",
       disp= "Displacement (cu.in.)",   hp= "Gross horsepower",
       drat= "Rear axle ratio",         wt= "Weight (lb/1000)",
       qsec= "1/4 mile time",           vs= "V/S",
       gear= "Number of forward gears",
       carb= "Number of carburettors",
       am= "Transmission (0 = automatic, 1 = manual)")
   xlab <- mtcars.info[xvar]
   ylab <- mtcars.info[yvar]
   plot(form, xlab=xlab, ylab=ylab)
}

## ---- sec-12.2 ----

## ---- ss-12.2.1 ----

## ---- mtcars-subs ----
plot.mtcars` <-
  function(x = disp, y = mpg){
    xvar <- deparse(substitute(x))
    yvar <- deparse(substitute(y))
    form <- formula(paste(yvar, "~", xvar))
    plot(form, xlab=xvar, ylab=yvar, data=mtcars)
  }

## ---- ss-12.2.2 ----

## ---- do-call ----
mean(rnorm(20))
do.call("mean", args=list(x=rnorm(20)))

## ---- do-possum ----
do.call("mean", list(x=possum$totlngth))

## ---- av-fun ----
`average` <-
  function(x=possum$chest, FUN=function(x)mean(x)){
    fun <- deparse(substitute(FUN))
    do.call(fun, list(x=x))
  }

## ---- av-exec ----
average()
average(FUN=median)

## ---- later-eval ----
mean.call <- call("mean", x=rnorm(5))
eval(mean.call)
eval(mean.call)

## ---- mean-call ----
mean.call

## ---- ss-12.2.3 ----

## ---- fun-name ----
test <- function(){
 fname <- as(sys.call(sys.parent())[[1]],
             "character")
  fname
}
test()

## ---- fun-newname ----
newtest <- test
newtest()

## ---- auto-name ----
gf <-
    function(width=2.25, height=2.25, pointsize=8){
        funtxt <- sys.call(1)
        fnam <- paste0(funtxt, ".pdf")
        print(paste0("Output is to the file '",
                     fnam, "'"))
        pdf(file=fnam, width=width, height=height,
            pointsize=pointsize)
    }

## ---- fig1 ----
fig1 <- function(){
    gf()             # Call with default parameters
    curve(sin, -pi, 2*pi)
    dev.off()
}
fig1()

## ---- sec-12.3 ----

## ---- breakIntoWords ----
breakIntoWords <- function(sentence="The quick brown fox"){
  words <- strsplit(sentence, split=' ', fixed=TRUE)
  unlist(words)
}

## ---- BIWtest ----
sentence <- 'The quick brown fox jumps over the lazy dog.'
breakIntoWords(sentence)

## ---- BIWhelp ----
prompt(breakIntoWords, file='man/breakIntoWords.Rd')

## ---- sec-12.4 ----

## ---- S4 ----
library(DAAG)
library(lme4)
hp.lmList <- lmList(o2 ~ wattsPerKg | id,
                    data=humanpower1)
slotNames(hp.lmList)

## ---- slot-call ----
hp.lmList@call
slot(hp.lmList, "call")

## ---- coef-lmList ----
coef(hp.lmList)

## ---- selectMethod ----
library(sp)
selectMethod("spplot",
             signature="SpatialGridDataFrame")

## ---- fromNS ----
getFromNamespace("spplot.grid", ns="sp")

## ---- showMethods ----
showMethods(classes='SpatialGridDataFrame')
