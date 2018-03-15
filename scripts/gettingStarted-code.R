## ---- Sec-1.2 ----

## ---- two5 ----
2+5

## ---- two5result ----
result <- 2+5
result

## ---- result ----
result

## ---- ls ----
ls()

## ---- print-2 ----
print(2)

## ---- just-2 ----
2

## ---- help-search ----
??Arithmetic
??base::Arith
  # Search base R only

## ---- sec-1.3 ----

## ---- install-Rcmdr ----
install.packages("Rcmdr", dependencies=TRUE)

## ---- install-bioC ----
source("http://bioconductor.org/biocLite.R")
biocLite()

## ---- bioC-add ----
biocLite(c("GenomicFeatures", "AnnotationDbi"))

## ---- sec-1.4 ----

## ---- calc ----
2+3        # Simple arithmetic
1:5        # The numbers 1, 2, 3, 4, 5
mean(1:5)  # Average of 1, 2, 3, 4, 5
sum(1:5)   # Sum of 1, 2, 3, 4, 5
(8:10)^2   # 8^2 (8 to the power of 2), 9^2, 10^2

## ---- logscales ----
log2(c(0.5, 1, 2, 4, 8))
log10(c(0.1, 1, 10, 100, 1000))

## ---- relational ----
(1:5) > 2  # Returns FALSE FALSE  TRUE  TRUE  TRUE

## ---- demo ----
demo(graphics)   # Type <Enter> for each new graph
library(lattice)
demo(lattice)

## ---- demo1 ----
demo(image)
demo(persp)

## ---- demo2 ----
library(vcd)
demo(mosaic)

## ---- avail-demo ----
## List demonstrations in attached packages
demo()
## List demonstrations in all installed packages
demo(package = .packages(all.available = TRUE))

## ---- sec-1.5 ----

## ---- volwt ----
volume <- c(351, 955, 662, 1203, 557, 460)
weight <- c(250, 840, 550, 1360, 640, 420)

## ---- describe ----
description <- c("Aird's Guide to Sydney",
 "Moon's Australia handbook",
 "Explore Australia Road Atlas",
 "Australian Motoring Guide",
 "Penguin Touring Atlas", "Canberra - The Guide")

## ---- ls1 ----
ls()

## ---- ls2 ----
ls(pattern="ume")   # Names that include "ume"

## ---- start-end ----
ls(pattern="^des")
  ## begins with 'des'
ls(pattern="ion$")
  ## ends with 'ion'

## ---- volvals ----
volume

## ---- vol6 ----
volume[6]

## ---- vol2wt ----
weight/volume

## ---- round-wt-vol ----
round(x=weight/volume, digits=2)

## ---- round-simpler ----
round(weight/volume, 2)

## ---- args-round ----
args(round)

## ---- table-type ----
type <- c("Guide","Guide","Roadmaps","Roadmaps",
          "Roadmaps","Guide")
table(type)

## ---- denplot ----
## Code
plot(weight ~ volume, pch=16, cex=1.5)
  # pch=16: use solid blob as plot symbol
  # cex=1.5: point size is 1.5 times default
## Alternative
plot(volume, weight, pch=16, cex=1.5)

## ---- axlab ----
plot(weight ~ volume, pch=16, cex=1.5,
     xlab="Volume (cubic mm)", ylab="Weight (g)")

## ---- identify ----
identify(weight ~ volume, labels=description)

## ---- sec-2.3 ----

## ---- gp2df ----
## Group columns together into a data frame
travelbooks <- data.frame(
   thickness = c(1.3, 3.9, 1.2, 2, 0.6, 1.5),
   width = c(11.3, 13.1, 20, 21.1, 25.8, 13.1),
   height = c(23.9, 18.7, 27.6, 28.5, 36, 23.4),
   weight = weight,  # Use values entered earlier
   volume = volume,  # Use values entered earlier
   type = c("Guide", "Guide", "Roadmaps", "Roadmaps",
            "Roadmaps", "Guide"),
   row.names = description
)
## Remove objects that are not now needed.
rm(volume, weight, description)

## ---- df-xcol ----
travelbooks[, 4]
travelbooks[, "weight"]
travelbooks$weight
travelbooks[["weight"]]   # Reference as a list.

## ---- data-param ----
plot( weight ~ volume, data=travelbooks)

## ---- with ----
## Take columns from the specified data frame
with(travelbooks, plot(weight ~ volume))

## ---- attach-df ----
attach(travelbooks)
plot( weight ~ volume)
detach(travelbooks)
 ## Detach when no longer
 ## required.

## ---- sec-1.6 ----

## ---- place-travelbooks ----
## Place the file in the working directory
## NB: DAAG must be installed
DAAG::datafile("travelbooks")

## ---- check-dir ----
dir(pattern="travel")
  # File(s) whose name(s) include 'travel'

## ---- input2df ----
## Input the file to the data frame travelbooks
travelbooks <- read.table("travelbooks.txt",
                          header=TRUE, row.names=1)

## ---- sec-1.7 ----

## ---- help ----
help()           # Help for the help function
help(plot)       # Show the help page for plot
?plot            # Shorthand for help(plot)
example(plot)    # Run examples from help(plot)
demo()           # List available demonstrations
vignette()       # Get information on vignettes
                 # NB also browseVignettes()

## ---- search ----
help.search("bar")
??bar            # Shorthand: help.search("bar")
??graphics::bar  # Search graphics package only

## ---- search-pkg ----
help.search("str", package="base")
help.search("char", package="base")

## ---- 1.7.2-Exercises ----

## ---- get-bestTimes ----
## Exercise 1
library(DAAG)
datafile("bestTimes")

## ---- bestTimes ----
## Exercise 1a
## file.show("bestTimes.txt")
bestTimes <- read.table("bestTimes.txt")

## ---- bestT-manip ----
## Exercise 1b
bestTimes$Time <- with(bestTimes,
                       h*60 + min + sec/60)
  # Time in minutes
names(bestTimes)[2:4]   # Check column names
bestTimes <- bestTimes[, -(2:4)]
                        # omit columns 2:4

## ---- alt-plot ----
## Exercise 1c
plot(Time ~ Distance, data=bestTimes)
## Now use a log scale
plot(log(Time) ~ log(Distance), data=bestTimes)
plot(Time ~ Distance, data=bestTimes, log="xy")

## ---- save-bestT ----
## Exercise 1d
save(bestTimes, file="bestTimes.RData")

