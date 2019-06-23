## ---- opt78 ----
options(width=78)

## ---- opt54 ----
options(width=54)

## ---- sec-4.2 ----

## ---- RData ----
volume <- c(351, 955, 662, 1203, 557, 460)
weight <- c(250, 840, 550, 1360, 640, 420)
save(volume, weight, file="books.RData")
  # Can save many objects in the same file
rm(volume, weight)      # Remove volume and weight
load("books.RData")     # Recover the saved objects

## ---- save-RData ----
save.image(file="archive.RData")

## ---- save-archive ----
fnam <- "2014Feb1.RData"
save.image(file=fnam)

## ---- dump ----
volume <- c(351, 955, 662, 1203, 557, 460)
weight <- c(250, 840, 550, 1360, 640, 420)
dump(c("volume", "weight"), file="volwt.R")
rm(volume, weight)
source("volwt.R")      # Retrieve volume & weight

## ---- sec-4.3 ----

## ---- fromZip ----
install.packages(pkgs="D:/DAAG_1.22.zip",
                 repos=NULL)

## ---- searchEtc ----
search()
sessionInfo()

## ---- setRepos ----
setRepositories()

## ---- setRepos12 ----
setRepositories(ind=1:2)  # CRAN and CRAN (extras)

## ---- search ----
search()

## ---- sessionInfo ----
sessionInfo()

## ---- attach-RData ----
attach("books.RData")

## ---- detach-RData ----
detach("file:books.RData")

## ---- R.home ----
R.home()

## ---- sysfile ----
system.file(package="DAAG")

## ---- sysfile2 ----
system.file("misc/viewtemps.RData", package="DAAG")

## ---- opts ----
options()$width

## ---- opts-wid60 ----
options(width=60)

## ---- opts-digits ----
sqrt(10)
opt <- options(digits=2) # Change until further notice,
                         # or until end of session.
sqrt(10)
options(opt)             # Return to earlier setting

## ---- round ----
c(round(9*sqrt(85/7), 2),  9*round(sqrt(85/7), 2))

## ---- 4.4.2-Exercises

## ---- table-NA ----
## Exercise 4
library(DAAG)
with(rainforest, table(complete.cases(root), species))

## ---- Acmena ----
## Exercise 10
Acmena <- subset(rainforest, species=="Acmena smithii")

## ---- Acmena-2species ----
## Exercise 10
AcSpecies <- subset(rainforest, species %in% c("Acacia mabellae",
                                               "Acmena smithii"))


