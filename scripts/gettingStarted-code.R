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

