## ---- sec-11.1 ----

## ---- load-tm ----
library(tm)

## ---- make-paths ----
## Create paths to the text files, stored in the
## subdirectory "texts" of the DAAGviz package.
txdir <- system.file("texts", package="DAAGviz")
dir(txdir, pattern=".txt$")
txfiles <- dir(txdir, pattern=".txt$", full.names=TRUE)

## ---- readLines-1 ----
## Input first file, with one text string per line
tx1 <- readLines(txfiles[1], encoding="UTF-8", warn=FALSE)
## Join the separate text strings end to end
tx1 <- paste(tx1, collapse=" ")

## ---- readLines2-3 ----
tx2 <- readLines(txfiles[2], encoding="UTF-8", warn=FALSE)
tx2 <- paste(tx2, collapse=" ")
tx3 <- readLines(txfiles[3], encoding="UTF-8", warn=FALSE)
tx3 <- paste(tx3, collapse=" ")

## ---- VectorSource ----
txcorp <- Corpus(VectorSource(c(tx1, tx2, tx3)))

## ---- DirSource ----
dirSource <- DirSource(directory=txdir,
                       pattern=".txt$")

## ---- dirTOcorpus ----
toUTF8 <- function(x) iconv(x, to="UTF-8",
                            sub = "byte")
txcorp <- Corpus(dirSource)
txcorp <- tm_map(txcorp,
    content_transformer(toUTF8))

## ---- make-tdm ----
ctl <- list(stopwords = c(stopwords(), "[1]"),
            removePunctuation = list(preserve_intra_word_dashes = FALSE),
            removeNumbers = TRUE, stopwords=c(stopwords(), "[1]"),
            minDocFreq = 2)
tx.tdm <- TermDocumentMatrix(txcorp, control=ctl)

## ---- findFreq100 ----
findFreqTerms(tx.tdm, 100)

## ---- load-wordcloud ----
library(wordcloud)

## ---- wordcloud1-3 ----
pal <- brewer.pal(6, "Dark2")
fnam1 <- as.matrix(tx.tdm)[,1]
wordcloud(names(fnam1), fnam1, max.words=80, colors=pal[-1],
          random.order=FALSE, scale=c(8.5,.5))
mtext(side=3, line=3.5, "A: Chapters 1 - 5", adj=0, cex=1.8)
fnam2 <- as.matrix(tx.tdm)[,2]
wordcloud(names(fnam2), fnam2, max.words=80, colors=pal[-1],
          random.order=FALSE, scale=c(4.5,.5))
mtext(side=3, line=3.5, "B: Chapters 6 - 7", adj=0, cex=1.8)
fnam3 <- as.matrix(tx.tdm)[,3]
wordcloud(names(fnam3), fnam3, max.words=80, colors=pal[-1],
          random.order=FALSE, scale=c(6.0,.5))
mtext(side=3, line=3.5, "C: Chapters 8 - 9", adj=0, cex=1.8)

## ---- sec-11.2 ----

## ---- from-pdf ----
uri <- "doc/ch1-5prelims.pdf"
pdfReadFun <- readPDF(PdftotextOptions = "-layout")
txx1 <- pdfReadFun(elem = list(uri = uri),
                   language = "en", id = "prelims")

## ---- ex-pdf-Corpus ----
txXpdf <- Corpus(DirSource(directory="doc", pattern=".pdf$"),
                 readerControl=list(reader=readPDF,
                 PdftotextOptions = "-layout"))

## ---- sec-11.3 ----

## ---- pathto ----
(pathto <- system.file("texts", package="tm"))
dir(pathto)

## ---- docnames-Ovid, eval=TRUE ----
dir(paste(pathto, "txt",sep="/"))

## ---- ovid ----
(ovid <-
   Corpus(DirSource(paste(pathto, "txt", sep="/")),
          readerControl=list(language="lat")))
