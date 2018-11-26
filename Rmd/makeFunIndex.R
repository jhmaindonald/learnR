makeFunIndex <-
function (sourceFile = "~/_notes/dm/modreg.txt", fileout = NULL,
          notfun = c("", "al", "Pr", "T", "F", "n", "P", "p", ".2szSD", ".4szSD",
                          "y", "A", "\\n", "transformation", "left", "f", "site.",
                          "a", "b", "II", "ARCH", "ARMA", "MA", "AR", "R_", "xf"))
{
    len <- nchar(sourceFile)
    if (is.null(fileout)) {
        if (substring(sourceFile, len - 3, len - 3) == ".")
            fnam <- substring(sourceFile, 1, len - 4)
        else fnam <- sourceFile
        fileout <- paste(fnam, ".idx", sep = "")
        print(c(`Output will be written to ` = fileout))
    }
    tx <- readLines(sourceFile, warn = FALSE, encoding='UTF-8')
    entrymat <- locatefun(tx, notfun)
    funentries <- paste("\\indexentry  ", "{", entrymat[, 1],
        "}{", entrymat[, 2], "}", sep = "")
    write(funentries, file = fileout)
    invisible(entrymat)
}

locatefun <-
function (txlines = DAAGURtx,
          notfun=c("", "al", "Pr", "T", "F", "n", "P",
                          "y", "A", "\\n", "transformation", "left", "f", "site.",
                          "a", "b", "II", "ARCH", "ARMA", "MA", "AR", "R_"))
{
    idtxt <- "\\.?[a-zA-Z][a-zA-Z0-9]*(\\.[a-zA-Z]+)*\\("
    z <- attr(regexpr("\f+", txlines), "match.length")
    z[z <= 0] <- 0
    page <- cumsum(z) + 1
    k <- 0
    findfun <- function(tx) {
        mn <- t(sapply(tx, function(x) {
            m <- regexpr(idtxt, x)
            c(m, attr(m, "match.length"))
        }))
        mn[, 2] <- mn[, 1] + mn[, 2]
        rownames(mn) <- paste(1:dim(mn)[1])
        mn
    }
    for (i in 1:100) {
        mn <- findfun(txlines)
        if (all(mn[, 1] == -1))
            break
        here <- mn[, 1] > 0
        page <- page[here]
        txlines <- txlines[here]
        mn <- mn[here, , drop = FALSE]
        m1 <- regexpr("\\(", txlines) - 1
        tx1 <- substring(txlines, mn[, 1], m1)
        if (i == 1)
            xy <- data.frame(nam = I(tx1), page = page)
        else xy <- rbind(xy, data.frame(nam = I(tx1), page = page))
        txlines <- substring(txlines, mn[, 2])
        here2 <- nchar(txlines) > 0
        txlines <- txlines[here2]
        page <- page[here2]
        if (length(txlines) == 0)
            break
    }
    zz <- !xy[, 1] %in% notfun
    xy <- xy[zz, ]
    nam <- xy$nam
    ch <- substring(nam, 1, 1)
    nam[ch %in% c("=", " ", ",")] <- substring(nam[ch %in% c("=",
                                                             " ", ",")], 2)
    nam <- gsub("_", "\\_", nam, fixed = TRUE)
    xy$nam <- nam
    ord <- order(xy[, 2])
    xy[ord, ]
}
