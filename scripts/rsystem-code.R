## ---- secA-3 ----

## ---- sys-getenv ----
names(Sys.getenv())

## ---- secA-4 ----

## ---- setRepos ----
setRepositories()

## ---- setRepos12 ----
setRepositories(ind=1:2)

## ---- secA-5 ----

## ---- secA-7 ----

## ---- libPaths ----
.libPaths("D:/R-3.0.2/library")

## ---- dotFirst ----
.First <- function().libPaths("D:/R-3.0.2/library")

## ---- Renviron ----
cat('LIB1="C:/Users/owner/R/win-library/2.15"\n',
    file = "~/.Renviron", append = TRUE)
cat('LIB2="C:/Users/owner/R/win-library/2.14"\n',
    file = "~/.Renviron", append = TRUE)
cat('LIB1="R_LIBS_USER=${LIB1};${LIB2}\n',
    file = "~/.Renviron", append = TRUE)

## ---- checkBuilt ----
update.packages(checkBuilt=TRUE, ask=FALSE)
## Check package status:
summary(packageStatus())

## ---- secA-8 ----

## ---- opts ----
options(papersize="a4")
options(editor="notepad")         # Preferred editor
options(pager="internal")
options(tab.width = 4)
options(width = 120)              # Wider console line
options(graphics.record=TRUE)
options(show.signif.stars=FALSE)  # Banish the stars
options(prompt="? ")              # Use '?' as prompt
options(continue="  ")            # Blank continuation
.libPaths("C:/my_R_library")      # Add library to path.
