## ---- install-pkgs ----
install.packages(c("rgdal","gstat","sp"),
                 dependencies=TRUE)

## ---- sec-13.1 ----

## ---- load-DAAG-oz ----
library(oz)
library(DAAG)

## ---- ss-13.1.1 ----

## ---- oz-sites ----
oz(sections=c(3:5, 11:16), col="gray")
chh <- par()$cxy[2]
with(possumsites, {
  points(Longitude,
         Latitude+c(0,0,0,.2,-.2, 0,0)*chh,
         col="blue")
  text(Latitude ~ Longitude,
       labels=rownames(possumsites),
       col="red", pos=c(2,4,2,1,3,2,2), xpd=TRUE)
  # pos = 1:below, 2:left, 3:above, 4:right
  # xpd=TRUE allows plotting outside figure region
})

## ---- ss-13.1.2 ----

## ---- load-dismo ----
library(dismo, quietly=TRUE)
  ## The raster and sp packages are dependencies

## ---- dismo-acton ----
cbr <- gmap("Canberra, ACT")
plot(cbr)
acton <- gmap("Acton, ACT", type="satellite")
plot(acton)

## ---- dismo-TBhouse ----
TBhouse <- gmap('11 Bowen St, Wellington, NZ',
                type='satellite', scale=2, zoom=20)
plot(TBhouse)

## ---- google-possums ----
## Extend longitude & latitude ranges slightly
lonlat <- with(possumsites,
               c(range(Longitude)+c(-3,3),
                 range(Latitude)+c(-2,2))
)
## Obtain map, as a ``RasterLayer'' object
googmap <- gmap(extent(lonlat))
plot(googmap, inter=TRUE)
## From latitude/longitude to Mercator projection
xy <- Mercator(with(possumsites,
                    cbind(Longitude, Latitude)))
## Points show location of sites on the map
points(xy)
## Add labels that give the names
text(xy, labels=row.names(possumsites))

## ---- drawExtent ----
newlims <- drawExtent()

## ---- googmap ----
googmap2 <- gmap(newlims)
plot(googmap2)

## ---- ss-13.1.3 ----

## ---- get-quakes ----
library(DAAGviz)
fullpath <- system.file('datasets/nzquakes.CSV',
                        package='DAAGviz')
quakes <- read.csv(fullpath)
quakes$Date <- as.Date(quakes$Date)
quakes$Energy <- 10^quakes$magnitude/1000000

## ---- load-plotKML ----
require(plotKML)

## ---- ready-data ----
## Prepare data for plotting
coordinates(quakes) <- ~ longitude+latitude
proj4string(quakes) <-
  CRS("+proj=longlat +datum=WGS84")

## ---- plotKML ----
plotKML(quakes['Energy'], points_names="")
  # Makes circle area proportional to Energy

## ---- sec-13.2 ----

## ---- logofile ----
library(rgdal)
logofile <- system.file("pictures/Rlogo.jpg",
                        package = "rgdal")[1]
rlogo <- readGDAL(logofile, silent=TRUE)

## ---- rlogo-class ----
class(rlogo)
names(rlogo)

## ---- rlogo-image ----
image(rlogo, red="band1",
      green="band2",
      blue="band3")

## ---- spplot-col3 ----
## Code
col3 <- c("red","green","blue")
spplot(rlogo, zcol=1:3, names.attr=col3,
       col.regions=grey(0:100/100), as.table=TRUE,
       layout=c(3,1), main=paste("3-layer (RGB)",
       "raster image - example"))

## ---- sp27 ----
sp27 <- system.file("pictures/SP27GTIF.TIF",
                    package = "rgdal")[1]

## ---- sp27-info ----
GDALinfo(sp27)

## ---- sp27-read ----
SP27GTIF <- readGDAL(sp27, output.dim=c(100,100),
                     silent=TRUE)
class(SP27GTIF)

## ---- sp27-spplot ----
spplot(SP27GTIF)

## ---- ss-13.2.1 ----

## ---- meuse-bubble ----
library(sp)
data(meuse); data(meuse.riv)
coordinates(meuse) <- ~ x + y
gph <- bubble(meuse, "zinc", pch=1, key.entries =  100 * 2^(0:4),
              main = "Zinc(ppm)", scales=list(axes=TRUE, tck=0.4))
add <- latticeExtra::layer(panel.lines(meuse.riv[,1], meuse.riv[,2],
                      col="gray"))
gph+add

## ---- sec-13.3 ----

## ---- show-shape ----
dsn <- system.file("vectors", package = "rgdal")[1]
dir(dsn, pattern="shp$")

## ---- ogrInfo ----
ogrInfo(dsn=dsn,
        layer="cities")

## ---- shapefiles-names ----
## Get names of files in shapefile collection
dir(dsn, pattern="cities")

## ---- shape-in ----
cities <- readOGR(dsn=dsn, layer="cities", verbose=FALSE)

## ---- cities-info ----
slotNames(cities)
names(cities)
  ## Returns the names in the data slot
length(levels(cities$COUNTRY))

## ---- cities-summary ----
summary(cities)

## ---- canada-plot ----
canada <- subset(cities, COUNTRY=="Canada")
trellis.par.set(sp.theme())
spplot(canada, zcol="POPULATION")

## ---- ASGC-shapefiles ----
url <- paste0("http://www.abs.gov.au/ausstats/subscriber.nsf/",
  "log?openagent&1259030001_sr11aaust_shape.zip&1259.0.30.001",
  "&Data%20Cubes&B8003880BC09FA5BCA2578CC00124E25&0",
  "&July%202011&14.07.2011&Latest")
downloadTo <- "../downloads/1259030001_sr11aaust_shape.zip"
download.file(url, destfile=downloadTo)

## ---- unzip-au-shape ----
unzip("../downloads/1259030001_sr11aaust_shape.zip", exdir="au-srs")
dir("../downloads/au-srs")

## ---- ogr-au-shape ----
auSRS <- readOGR("../downloads/au-srs", layer="SR11aAust")

## ---- xtract-vic ----
vicSRS <- subset(auSRS, STATE_CODE==2)
unique(vicSRS@data[,"SR_NAME11"])


