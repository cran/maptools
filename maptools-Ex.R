pkgname <- "maptools"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('maptools')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CCmaps")
### * CCmaps

flush(stderr()); flush(stdout())

### Name: CCmaps
### Title: Conditioned choropleth maps
### Aliases: CCmaps
### Keywords: spatial

### ** Examples

nc.sids <- readShapeSpatial(system.file("shapes/sids.shp",
 package="maptools")[1], IDvar="FIPSNO",
 proj4string=CRS("+proj=longlat +ellps=clrk66"))
nc.sids$ft.SID74 <- sqrt(1000)*(sqrt(nc.sids$SID74/nc.sids$BIR74) +
 sqrt((nc.sids$SID74+1)/nc.sids$BIR74))
nc.sids$ft.NWBIR74 <- sqrt(1000)*(sqrt(nc.sids$NWBIR74/nc.sids$BIR74) +
 sqrt((nc.sids$NWBIR74+1)/nc.sids$BIR74))
library(lattice)
sh_nw4 <- equal.count(nc.sids$ft.NWBIR74, number=4, overlap=1/5)
CCmaps(nc.sids, "ft.SID74", list("Nonwhite_births"=sh_nw4),
 col.regions=colorRampPalette(c("yellow1", "brown3"))(20),
 main="Transformed SIDS rates 1974-8")



cleanEx()
nameEx("ContourLines2SLDF")
### * ContourLines2SLDF

flush(stderr()); flush(stdout())

### Name: ContourLines2SLDF
### Title: Converter functions to build SpatialLinesDataFrame objects
### Aliases: ArcObj2SLDF ContourLines2SLDF MapGen2SL
### Keywords: spatial

### ** Examples

#data(co37_d90_arc) # retrieved as: 
# library(RArcInfo)
# fl <- "http://www.census.gov/geo/cob/bdy/co/co90e00/co37_d90_e00.zip"
# download.file(fl, "co37_d90_e00.zip")
# e00 <- zip.file.extract("co37_d90.e00", "co37_d90_e00.zip")
# e00toavc(e00, "ncar")
# arc <- get.arcdata(".", "ncar")
#res <- arcobj2SLDF(arc)
#plot(res)
#invisible(title(""))
res <- ContourLines2SLDF(contourLines(volcano))
plot(res, col=terrain.colors(nrow(as(res, "data.frame"))))
title("Volcano contours as SpatialLines")



cleanEx()
nameEx("GE_SpatialGrid")
### * GE_SpatialGrid

flush(stderr()); flush(stdout())

### Name: GE_SpatialGrid
### Title: Create SpatialGrid for PNG output to GE
### Aliases: GE_SpatialGrid Sobj_SpatialGrid
### Keywords: spatial

### ** Examples

opt_exask <- options(example.ask=FALSE)
qk <- SpatialPointsDataFrame(quakes[, c(2:1)], quakes)
summary(Sobj_SpatialGrid(qk)$SG)
t2 <- Sobj_SpatialGrid(qk, n=10000)$SG
summary(t2)
prod(slot(slot(t2, "grid"), "cells.dim"))
proj4string(qk) <- CRS("+proj=longlat")
tf <- tempfile()
SGqk <- GE_SpatialGrid(qk)
png(file=paste(tf, ".png", sep=""), width=SGqk$width, height=SGqk$height,
  bg="transparent")
par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
plot(qk, xlim=SGqk$xlim, ylim=SGqk$ylim, setParUsrBB=TRUE)
dev.off()
kmlOverlay(SGqk, paste(tf, ".kml", sep=""), paste(tf, ".png", sep=""))
## Not run: 
##D qk0 <- quakes
##D qk0$long <- ifelse(qk0$long <= 180, qk0$long, qk0$long-360)
##D qk0a <- SpatialPointsDataFrame(qk0[, c(2:1)], qk0)
##D proj4string(qk0a) <- CRS("+proj=longlat")
##D writeOGR(qk0a, paste(tf, "v.kml", sep=""), "Quakes", "KML")
##D system(paste("googleearth ", tf, ".kml", sep=""))
## End(Not run)
options(example.ask=opt_exask)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("Rgshhs")
### * Rgshhs

flush(stderr()); flush(stdout())

### Name: Rgshhs
### Title: Read GSHHS data into sp object
### Aliases: Rgshhs getRgshhsMap
### Keywords: spatial

### ** Examples

if (!rgeosStatus()) gpclibPermit()
gshhs.c.b <- system.file("share/gshhs_c.b", package="maptools")
WEx <- c(-12, 3)
WEy <- c(48, 59)
WE <- getRgshhsMap(gshhs.c.b, xlim=WEx, ylim=WEy)
plot(WE, col="khaki", xlim=WEx, ylim=WEy, xaxs="i", yaxs="i", axes=TRUE)
NZx <- c(160,180)
NZy <- c(-50,-30)
NZ <- Rgshhs(gshhs.c.b, xlim=NZx, ylim=NZy)
plot(NZ$SP, col="khaki", pbg="azure2", xlim=NZx, ylim=NZy, xaxs="i", yaxs="i", axes=TRUE)
GLx <- c(265,285)
GLy <- c(40,50)
GL <- Rgshhs(gshhs.c.b, xlim=GLx, ylim=GLy)
plot(GL$SP, col="khaki", pbg="azure2", xlim=GLx, ylim=GLy, xaxs="i", yaxs="i", axes=TRUE)
BNLx <- c(2,8)
BNLy <- c(49,54)
wdb_lines <- system.file("share/wdb_borders_c.b", package="maptools")
BNLp <- Rgshhs(gshhs.c.b, xlim=BNLx, ylim=BNLy)
BNLl <- Rgshhs(wdb_lines, xlim=BNLx, ylim=BNLy)
plot(BNLp$SP, col="khaki", pbg="azure2", xlim=BNLx, ylim=BNLy, xaxs="i", yaxs="i", axes=TRUE)
lines(BNLl$SP)



cleanEx()
nameEx("SpatialLines2PolySet")
### * SpatialLines2PolySet

flush(stderr()); flush(stdout())

### Name: SpatialLines2PolySet
### Title: Convert sp line and polygon objects to PBSmapping PolySet
###   objects
### Aliases: SpatialLines2PolySet SpatialPolygons2PolySet
###   PolySet2SpatialPolygons PolySet2SpatialLines
### Keywords: spatial

### ** Examples

library(PBSmapping)
library(maps)
nor_coast_lines <- map("world", interior=FALSE, plot=FALSE, xlim=c(4,32),
 ylim=c(58,72))
nor_coast_lines <- pruneMap(nor_coast_lines, xlim=c(4,32), ylim=c(58,72))
nor_coast_lines_sp <- map2SpatialLines(nor_coast_lines,
 proj4string=CRS("+proj=longlat +datum=wgs84"))
nor_coast_lines_PS <- SpatialLines2PolySet(nor_coast_lines_sp)
summary(nor_coast_lines_PS)
plotLines(nor_coast_lines_PS)
o3 <- PolySet2SpatialLines(nor_coast_lines_PS)
plot(o3, axes=TRUE)
nor_coast_poly <- map("world", "norway", fill=TRUE, col="transparent",
 plot=FALSE, ylim=c(58,72))
IDs <- sapply(strsplit(nor_coast_poly$names, ":"), function(x) x[1])
nor_coast_poly_sp <- map2SpatialPolygons(nor_coast_poly, IDs=IDs,
 proj4string=CRS("+proj=longlat +datum=wgs84"))
nor_coast_poly_PS <- SpatialPolygons2PolySet(nor_coast_poly_sp)
summary(nor_coast_poly_PS)
plotPolys(nor_coast_poly_PS)
o1 <- PolySet2SpatialPolygons(nor_coast_poly_PS)
plot(o1, axes=TRUE)



cleanEx()
nameEx("SplashDams")
### * SplashDams

flush(stderr()); flush(stdout())

### Name: SplashDams
### Title: Data for Splash Dams in western Oregon
### Aliases: SplashDams
### Keywords: datasets

### ** Examples

data(SplashDams)
plot(SplashDams, axes=TRUE)



cleanEx()
nameEx("as.ppp")
### * as.ppp

flush(stderr()); flush(stdout())

### Name: as.ppp
### Title: coercion between sp objects and spatstat objects
### Aliases: as.owin.SpatialGridDataFrame as.owin.SpatialPixelsDataFrame
###   as.owin.SpatialPolygons as.im.SpatialGridDataFrame
###   coerce,SpatialGridDataFrame,owin-method
###   coerce,SpatialPixelsDataFrame,owin-method
###   coerce,SpatialPolygons,owin-method as.SpatialPolygons.tess
###   as.SpatialPolygons.owin coerce,tess,SpatialPolygons-method
###   coerce,owin,SpatialPolygons-method
###   coerce,SpatialGridDataFrame,im-method as.ppp.SpatialGridDataFrame
###   as.ppp.SpatialPoints as.ppp.SpatialPointsDataFrame
###   coerce,SpatialPoints,ppp-method
###   coerce,SpatialPointsDataFrame,ppp-method as.psp.Line as.psp.Lines
###   as.psp.SpatialLines as.psp.SpatialLinesDataFrame
###   coerce,Line,psp-method coerce,Lines,psp-method
###   coerce,SpatialLines,psp-method
###   coerce,SpatialLinesDataFrame,psp-method as.SpatialLines.psp
###   coerce,psp,SpatialLines-method coerce,im,SpatialGridDataFrame-method
###   coerce,ppp,SpatialGridDataFrame-method
###   coerce,ppp,SpatialPointsDataFrame-method
###   coerce,ppp,SpatialPoints-method as.SpatialPointsDataFrame.ppp
###   as.SpatialGridDataFrame.ppp as.SpatialPoints.ppp
###   as.SpatialGridDataFrame.im
### Keywords: spatial

### ** Examples

library(spatstat)
data(meuse)
coordinates(meuse) = ~x+y
zn1 <- as(meuse["zinc"], "ppp")
zn1
plot(zn1)
as(as(meuse, "SpatialPoints"), "ppp")
data(meuse.grid)
gridded(meuse.grid) = ~x+y
mg_owin <- as(meuse.grid, "owin")
zn1a <- ppp(x=zn1$x, y=zn1$y, marks=zn1$marks, window=mg_owin)
zn1a
plot(zn1a)
rev_ppp_SP <- as.SpatialPoints.ppp(zn1a)
summary(rev_ppp_SP)
rev_ppp_SPDF <- as.SpatialPointsDataFrame.ppp(zn1a)
summary(rev_ppp_SPDF)
rev_ppp_SGDF <- as.SpatialGridDataFrame.ppp(zn1a)
summary(rev_ppp_SGDF)
data(meuse.riv)
mr <- Line(meuse.riv)
mr_psp <- as(mr, "psp")
mr_psp
plot(mr_psp)
xx_back <- as(mr_psp, "SpatialLines")
plot(xx_back)
xx <- readShapeLines(system.file("shapes/fylk-val.shp", package="maptools")[1],
 proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
xx_psp <- as(xx, "psp")
xx_psp
plot(xx_psp)
xx_back <- as(xx_psp, "SpatialLines")
plot(xx_back)
mg_owin <- as(as(meuse.grid["ffreq"], "SpatialPixelsDataFrame"), "owin")
mg_owin
ho_sp <- SpatialPolygons(list(Polygons(list(Polygon(cbind(c(0,1,1,0,0),
  c(0,0,1,1,0))), Polygon(cbind(c(0.6,0.4,0.4,0.6,0.6), 
  c(0.2,0.2,0.4,0.4,0.2)), hole=TRUE)), ID="ho")))
plot(ho_sp, col="red", pbg="pink")
ho <- as(ho_sp, "owin")
plot(ho)
pp <- runifpoint(500, win=ho)
plot(pp)
ho_orig <- owin(poly=list(list(x=c(0,1,1,0), y=c(0,0,1,1)),
  list(x=c(0.6,0.4,0.4,0.6), y=c(0.2,0.2,0.4,0.4))))
identical(ho, ho_orig)
ho_sp1 <- as(ho, "SpatialPolygons")
all.equal(ho_sp, ho_sp1, check.attributes=FALSE)
A <- tess(xgrid=0:4,ygrid=0:4)
A_sp <- as(A, "SpatialPolygons")
plot(A_sp)
text(coordinates(A_sp), labels=row.names(A_sp), cex=0.6)
mg_dist <- meuse.grid["dist"]
fullgrid(mg_dist) <- TRUE
image(mg_dist, axes=TRUE)
mg_im <- as(mg_dist, "im")
plot(mg_im)
mg2 <- as.SpatialGridDataFrame.im(mg_im)
image(mg2, axes=TRUE)



cleanEx()
nameEx("asciigrid")
### * asciigrid

flush(stderr()); flush(stdout())

### Name: readAsciiGrid
### Title: read/write to/from (ESRI) asciigrid format
### Aliases: readAsciiGrid writeAsciiGrid
### Keywords: programming

### ** Examples

x <- readAsciiGrid(system.file("grids/test.ag", package="maptools")[1])
summary(x)
image(x)
xp <- as(x, "SpatialPixelsDataFrame")
abline(h=332000, lwd=3)
xpS <- xp[coordinates(xp)[,2] < 332000,]
summary(xpS)
xS <- as(xpS, "SpatialGridDataFrame")
summary(xS)
tmpfl <- paste(tempdir(), "testS.ag", sep="/")
writeAsciiGrid(xS, tmpfl)
axS <- readAsciiGrid(tmpfl)
opar <- par(mfrow=c(1,2))
image(xS, main="before export")
image(axS, main="after import")
par(opar)
unlink(tmpfl)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("checkPolygonsHoles")
### * checkPolygonsHoles

flush(stderr()); flush(stdout())

### Name: checkPolygonsHoles
### Title: Check holes in Polygons objects
### Aliases: checkPolygonsHoles rgeosStatus gpclibPermit gpclibPermitStatus
### Keywords: spatial

### ** Examples

if (!rgeosStatus()) gpclibPermit()
nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +ellps=clrk66"))
pl <- slot(nc1, "polygons")
sapply(slot(pl[[4]], "Polygons"), function(x) slot(x, "hole"))
pl[[4]] <- Polygons(list(slot(pl[[4]], "Polygons")[[1]],
 Polygon(slot(slot(pl[[4]], "Polygons")[[2]], "coords"), hole=TRUE),
 slot(pl[[4]], "Polygons")[[3]]), slot(pl[[4]], "ID"))
sapply(slot(pl[[4]], "Polygons"), function(x) slot(x, "hole"))
pl_new <- lapply(pl, checkPolygonsHoles)
sapply(slot(pl_new[[4]], "Polygons"), function(x) slot(x, "hole"))
srs <- slot(slot(pl[[1]], "Polygons")[[1]], "coords")
hle2 <- structure(c(-81.64093, -81.38380, -81.34165, -81.66833, -81.64093, 
 36.57865, 36.57234, 36.47603, 36.47894, 36.57865), .Dim = as.integer(c(5, 2)))
hle3 <- structure(c(-81.47759, -81.39118, -81.38486, -81.46705, -81.47759, 
 36.56289, 36.55659, 36.49907, 36.50380, 36.56289), .Dim = as.integer(c(5, 2)))
x <- Polygons(list(Polygon(srs), Polygon(hle2), Polygon(hle3)),
 ID=slot(pl[[1]], "ID"))
sapply(slot(x, "Polygons"), function(x) slot(x, "hole"))
res <- checkPolygonsHoles(x)
sapply(slot(res, "Polygons"), function(x) slot(x, "hole"))
## Not run: 
##D opar <- par(mfrow=c(1,2))
##D SPx <- SpatialPolygons(list(x))
##D plot(SPx)
##D text(t(sapply(slot(x, "Polygons"), function(i) slot(i, "labpt"))),
##D  labels=sapply(slot(x, "Polygons"), function(i) slot(i, "hole")), cex=0.6)
##D title(xlab="Hole slot values before checking")
##D SPres <- SpatialPolygons(list(res))
##D plot(SPres)
##D text(t(sapply(slot(res, "Polygons"), function(i) slot(i, "labpt"))),
##D  labels=sapply(slot(res, "Polygons"), function(i) slot(i, "hole")), cex=0.6)
##D title(xlab="Hole slot values after checking")
##D par(opar)
##D p1 <- Polygon(cbind(x=c(0, 0, 10, 10, 0), y=c(0, 10, 10, 0, 0))) # I
##D p2 <- Polygon(cbind(x=c(3, 3, 7, 7, 3), y=c(3, 7, 7, 3, 3))) # H
##D p8 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1))) # H
##D p9 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(5, 6, 6, 5, 5))) # H
##D p3 <- Polygon(cbind(x=c(20, 20, 30, 30, 20), y=c(20, 30, 30, 20, 20))) # I
##D p4 <- Polygon(cbind(x=c(21, 21, 29, 29, 21), y=c(21, 29, 29, 21, 21))) # H
##D p14 <- Polygon(cbind(x=c(21, 21, 29, 29, 21), y=c(21, 29, 29, 21, 21))) # H
##D p5 <- Polygon(cbind(x=c(22, 22, 28, 28, 22), y=c(22, 28, 28, 22, 22))) # I
##D p15 <- Polygon(cbind(x=c(22, 22, 28, 28, 22), y=c(22, 28, 28, 22, 22))) # I
##D p6 <- Polygon(cbind(x=c(23, 23, 27, 27, 23), y=c(23, 27, 27, 23, 23))) # H
##D p7 <- Polygon(cbind(x=c(13, 13, 17, 17, 13), y=c(13, 17, 17, 13, 13))) # I
##D p10 <- Polygon(cbind(x=c(24, 24, 26, 26, 24), y=c(24, 26, 26, 24, 24))) # I
##D p11 <- Polygon(cbind(x=c(24.25, 24.25, 25.75, 25.75, 24.25),
##D  y=c(24.25, 25.75, 25.75, 24.25, 24.25))) # H
##D p12 <- Polygon(cbind(x=c(24.5, 24.5, 25.5, 25.5, 24.5),
##D  y=c(24.5, 25.5, 25.5, 24.5, 24.5))) # I
##D p13 <- Polygon(cbind(x=c(24.75, 24.75, 25.25, 25.25, 24.75),
##D  y=c(24.75, 25.25, 25.25, 24.75, 24.75))) # H
##D lp <- list(p1, p2, p13, p7, p6, p5, p4, p3, p8, p11, p12, p9, p10, p14, p15)
##D #           1   2    3   4   5   6   7   8   9   10   11  12   13   14   15
##D #           0   1   11   0   6   0   8   0   1   13    0   1    0  (7)  (6)
##D #           I   H    H   I   H   I   H   I   H    H    I   H    I   ?    ?
##D pls <- Polygons(lp, ID="1")
##D comment(pls)
##D pls1 <- checkPolygonsHoles(pls)
##D comment(pls1)
##D opar <- par(mfrow=c(1,2))
##D plot(SpatialPolygons(list(pls)), col="magenta", pbg="cyan", usePolypath=FALSE)
##D title(xlab="Hole slot values before checking")
##D plot(SpatialPolygons(list(pls1)), col="magenta", pbg="cyan", usePolypath=FALSE)
##D title(xlab="Hole slot values after checking")
##D par(opar)
## End(Not run)



cleanEx()
nameEx("dotsInPolys")
### * dotsInPolys

flush(stderr()); flush(stdout())

### Name: dotsInPolys
### Title: Put dots in polygons
### Aliases: dotsInPolys
### Keywords: spatial

### ** Examples

nc_SP <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat  +ellps=clrk66"))
## Not run: 
##D library(spgpc)
##D pls <- slot(nc_SP, "polygons")
##D pls_new <- lapply(pls, checkPolygonsHoles)
##D nc_SP <- SpatialPolygonsDataFrame(SpatialPolygons(pls_new,
##D  proj4string=CRS(proj4string(nc_SP))), data=as(nc_SP, "data.frame"))
## End(Not run)
try1 <- dotsInPolys(nc_SP, as.integer(nc_SP$SID74))
plot(nc_SP, axes=TRUE)
plot(try1, add=TRUE, pch=18, col="red")
try2 <- dotsInPolys(nc_SP, as.integer(nc_SP$SID74), f="regular")
plot(nc_SP, axes=TRUE)
plot(try2, add=TRUE, pch=18, col="red")



cleanEx()
nameEx("elide-methods")
### * elide-methods

flush(stderr()); flush(stdout())

### Name: elide-methods
### Title: Methods for Function elide in Package 'maptools'
### Aliases: elide-methods elide,SpatialPoints-method
###   elide,SpatialPointsDataFrame-method elide,SpatialLines-method
###   elide,SpatialLinesDataFrame-method elide,SpatialPolygons-method
###   elide,SpatialPolygonsDataFrame-method elide
### Keywords: methods spatial

### ** Examples

data(meuse)
coordinates(meuse) <- c("x", "y")
proj4string(meuse) <- CRS("+init=epsg:28992")
data(meuse.riv)
river_polygon <- Polygons(list(Polygon(meuse.riv)), ID="meuse")
rivers <- SpatialPolygons(list(river_polygon))
proj4string(rivers) <- CRS("+init=epsg:28992")
rivers1 <- elide(rivers, reflect=c(TRUE, TRUE), scale=TRUE)
meuse1 <- elide(meuse, bb=bbox(rivers), reflect=c(TRUE, TRUE), scale=TRUE)
opar <- par(mfrow=c(1,2))
plot(rivers, axes=TRUE)
plot(meuse, add=TRUE)
plot(rivers1, axes=TRUE)
plot(meuse1, add=TRUE)
par(opar)
meuse1 <- elide(meuse, shift=c(10000, -10000))
bbox(meuse)
bbox(meuse1)
rivers1 <- elide(rivers, shift=c(10000, -10000))
bbox(rivers)
bbox(rivers1)
meuse1 <- elide(meuse, rotate=-30, center=apply(bbox(meuse), 1, mean))
bbox(meuse)
bbox(meuse1)
plot(meuse1, axes=TRUE)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("gcDestination")
### * gcDestination

flush(stderr()); flush(stdout())

### Name: gcDestination
### Title: Find destination in geographical coordinates
### Aliases: gcDestination
### Keywords: spatial

### ** Examples

data(state)
res <- gcDestination(state.center$x, state.center$y, 45, 250, "km")
plot(state.center$x, state.center$y, asp=1, pch=16)
arrows(state.center$x, state.center$y, res[,1], res[,2], length=0.05)
llist <- vector(mode="list", length=length(state.center$x))
for (i in seq(along=llist)) llist[[i]] <- gcDestination(state.center$x[i],
  state.center$y[i], seq(0, 360, 5), 250, "km")
plot(state.center$x, state.center$y, asp=1, pch=3)
nll <- lapply(llist, lines)



cleanEx()
nameEx("getKMLcoordinates")
### * getKMLcoordinates

flush(stderr()); flush(stdout())

### Name: getKMLcoordinates
### Title: Get a list of coordinates out of a KML file
### Aliases: getKMLcoordinates
### Keywords: spatial

### ** Examples

data(wrld_simpl)
## creates a KML file containing the polygons of South Africa (plus hole)
sw <- slot(wrld_simpl[wrld_simpl$NAME=="South Africa",], "polygons")[[1]]
tf <- tempfile()
kmlPolygon(sw, kmlfile=tf, name="South Africa", col="#df0000aa", lwd=5, 
    border=4, kmlname="R Test", 
    kmldescription="This is <b>only</b> a <a href='http://www.r-project.org'>R</a> test.")
zz <- getKMLcoordinates(tf, ignoreAltitude=TRUE)
str(zz)



cleanEx()
nameEx("getinfo.shape")
### * getinfo.shape

flush(stderr()); flush(stdout())

### Name: getinfo.shape
### Title: Get shapefile header information
### Aliases: getinfo.shape print.shapehead
### Keywords: spatial

### ** Examples

res <- getinfo.shape(system.file("shapes/fylk-val.shp", package="maptools")[1])
res
str(res)



cleanEx()
nameEx("gzAzimuth")
### * gzAzimuth

flush(stderr()); flush(stdout())

### Name: gzAzimuth
### Title: Find azimuth for geographical coordinates
### Aliases: gzAzimuth trackAzimuth
### Keywords: spatial

### ** Examples

name <- c("Mecca", "Anchorage", "Washington")
long <- c(39.823333, -149.883333, -77.0166667)
lat <- c(21.423333, 61.2166667, 38.9)
x <- cbind(long, lat)
row.names(x) <- name
crib <- c(-9.098363, 56.575960)
r1 <- gzAzimuth(x[2:3,], x[1,])
r1
all.equal(r1, crib)
r2 <- gzAzimuth(x[2:3,], x[1,], type="abdali")
r2
all.equal(r2, crib)
trackAzimuth(x)



cleanEx()
nameEx("holepolys")
### * holepolys

flush(stderr()); flush(stdout())

### Name: gpcholes
### Title: Hisaji Ono's lake/hole problem
### Aliases: gpcholes h1pl h2pl
### Keywords: datasets

### ** Examples

data(gpcholes)
opar <- par(mfrow=c(1,2))
plot(SpatialPolygons(list(h2pl)), col="red", pbg="white", border="blue")
plot(SpatialPolygons(list(h1pl)), col="red", pbg="white", border="blue")
par(opar)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("kmlLine")
### * kmlLine

flush(stderr()); flush(stdout())

### Name: kmlLine
### Title: Create and write a KML file on the basis of a given Lines object
### Aliases: kmlLine
### Keywords: spatial

### ** Examples

xx <- readShapeSpatial(system.file("shapes/fylk-val-ll.shp",
      package="maptools")[1], proj4string=CRS("+proj=longlat"))
out <- sapply(slot(xx, "lines"), function(x) { kmlLine(x,
    name=slot(x, "ID"), col="blue", lwd=1.5, 
    description=paste("river:", slot(x, "ID"))) })
tf <- tempfile()
kmlFile <- file(tf, "w")
tf
cat(kmlLine(kmlname="R Test", kmldescription="<i>Hello</i>")$header, 
    file=kmlFile, sep="\n")
cat(unlist(out["style",]), file=kmlFile, sep="\n")
cat(unlist(out["content",]), file=kmlFile, sep="\n")
cat(kmlLine()$footer, file=kmlFile, sep="\n")
close(kmlFile)



cleanEx()
nameEx("kmlOverlay")
### * kmlOverlay

flush(stderr()); flush(stdout())

### Name: kmlOverlay
### Title: Create and write KML file for PNG image overlay
### Aliases: kmlOverlay
### Keywords: spatial

### ** Examples

opt_exask <- options(example.ask=FALSE)
qk <- SpatialPointsDataFrame(quakes[, c(2:1)], quakes)
proj4string(qk) <- CRS("+proj=longlat")
tf <- tempfile()
SGqk <- GE_SpatialGrid(qk)
png(file=paste(tf, ".png", sep=""), width=SGqk$width, height=SGqk$height,
  bg="transparent")
par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
plot(qk, xlim=SGqk$xlim, ylim=SGqk$ylim, setParUsrBB=TRUE)
dev.off()
kmlOverlay(SGqk, paste(tf, ".kml", sep=""), paste(tf, ".png", sep=""))
## Not run: 
##D library(rgdal)
##D qk0 <- quakes
##D qk0$long <- ifelse(qk0$long <= 180, qk0$long, qk0$long-360)
##D qk0a <- SpatialPointsDataFrame(qk0[, c(2:1)], qk0)
##D proj4string(qk0a) <- CRS("+proj=longlat")
##D writeOGR(qk0a, paste(tf, "v.kml", sep=""), "Quakes", "KML")
##D system(paste("googleearth ", tf, ".kml", sep=""))
## End(Not run)
options(example.ask=opt_exask)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("kmlPoints")
### * kmlPoints

flush(stderr()); flush(stdout())

### Name: kmlPoints
### Title: Create and write a KML file on the basis of a given Points
###   object
### Aliases: kmlPoints
### Keywords: spatial

### ** Examples

data(SplashDams)
num <- length(SplashDams)
td <- tempdir()
kmlfile <- paste(td, "OregonSplashDams.kml", sep="/")
kmlname <- "Oregon Splash Dams"
kmldescription <- "Data for Splash Dams in western Oregon. See http://www.fs.fed.us/pnw/lwm/aem/people/burnett.html#projects_activities for more information."
icon <- "http://google.com/mapfiles/kml/paddle/wht-diamond.png"
name <- paste("Dam on",SplashDams$streamName)
description <- paste("<b>owner:</b>",SplashDams$owner,"<br><b>dates:</b>",SplashDams$datesUsed)

kmlPoints(SplashDams, kmlfile=kmlfile, name=name, description=description,
          icon=icon, kmlname=kmlname, kmldescription=kmldescription)



cleanEx()
nameEx("kmlPolygon")
### * kmlPolygon

flush(stderr()); flush(stdout())

### Name: kmlPolygon
### Title: Create and write a KML file on the basis of a given Polygons
###   object
### Aliases: kmlPolygon
### Keywords: spatial

### ** Examples

data(wrld_simpl)
## creates a KML file containing the polygons of South Africa (plus hole)
sw <- slot(wrld_simpl[wrld_simpl$NAME=="South Africa",], "polygons")[[1]]
tf <- tempfile()
kmlPolygon(sw, kmlfile=tf, name="South Africa", col="#df0000aa", lwd=5, 
    border=4, kmlname="R Test", 
    kmldescription="This is <b>only</b> a <a href='http://www.r-project.org'>R</a> test.")
tf

## creates a KML file containing the polygons of South Africa, Switzerland, and Canada
sw  <- wrld_simpl[wrld_simpl$NAME %in% c("South Africa", "Switzerland", "Canada"),]
out <- sapply(slot(sw, "polygons"), function(x) { kmlPolygon(x,
    name=as(sw, "data.frame")[slot(x, "ID"), "NAME"], 
    col="red", lwd=1.5, border='black', 
    description=paste("ISO3:", slot(x, "ID"))) })
tf <- tempfile()
kmlFile <- file(tf, "w")
tf
cat(kmlPolygon(kmlname="R Test", kmldescription="<i>Hello</i>")$header, 
    file=kmlFile, sep="\n")
cat(unlist(out["style",]), file=kmlFile, sep="\n")
cat(unlist(out["content",]), file=kmlFile, sep="\n")
cat(kmlPolygon()$footer, file=kmlFile, sep="\n")
close(kmlFile)



cleanEx()
nameEx("leglabs")
### * leglabs

flush(stderr()); flush(stdout())

### Name: leglabs
### Title: Make legend labels
### Aliases: leglabs
### Keywords: spatial

### ** Examples

mappolys <- readShapeSpatial(system.file("shapes/columbus.shp", package="maptools")[1], ID="NEIGNO")
brks <- round(quantile(mappolys$CRIME, probs=seq(0,1,0.2)), digits=2)
colours <- c("salmon1", "salmon2", "red3", "brown", "black")
plot(mappolys, col=colours[findInterval(mappolys$CRIME, brks,
 all.inside=TRUE)])
legend(x=c(5.8, 7.1), y=c(13, 14.5), legend=leglabs(brks),
  fill=colours, bty="n")
invisible(title(main=paste("Columbus OH: residential burglaries and vehicle",
 "thefts per thousand households, 1980", sep="\n")))



cleanEx()
nameEx("map2SpatialPolygons")
### * map2SpatialPolygons

flush(stderr()); flush(stdout())

### Name: map2SpatialPolygons
### Title: Convert map objects to sp classes
### Aliases: map2SpatialPolygons map2SpatialLines pruneMap
### Keywords: spatial

### ** Examples

library(maps)
nor_coast_poly <- map("world", "norway", fill=TRUE, col="transparent",
 plot=FALSE)
range(nor_coast_poly$x, na.rm=TRUE)
range(nor_coast_poly$y, na.rm=TRUE)
nor_coast_poly <- map("world", "norway", fill=TRUE, col="transparent",
 plot=FALSE, ylim=c(58,72))
nor_coast_poly$names
IDs <- sapply(strsplit(nor_coast_poly$names, ":"), function(x) x[1])
nor_coast_poly_sp <- map2SpatialPolygons(nor_coast_poly, IDs=IDs,
 proj4string=CRS("+proj=longlat +datum=wgs84"))
sapply(slot(nor_coast_poly_sp, "polygons"),
 function(x) length(slot(x, "Polygons")))
plot(nor_coast_poly_sp, col="grey", axes=TRUE)
nor_coast_lines <- map("world", interior=FALSE, plot=FALSE, xlim=c(4,32),
 ylim=c(58,72))
plot(nor_coast_lines, type="l")
nor_coast_lines <- pruneMap(nor_coast_lines, xlim=c(4,32), ylim=c(58,72))
lines(nor_coast_lines, col="red")
nor_coast_lines_sp <- map2SpatialLines(nor_coast_lines,
 proj4string=CRS("+proj=longlat +datum=wgs84"))
plot(nor_coast_poly_sp, col="grey", axes=TRUE)
plot(nor_coast_lines_sp, col="blue", add=TRUE)



cleanEx()
nameEx("nowrapRecenter")
### * nowrapRecenter

flush(stderr()); flush(stdout())

### Name: nowrapRecenter
### Title: Break polygons at meridian for recentering
### Aliases: nowrapRecenter nowrapSpatialPolygons
### Keywords: spatial

### ** Examples

## Not run: 
##D if (!rgeosStatus()) gpclibPermit()
##D library(maps)
##D world <- map("world", fill=TRUE, col="transparent", plot=FALSE)
##D worldSpP <- map2SpatialPolygons(world, world$names, CRS("+proj=longlat"))
##D worldSpP <- worldSpP[-grep("Antarctica", row.names(worldSpP)),]
##D # incomplete polygons
##D worldSpP <- worldSpP[-grep("Ghana", row.names(worldSpP)),]
##D # self-intersection mouth of Volta
##D worldSpP <- worldSpP[-grep("UK:Great Britain", row.names(worldSpP)),]
##D # self-intersection Humber estuary
##D worldSpPr <- recenter(worldSpP)
##D plot(worldSpPr)
##D title("Pacific view without polygon splitting")
##D worldSpPnr <- nowrapRecenter(worldSpP)
##D plot(worldSpPnr)
##D title("Pacific view with polygon splitting")
## End(Not run)
crds <- matrix(c(-1, 1, 1, -1, 50, 50, 52, 52), ncol=2)
rcrds <- rbind(crds, crds[1,])
SR <- SpatialPolygons(list(Polygons(list(Polygon(rcrds)), ID="r1")),
 proj4string=CRS("+proj=longlat"))
bbox(SR)
SRr <- recenter(SR)
bbox(SRr)
SRnr <- nowrapRecenter(SR)
bbox(SRnr)



cleanEx()
nameEx("pal2SpatialPolygons")
### * pal2SpatialPolygons

flush(stderr()); flush(stdout())

### Name: pal2SpatialPolygons
### Title: Making SpatialPolygons objects from RArcInfo input
### Aliases: pal2SpatialPolygons
### Keywords: spatial

### ** Examples

nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], ID="FIPS")
plot(nc1)
text(coordinates(nc1), labels=row.names(nc1), cex=0.6)
library(maps)
ncmap <- map("county", "north carolina", fill=TRUE, col="transparent",
 plot=FALSE)
IDs <- sapply(strsplit(ncmap$names, "[,:]"), function(x) x[2])
nc2 <- map2SpatialPolygons(ncmap, IDs)
plot(nc2)
text(coordinates(nc2), labels=row.names(nc2), cex=0.6)
library(RArcInfo)
td <- tempdir()
tmpcover <- paste(td, "nc", sep="/")
if (!file.exists(tmpcover)) e00toavc(system.file("share/co37_d90.e00",
 package="maptools")[1], tmpcover)
arc <- get.arcdata(td, "nc")
pal <- get.paldata(td, "nc")
pat <- get.tabledata(paste(td, "info", sep="/"), "NC.PAT")
sapply(pal[[2]], function(x) length(x[[1]]))
IDs <- paste(pat$ST[-1], pat$CO[-1], sep="")
nc3 <- pal2SpatialPolygons(arc, pal, IDs=IDs)
plot(nc3)
text(coordinates(nc3), labels=row.names(nc3), cex=0.6)



cleanEx()
nameEx("pointLabel")
### * pointLabel

flush(stderr()); flush(stdout())

### Name: pointLabel
### Title: Label placement for points to avoid overlaps
### Aliases: pointLabel
### Keywords: aplot

### ** Examples

n <- 50
x <- rnorm(n)*10
y <- rnorm(n)*10
plot(x, y, col = "red", pch = 20)
pointLabel(x, y, as.character(round(x,5)), offset = 0, cex = .7)

plot(x, y, col = "red", pch = 20)
pointLabel(x, y, expression(over(alpha, beta[123])), offset = 0, cex = .8)




cleanEx()
nameEx("readGPS")
### * readGPS

flush(stderr()); flush(stdout())

### Name: readGPS
### Title: GPSbabel read interface
### Aliases: readGPS
### Keywords: spatial

### ** Examples

## Not run: 
##D b1 <- readGPS(f="usb:")
##D str(b1)
##D b2 <- b1[1:172,]
##D wp0 <- b2[,c(2,3,4,8,9,19)]
##D str(wp0)
##D wp0$long <- wp0$V9
##D wp0$lat <- as.numeric(as.character(wp0$V8))
##D wp0$id <- as.character(wp0$V2)
##D wp0$alt <- as.numeric(substring(as.character(wp0$V19), 1,
##D  (nchar(as.character(wp0$V19))-1)))
##D wp0$time <- as.POSIXct(strptime(paste(as.character(wp0$V3),
##D  as.character(wp0$V4)), format="%d-%b-%y %H:%M:%S"))
##D str(wp0)
##D wp1 <- wp0[,-(1:6)]
##D str(wp1)
##D summary(wp1)
## End(Not run)



cleanEx()
nameEx("readShapeLines")
### * readShapeLines

flush(stderr()); flush(stdout())

### Name: readShapeLines
### Title: Read arc shape files into SpatialLinesDataFrame objects
### Aliases: readShapeLines writeLinesShape
### Keywords: spatial

### ** Examples

xx <- readShapeLines(system.file("shapes/fylk-val.shp", package="maptools")[1],
 proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
plot(xx, col="blue")
summary(xx)
xxx <- xx[xx$LENGTH > 30000,]
plot(xxx, col="red", add=TRUE)
tmpfl <- paste(tempdir(), "xxline", sep="/")
writeLinesShape(xxx, tmpfl)
getinfo.shape(paste(tmpfl, ".shp", sep=""))
axx <- readShapeLines(tmpfl, proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
plot(xxx, col="black", lwd=4)
plot(axx, col="yellow", lwd=1, add=TRUE)
unlink(paste(tmpfl, ".*", sep=""))
xx <- readShapeLines(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +datum=NAD27"))
plot(xx, col="blue")



cleanEx()
nameEx("readShapePoints")
### * readShapePoints

flush(stderr()); flush(stdout())

### Name: readShapePoints
### Title: Read points shape files into SpatialPointsDataFrame objects
### Aliases: readShapePoints writePointsShape
### Keywords: spatial

### ** Examples

library(maptools)
xx <- readShapePoints(system.file("shapes/baltim.shp", package="maptools")[1])
plot(xx)
summary(xx)
xxx <- xx[xx$PRICE < 40,]
tmpfl <- paste(tempdir(), "xxpts", sep="/")
writePointsShape(xxx, tmpfl)
getinfo.shape(paste(tmpfl, ".shp", sep=""))
axx <- readShapePoints(tmpfl)
plot(axx, col="red", add=TRUE)
unlink(paste(tmpfl, ".*", sep=""))
xx <- readShapePoints(system.file("shapes/pointZ.shp", package="maptools")[1])
dimensions(xx)
plot(xx)
summary(xx)



cleanEx()
nameEx("readShapePoly")
### * readShapePoly

flush(stderr()); flush(stdout())

### Name: readShapePoly
### Title: Read polygon shape files into SpatialPolygonsDataFrame objects
### Aliases: readShapePoly writePolyShape
### Keywords: spatial

### ** Examples

library(maptools)
xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
plot(xx, border="blue", axes=TRUE, las=1)
text(coordinates(xx), labels=row.names(xx), cex=0.6)
as(xx, "data.frame")[1:5, 1:6]
xxx <- xx[xx$SID74 < 2,]
plot(xxx, border="red", add=TRUE)
tmpfl <- paste(tempdir(), "xxpoly", sep="/")
writePolyShape(xxx, tmpfl)
getinfo.shape(paste(tmpfl, ".shp", sep=""))
axx <- readShapePoly(tmpfl, proj4string=CRS("+proj=longlat +ellps=clrk66"))
plot(xxx, border="black", lwd=4)
plot(axx, border="yellow", lwd=1, add=TRUE)
unlink(paste(tmpfl, ".*", sep=""))



cleanEx()
nameEx("readShapeSpatial")
### * readShapeSpatial

flush(stderr()); flush(stdout())

### Name: readShapeSpatial
### Title: Read shape files into Spatial*DataFrame objects
### Aliases: readShapeSpatial writeSpatialShape
### Keywords: spatial

### ** Examples

library(maptools)
xx <- readShapeSpatial(system.file("shapes/sids.shp", package="maptools")[1],
 IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
summary(xx)
xxx <- xx[xx$SID74 < 2,]
tmpfl <- paste(tempdir(), "xxpoly", sep="/")
writeSpatialShape(xxx, tmpfl)
getinfo.shape(paste(tmpfl, ".shp", sep=""))
unlink(paste(tmpfl, ".*", sep=""))
xx <- readShapeSpatial(system.file("shapes/fylk-val.shp",
 package="maptools")[1], proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
summary(xx)
xxx <- xx[xx$LENGTH > 30000,]
plot(xxx, col="red", add=TRUE)
tmpfl <- paste(tempdir(), "xxline", sep="/")
writeSpatialShape(xxx, tmpfl)
getinfo.shape(paste(tmpfl, ".shp", sep=""))
unlink(paste(tmpfl, ".*", sep=""))
xx <- readShapeSpatial(system.file("shapes/baltim.shp", package="maptools")[1])
summary(xx)
xxx <- xx[xx$PRICE < 40,]
tmpfl <- paste(tempdir(), "xxpts", sep="/")
writeSpatialShape(xxx, tmpfl)
getinfo.shape(paste(tmpfl, ".shp", sep=""))
unlink(paste(tmpfl, ".*", sep=""))



cleanEx()
nameEx("readSplus")
### * readSplus

flush(stderr()); flush(stdout())

### Name: readSplus
### Title: Read exported WinBUGS maps
### Aliases: readSplus
### Keywords: spatial

### ** Examples

geobugs <- readSplus(system.file("share/Splus.map", package="maptools"))
plot(geobugs, axes=TRUE, col=1:3)
row.names(geobugs)
pls <- slot(geobugs, "polygons")
sapply(pls, function(i) sapply(slot(i, "Polygons"), slot, "hole"))
pls1 <- lapply(pls, checkPolygonsHoles)
sapply(pls1, function(i) sapply(slot(i, "Polygons"), slot, "hole"))
plot(SpatialPolygons(pls1), axes=TRUE, col=1:3)



cleanEx()
nameEx("sp2Mondrian")
### * sp2Mondrian

flush(stderr()); flush(stdout())

### Name: sp2Mondrian
### Title: write map data for Mondrian
### Aliases: sp2Mondrian
### Keywords: spatial

### ** Examples

## Not run: 
##D xx <- readShapePoly(system.file("shapes/columbus.shp", package="maptools")[1])
##D sp2Mondrian(xx, file="columbus1.txt")
##D xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1])
##D sp2Mondrian(xx, file="sids1.txt")
## End(Not run)



cleanEx()
nameEx("sp2WB")
### * sp2WB

flush(stderr()); flush(stdout())

### Name: sp2WB
### Title: Export SpatialPolygons object as S-Plus map for WinBUGS
### Aliases: sp2WB
### Keywords: spatial

### ** Examples

xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
plot(xx, border="blue", axes=TRUE, las=1)
tf <- tempfile()
sp2WB(as(xx, "SpatialPolygons"), filename=tf)
xxx <- readSplus(tf, proj4string=CRS("+proj=longlat +ellps=clrk66"))
all.equal(xxx, as(xx, "SpatialPolygons"), tolerance=.Machine$double.eps^(1/4),
 check.attributes=FALSE)
## Not run: 
##D x <- readAsciiGrid(system.file("grids/test.ag", package="maptools")[1])
##D xp <- as(x, "SpatialPixelsDataFrame")
##D pp <- as.SpatialPolygons.SpatialPixels(xp)
##D sp2WB(pp, filename="test.map")
## End(Not run)



cleanEx()
nameEx("sp2tmap")
### * sp2tmap

flush(stderr()); flush(stdout())

### Name: sp2tmap
### Title: Convert SpatialPolygons object for Stata tmap command
### Aliases: sp2tmap
### Keywords: spatial

### ** Examples

## Not run: 
##D xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
##D  IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
##D plot(xx, border="blue", axes=TRUE, las=1)
##D tmapdf <- sp2tmap(as(xx, "SpatialPolygons"))
##D write.dta(tmapdf, file="NCmap.dta", version=7)
##D NCdf <- as(xx, "data.frame")
##D NCdf$ID_n <- attr(tmapdf, "ID_names")
##D write.dta(NCdf, file="NC.dta", version=7)
## End(Not run)



cleanEx()
nameEx("spCbind-methods")
### * spCbind-methods

flush(stderr()); flush(stdout())

### Name: spCbind-methods
### Title: cbind for spatial objects
### Aliases: spCbind-methods
###   spCbind,SpatialPointsDataFrame,data.frame-method
###   spCbind,SpatialPointsDataFrame,vector-method
###   spCbind,SpatialLinesDataFrame,data.frame-method
###   spCbind,SpatialLinesDataFrame,vector-method
###   spCbind,SpatialPolygonsDataFrame,data.frame-method
###   spCbind,SpatialPolygonsDataFrame,vector-method spCbind
### Keywords: methods spatial

### ** Examples

xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
  IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
xtra <- read.dbf(system.file("share/nc_xtra.dbf", package="maptools")[1])
o <- match(xx$CNTY_ID, xtra$CNTY_ID)
xtra1 <- xtra[o,]
row.names(xtra1) <- xx$FIPSNO
xx1 <- spCbind(xx, xtra1)
names(xx1)
identical(xx1$CNTY_ID, xx1$CNTY_ID.1)



cleanEx()
nameEx("spRbind-methods")
### * spRbind-methods

flush(stderr()); flush(stdout())

### Name: spRbind-methods
### Title: rbind for spatial objects
### Aliases: spRbind-methods spRbind,SpatialPoints,SpatialPoints-method
###   spRbind,SpatialPointsDataFrame,SpatialPointsDataFrame-method
###   spRbind,SpatialLines,SpatialLines-method
###   spRbind,SpatialLinesDataFrame,SpatialLinesDataFrame-method
###   spRbind,SpatialPolygons,SpatialPolygons-method
###   spRbind,SpatialPolygonsDataFrame,SpatialPolygonsDataFrame-method
###   spRbind
### Keywords: methods spatial

### ** Examples

xx <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
  IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
summary(xx)
xx$FIPSNO
xx1 <- xx[xx$CNTY_ID < 1982,]
xx2 <- xx[xx$CNTY_ID >= 1982,]
xx3 <- spRbind(xx2, xx1)
summary(xx3)
xx3$FIPSNO



cleanEx()
nameEx("state.vbm")
### * state.vbm

flush(stderr()); flush(stdout())

### Name: state.vbm
### Title: US State Visibility Based Map
### Aliases: state.vbm
### Keywords: datasets

### ** Examples

  data(state.vbm)
  plot(state.vbm)

  tmp <- state.x77[,'HS Grad']
  tmp2 <- cut(tmp, seq(min(tmp),max(tmp), length.out=11),
    include.lowest=TRUE)
  plot(state.vbm,col=cm.colors(10)[tmp2])



cleanEx()
nameEx("sun-methods")
### * sun-methods

flush(stderr()); flush(stdout())

### Name: sun-methods
### Title: Methods for sun ephemerides calculations
### Aliases: crepuscule sunriset solarnoon solarpos crepuscule-methods
###   crepuscule,SpatialPoints,POSIXct-method
###   crepuscule,matrix,POSIXct-method solarnoon-methods
###   solarnoon,SpatialPoints,POSIXct-method
###   solarnoon,matrix,POSIXct-method solarpos-methods
###   solarpos,SpatialPoints,POSIXct-method solarpos,matrix,POSIXct-method
###   sunriset-methods sunriset,SpatialPoints,POSIXct-method
###   sunriset,matrix,POSIXct-method
### Keywords: methods manip utilities

### ** Examples

## Location of Helsinki, Finland, in decimal degrees,
## as listed in NOAA's website
hels <- matrix(c(24.97, 60.17), nrow=1)
Hels <- SpatialPoints(hels, proj4string=CRS("+proj=longlat +datum=WGS84"))
d041224 <- as.POSIXct("2004-12-24", tz="EET")
## Astronomical dawn
crepuscule(hels, d041224, solarDep=18, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=18, direction="dawn", POSIXct.out=TRUE)
## Nautical dawn
crepuscule(hels, d041224, solarDep=12, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=12, direction="dawn", POSIXct.out=TRUE)
## Civil dawn
crepuscule(hels, d041224, solarDep=6, direction="dawn", POSIXct.out=TRUE)
crepuscule(Hels, d041224, solarDep=6, direction="dawn", POSIXct.out=TRUE)
solarnoon(hels, d041224, POSIXct.out=TRUE)
solarnoon(Hels, d041224, POSIXct.out=TRUE)
solarpos(hels, as.POSIXct(Sys.time(), tz="EET"))
solarpos(Hels, as.POSIXct(Sys.time(), tz="EET"))
sunriset(hels, d041224, direction="sunrise", POSIXct.out=TRUE)
sunriset(Hels, d041224, direction="sunrise", POSIXct.out=TRUE)
## Using a sequence of dates
Hels_seq <- seq(from=d041224, length.out=365, by="days")
up <- sunriset(Hels, Hels_seq, direction="sunrise", POSIXct.out=TRUE)
down <- sunriset(Hels, Hels_seq, direction="sunset", POSIXct.out=TRUE)
day_length <- down$time - up$time
plot(Hels_seq, day_length, type="l")

## Using a grid of spatial points for the same point in time
grd <- GridTopology(c(-179,-89), c(1,1), c(359,179))
SP <- SpatialPoints(coordinates(grd),
                    proj4string=CRS("+proj=longlat +datum=WGS84"))
wint <- as.POSIXct("2004-12-21", tz="GMT")
win <- crepuscule(SP, wint, solarDep=6, direction="dawn")
SPDF <- SpatialGridDataFrame(grd,
 proj4string=CRS("+proj=longlat +datum=WGS84"),
 data=data.frame(winter=win))
image(SPDF, axes=TRUE, col=cm.colors(40))



cleanEx()
nameEx("symbolsInPolys")
### * symbolsInPolys

flush(stderr()); flush(stdout())

### Name: symbolsInPolys
### Title: Place grids of points over polygons
### Aliases: symbolsInPolys
### Keywords: spatial

### ** Examples

nc_SP <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +ellps=clrk66"))
## Not run: 
##D library(spgpc)
##D pls <- slot(nc_SP, "polygons")
##D pls_new <- lapply(pls, checkPolygonsHoles)
##D nc_SP <- SpatialPolygonsDataFrame(SpatialPolygons(pls_new,
##D  proj4string=CRS(proj4string(nc_SP))), data=as(nc_SP, "data.frame"))
## End(Not run)
symbs <- c("-", "+", "x")
np <- sapply(slot(nc_SP, "polygons"), function(x) length(slot(x, "Polygons")))
try1 <- symbolsInPolys(nc_SP, 100, symb=symbs[np])
plot(nc_SP, axes=TRUE)
plot(try1, add=TRUE, pch=as.character(try1$symb))



cleanEx()
nameEx("thinnedSpatialPoly")
### * thinnedSpatialPoly

flush(stderr()); flush(stdout())

### Name: thinnedSpatialPoly
### Title: Douglas-Peuker line generalization for Spatial Polygons
### Aliases: thinnedSpatialPoly
### Keywords: spatial

### ** Examples

xx <- readShapeSpatial(system.file("shapes/sids.shp", package="maptools")[1],
      IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
object.size(as(xx, "SpatialPolygons"))
xxx <- thinnedSpatialPoly(xx, tolerance=0.05, minarea=0.001)
object.size(as(xxx, "SpatialPolygons"))
par(mfrow=c(2,1))
plot(xx)
plot(xxx)
par(mfrow=c(1,1))



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("unionSpatialPolygons")
### * unionSpatialPolygons

flush(stderr()); flush(stdout())

### Name: unionSpatialPolygons
### Title: Aggregate Polygons in a SpatialPolygons object
### Aliases: unionSpatialPolygons
### Keywords: spatial

### ** Examples

if (!rgeosStatus()) gpclibPermit()
nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +datum=NAD27"))
lps <- coordinates(nc1)
ID <- cut(lps[,1], quantile(lps[,1]), include.lowest=TRUE)
reg4 <- unionSpatialPolygons(nc1, ID)
row.names(reg4)



cleanEx()
nameEx("wrld_simpl")
### * wrld_simpl

flush(stderr()); flush(stdout())

### Name: wrld_simpl
### Title: Simplified world country polygons
### Aliases: wrld_simpl
### Keywords: datasets

### ** Examples

data(wrld_simpl)
plot(wrld_simpl)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
