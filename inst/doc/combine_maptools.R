### R code from vignette source 'combine_maptools.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: combine_maptools.Rnw:40-43
###################################################
owidth <- getOption("width")
options("width"=90)
.PngNo <- 0


###################################################
### code chunk number 2: afig (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 6.5, height = 3.5, pointsize = 12, bg = "white")
## opar <- par(mar=c(3,3,1,1)+0.1)


###################################################
### code chunk number 3: afigl (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 6.5, height = 3.5, pointsize = 12, bg = "white")


###################################################
### code chunk number 4: bfigl (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 6.5, height = 5, pointsize = 12, bg = "white")


###################################################
### code chunk number 5: bfig (eval = FALSE)
###################################################
## .PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".pdf", sep="")
## pdf(file=file, width = 6.5, height = 5, pointsize = 12, bg = "white")
## opar <- par(mar=c(3,3,1,1)+0.1)


###################################################
### code chunk number 6: zfig (eval = FALSE)
###################################################
## par(opar)
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 7: zfigl (eval = FALSE)
###################################################
## dev.null <- dev.off()
## cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 8: combine_maptools.Rnw:97-107
###################################################
owd <- getwd()
setwd(system.file("shapes", package="maptools"))
library(maptools)
nc90 <- readShapeSpatial("co37_d90")
proj4string(nc90) <- CRS("+proj=longlat +datum=NAD27")
sc90 <- readShapeSpatial("co45_d90")
proj4string(sc90) <- CRS("+proj=longlat +datum=NAD27")
va90 <- readShapeSpatial("co51_d90")
proj4string(va90) <- CRS("+proj=longlat +datum=NAD27")
setwd(owd)


###################################################
### code chunk number 9: combine_maptools.Rnw:113-120
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".pdf", sep="")
pdf(file=file, width = 6.5, height = 5, pointsize = 12, bg = "white")
opar <- par(mar=c(3,3,1,1)+0.1)
oopar <- par(mar=c(3,2,1,1)+0.1)
plot(va90, xlim=c(-85,-75), ylim=c(32,40), axes=TRUE, border="grey10")
plot(nc90, add=TRUE, border="grey40")
plot(sc90, add=TRUE, border="grey70")
par(oopar)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 10: combine_maptools.Rnw:144-145
###################################################
library(maptools)


###################################################
### code chunk number 11: combine_maptools.Rnw:147-151
###################################################
names(sc90)
sc90a <- spChFIDs(sc90, paste(sc90$ST, sc90$CO, sep=""))
sc90a <- sc90a[,-(1:4)]
names(sc90a)


###################################################
### code chunk number 12: combine_maptools.Rnw:153-154
###################################################
proj4string(sc90a) <- CRS(proj4string(sc90a))


###################################################
### code chunk number 13: combine_maptools.Rnw:165-166
###################################################
names(nc90)


###################################################
### code chunk number 14: combine_maptools.Rnw:168-169 (eval = FALSE)
###################################################
## nc90a <- spChFIDs(nc90, paste(nc90$ST, nc90$CO, sep=""))


###################################################
### code chunk number 15: combine_maptools.Rnw:171-172
###################################################
try1 <- try(spChFIDs(nc90, paste(nc90$ST, nc90$CO, sep="")))


###################################################
### code chunk number 16: combine_maptools.Rnw:174-175
###################################################
cat(try1)


###################################################
### code chunk number 17: combine_maptools.Rnw:185-186
###################################################
table(table(paste(nc90$ST, nc90$CO, sep="")))


###################################################
### code chunk number 18: combine_maptools.Rnw:212-213
###################################################
nc90a <- unionSpatialPolygons(nc90, IDs=paste(nc90$ST, nc90$CO, sep=""))


###################################################
### code chunk number 19: combine_maptools.Rnw:223-226
###################################################
nc90_df <- as(nc90, "data.frame")[!duplicated(nc90$CO),-(1:4)]
row.names(nc90_df) <- paste(nc90_df$ST, nc90_df$CO, sep="")
nc90b <- SpatialPolygonsDataFrame(nc90a, nc90_df)


###################################################
### code chunk number 20: combine_maptools.Rnw:246-253
###################################################
va90a <- spChFIDs(va90, paste(va90$ST, va90$CO, sep=""))
va90a <- va90a[,-(1:4)]
va90_pl <- slot(va90a, "polygons")
va90_pla <- lapply(va90_pl, checkPolygonsHoles)
p4sva <- CRS(proj4string(va90a))
vaSP <- SpatialPolygons(va90_pla, proj4string=p4sva)
va90b <- SpatialPolygonsDataFrame(vaSP, data=as(va90a, "data.frame"))


###################################################
### code chunk number 21: combine_maptools.Rnw:293-297
###################################################
nc_sc_va90 <- spRbind(spRbind(nc90b, sc90a), va90b)
FIPS <- row.names(nc_sc_va90)
str(FIPS)
length(slot(nc_sc_va90, "polygons"))


###################################################
### code chunk number 22: combine_maptools.Rnw:326-328
###################################################
t1 <- read.fwf(system.file("share/90mfips.txt", package="maptools"), skip=21,
 widths=c(4,4,4,4,2,6,2,3,3,1,7,5,3,51), colClasses = "character")


###################################################
### code chunk number 23: combine_maptools.Rnw:330-336
###################################################
t2 <- t1[1:2004,c(1,7,8,14)]
t3 <- t2[complete.cases(t2),]
cnty1 <- t3[t3$V7 != "  ",]
ma1 <- t3[t3$V7 == "  ",c(1,4)]
cnty2 <- cnty1[which(!is.na(match(cnty1$V7, c("37", "45", "51")))),]
cnty2$FIPS <- paste(cnty2$V7, cnty2$V8, sep="")


###################################################
### code chunk number 24: combine_maptools.Rnw:356-361
###################################################
MA_FIPS <- cnty2$V1[match(FIPS, cnty2$FIPS)]
MA <- ma1$V14[match(MA_FIPS, ma1$V1)]
MA_df <- data.frame(MA_FIPS=MA_FIPS, MA=MA, row.names=FIPS)
nc_sc_va90a <- spCbind(nc_sc_va90, MA_df)
ncscva_MA <- unionSpatialPolygons(nc_sc_va90a, nc_sc_va90a$MA_FIPS)


###################################################
### code chunk number 25: combine_maptools.Rnw:367-374
###################################################
.PngNo <- .PngNo + 1; file <- paste("Fig-bitmap-", .PngNo, ".pdf", sep="")
pdf(file=file, width = 6.5, height = 5, pointsize = 12, bg = "white")
opar <- par(mar=c(3,3,1,1)+0.1)
oopar <- par(mar=c(3,2,1,1)+0.1)
plot(nc_sc_va90, border="grey", axes=TRUE)
plot(ncscva_MA, lwd=2, add=TRUE)
text(coordinates(ncscva_MA), labels=row.names(ncscva_MA), cex=0.6)
par(oopar)
par(opar)
dev.null <- dev.off()
cat("\\includegraphics[width=0.95\\textwidth]{", file, "}\n\n", sep="")


###################################################
### code chunk number 26: combine_maptools.Rnw:392-397
###################################################
np <- sapply(slot(ncscva_MA, "polygons"), function(x) length(slot(x, "Polygons")))
table(np)
MA_fips <- row.names(ncscva_MA)
MA_name <- ma1$V14[match(MA_fips, ma1$V1)]
data.frame(MA_fips, MA_name)[np > 1,]


###################################################
### code chunk number 27: combine_maptools.Rnw:405-406
###################################################
options("width"=owidth)


