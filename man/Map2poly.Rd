% Copyright 2003 by Roger S. Bivand
\name{Map2poly}
\alias{Map2poly}
\alias{Map2lines}
\alias{Map2points}
%\alias{Map2maplim}
\alias{Map2bbs}
\alias{shape2poly}
\alias{shape2lines}
\alias{shape2points}
%\alias{shp2maplim}
\alias{shape2bbs}
\alias{convert.pl}
%\alias{ringDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create polygon lists and bounding boxes from imported shapefiles}
\description{
  Map2poly() is a simple function to make imported GIS vector polygons into a "polylist" object from a "Map" object, and Map2bbs() retrieves polygon bounding boxes; analogous functions for the "shapefiles" package are shape2poly() and shape2bbs() (thanks to Stéphane Dray for his contribution); convert.pl() serves to convert the deprecated "multipart" "polylist" format to an NA-separated format now used by plot() for polylist objects and poly2nb(). 
%\code{ringDir()} attempts to find out whether polygon ring 
%coordinates run clockwise (external boundary of polygon object), or 
%anti-clockwise (internal boundary - hole). 
\code{*2lines()} and \code{*2points()} do the same as \code{*2poly()} for shapefiles of types 3=lines and 1=points; polygons are type=5.
}
\usage{
Map2poly(Map, region.id = NULL)
Map2lines(Map)
Map2points(Map)
%Map2maplim(Map)
Map2bbs(Map)
shape2poly(shape, region.id = NULL)
shape2lines(shape)
shape2points(shape)
%shp2maplim(shape)
shape2bbs(shape)
convert.pl(pl)
%ringDir(xy, ring)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Map}{object of class \code{"Map"}, imported from a GIS polygon vector file using unpublished package "maptools"}
  \item{shape}{list returned by \code{read.shapefile()}, imported from a GIS polygon vector file using the package "shapefiles"}
  \item{region.id}{character vector of region ids to be added to the neighbours list as attribute \code{region.id}}
  \item{pl}{list of old-style multipart polygons of class \code{polylist}}
%  \item{xy}{the coordinate object for a single member of a polygon list}
%  \item{ring}{the ring (one of \code{1:attr(xy, "nParts"))} index number}
}
\details{
  \url{http://sal.agecon.uiuc.edu/datasets/columbus.zip}
}

\value{
  \code{Map2poly}, \code{shape2poly}, \code{convert.pl} return polylist objects, lists of polygon boundary coordinates (divided by NA where the polygon object is represented by more than one polygon);
  \code{Map2lines}, \code{shape2lines} return lineslist objects;
  \code{Map2points}, \code{shape2points} return (n x 2) matrices; and
  \code{Map2bbs}, \code{shape2bbs} return bounding box matrixes, c(x1, y1, x2, y2) with coordinates for the lower left corner and upper right corner.
}

\author{Roger Bivand and Stéphane Dray, \email{Roger.Bivand@nhh.no}}
 
\seealso{\code{\link{read.shape}}, \code{\link[shapefiles]{read.shapefile}}}


\examples{
shp <- try(library(shapefiles))
if (class(shp) != "try-error") {
  ShapeDir <- system.file("shapes", package="maptools")[1]
  try1 <- read.shapefile(paste(ShapeDir, "columbus", sep="/"))
  shppolys <- shape2poly(try1, as.character(try1$dbf$dbf$NEIGNO))
  plot(shppolys)
  title(main="Polygons for Columbus OH from shapefiles package")
  try3a <- read.shapefile(paste(ShapeDir, "baltim", sep="/"))
  baltptsa <- shape2points(try3a)
  xylims <- attr(baltptsa, "maplim")
  plot(xylims$x, xylims$y, asp=1, type='n', xlab="", ylab="")
  points(baltptsa)
  title(main="Baltimore points from shapefiles package")
  try4a <- read.shapefile(paste(ShapeDir, "fylk-val", sep="/"))
  fylk.vala <- shape2lines(try4a)
  xylims <- attr(fylk.vala, "maplim")
  plot(xylims$x, xylims$y, asp=1, type='n', xlab="", ylab="")
  for (i in 1:length(fylk.vala)) lines(fylk.vala[[i]])
  title(main="Norwegian river centrelines from shapefiles package")
}
try2 <- read.shape(system.file("shapes/columbus.shp", 
  package="maptools")[1])
mappolys <- Map2poly(try2, as.character(try2$att.data$NEIGNO))
plot(mappolys)
title(main="Polygons for Columbus OH from maptools package")
if (class(shp) != "try-error") {
  plot(shppolys)
  plot(mappolys, add=TRUE, border="red", lty="dotted")
  title(main="Polygons for Columbus OH overplotted from both packages")
}
try3 <- read.shape(system.file("shapes/baltim.shp", package="maptools")[1])
baltpts <- Map2points(try3)
xylims <- attr(baltpts, "maplim")
plot(xylims$x, xylims$y, asp=1, type='n', xlab="", ylab="")
points(baltpts)
title(main="Baltimore points from maptools package")
try4 <- read.shape(system.file("shapes/fylk-val.shp", package="maptools")[1])
fylk.val <- Map2lines(try4)
xylims <- attr(fylk.val, "maplim")
plot(xylims$x, xylims$y, asp=1, type='n', xlab="", ylab="")
for (i in 1:length(fylk.val)) lines(fylk.val[[i]])
title(main="Norwegian river centrelines from maptools package")
}

}
\keyword{spatial}
