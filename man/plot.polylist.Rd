% Copyright 2001-3 by Roger S. Bivand
\name{plot.polylist}
\alias{plotpolys}
\alias{plot.polylist}
\alias{leglabs}
\alias{findInterval2}
\title{Plot polygons}
\description{
A helper function for plotting polygons in a global bounding box. \code{findInterval2} is a modified version of \code{findInterval2} that permits the vector of break points to start at -Inf and/or finish at Inf. \code{leglabs} makes character strings from the same break points. The \code{plot.polylist()} function may be used as a generic S3 method. NOTE! \code{plotpolys()} DEPRECATED: use generic plot() for polylist objects function instead.
}
\usage{
plot.polylist(x, col, border = par("fg"), add = FALSE, xlim=NULL,
 ylim=NULL, xpd = NULL, density = NULL, angle = 45, ...)
leglabs(vec, under="under", over="over", between="-")
findInterval2(y, vec, rightmost.closed = FALSE, all.inside = TRUE)
plotpolys(pl, bb, col = NA, border = par("fg"), add = FALSE, xlim, ylim, ...)
}

\arguments{
  \item{pl, x}{list of polygons of class \code{polylist}}
  \item{bb}{matrix of polygon bounding boxes - columns are LL(x), LL(y), UR(x), UR(y); note that this argument may be omitted if the polygon list object has an "maplim" attribute, or if both xlim and ylim arguments are given}
  \item{col}{colurs to use for filling the polygons}
  \item{border}{the color to draw the border}
  \item{add}{add to existing plot}
  \item{xlim, ylim}{numeric of length 2, giving the x and y coordinates ranges}
  \item{xpd}{(where) should clipping take place?}
  \item{density}{the density of shading lines, in lines per inch}
  \item{angle}{the slope of shading lines, given as an angle in degrees (counter-clockwise)}
  \item{...}{other arguments passed to plot to set the plot window - not passed to polygon}
  \item{vec}{vector of break values}
  \item{under}{character value for under}
  \item{over}{character value for over}
  \item{between}{character value for between}
  \item{y}{numeric vector to be broken into class intervals}
  \item{rightmost.closed}{as in findInterval()}
  \item{all.inside}{as in findInterval()}
}
\author{Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{Map2poly}}, \code{\link{findInterval}}}

\examples{
try2 <- read.shape(system.file("shapes/columbus.shp", package="maptools")[1])
mappolys <- Map2poly(try2, as.character(try2$att.data$NEIGNO))
brks <- round(quantile(try2$att.data$CRIME, probs=seq(0,1,0.2)), digits=2)
colours <- c("salmon1", "salmon2", "red3", "brown", "black")
plot(mappolys, col=colours[findInterval2(try2$att.data$CRIME, brks)])
legend(x=c(5.8, 7.1), y=c(13, 14.5), legend=leglabs(brks),
  fill=colours, bty="n")
title(main=paste("Columbus OH: residential burglaries and vehicle\nthefts",
  "per thousand households, 1980"))
try3 <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
mappolys <- Map2poly(try3, as.character(try3$att.data$FIPSNO))
cols <- c("white", "orange", "red")
np <- integer(length(mappolys))
for (i in 1:length(mappolys)) np[i] <- attr(mappolys[[i]], "nPart")
plot(mappolys, col=cols[np])
title(main="White: one part, orange: two part, red: three part polygons")
plot(mappolys, density=c(10, 20, 30)[np], angle=c(-45, 0, 45)[np], axes=FALSE)
}
\keyword{spatial}
