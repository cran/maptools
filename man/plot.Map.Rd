\name{plot.Map}
\alias{plot.Map}
\alias{color.ramp}

\title{Plot a Map object}
\description{
  The function plots a map object
}
\usage{
plot.Map(x, recs, auxvar = NULL, add = FALSE, fg = "gray", ol = "black", 
  prbg = NULL, glyph = 16, type = "q", nclass = 5, ...)
color.ramp(nclass, color='red', nvec=NULL, type='q')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a Map object returned by read.shape()}
  \item{recs}{a vector with the shapes to plot, if missing defaults to all shapes}
  \item{auxvar}{if the Map has polygon shapes, a variable from which to derive polygon fill colours using the computed class intervals}
  \item{add}{default FALSE, if TRUE add to existing plot}
  \item{fg}{foreground colour, can be used to pass through point and polygon colours, permitting the built-in class interval calculation to be avoided}
  \item{ol}{line/boundary colour}
  \item{prbg}{if not NULL, draw a rectangle around the plot in this colour}
  \item{glyph}{points plotted as this pch code}
  \item{type}{default "q" for quantile classes, can also be "e" for equal intervals}
  \item{nclass}{number of classes for class intervals}
  \item{\dots}{passed through to plotting functions}
  \item{color}{default "red"}
  \item{nvec}{data vector to convert to class intervals}
}

\value{
  color.ramp() returns a list with componemts:
  \item{ramp}{vector of colours}
  \item{col.class}{vector of classes}
}

\author{Nicholas J. Lewin-Koh, modified by Roger Bivand \email{Roger.Bivand@nhh.no}}

\seealso{\code{\link{read.shape}}}
\examples{
x <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
plot(x)
n <- attr(x$Shapes,'nshps')
nParts <- integer(n)
for (i in 1:n) nParts[i] <- attr(x$Shapes[[i]], "nParts")
table(nParts)
cols <- c("azure", "blue", "orange")
fgs <- cols[nParts]
plot(x, fg=fgs)

}
\keyword{spatial}

