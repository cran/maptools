\name{state.vbm}
\alias{state.vbm}
\docType{data}
\title{ US State Visibility Based Map}
\description{
  A SpatialPolygonsDataFrame object (for use with the maptools package) to plot a Visibility Based Map.
}
\usage{
data(state.vbm)
}
\details{
  A SpatialPolygonsDataFrame object
  (for use with the maptools package) to plot a map of the
  US states where the sizes of the states have been adjusted to be more
  equal.

  This map can be useful for plotting state data using colors patterns
  without the larger states dominating and the smallest states being
  lost.

  The original map is copyrighted by Mark Monmonier.  Official
  publications based on this map should acknowledge him.
  Comercial publications of maps based on this probably need permission
  from him to use.
}
\source{
  The data was converted from the maps library for S-PLUS.  S-PLUS uses
  the map with permission from the author.  This version of the data
  has not received permission from the author (no attempt made, not that
  it was refused), most of my uses I feel fall under fair use and do not
  violate copyright, but you will need to decide for yourself and your
  applications.
}
\author{Greg Snow \email{greg.snow@imail.org} (of this compilation)}

\references{
  \code{http://www.markmonmonier.com/index.htm},
  \url{http://euclid.psych.yorku.ca/SCS/Gallery/bright-ideas.html}
}
\examples{
  data(state.vbm)
  plot(state.vbm)

  tmp <- state.x77[,'HS Grad']
  tmp2 <- cut(tmp, seq(min(tmp),max(tmp), length.out=11),
    include.lowest=TRUE)
  plot(state.vbm,col=cm.colors(10)[tmp2])
}
\keyword{datasets}
