\name{dbf.read}
\alias{dbf.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read DBF a file}
\description{
  The function reads a DBF file into a data frame, converting character fields to factors, and trying to respect NULL fields.
}
\usage{
dbf.read(filen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filen}{name of input file}
}

\value{
  a data frame of data from the DBF file; note that the field names are adjusted to use in R using \code{make.names()}, and so will permit the underscore character from R release 1.9.0.
}

\references{\url{http://shapelib.maptools.org/}}

\author{Nicholas Lewin-Koh and  Roger Bivand; shapelib by Frank Warmerdam}

\note{From maptools 0.4-7, this function is placed in the user-visible namespace on a trial basis, and reports of any malfunction should be sent to the package maintainer, Roger Bivand \email{Roger.Bivand@nhh.no}. It is likely that this function and its arguments will be changed.}

\seealso{\code{\link{read.shape}}}
\examples{
x <- dbf.read(system.file("shapes/sids.dbf", package="maptools")[1])
str(x)
summary(x)
}
\keyword{spatial}
