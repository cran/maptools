\name{dbf.write}
\alias{dbf.write}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write a DBF file}
\description{
  The function tries to write a data frame to a DBF file.
}
\usage{
dbf.write(dataframe, filename, factor2char=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{a data frame object}
  \item{filename}{a file name to be written to}
  \item{factor2char}{logical, default TRUE, convert factor columns to character}
}
\details{
  The function calls code from shapelib to write a DBF format. At present, NAs will cause an error and no file will be written. If factors should be represented as character fields, the data frame must either be pre-processed to convert those colums using \code{I(as.character())}, or the \code{factor2char} argument set to TRUE. In addition logical columns are converted to integer; maximum precision (number of digits including minus sign and decimal sign) for numeric is 19 - scale (digits after the decimal sign) is calculated internally based on the number og digits before the decimal sign.
}
\value{
  no return value.
}
\references{\url{http://shapelib.maptools.org/}}

\author{Nicholas J. Lewin-Koh, modified by Roger Bivand; shapelib by Frank Warmerdam}

\note{From maptools 0.4-7, this function is placed in the user-visible namespace on a trial basis, and reports of any malfunction should be sent to the package maintainer, Roger Bivand \email{Roger.Bivand@nhh.no}. It is likely that this function and its arguments will be changed.}


\seealso{\code{\link{dbf.read}}}
\examples{
data(warpbreaks)
str(warpbreaks)
try1 <- paste(tempfile(), ".dbf", sep="")
dbf.write(warpbreaks, try1, factor2char=FALSE)
in1 <- dbf.read(try1)
str(in1)
try2 <- paste(tempfile(), ".dbf", sep="")
dbf.write(warpbreaks, try2, factor2char=TRUE)
in2 <- dbf.read(try2)
str(in2)
}
\keyword{spatial}
