\name{read.shape}
\alias{read.shape}
%\alias{dbf.read}
\alias{getinfo.shape}

\title{Read shapefile into Map object}
\description{
  Read shapefile into Map object; the file should be given including its ".shp" extension, and the function will reconstruct the names of the database (dbf) file and the index (shx) file from these.
}
\usage{
read.shape(filen, dbf.data = TRUE, verbose=TRUE)
%dbf.read(filen)
getinfo.shape(filen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filen}{name of file with *.shp extension, or *.dbf for dbf.read()}
  \item{dbf.data}{read DBF data together with shapes, default TRUE}
  \item{verbose}{default TRUE --- report type of shapefile and number of shapes}
}
\details{
  The function calls code from shapelib to read shapefiles, a file format used by ESRI GIS software among others
}
\value{
read.shape() returns either a list of shapes of class ShapeList, or if dbf.data = TRUE a Map object with:
  \item{Shapes}{a list of shapes of class ShapeList; both the individual shapes and the list have attributes}
  \item{att.data}{a data frame of data from the associated DBF file; note that the field names are adjusted to use in R using \code{make.names()}, and so will permit the underscore character from R release 1.9.0.}
}
\references{\url{http://gdal.velocet.ca/projects/shapelib/}}
\author{Nicholas J. Lewin-Koh, modified by Roger Bivand \email{Roger.Bivand@nhh.no}; shapelib by Frank Warmerdam}

\seealso{\code{\link{plot.Map}}}
\examples{
x <- read.shape(system.file("shapes/sids.shp", package="maptools")[1])
length(x$Shapes)
unlist(lapply(x$att.data, class))
str(getinfo.shape(system.file("shapes/fylk-val.shp", package="maptools")[1]))
}
\keyword{spatial}

