\name{read.shape}
\alias{read.shape}
\alias{dbf.read}
\alias{getinfo.shape}

\title{Read shapefile into Map object}
\description{
  
}
\usage{
read.shape(filen, dbf.data = TRUE)
dbf.read(filen)
getinfo.shape(filen)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filen}{name of file with *.shp extension, or *.dbf for dbf.read()}
  \item{dbf.data}{read DBF data together with shapes, default TRUE}
}
\details{
  The function calls code from shapelib to read shapefiles, a file format used by ESRI GIS software among others
}
\value{
read.shape() returns either a list of shapes of class ShapeList, or if dbf.data = TRUE a list with:
  \item{Shapes}{a list of shapes of class ShapeList; both the individual shapes and the list have attributes}
  \item{att.data}{a data frame of data from the associated DBF file}
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

