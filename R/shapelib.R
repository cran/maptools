# Copyright 2000-2001 (c) Nicholas Lewin-Koh 
# modifications 2001-2003 Roger Bivand


read.shape <- function(filen, dbf.data=TRUE) {
  shp.lst <- .Call("Rshapeget", as.character(filen), PACKAGE="maptools")
  class(shp.lst) <- "ShapeList"
  if (dbf.data) {
    df <- dbf.read(filen)
    map <- list(Shapes=shp.lst, att.data=df)
    class(map) <- "Map"
    return(map)
  }
  else {
    return(shp.lst)
  }
}

dbf.read <- function(filen) {
  df <- .Call("Rdbfread", as.character(filen), PACKAGE="maptools")
  df <- data.frame(lapply(df,
    function(x) {if(is.character(x)) {factor(x)} else x }))
  df
}

#reads an ESRI shapefile into a map object
#set the variables for the header info

getinfo.shape <- function(filen) {
  shptype <- 0
  nRecords <- 0
  MinBounds <- c(0,0,0,0)
  MaxBounds <- c(0,0,0,0)
  shapehead <-.C("Rshapeinfo", as.character(filen),
               as.integer(shptype), as.integer(nRecords), as.double(MinBounds),
               as.double(MaxBounds), PACKAGE="maptools")
  shapehead
}

