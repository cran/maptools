# Copyright 2000-2001 (c) Nicholas Lewin-Koh 
# modifications 2001-2004 Roger Bivand


read.shape <- function(filen, dbf.data=TRUE) {
  shp.lst <- .Call("Rshapeget", as.character(filen), PACKAGE="maptools")
  n <- length(shp.lst)
  for (i in 1:n) {
    attr(shp.lst[[i]], "nVerts") <- as.integer(shp.lst[[i]]$nVerts)
    attr(shp.lst[[i]], "nParts") <- as.integer(shp.lst[[i]]$nParts)
    attr(shp.lst[[i]], "shp.type") <- as.integer(shp.lst[[i]]$shp.type)
    attr(shp.lst[[i]], "bbox") <- as.double(shp.lst[[i]]$bbox)
  }
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
  onames <- names(df)
  inames <- make.names(onames, unique=TRUE)
  names(df) <- inames
  if (!(identical(onames, inames))) {
    for (i in 1:length(onames))
      if (!(identical(onames[i], inames[i]))) 
        cat("Field name: ", onames[i], " changed to: ", inames[i], "\n")
  }
  df <- data.frame(lapply(df,
    function(x) {if(is.character(x)) {factor(x)} else x }))
  df
}

#reads an ESRI shapefile into a map object
#set the variables for the header info

dbf.write <- function(dataframe, filename, precision){
  if (any(sapply(dataframe, function(x) !is.null(dim(x)))))
    stop("Can't handle multicolumn columns")
  invisible( .External("DoWritedbf", as.character(filename), 
    dataframe, as.integer(precision), PACKAGE="maptools"))
}

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

