# Copyright 2000-2001 (c) Nicholas Lewin-Koh 
# modifications 2001-2004 Roger Bivand
# reads an ESRI shapefile into a map object
# set the variables for the header info


read.shape <- function(filen, dbf.data=TRUE, verbose=TRUE) {
  filen <- path.expand(filen)
  shp.lst <- .Call("Rshapeget", as.character(filen), PACKAGE="maptools")
  if (verbose) {
    shinfo <- getinfo.shape(filen)
    print(shinfo)
  }
  n <- length(shp.lst)
  for (i in 1:n) {
    attr(shp.lst[[i]], "nVerts") <- as.integer(shp.lst[[i]]$nVerts)
    attr(shp.lst[[i]], "nParts") <- as.integer(shp.lst[[i]]$nParts)
    attr(shp.lst[[i]], "shp.type") <- as.integer(shp.lst[[i]]$shp.type)
    attr(shp.lst[[i]], "bbox") <- as.double(shp.lst[[i]]$bbox)
  }
  class(shp.lst) <- "ShapeList"
  if (dbf.data) {
    library(foreign)
    df <- read.dbf(filen)
    map <- list(Shapes=shp.lst, att.data=df)
    class(map) <- "Map"
    return(map)
  }
  else {
    return(shp.lst)
  }
}

getinfo.shape <- function(filen) {
  shptype <- 0
  nRecords <- 0
  MinBounds <- c(0,0,0,0)
  MaxBounds <- c(0,0,0,0)
  shapehead <-.C("Rshapeinfo", as.character(filen),
               as.integer(shptype), as.integer(nRecords), as.double(MinBounds),
               as.double(MaxBounds), PACKAGE="maptools")
  class(shapehead) <- "shapehead"
  shapehead
}

print.shapehead <- function(x, ...) {
    types <- c("Point", NA, "Line", NA, "Polygon")
    cat("Shapefile Type:", types[x[[2]]], "  # of Shapes:", 
      x[[3]], "\n")
}

#dbf.read <- function(filen) {
#  filen <- path.expand(filen)
#  df <- .Call("Rdbfread", as.character(filen), PACKAGE="maptools")
#  onames <- names(df)
#  inames <- make.names(onames, unique=TRUE)
#  names(df) <- inames
#  if (!(identical(onames, inames))) {
#    for (i in 1:length(onames))
#      if (!(identical(onames[i], inames[i]))) 
#        cat("Field name: ", onames[i], " changed to: ", inames[i], "\n")
#  }
#  df <- data.frame(lapply(df,
#    function(x) {if(is.character(x)) {factor(x)} else x }))
#  df
##}


#dbf.write <- function(dataframe, filename, factor2char=TRUE) {
## need to check precision, and that factors are converted to character
## how to handle NAs?
#  if (!is.data.frame(dataframe)) stop("not a data frame")
#  if (!all(complete.cases(dataframe))) stop("NAs not permitted")
#  if (any(sapply(dataframe, function(x) !is.null(dim(x)))))
#    stop("Can't handle multicolumn columns")
#  if (factor2char) {
#    dataframe <- as.data.frame(lapply(dataframe, function(x) {
#      if (is.factor(x)) {
#        x <- as.character(x)
#        class(x) <- "AsIs"
#        x
#     } else x } ))
#  } else {
#    dataframe <- as.data.frame(lapply(dataframe, function(x) {
#      if (is.factor(x)) {
#        x <- as.integer(x)
#        x
#     } else x } ))
#  }
#  dataframe <- as.data.frame(lapply(dataframe, function(x) {
#    if (is.logical(x)) {
#      x <- as.integer(x)
#      x
#    } else x } ))
#  m <- ncol(dataframe)
#  precision <- integer(m)
#  scale <- integer(m)
#  dfnames <- names(dataframe)
#  for (i in 1:m) {
#    nlen <- nchar(dfnames[i])
#    if (is.integer(dataframe[,i])) {
#      rx <- range(dataframe[,i])
#      mrx <- as.integer(max(ceiling(log10(abs(rx))))+3)
#      precision[i] <- as.integer(max(nlen, mrx))
#      if (precision[i] > 19) precision[i] <- as.integer(19)
#      scale[i] <- as.integer(0)
#    } else if (is.double(dataframe[,i])) {
#      precision[i] <- as.integer(19)
#      rx <- range(dataframe[,i])
#      mrx <- as.integer(max(ceiling(log10(abs(rx)))))
#      scale[i] <- as.integer(precision[i] - ifelse(mrx > 0, mrx+3, 3))
#      if (scale[i] > 15) scale[i] <- as.integer(15)
#    } else if (is.character(dataframe[,i])) {
#      mf <- max(nchar(dataframe[,i]))
#      precision[i] <- as.integer(max(nlen, mf))
#      if (precision[i] > 254) precision[i] <- as.integer(254)
#      scale[i] <- as.integer(0)
#    } else stop("unknown column type in data frame")
#  }
#  invisible( .External("DoWritedbf", as.character(filename), 
#    dataframe, as.integer(precision), as.integer(scale), PACKAGE="maptools"))
#}
#
#
write.pointShape <- function(object, file, coordinates, factor2char=TRUE) {
  file <- path.expand(file)
  if (nchar(basename(file)) > 8) 
    stop("shapefile names must conform to the 8.3 format")
  if (!is.matrix(coordinates)) stop("coordinates must be a matrix")
  if (!is.numeric(coordinates)) stop("coordinates must be numeric")
  if (ncol(coordinates) != 2) stop("coordinates must have 2 columns")
  if (nrow(object) != nrow(coordinates))
    stop("different number of rows in coordinates and data frame")
  library(foreign)
  write.dbf(object, paste(file, ".dbf", sep=""), factor2char=factor2char)
  res <- .Call("shpwritepoint", as.character(file), as.double(coordinates),
    PACKAGE="maptools")
  invisible(res)
}

write.polylistShape <- function(polylist, df, file, factor2char=TRUE) {
  file <- path.expand(file)
  if (nchar(basename(file)) > 8) 
    stop("shapefile names must conform to the 8.3 format")
  if (!inherits(polylist, "polylist")) stop("not a polylist object")
  if (nrow(df) != length(polylist))
    stop("different number of rows in polylist and data frame")
  library(foreign)
  write.dbf(df, paste(file, ".dbf", sep=""), factor2char=factor2char)
  res <- .Call("shpwritepolys", as.character(file), polylist, 
    PACKAGE="maptools")
  invisible(res)
}

