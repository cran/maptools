# Copyright 2000-2001 (c) Nicholas Lewin-Koh 
# modifications 2001-2003 Roger Bivand, 
# shape2poly based on code by Stéphane Dray


plotpolys <- function(pl, bb, col=NA, border=par("fg"), add=FALSE, 
	xlim=NULL, ylim=NULL, ...) {
	if (!inherits(pl, "polylist")) stop("Not a polygon list")
	if (!add) {
		maplim <- attr(pl, "maplim")
		if (is.null(maplim) && missing(bb))
			if (is.null(xlim) || is.null(ylim))
				stop("map limits missing")
		if (is.null(xlim)) {
			if (is.null(maplim))
				xlim <- c(min(bb[,1]), max(bb[,3]))
			else xlim <- maplim$x
		}
		if (is.null(ylim)) {
			if (is.null(maplim))
				ylim <- c(min(bb[,2]), max(bb[,4]))
			else ylim <- maplim$y
		}
		plot(x=xlim, y=ylim, xlim=xlim, ylim=ylim, type="n",
		asp=1, xlab="", ylab="")
	}
	if (length(col) != length(pl)) {
		col <- rep(col, length(pl), length(pl))
	}
	for (j in 1:length(pl)) polygon(pl[[j]], col=col[j], border=border, ...)
}

shape2poly <- function(shape, region.id=NULL) {
    if (is.null(shape$shp)) stop("No shp component in this list")
    if (shape$shp$header$shape.type != 5) stop("Not a polygon shapefile")
    nrecord <- length(shape$shp$shp)
    res <- vector(mode="list", length=nrecord)
    if (is.null(region.id) || length(region.id) != nrecord) {
	attr(res, "region.id") <- as.character(1:nrecord)
    } else {
	attr(res, "region.id") <- as.character(region.id)
    }
    np <- integer(nrecord)
    for (i in 1:nrecord) np[i] <- shape$shp$shp[[i]]$num.parts
    for (i in 1:nrecord) {
	if (np[i] > 1) {
	    res[[i]] <- .getMultishape(shape$shp$shp[[i]], np[i])
	} else {
	    res[[i]] <- as.matrix(shape$shp$shp[[i]]$points)
	    attr(res[[i]], "pstart") <- list(from=1, 
		to=shape$shp$shp[[i]]$num.points)
	    rownames(res[[i]]) <- NULL
	    colnames(res[[i]]) <- NULL
	}
	attr(res[[i]], "nParts") <- np[i]
	rD <- integer(np[i])
	for (j in 1:np[i]) rD[j] <- ringDir(res[[i]], j)
	attr(res[[i]], "ringDir") <- rD
	attr(res[[i]], "bbox") <- as.vector(shape$shp$shp[[i]]$box)
    }

    class(res) <- "polylist"
    attr(res, "maplim") <- shp2maplim(shape)
    return(res)

}

shape2lines <- function(shape) {
    if (is.null(shape$shp)) stop("No shp component in this list")
    if (shape$shp$header$shape.type != 3) stop("maptype not line/arc")
	n <- length(shape$shp$shp)
	res <- vector(mode="list", length=n)
	nParts <- integer(n)
	for (i in 1:n) nParts[i] <- shape$shp$shp[[i]]$num.parts
	for (i in 1:n) {
		if (nParts[i] > 1)
			res[[i]] <- .getMultishape(shape$shp$shp[[i]], 
				nParts[i])
		else {
			res[[i]] <- as.matrix(shape$shp$shp[[i]]$points)
			attr(res[[i]], "pstart") <- list(from=1, 
				to=shape$shp$shp[[i]]$num.points)
			rownames(res[[i]]) <- NULL
			colnames(res[[i]]) <- NULL
		}
		attr(res[[i]], "nParts") <- nParts[i]
	}
	class(res) <- "lineslist"
	attr(res, "maplim") <- shp2maplim(shape)
	res
}

shape2points <- function(shape) {
	if (is.null(shape$shp)) stop("No shp component in this list")
	if (shape$shp$header$shape.type != 1)
		stop("maptype not points")
	n <- length(shape$shp$shp)
	res <- shape$shp$shp[,2:3]
	class(res) <- "Mappoints"
	attr(res, "maplim") <- shp2maplim(shape)
	res
}


# based on SHPRingDir_2d, modified to use current ring only, and to strip
# out last vertex if identical with first

ringDir <- function(xy, ring) {
	nParts <- attr(xy, "nParts")
	if (ring > nParts) stop("ring too large")
	from <- attr(xy, "pstart")$from
	to <- attr(xy, "pstart")$to
	a <- xy[from[ring]:to[ring],1]
	b <- xy[from[ring]:to[ring],2]
	nvx <- length(b)

	if((a[1] == a[nvx]) && (b[1] == b[nvx])) {
		a <- a[-nvx]
		b <- b[-nvx]
		nvx <- nvx - 1
	}

	tX <- 0.0
	dfYMax <- max(b)
	ti <- 1
	for (i in 1:nvx) {
		if (b[i] == dfYMax && a[i] > tX) ti <- i
	}
	if ( (ti > 1) & (ti < nvx) ) { 
		dx0 = a[ti-1] - a[ti]
      		dx1 = a[ti+1] - a[ti]
      		dy0 = b[ti-1] - b[ti]
      		dy1 = b[ti+1] - b[ti]
   	} else {
#   /* if the tested vertex is at the origin then continue from 0 (1) */ 
     		dx1 = a[2] - a[1]
      		dx0 = a[nvx] - a[1]
      		dy1 = b[2] - b[1]
      		dy0 = b[nvx] - b[1]
   	}
	v3 = ( (dx0 * dy1) - (dx1 * dy0) )
	if ( v3 > 0 ) return (1)
   	else return (-1)
}

shp2maplim <- function(shape) {
	if (is.null(shape$shp)) stop("No shp component in this list")
    	mapxlim<-c(shape$shp$header$xmin, shape$shp$header$xmax)
	mapylim<-c(shape$shp$header$ymin, shape$shp$header$ymax)
	list(x=mapxlim, y=mapylim)
}

.getMultishape <- function(shp, nParts) {
	Pstart <- shp$parts
	nVerts <- shp$num.points
	from <- integer(nParts)
	to <- integer(nParts)
	from[1] <- 1
	for (j in 1:nParts) {
		if (j == nParts) to[j] <- nVerts
		else {
			to[j] <- Pstart[j+1]
			from[j+1] <- to[j]+1
		}
	}
	res <- as.matrix(shp$points[from[1]:to[1],])
	if (nParts > 1) {
	    for (j in 2:nParts) {
	        res <- rbind(res, c(NA, NA))
	        res <- rbind(res, as.matrix(shp$points[from[j]:to[j],]))
	    }
	}
	rownames(res) <- NULL
	colnames(res) <- NULL
	for (j in 1:nParts) {
		from[j] <- from[j] + (j-1)
		to[j] <- to[j] + (j-1)
	}
	attr(res, "pstart") <- list(from=from, to=to)
	res
}

shape2bbs <- function(shape) {
    if (is.null(shape$shp)) stop("No shp component in this list")
    if (shape$shp$header$shape.type != 5) stop("Not a polygon shapefile")
    n <- length(shape$shp$shp)
    res <- matrix(0, ncol=4, nrow=n)
    for (i in 1:n) res[i,] <- as.vector(shape$shp$shp[[i]]$box)
    res
}

Map2lines <- function(Map) {
	if (class(Map) != "Map") stop("not a Map")
	if (attr(Map$Shapes,'shp.type') != 'arc')
		stop("maptype not line/arc")
	n <- attr(Map$Shapes,'nshps')
	res <- vector(mode="list", length=n)
	nParts <- integer(n)
	for (i in 1:n) nParts[i] <- attr(Map$Shapes[[i]], "nParts")
	for (i in 1:n) {
		if (nParts[i] > 1)
			res[[i]] <- .getMultiShp(Map$Shapes[[i]], nParts[i])
		else {
			res[[i]] <- Map$Shapes[[i]]$verts
			attr(res[[i]], "pstart") <- list(from=1, 
				to=attr(Map$Shapes[[i]], "nVerts"))
		}
		attr(res[[i]], "nParts") <- nParts[i]
	}
	class(res) <- "lineslist"
	attr(res, "maplim") <- Map2maplim(Map)
	res
}

Map2points <- function(Map) {
	if (class(Map) != "Map") stop("not a Map")
	if (attr(Map$Shapes,'shp.type') != 'point')
		stop("maptype not points")
	n <- attr(Map$Shapes,'nshps')
	res <- matrix(NA, nrow=n, ncol=2)
	for (i in 1:n) res[i,] <- Map$Shapes[[i]]$verts
	class(res) <- "Mappoints"
	attr(res, "maplim") <- Map2maplim(Map)
	res
}


Map2poly <- function(Map, region.id=NULL) {
	if (class(Map) != "Map") stop("not a Map")
	if (attr(Map$Shapes,'shp.type') != 'poly')
		stop("maptype not poly")
	res <- .get.polylist(Map=Map, region.id=region.id)
	attr(res, "maplim") <- Map2maplim(Map)
	res
}

.get.polylist <- function(Map, region.id=NULL) {
	n <- attr(Map$Shapes,'nshps')
	res <- vector(mode="list", length=n)
	nParts <- integer(n)
	for (i in 1:n) nParts[i] <- attr(Map$Shapes[[i]], "nParts")
	for (i in 1:n) {
		if (nParts[i] > 1)
			res[[i]] <- .getMultiShp(Map$Shapes[[i]], nParts[i])
		else {
			res[[i]] <- Map$Shapes[[i]]$verts
			attr(res[[i]], "pstart") <- list(from=1, 
				to=attr(Map$Shapes[[i]], "nVerts"))
		}
		attr(res[[i]], "bbox") <- as.vector(attr(Map$Shapes[[i]],
			 "bbox"))
		attr(res[[i]], "RingDir") <- as.vector(attr(Map$Shapes[[i]],
			"RingDir"))
		attr(res[[i]], "nParts") <- nParts[i]
		rD <- integer(nParts[i])
		for (j in 1:nParts[i]) rD[j] <- ringDir(res[[i]], j)
		attr(res[[i]], "ringDir") <- rD
	}
	if (is.null(region.id) || length(region.id) != n) {
		attr(res, "region.id") <- as.character(1:n)
	} else {
		attr(res, "region.id") <- as.character(region.id)
	}
	class(res) <- "polylist"
	invisible(res)
}

.getMultiShp <- function(shp, nParts) {
	Pstart <- shp$Pstart
	nVerts <- attr(shp, "nVerts")
	from <- integer(nParts)
	to <- integer(nParts)
	from[1] <- 1
	for (j in 1:nParts) {
		if (j == nParts) to[j] <- nVerts
		else {
			to[j] <- Pstart[j+1]
			from[j+1] <- to[j]+1
		}
	}
	res <- shp$verts[from[1]:to[1],]
	if (nParts > 1) {
	    for (j in 2:nParts) {
	        res <- rbind(res, c(NA, NA))
	        res <- rbind(res, shp$verts[from[j]:to[j],])
	     }
	}
	for (j in 1:nParts) {
		from[j] <- from[j] + (j-1)
		to[j] <- to[j] + (j-1)
	}
	attr(res, "pstart") <- list(from=from, to=to)
	res
}

.get.polybbs <- function(Map) {
	n <- length(Map$Shapes)
	res <- matrix(0, ncol=4, nrow=n)
	for (i in 1:n) res[i,] <- attr(Map$Shapes[[i]], "bbox")
	res
}

Map2bbs <- function(Map) {
	if (class(Map) != "Map") stop("not a Map")
	if (attr(Map$Shapes,'shp.type') != 'poly')
		stop("maptype not poly")
	res <- .get.polybbs(Map)
	res
}

Map2maplim <- function(Map) {
	if (class(Map) != "Map") stop("not a Map")
    	mapxlim<-c(attr(Map$Shapes, 'minbb')[1], 
		attr(Map$Shapes, 'maxbb')[1])
	mapylim<-c(attr(Map$Shapes, 'minbb')[2], 
		attr(Map$Shapes, 'maxbb')[2])
	list(x=mapxlim, y=mapylim)
}


convert.pl <- function(pl) {
	if (!inherits(pl, "multiparts")) stop("not a mulitpart polylist")
	res <- vector(mode="list", length=length(pl))
	for (i in 1:length(pl)) {
		lp <- length(pl[[i]])
		res[[i]] <- pl[[i]][[1]]
		if (lp > 1) {
			for (j in 2:lp) {
				res[[i]] <- rbind(res[[i]], c(NA, NA))
				res[[i]] <- rbind(res[[i]], pl[[i]][[j]])
			}
		}
	}
	if (!is.null(attr(pl, "region.id")))
		attr(res, "region.id") <- attr(pl, "region.id")
	class(res) <- "polylist"
	res
}


plot.Map <- function(x, recs, auxvar=NULL, add=FALSE, fg ='gray', 
                   ol='black', prbg=NULL, glyph=16, 
                   type='q', nclass=5, ...) 
{
  theMap <- x
  if(!inherits(theMap, "Map"))
  stop("Map.obj must be of class Map")

  if(missing(recs)) recs <- 1:attr(theMap$Shapes,'nshps')

  if (length(fg) != length(recs)) fg <- rep(fg[1], length(recs))

  xylims <- Map2maplim(theMap)

  if(!add){
     plot(xylims$x, xylims$y, asp=1, type='n',...)
  }
  if(!is.null(prbg)) {
    plim <- par()$usr
    rect(plim[1], plim[2], plim[3], plim[4], col=prbg) #,border=par()$bg)
   }

  if(attr(theMap$Shapes,'shp.type') == 'point' ) {
    for(i in recs) {
      points(theMap$Shapes[[i]]$verts, pch=glyph, col=fg[i])
    }
  }
  if(attr(theMap$Shapes,'shp.type') == 'arc'){
    for(i in recs) {
      if(attr(theMap$Shapes[[i]], 'nParts') == 1) {
        lines(theMap$Shapes[[i]]$verts, col=ol)
      }
      if(attr(theMap$Shapes[[i]], 'nParts') > 1){
        for(j in 1:attr(theMap$Shapes[[i]], 'nParts')) {
	  if(j < attr(theMap$Shapes[[i]], 'nParts'))
             lines(theMap$Shapes[[i]]$verts[j:(j+1)-1], col= ol)
          else
             lines(theMap$Shapes[[i]]$verts[j:attr(theMap$Shapes[[i]],
             'nVerts')], col= ol)  
        }
      }
    }
  }
  if(attr(theMap$Shapes,'shp.type') == 'poly'){
    if(!is.null(auxvar) && nclass > 1) {
      col.rmp <- color.ramp(nclass, nvec=auxvar)
      for(i in recs) {
        if(attr(theMap$Shapes[[i]], 'nParts') == 1) {
          polygon(theMap$Shapes[[i]]$verts,
                col=col.rmp$ramp[col.rmp$col.class[i]],
                border= ol)
        }
        if(attr(theMap$Shapes[[i]], 'nParts') > 1) {
          for(j in 1:attr(theMap$Shapes[[i]], 'nParts')) {
	    if(j < attr(theMap$Shapes[[i]], 'nParts')) {
              polygon(theMap$Shapes[[i]]$verts[(theMap$Shapes[[i]]$Pstart[j]+1):
                    theMap$Shapes[[i]]$Pstart[j+1],],
		    col=col.rmp$ramp[col.rmp$col.class[i]], border=ol)
            } else {
              polygon(theMap$Shapes[[i]]$verts[(theMap$Shapes[[i]]$Pstart[j]+1):
                    attr(theMap$Shapes[[i]],'nVerts'),],
                    col=col.rmp$ramp[col.rmp$col.class[i]],border= ol)
            }
          }
        }
      }
    } else {
      for(i in recs) {
        if(attr(theMap$Shapes[[i]],'nParts') == 1) {
          polygon(theMap$Shapes[[i]]$verts, col=fg[i], border= ol)
        }
        if(attr(theMap$Shapes[[i]],'nParts') > 1) {
          for(j in 1:attr(theMap$Shapes[[i]], 'nParts')) {
	    if(j<attr(theMap$Shapes[[i]], 'nParts')) {
              polygon(theMap$Shapes[[i]]$verts[(theMap$Shapes[[i]]$Pstart[j]+1):
                    theMap$Shapes[[i]]$Pstart[j+1],], col=fg[i] ,border= ol)
            } else {
              polygon(theMap$Shapes[[i]]$verts[(theMap$Shapes[[i]]$Pstart[j]+1):
                    attr(theMap$Shapes[[i]],'nVerts'),],
                    col=fg[i], border=ol)
            }
          }
        }
      }
    }
  }
  if(attr(theMap$Shapes,'shp.type')=='multipoint'){
    stop("Multipoint shape type not yet plotted")
  }
}


