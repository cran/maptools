# Copyright 2000-2001 (c) Nicholas Lewin-Koh 
# modifications 2001-2004 Roger Bivand, 
# shape2poly based on code by Stéphane Dray

plot.polylist <- function(x, col, border=par("fg"), add=FALSE, 
	xlim=NULL, ylim=NULL, xpd=NULL, density=NULL, angle=45, 
	pbg=par("bg"), forcefill=TRUE, ...) {
	if (!inherits(x, "polylist")) stop("Not a polygon list")

	usrpoly <- function(x) {
		p <- matrix(c(x[1], x[2], x[2], x[1], x[3], x[3], 
			x[4], x[4]), ncol=2)
		p
	}
	if (!add) {
		maplim <- attr(x, "maplim")
		if (is.null(maplim))
			if (is.null(xlim) || is.null(ylim))
				stop("map limits missing")
		if (is.null(xlim)) xlim <- maplim$x
		if (is.null(ylim)) ylim <- maplim$y
		plot(x=xlim, y=ylim, xlim=xlim, ylim=ylim, type="n",
			asp=1, xlab="", ylab="", ...)
		polygon(usrpoly(par("usr")), col = pbg, border = NA)
	}
	pO <- attr(x, "plotOrder")
	if (is.null(pO)) pO <- 1:length(x)
	if (missing(col)) {
		if (length(density) != length(x)) {
			density <- rep(density, length(x), length(x))
		}
		if (length(angle) != length(x)) {
			angle <- rep(angle, length(x), length(x))
		}
		for (j in pO) polygonholes(x[[j]], border=border, 
			xpd=xpd, density=density[j], angle=angle[j], pbg=pbg,
			forcefill=forcefill)
	} else {
		if (length(col) != length(x)) {
			col <- rep(col, length(x), length(x))
		}
		for (j in pO) 
			polygonholes(x[[j]], col=col[j], border=border, 
			xpd=xpd, pbg=pbg, forcefill=forcefill)
	}
	if (forcefill) warning("From next release, default fill behaviour will change")
}

polygonholes <- function(coords, col=NA, border=NULL, xpd=NULL, density=NULL,
	angle=45, pbg=par("bg"), forcefill=TRUE) {
	nParts <- attr(coords, "nParts")
	if (is.null(nParts)) nParts <- 1
	pFrom <- attr(coords, "pstart")$from
	if (is.null(pFrom)) pFrom <- 1
	pTo <- attr(coords, "pstart")$to
	if (is.null(pTo)) pTo <- dim(coords)[1]
	if (is.na(col)) hatch <- TRUE
	else hatch <- FALSE
	pO <- attr(coords, "plotOrder")
	if (is.null(pO)) pO <- 1:nParts
	for (i in pO) {
		if (hatch) {
			if (forcefill || attr(coords, "ringDir")[i] == 1) {
				polygon(coords[pFrom[i]:pTo[i],], 
				border = border, xpd = xpd, 
				density = density, angle = angle)
			} else { 
				polygon(coords[pFrom[i]:pTo[i],], 
				border = border, xpd = xpd, col=pbg, 
				density = NULL)
			}
		} else {
			if (forcefill || attr(coords, "ringDir")[i] == 1) {
				polygon(coords[pFrom[i]:pTo[i],], 
					border = border, xpd = xpd, col=col)
			} else {
				polygon(coords[pFrom[i]:pTo[i],], 
					border = border, xpd = xpd, col=pbg)
			}
		}
	}
}


plotpolys <- function(pl, bb, col=NA, border=par("fg"), add=FALSE, 
	xlim=NULL, ylim=NULL, ...) {
	.Deprecated("plot.polylist", package="maptools")
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
   	} else if (ti == nvx) {
		dx0 = a[ti-1] - a[ti]
      		dx1 = a[1] - a[ti]
      		dy0 = b[ti-1] - b[ti]
      		dy1 = b[1] - b[ti]
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


Map2poly <- function(Map, region.id=NULL, raw=TRUE) {
	if (class(Map) != "Map") stop("not a Map")
	if (attr(Map$Shapes,'shp.type') != 'poly')
		stop("maptype not poly")
	res <- .get.polylist(Map=Map, region.id=region.id, raw=raw)
	attr(res, "maplim") <- Map2maplim(Map)
	pO <- as.integer(1:attr(Map$Shapes,'nshps'))
	after <- as.integer(rep(NA, attr(Map$Shapes,'nshps')))
	rD <- sapply(res, function(x) 
		attr(x, "ringDir")[attr(x, "plotOrder")[1]])
	r1 <- .mtInsiders(res, rD)
	if (!all(sapply(r1, is.null))) {
		lres <- .mtlbuild(.mtafters(r1), rD)
		pO <- lres$pO
		after <- lres$after
	}
#	pO <- as.integer(1:attr(Map$Shapes,'nshps'))
#	after <- as.integer(rep(NA, attr(Map$Shapes,'nshps')))
#	r1 <- .mtInsiders(res)
#	if (!all(sapply(r1, is.null))) {
#		after <- as.integer(sapply(r1, 
#			function(x) ifelse(is.null(x), NA, max(x))))
#		pO <- order(after, na.last=FALSE)
#	}
	attr(res, "after") <- after
	attr(res, "plotOrder") <- pO
	if (!raw) {
		rD <- sapply(res, function(x) attr(x, 
			"ringDir")[which(attr(x, "plotOrder") == 1)])
		if (any((rD == -1) & is.na(after))) {
			oddCC <- which((rD == -1) & is.na(after))
			for (i in oddCC) {
				tgt <- which(attr(res[[i]], "plotOrder") == 1)
				nParts <- attr(res[[i]], "nParts")
				tmp <- as.matrix(res[[i]])
				from <- attr(res[[i]], "pstart")$from[tgt]
				to <- attr(res[[i]], "pstart")$to[tgt]
				tmp[from:to,] <- res[[i]][to:from, ]
 				attributes(tmp) <- attributes(res[[i]])
				rD <- vector(length=nParts, mode="integer")
				for (j in 1:nParts) rD[j] <- ringDir(tmp, j)
				attr(tmp, "ringDir") <- rD
				res[[i]] <- tmp
				warning(paste("ring direction changed in polygon", i))
			}
		}
	}
	if (raw) warning("From next release, default hole handling will change")
	res
}

.mtInsiders <- function(pl, rD) {
	bbox1 <- function(x) {
		r1 <- range(x[,1], na.rm=TRUE)
		r2 <- range(x[,2], na.rm=TRUE)
		res <- c(r1[1], r2[1], r1[2], r2[2])
		res
	}

	n <- length(pl)
	bbs <- matrix(0, nrow=n, ncol=4)
	for (i in 1:n) bbs[i,] <- bbox1(pl[[i]])
	res <- .Call("mtInsiders", as.integer(n), as.double(bbs), 
		PACKAGE="maptools")
	res1 <- vector(mode="list", length=n)

	for (i in 1:n) {
		if (!is.null(res[[i]])) {
			ri <- res[[i]]
			ixc <- pl[[i]][1,1]
			iyc <- pl[[i]][1,2]
			int <- logical(length(ri))
			for (j in 1:length(ri)) {
				xj <- pl[[ri[j]]]
				jxc <- na.omit(xj[,1])
				jyc <- na.omit(xj[,2])
				pip <- mt.point.in.polygon(ixc, 
					iyc, jxc, jyc)
				int[j] <- ((pip == 1) | (pip > 1))
#				int[j] <- ((pip == 1) | 
#					((pip > 1) & ((rD[i] == 1) & 
#					(rD[ri[j]] == -1))))

			}
			rj <- ri[int]
			if (length(rj) > 0) {
				res1[[i]] <- as.integer(rj)
			}
		}
	}
	res1
}

.mtafters <- function(rl) {

# argument is output from .insiders() - a list with either NULL components 
# (not included in any other polygon) or lists of polygons in which the polygon
# in question is included; outputs a from:to matrix

	n <- length(rl)
	res <- NULL
	for (i in 1:n) {
		if (is.null(rl[[i]]))
			res <- rbind(res, c(i, NA))
		else {
			for (j in 1:length(rl[[i]])) {
				res <- rbind(res, c(i, rl[[i]][j]))
			}
		}
	}
	res
}

.mtlbuild1 <- function(x) {

# reverse list builder with from:to matrix as argument, used to try to find
# circularities

	lx <- vector(mode="list", length=length(unique(x[,1])))
	rle.x <- rle(x[,1])
	cs1.x <- cumsum(rle.x$lengths)
	cs0.x <- c(1, cs1.x[1:(length(lx)-1)]+1)
	ii <- 1
	for (i in 1:length(lx)) {
		if (rle.x$value[ii] == i) {
			lx[[i]] <- as.integer(x[cs0.x[ii]:cs1.x[ii],2])
			ii <- ii+1
		}
	}
	lx
}

.mtcircs <- function(x) {

# try to find circularities from reverse list as argument (polygons reported
# as being inside each other despite checking ring direction in .insiders);
# only the first loop will be run in normal cases

	res <- NULL
	for (i in 1:length(x)) {
		if (!is.na(match(i, unlist(x[x[[i]]])))) {
			hits <- rep(FALSE, length(x[[i]]))
			for (j in 1:length(hits)) {
				jj <- x[[i]][j]
				hits[j] <- (i %in% x[[jj]])
			}
			if (length(which(hits)) > 1) stop("multiple circulars")
			pair <- c(i, x[[i]][hits])
			res <- rbind(res, pair)
		}			
	}
	res1 <- NULL
	if (!is.null(res)) {
		if (nrow(res) %% 2 != 0) stop("odd circulars")
		gone <- rep(FALSE, nrow(res))
		for (i in 1:nrow(res)) {
			if (!gone[i]) {
				from <- res[i,1]
				to <- res[i,2]
				hit <- match(from, res[,2])
				if (!gone[hit]) {
					if (res[hit,1] != to) 
						stop("mismatched circular")
					res1 <- rbind(res1, c(from, to))
					gone[i] <- TRUE
				}
			}
		}
	}
	res1
}

.mtlbuild <- function(x, rD) {

# list analysis of matrix output from .afters combined with current ring
# directions (which may be quite wrong) to generate a plot order and 
# vector of afters (NA for no dependency, 1 for dependency on being plotted
# after another polygon)

	ids <- x[,1]
	ins <- x[,2]
	n <- length(unique(ids))
	nas <- which(is.na(ins))
	ntop <- length(nas)
	pO <- vector(length=n, mode="integer")
	after <- rep(as.integer(NA), length=n)
	gone <- rep(FALSE, n)
	j <- 1
	for (i in 1:ntop) {
		ii <- ids[nas[i]]
		if (!gone[ii]) {
			gone[ii] <- TRUE
			pO[j] <- ii
			j <- j+1
		} else warning(paste("level 1 circularity at", ii))
		ihits <- which(ins == ii)

# for each top level (not inside any other) polygon, check to see if any
# polygons are inside it, and insert orders to match; from outer to deepest in;
# the gone vector is used to avoid multiple assignments to the plot
# order list that can happen with circularity

		if (length(ihits) > 0) {
			tihits <- ids[ihits]
			rtihits <- rle(ids[ids %in%tihits])
			o <- order(rtihits$lengths)
			for (jj in 1:length(rtihits$values)) {
				jjj <- rtihits$values[o][jj]
				if (!gone[jjj]) {
					gone[jjj] <- TRUE
					pO[j] <- jjj
					j <- j+1
				} else warning(paste("level 2 circularity at", 
					jjj))
				after[jjj] <- as.integer(1)
			}
		}
	}
	xcircs <- .mtcircs(.mtlbuild1(x))

# Further attempts to trap circularities, possibly no longer needed, first
# introduced before point-in-polygon test added to .insiders; TODO check
# whether is.null(xcircs) is always TRUE

	if (!is.null(xcircs)) {
		for (i in 1:nrow(xcircs)) {
			from <- xcircs[i,1]
			to <- xcircs[i,2]
			rDfrom <- rD[from]
			rDto <- rD[to]
			pOfrom <- which(pO == from)
			pOto <- which(pO == to)
			if (rDfrom == 1) {
				if (pOfrom < pOto) {
					pO[pOto] <- from
					pO[pOfrom] <- to
				}
			}
			if (rDto == 1) {
				if (pOfrom > pOto) {
					pO[pOto] <- from
					pO[pOfrom] <- to
				}
			}			
		}
	}
	list(pO=pO, after=after)
}

.get.polylist <- function(Map, region.id=NULL, raw=TRUE) {
	n <- attr(Map$Shapes,'nshps')
	res <- vector(mode="list", length=n)
	nParts <- integer(n)
	for (i in 1:n) nParts[i] <- attr(Map$Shapes[[i]], "nParts")
	for (i in 1:n) {
		if (nParts[i] > 1) {
			res[[i]] <- .getMultiShp(Map$Shapes[[i]], nParts[i], 
				raw=raw)
		} else {
			res[[i]] <- Map$Shapes[[i]]$verts
			attr(res[[i]], "pstart") <- list(from=1, 
				to=nVerts <- nrow(Map$Shapes[[i]]$verts))
#				attr(Map$Shapes[[i]], "nVerts"))
			attr(res[[i]], "after") <- 1
			attr(res[[i]], "plotOrder") <- 1
			attr(res[[i]], "bbox") <- 
				as.vector(attr(Map$Shapes[[i]], "bbox"))
			attr(res[[i]], "RingDir") <- 
				as.vector(attr(Map$Shapes[[i]], "RingDir"))
			attr(res[[i]], "nParts") <- nParts[i]
			attr(res[[i]], "ringDir") <- ringDir(res[[i]], 1)
		}
	}
	if (is.null(region.id) || length(region.id) != n) {
		attr(res, "region.id") <- as.character(1:n)
	} else {
		attr(res, "region.id") <- as.character(region.id)
	}
	class(res) <- "polylist"
	invisible(res)
}

.getMultiShp <- function(shp, nParts, raw=TRUE) {
	Pstart <- shp$Pstart
#	nVerts <- attr(shp, "nVerts")
	nVerts <- nrow(shp$verts)
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
	attr(res, "nParts") <- nParts
	attr(res, "pstart") <- list(from=from, to=to)
	attr(res, "bbox") <- as.vector(attr(shp, "bbox"))
	attr(res, "RingDir") <- as.vector(attr(shp, "RingDir"))
	rD <- integer(nParts)
	for (j in 1:nParts) rD[j] <- ringDir(res, j)
	attr(res, "ringDir") <- rD
	pO <- as.integer(1:nParts)
	after <- as.integer(rep(NA, nParts))
	res1 <- vector(mode="list", length=nParts)
	for (i in 1:nParts) res1[[i]] <- res[from[i]:to[i],]
	r1 <- .mtInsiders(res1, rD)
	if (!all(sapply(r1, is.null))) {
		lres <- .mtlbuild(.mtafters(r1), rD)
		pO <- lres$pO
		after <- lres$after
	}
#	r1 <- .mtInsiders(res1)
#	if (!all(sapply(r1, is.null))) {
#		after <- as.integer(sapply(r1, 
#			function(x) ifelse(is.null(x), NA, max(x))))
#		pO <- order(after, na.last=FALSE)
#	}

	attr(res, "after") <- after
	attr(res, "plotOrder") <- pO
	if (!raw) {
		top <- which(pO == 1)
		if (any((rD[-top] == -1) & is.na(after[-top]))) {
			oddCC <- which((rD == -1) & is.na(after))
			for (i in oddCC) {
				if (i != top) {
					from1 <- from[i]
					to1 <- to[i]
					res[from[i]:to[i],] <- res[to[i]:from[i],]
					attr(res, "ringDir")[i] <- ringDir(res, i)
					warning(paste("ring direction changed in subpolygon"))
				}
			}
		}


	}
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

  ret <- NULL
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
      ret <- col.rmp
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
  invisible(ret)
}


