# Copyright 2003-4 (c) Roger Bivand

subset.polylist <- function(x, subset, fit.bbox=TRUE, ...) {
	if (!inherits(x, "polylist")) stop("x not a polylist object")
	if (!is.logical(subset)) stop("subset not a logical vector")
	if (length(x) != length(subset)) stop("x and subset different lengths")
	res <- subset.default(x, subset)
	attr(res, "region.id") <- subset.default(attr(x, "region.id"), subset)
	old.ids <- 1:length(x)
	new.ids <- match(old.ids, which(subset))
	after <- new.ids[subset.default(attr(x, "after"), subset)]
	pO <- order(after, na.last=FALSE)
	attr(res, "after") <- after
	attr(res, "plotOrder") <- pO
	class(res) <- "polylist"
	attr(res, "maplim") <- attr(x, "maplim")
	if (fit.bbox) attr(res, "maplim") <- maplimFromBbox(res)
	res
}

maplimFromBbox <- function(plist) {
	bboxes <- sapply(plist, function(x) attr(x, "bbox"))
	mapxlim <- range(c(bboxes[c(1,3),]))
	mapylim <- range(c(bboxes[c(2,4),]))
	list(x=mapxlim, y=mapylim)
}

