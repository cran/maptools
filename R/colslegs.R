# Copyright 2000-2001 (c) Nicholas Lewin-Koh 
# modifications 2001-2003 Roger Bivand


# Returns a vector of HSV values
color.ramp <- function(nclass, color='red', nvec=NULL, type='q') {
  eq.split <- function(ncl) {
    mult <- rep((1/ncl), (ncl-1))
    mult*seq(1, (ncl-1))
  }
  color.list <- list(cname=c("blue","green","yellow","red"),
                   hsvcol=c(.7,.375,.19,0))
  cind <- match(color, color.list$cname)
  if(nvec) {
     if(type=='q') {
       pr <- eq.split(nclass)
       brks <- c(min(nvec), quantile(nvec, pr, names=FALSE), max(nvec))
       classvec <- cut(nvec, brks, labels=FALSE, include.lowest=T)
       ramp <- hsv(rep(color.list$hsvcol[cind], nclass), c(pr, 1))  
       return(list(ramp=ramp, col.class=classvec))
     }
     else if(type=='e') {
       pr <- eq.split(nclass)
       brks <- c(min(nvec), pr*diff(range(nvec)), max(nvec))
       classvec <- cut(nvec, brks, labels=FALSE, include.lowest=T)
       ramp <- hsv(rep(color.list$hsvcol[cind], nclass), c(pr, 1))  
       return(list(ramp=ramp, col.class=classvec)) 
     }
  }
  return(NULL)
}

leglabs <- function(vec, under="under", over="over", between="-") {
	x <- vec
	res <- character(length(x)-1)
	res[1] <- paste(under, x[2])
	for (i in 2:(length(x)-2)) res[i] <- paste(x[i], between, x[i+1])
	res[length(x)-1] <- paste(over, x[length(x)-1])
	res
}

findInterval2 <- function (x, vec, rightmost.closed = FALSE, all.inside = TRUE) 
{
    nx <- length(x)
    if (any(is.na(vec) | is.nan(vec))) stop ("NAs found in vec")
    if (is.unsorted(vec)) 
        stop("`vec' must be sorted non-decreasingly")
    if (vec[1] == -Inf) vec[1] <- -(.Machine$double.xmax)
    if (vec[length(vec)] == Inf) 
	vec[length(vec)] <- .Machine$double.xmax
    .C("find_interv_vec", xt = as.double(vec), n = length(vec), 
        x = as.double(x), nx = nx, as.logical(rightmost.closed), 
        as.logical(all.inside), index = integer(nx), DUP = FALSE,
	PACKAGE = "base")$index
}


#ct <- cutree(hclust(dist(x, method="euclidean"), method="complete"), k=4)
#o <- order(matrix(unlist(tapply(x, ct, range)), ncol=2, byrow=TRUE)[,1])
#barplot(table(ct)[o])
#by(x, ct, range)

#k4 <- kmeans(x, 4)
#o <- order(matrix(unlist(tapply(x, k4$cluster, range)), ncol=2, byrow=TRUE)[,1])
#barplot(table(k4$cluster)[o])


#unlist(tapply(x, rev(order(k4$centers))[k4$cluster], range))
#matrix(unlist(tapply(x, rev(order(k4$centers))[k4$cluster], range)), 4, 2, byrow=TRUE)
#by(x, rev(order(k4$centers))[k4$cluster], range)

#The set of classification methods is large (Dent p. 145), but there are a few to remember:

#        * Equal Intervals ("Constant Interval"): each class has same difference in value
#        * Quantile (N-tile): each class has same number of units
#        * Natural Breaks: visual examination; manual determination 

#    Then a lot of ones that you might need to use once in a while
  # Arithmetic progression: constant increase (decrease) in "width" of class
  # Geometric progression: constant multiplier used to derive width of class
  # Jenk's Iterative ("optimal") minimize within class standard deviations (variance) [ESRI calls this "natural breaks"]
#    (see Dent 147-149 on use of F-ratio and weighting)
  # Arbitrary breaks: given externally (laws, regulations, natural process)
  # Standard deviations: statistical distribution
  # Nested Means works by successive halving at the mean (2,4,8,16, ...)

#(Chrisman)



