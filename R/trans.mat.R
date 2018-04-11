trans.mat <- function(bathy,min.depth=0,max.depth=NULL) {
	
	# require(gdistance)
	
	ras <- bathy
	ras[bathy > min.depth] <- 0.00000001
	ras[bathy <= min.depth] <- 1
	if (!is.null(max.depth)) ras[bathy <= max.depth] <- 0.00000001
	if (is.null(max.depth)) max.depth <- min(bathy, na.rm=TRUE)
	
	lat <- as.numeric(colnames(bathy))
	lon <- as.numeric(rownames(bathy))
	
	r <- raster::raster(ncol=nrow(bathy),nrow=ncol(bathy),xmn=min(lon),xmx=max(lon),ymn=min(lat),ymx=max(lat))
	raster::values(r) <- as.vector(ras[,rev(1:ncol(ras))])
	
	trans <- gdistance::transition(r, transitionFunction = mean, directions = 16)
	transC <- gdistance::geoCorrection(trans,type="c",multpl=FALSE)

	transC@history <- list(min.depth, max.depth, bathy)
	return(transC)
}