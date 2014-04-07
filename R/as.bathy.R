as.bathy <- function(x){

	# require(raster)
	# require(sp)

	if (is(x,"bathy")) stop("Object is already of class 'bathy'")

	if (is(x,"SpatialGridDataFrame")) x <- raster::raster(x)

	# if x is a RasterLayer do this
	if (is(x,"RasterLayer")) {
		lat.min <- x@extent@xmin
		lat.max <- x@extent@xmax
		lon.min <- x@extent@ymin
		lon.max <- x@extent@ymax
		
		nlat <- x@ncols
		nlon <- x@nrows
		
		lon <- seq(lon.min, lon.max, length.out = nlon)
		lat <- seq(lat.min, lat.max, length.out = nlat)
		
		bathy <- t(raster::as.matrix(raster::flip(x,direction="y")))
		colnames(bathy) <- lon
		rownames(bathy) <- lat
	}
	
	# if not, it has to be a 3-column table (xyz format)
	if (ncol(x)==3 & !exists("bathy")) {
		bath <- na.omit(x)
		bath <- bath[order(bath[,2],bath[,1], decreasing=FALSE),]

		unique(bath[,2]) -> lat
		unique(bath[,1]) -> lon
	
		length(lon) -> brow
		length(lat) -> bcol

		bathy <- matrix(bath[,3], nrow=brow, ncol=bcol, byrow=FALSE, dimnames= list(lon,lat))
	}

	if (!exists("bathy")) stop("as.bathy requires a 3-column table, or an object of class RasterLayer or SpatialDataFrame")

	check.bathy(bathy) -> ordered.mat
	class(ordered.mat) <- "bathy"
	return(ordered.mat)

}
