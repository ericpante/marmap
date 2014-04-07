as.SpatialGridDataFrame <- function(bathy) {
	
	# require(raster)
	# require(sp)
	
	out <- as(as.raster(bathy), "SpatialGridDataFrame")
	return(out)
}


