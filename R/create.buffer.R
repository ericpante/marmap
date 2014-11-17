create.buffer <- function(mat, loc, radius, km=FALSE){
	
	if (!is(mat,"bathy")) stop("mat must be an object of class bathy")
	if (!is.data.frame(loc)) stop("loc must be a two-column data.frame (longitude and latitude)")
	if (!is.numeric(radius)) stop("radius must be numeric")
	if (length(radius) > 1) warning("only the first value of radius was used")
	
	xyz <- as.xyz(mat)
	
	if (km) radius <- 180 * radius/(pi*6372.798)
	
	map <- sp::SpatialPixelsDataFrame(points = xyz[,1:2], data = xyz, tolerance = 0.006)
    loc <- sp::SpatialPointsDataFrame(loc, data = loc)
	
	adehabitatMA::adeoptions(epsilon=0.001)
	temp <- adehabitatMA::buffer(loc, map, radius)
	adehabitatMA::adeoptions(epsilon=0.00000001)
		
	temp <- -as.bathy(as(temp,'SpatialGridDataFrame'))
	
	mat[temp==0] <- NA
	return(mat)
	
}