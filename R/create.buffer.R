create.buffer <- function(mat, loc, radius){
	
	if (!is(mat,"bathy")) stop("mat must be an object of class bathy")
	if (!is.data.frame(loc)) stop("loc must be a two-column data.frame (longitude and latitude)")
	if (!is.numeric(radius)) stop("radius must be numeric")
	if (length(radius) > 1) warning("only the first value of radius was used")
	
	xyz <- as.xyz(mat)
	
	map <- sp::SpatialPixelsDataFrame(points = xyz[,1:2], data = xyz, tolerance = 0.006)
    loc <- sp::SpatialPointsDataFrame(loc, data = loc)
	
	assign(".adeoptions",list(epsilon=0.001),envir=.adehabitatMAEnv)
	temp <- adehabitatMA::buffer(loc, map, radius)
	assign(".adeoptions",list(epsilon=0.00000001),envir=.adehabitatMAEnv)
	
	temp <- -as.bathy(as(temp,'SpatialGridDataFrame'))
	
	mat[temp==0] <- NA
	return(mat)
	
}