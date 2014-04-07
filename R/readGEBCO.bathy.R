readGEBCO.bathy <- function(file, db="GEBCO_1min", resolution=1){

	# require(ncdf)

	# check resolution value ## is.wholenumber function from is.integer {base}
	"Argument 'resolution' must be a positive integer\n" -> message
	is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol	
	# if resolution is <1 OR not a whole number:
	if(resolution<1 | !is.wholenumber(resolution)) stop(message) else {

		# get data from netCDF file
		nc <- ncdf::open.ncdf(file)
		ncells <- length(ncdf::get.var.ncdf(nc, "xysize"))
		z <- ncdf::get.var.ncdf(nc,"z")
		xrg <- ncdf::get.var.ncdf(nc, "x_range")
		yrg <- ncdf::get.var.ncdf(nc, "y_range")
	
		# dimensions of the matrix, depending on type of database
		if(db == "GEBCO_1min") db.scale <- 1/60
		if(db == "GEBCO_08")   db.scale <- 1/120
		xdim <- seq(xrg[1],xrg[2], by=db.scale)
		ydim <- seq(yrg[1],yrg[2], by=db.scale)
		
		# for some reason sometimes the z vector is shorter than the product of the 
		# length of the latitude and longitude vectors
		# so we check that z and xy are compatible, otherwise we crop the matrix dimentions. 
		if(length(xdim) * length(ydim) != ncells) {
			warning("The number of cells in the .nc file (", ncells , ") do not correspond exactly to the range of latitude x longitude values (", length(xdim) * length(ydim), ")...\n  Cropping the matrix dimentions (see ?readGEBCO.bathy for details)...")
			xdim<-xdim[-length(xdim)];ydim<-ydim[-length(ydim)] # removing last x and y values
		}

		# build matrix 
		mat <- matrix(data=z, nrow=length(xdim),ncol=length(ydim), byrow=F)
		mat <- mat[,ncol(mat):1] # (merci benoit!)
		rownames(mat) <- xdim
		colnames(mat) <- ydim
		mat <- check.bathy(mat[,ncol(mat):1])
	
		# adapt the resolution
		xindex <- seq(1,length(xdim),by=resolution)
		yindex <- seq(1,length(ydim),by=resolution)
		mat[xindex,yindex] -> final
		
		class(final) <- "bathy"
		return(final)
	}
}