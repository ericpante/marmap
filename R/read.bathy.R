read.bathy <-
function(xyz, header=FALSE, sep=","){

### xyz: three-column table with longitude (x), latitude (y) and depth (z) (no default)
### header: whether this table has a row of column names (default = FALSE)
### sep: character separating columns, (default=",")

	read.table(xyz, header=header, sep=sep) -> bath	

	bath = bath[order(bath$V2,bath$V1, decreasing=FALSE),]

	unique(bath[,2]) -> lat
	unique(bath[,1]) -> lon
	
	length(lon) -> brow
	length(lat) -> bcol

	matrix(bath[,3], nrow=brow, ncol=bcol, byrow=FALSE, dimnames= list(lon,lat)) -> mat
	# if(header==FALSE) names(mat) <- c("lon","lat","depth")
	
	check.bathy(mat) -> ordered.mat
	class(ordered.mat) <- "bathy"
	return(ordered.mat)
	
}
