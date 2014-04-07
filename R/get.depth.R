get.depth=function(mat){
	
	locator(n=2)->coord
	as.numeric(rownames(mat)) -> lon
	as.numeric(colnames(mat)) -> lat
	
	if(length(coord$x) == 1) {	
		which(abs(lon-coord$x)==min(abs(lon-coord$x))) -> x
		which(abs(lat-coord$y)==min(abs(lat-coord$y))) -> y
		return(mat[x,y])
		}
		
	if(length(coord$x) == 2) {
		which(abs(lon-coord$x[1])==min(abs(lon-coord$x[1]))) -> x1
		which(abs(lat-coord$y[1])==min(abs(lat-coord$y[1]))) -> y1
		which(abs(lon-coord$x[2])==min(abs(lon-coord$x[2]))) -> x2
		which(abs(lat-coord$y[2])==min(abs(lat-coord$y[2]))) -> y2
		new.bathy = mat[x1:x2, y1:y2]
		return(summary.bathy(new.bathy))
		}
}