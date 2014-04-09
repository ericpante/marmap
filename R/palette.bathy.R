palette.bathy <- function(mat, layers, land=FALSE, default.col="white"){

	deep <- sapply(layers, function(x) as.numeric(x[1]))
	shallow <- sapply(layers, function(x) as.numeric(x[2]))
	palcol <- lapply(layers, function(x) x[-c(1,2)])

	if(land == FALSE) {
		MAT.AMP <- abs(min(mat))  # matrix amplitude
		vec <- rep(default.col, MAT.AMP + 2) # +2 to account for depth zero and matrix start
	
		for(i in 1:length(deep)){
			index1 <- 1 + MAT.AMP - abs(deep[i])
			index2 <- 1 + MAT.AMP - abs(shallow[i])
			pcol <- colorRampPalette(palcol[[i]])
			vec[index1:index2] <- pcol(length(index1:index2))
		}
		} else {
		MAT.AMP <- abs(min(mat)) + abs(max(mat))  # diff(range(mat)) -> MAT.AMP
		vec <- rep(default.col,MAT.AMP)
		
		for(i in 1:length(deep)){
			if(deep[i] <= 0)    index1 <- 1 + abs(min(mat)) - abs(deep[i])    else index1 <- 1 + abs(min(mat)) + abs(deep[i])
			if(shallow[i] <= 0) index2 <- 1 + abs(min(mat)) - abs(shallow[i]) else index2 <- 1 + abs(min(mat)) + abs(shallow[i])
			pcol <- colorRampPalette(palcol[[i]])
			vec[index1:index2] <- pcol(length(index1:index2))
		}
	}	
	return(vec)
}