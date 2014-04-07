palette.bathy <- function(mat, deep, shallow, palcol, land=FALSE, default.col="white"){
	
	if(!is.matrix(mat))               stop("'mat' must be a matrix.")
	if(length(deep)!=length(shallow)) stop("'deep' and 'shallow' vectors should have the same length.")
	if(!is.list(palcol))              stop("'palcol' should be a list.")
	if(length(deep)!=length(palcol))  stop("'palcol' list should have the same number of elements as 'deep' and 'shallow'.")
	if(!is.logical(land))             stop("'land' should be TRUE or FALSE.")
	
	if(land == FALSE) {
		abs(min(mat)) -> MAT.AMP # matrix amplitude
		vec=rep(default.col,MAT.AMP+2) # +2 to account for depth zero and matrix start
		
		for(i in 1:length(deep)){
			index1=1+MAT.AMP-abs(deep[i])
			index2=1+MAT.AMP-abs(shallow[i])
			colorRampPalette(palcol[[i]]) -> pcol
			vec[index1:index2] <- pcol(length(index1:index2))
		} # end for loop
	} # end if statement
	
	if(land == TRUE) {
		abs(min(mat)) + abs(max(mat)) -> MAT.AMP  # diff(range(mat)) -> MAT.AMP
		vec=rep(default.col,MAT.AMP)
		
		for(i in 1:length(deep)){
			if(deep[i] <= 0)    index1=1+abs(min(mat))-abs(deep[i])    else index1=1+abs(min(mat))+abs(deep[i])
			if(shallow[i] <= 0) index2=1+abs(min(mat))-abs(shallow[i]) else index2=1+abs(min(mat))+abs(shallow[i])
			colorRampPalette(palcol[[i]]) -> pcol
			vec[index1:index2] <- pcol(length(index1:index2))
		} # end for loop
	} # end if statement

	return(vec)
} # end function