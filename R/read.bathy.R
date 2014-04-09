read.bathy <- function(xyz, header=FALSE, sep=","){

### xyz: three-column table with longitude (x), latitude (y) and depth (z) (no default)
### header: whether this table has a row of column names (default = FALSE)
### sep: character separating columns, (default=",")

	bath <- read.table(xyz, header = header, sep = sep)
	bath <- bath[order(bath[, 2], bath[, 1], decreasing = FALSE), ]

    lat <- unique(bath[, 2]) ; bcol <- length(lat)
    lon <- unique(bath[, 1]) ; brow <- length(lon)


	# Internal function adding NA's in vector vec at position pos.missing
	# vec: vector of data
	# pos.missing: vector. Position of missing value in the future bathy object
	# returns un updated vector containing both the original dat and NAs in the right place.
	# The length of the resulting vector equals the sum of lengths of vec and pos.missing
	
	add.nas <- function(vec,pos.missing) {

		vec.new <- NULL
		n.mis <- length(pos.missing)

		for (i in 1:n.mis) {
			modify.vec <- TRUE
			range <- 1:(pos.missing[i]-1)
	
			if (identical(range,1:0)) {
				range <- 0
				modify.vec <- FALSE
			}
	
			vec.new <- c(vec.new,vec[range],NA)
			if (modify.vec) vec <- vec[-range]
			pos.missing <- pos.missing - pos.missing[i]
		}

		vec.new <- c(vec.new,vec)
		return(vec.new)
	}


	if ((bcol*brow) == nrow(bath)) {
		mat <- matrix(bath[, 3], nrow = brow, ncol = bcol, byrow = FALSE, dimnames = list(lon, lat))
		} else {
			id <- apply(bath[,-3],1,function(x) c(which(lon==x[1]) , which(lat==x[2])) )
			pos.missing <- which(table(id[1,],id[2,])==0)
			mat <- matrix(add.nas(bath[,3],pos.missing), nrow = brow, ncol = bcol, byrow = FALSE, dimnames = list(lon, lat))
		}
		
    ordered.mat <- check.bathy(mat)
    class(ordered.mat) <- "bathy"
    return(ordered.mat)
	
}
