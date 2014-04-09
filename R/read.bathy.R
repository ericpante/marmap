read.bathy <- function(xyz, header=FALSE, sep=","){

### xyz: three-column table with longitude (x), latitude (y) and depth (z) (no default)
### header: whether this table has a row of column names (default = FALSE)
### sep: character separating columns, (default=",")

	bath <- read.table(xyz, header = header, sep = sep)
	bath <- bath[order(bath[, 2], bath[, 1], decreasing = FALSE), ]

    lat <- unique(bath[, 2]) ; bcol <- length(lat)
    lon <- unique(bath[, 1]) ; brow <- length(lon)

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
