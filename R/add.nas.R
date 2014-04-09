add.nas <- function(vec, pos.missing) {

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