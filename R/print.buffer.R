print.buffer <- function(x, ...){

	if (!is(x,"buffer")) stop("x must be an object of class buffer")

	message("Object of class 'buffer' containing:")
	message("1. a 'bathy' object filled with NAs except inside the buffer")
	message("2. a two column data.frame locating the center of the buffer")
	message("3. the radius of the buffer in the same unit as the 'bathy' object")
	message("4. the radius of the buffer in kilometers")
	message("")
	message("Summary of the 'bathy' object")
	print(summary(x[[1]]))
	message("")
	message("Coordinates of the center of the buffer")
	print(x[[2]])
	message("")
	message("Radius of the buffer")
	print(x[[3]])
	message("")
	message("Radius (in kilometers) of the buffer")
	print(x[[4]])
}