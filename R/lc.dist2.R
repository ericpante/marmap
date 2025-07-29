lc.dist2 <- function(trans, loc, res = c("dist", "path"), unit = "meter", speed = 8, round = 0) {

	# require(gdistance)
	# require(sp)

	min.depth <- trans@history[[1]]
	max.depth <- trans@history[[2]]
	bathymetry <- trans@history[[3]]
	trans@history <- list()

	loc.depth <- get.depth(bathymetry, x = loc, locator = FALSE)

	if (any(loc.depth[,3] > min.depth) | any(loc.depth[,3] < max.depth)) print(loc.depth)
	if (any(loc.depth[,3] > min.depth) | any(loc.depth[,3] < max.depth)) warning(paste("One or more points are located outside of the [",min.depth, ";", max.depth,"] depth range. You will get unrealistically huge distances unless you either increase the range of possible depths in trans.mat() or you move the problematic points in a spot where their depths fall within the [",min.depth, ";", max.depth,"] depth range.\nYou can use get.depth() to determine the depth of any point on a bathymetric map", sep=""))

	if (res=="dist") {
		x <- gdistance::costDistance(trans,as.matrix(loc))
		const1 <- 0.000621371
		const2 <- 0.0005399566364038877
		cost = switch(unit,
			meter = round(x, round),
			km =  round(x/1000, round),
			miles = round(x*const1, round) ,
			nmiles = round(x*const2, round),
			hours = round(x*const2 / speed, round),
			hours.min = paste(floor(x*const2 / speed), round(((x*const2 / speed)-floor(x*const2/speed))*60), sep=":")
			)
		print(cost)
	}

	if (res=="path") {
		nb.loc <- nrow(loc)
		path <- list()
		comb <- combn(1:nb.loc,2)
		pb <- txtProgressBar(min = 0, max = ncol(comb), style = 3)

		for (i in 1:ncol(comb)) {
			origin <- sp::SpatialPoints(loc[comb[1,i],])
			goal <- sp::SpatialPoints(loc[comb[2,i],])

			temp <- gdistance::shortestPath(trans,origin,goal,output="SpatialLines")
			path[[i]] <- temp@lines[[1]]@Lines[[1]]@coords

			setTxtProgressBar(pb, i)
		}

	close(pb)
	return(path)
	}

}
