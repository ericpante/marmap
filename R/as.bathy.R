as.bathy <- function(x) {
    if (inherits(x, "bathy")) 
        stop("Object is already of class 'bathy'")
    
    if (inherits(x, "SpatialGridDataFrame")) 
        x <- raster::raster(x)
    
    if (inherits(x, "RasterLayer")) {
        extent <- x@extent
        lat <- seq(extent@xmin, extent@xmax, length.out = x@ncols)
        lon <- seq(extent@ymin, extent@ymax, length.out = x@nrows)
        bathy <- t(raster::as.matrix(raster::flip(x, direction = "y")))
        colnames(bathy) <- lon
        rownames(bathy) <- lat
    } else if (ncol(x) == 3) {
        bath <- x[order(x[, 2], x[, 1]), ]
        lat <- unique(bath[, 2])
        lon <- unique(bath[, 1])
        
        if (length(lat) * length(lon) == nrow(bath)) {
            bathy <- matrix(
                bath[, 3], 
                nrow = length(lon), 
                ncol = length(lat), 
                byrow = FALSE, 
                dimnames = list(lon, lat)
            )
        } else {
            colnames(bath) <- c("V1", "V2", "V3")
            bathy <- reshape2::acast(bath, V1 ~ V2, value.var = "V3")
        }
    } else {
        stop("as.bathy requires a 3-column table, or an object of class RasterLayer or SpatialDataFrame")
    }
    
    # Finalize bathy object
    bathy <- check.bathy(bathy)
    class(bathy) <- "bathy"
    return(bathy)
}
