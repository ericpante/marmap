getNOAA.bathy <- function(lon1, lon2, lat1, lat2, resolution = 4, keep = FALSE, 
                          antimeridian = FALSE, path = NULL) {
    # Validate inputs
    if (lon1 == lon2 || lat1 == lat2) 
        stop("Longitudinal or latitudinal range is invalid")
    if (any(lat1 > 90, lat1 < -90, lat2 > 90, lat2 < -90)) 
        stop("Latitudes must be between -90 and 90")
    if (any(lon1 < -180, lon1 > 180, lon2 < -180, lon2 > 180)) 
        stop("Longitudes must be between -180 and 180")
    if (resolution <= 0) 
        stop("Resolution must be greater than 0")
    
    # Adjust resolution and select database
    resolution <- ifelse(resolution < 0.5, 0.25, ifelse(resolution < 1, 0.5, 1))
    database <- switch(as.character(resolution),
                       "0.25" = "27ETOPO_2022_v1_15s_bed_elev",
                       "0.5" = "27ETOPO_2022_v1_30s_bed",
                       "1" = "27ETOPO_2022_v1_60s_bed")
    
    # Normalize coordinate ranges
    x1 <- min(lon1, lon2); x2 <- max(lon1, lon2)
    y1 <- min(lat1, lat2); y2 <- max(lat1, lat2)
    path <- path %||% "."
    
    # Compute number of cells
    compute_cells <- function(lon1, lon2, lat1, lat2) {
        ncell.lon <- (lon2 - lon1) * 60 / resolution
        ncell.lat <- (lat2 - lat1) * 60 / resolution
        if (ncell.lon < 2 || ncell.lat < 2)
            stop("Grid size is too small. Increase the range or resolution.")
        return(list(ncell.lon = floor(ncell.lon), ncell.lat = floor(ncell.lat)))
    }
    
    # Prepare file name
    FILE <- file.path(path, paste0("marmap_coord_", x1, "_", y1, "_", x2, "_", y2, 
                                   "_res_", resolution, if (antimeridian) "_anti", ".csv"))
    
    # Load existing data if available
    if (file.exists(FILE)) {
        message("File already exists; loading '", FILE, "'")
        return(read.bathy(FILE, header = TRUE))
    }
    
    # Fetch data
    fetch <- function(x1, y1, x2, y2, ncell.lon, ncell.lat) {
        WEB.REQUEST <- paste0(
            "https://gis.ngdc.noaa.gov/arcgis/rest/services/DEM_mosaics/DEM_all/ImageServer/exportImage?bbox=", 
            x1, ",", y1, ",", x2, ",", y2, "&bboxSR=4326&size=", 
            ncell.lon, ",", ncell.lat, "&imageSR=4326&format=tiff&pixelType=F32&interpolation=+RSP_NearestNeighbor&compression=LZ77&renderingRule={%22rasterFunction%22:%22none%22}&mosaicRule={%22where%22:%22Name=%", 
            database, "%27%22}&f=image"
        )
        tmp_file <- tempfile(fileext = ".tif")
        download.file(WEB.REQUEST, tmp_file, mode = "wb")
        on.exit(file.remove(tmp_file), add = TRUE)
        
        dat <- tryCatch({
            raster::raster(tmp_file)
        }, error = function(e) {
            stop("Failed to fetch data from NOAA server")
        })
        return(as.xyz(as.bathy(dat)))
    }
    
    # Fetch antimeridian or regular data
    message("Querying NOAA database ...")
    cell_info <- compute_cells(x1, x2, y1, y2)
    if (antimeridian) {
        left <- fetch(x2, y1, 180, y2, cell_info$ncell.lon / 2, cell_info$ncell.lat)
        right <- fetch(-180, y1, x1, y2, cell_info$ncell.lon / 2, cell_info$ncell.lat)
        bath <- rbind(as.bathy(left)[-nrow(left), ], as.bathy(right))
        rownames(bath) <- as.numeric(rownames(bath)) + 360
    } else {
        bath <- fetch(x1, y1, x2, y2, cell_info$ncell.lon, cell_info$ncell.lat)
    }
    
    # Save and return data
    if (keep) {
        write.table(bath, FILE, sep = ",", quote = FALSE, row.names = FALSE)
    }
    return(as.bathy(bath))
}
