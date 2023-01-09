getNOAA.bathy <-
  function(lon1, lon2, lat1, lat2, resolution = 4, keep = FALSE, antimeridian = FALSE, path = NULL){

    if (lon1 == lon2) stop("The longitudinal range defined by lon1 and lon2 is incorrect")
    if (lat1 == lat2) stop("The latitudinal range defined by lat1 and lat2 is incorrect")
    if (lat1 > 90 | lat1 < -90 | lat2 > 90 | lat2 < -90) stop("Latitudes should have values between -90 and +90")
    if (lon1 < -180 | lon1 > 180 | lon2 < -180 | lon2 > 180) stop("Longitudes should have values between -180 and +180")
    if (resolution < 0) stop("The resolution must be greater than 0")

    # if (resolution < 1) resolution <- round(resolution/0.25) * 0.25

    if (resolution < 0.5) {
      resolution <- 0.25
    } else {
      if (resolution < 1) {
        resolution <- 0.5
      }
    }
    if (resolution == 0.25) database <- "27ETOPO_2022_v1_15s_bed_elev"
    if (resolution == 0.50) database <- "27ETOPO_2022_v1_30s_bed"
    if (resolution  > 0.50) database <- "27ETOPO_2022_v1_60s_bed"

    if (is.null(path)) path <- "."
    x1 = x2 = y1 = y2 = NULL

    if (lon1 < lon2) {
      x1 <- lon1
      x2 <- lon2
    } else {
      x2 <- lon1
      x1 <- lon2
    }

    if (lat1 < lat2) {
      y1 <- lat1
      y2 <- lat2
    } else {
      y2 <- lat1
      y1 <- lat2
    }

    if (antimeridian) {
      if (x1 == -180 & x2 == 180) {
        x1 <- 0
        x2 <- 0
      }

      l1 <- x2
      l2 <- 180
      l3 <- -180
      l4 <- x1

      ncell.lon.left <- (l2 - l1) * 60 / resolution
      ncell.lon.right <- (l4 - l3) * 60 / resolution
      ncell.lat <- (y2 - y1) * 60 / resolution

      if ((ncell.lon.left + ncell.lon.right) < 2 & ncell.lat < 2) stop("It's impossible to fetch an area with less than one cell. Either increase the longitudinal and longitudinal ranges or the resolution (i.e. use a smaller res value)")
      if ((ncell.lon.left + ncell.lon.right) < 2) stop("It's impossible to fetch an area with less than one cell. Either increase the longitudinal range or the resolution (i.e. use a smaller resolution value)")
      if (ncell.lat < 2) stop("It's impossible to fetch an area with less than one cell. Either increase the latitudinal range or the resolution (i.e. use a smaller resolution value)")

    } else {

      ncell.lon <- (x2 - x1) * 60 / resolution
      ncell.lat <- (y2 - y1) * 60 / resolution

      if (ncell.lon < 2 & ncell.lat < 2) stop("It's impossible to fetch an area with less than one cell. Either increase the longitudinal and longitudinal ranges or the resolution (i.e. use a smaller res value)")
      if (ncell.lon < 2) stop("It's impossible to fetch an area with less than one cell. Either increase the longitudinal range or the resolution (i.e. use a smaller resolution value)")
      if (ncell.lat < 2) stop("It's impossible to fetch an area with less than one cell. Either increase the latitudinal range or the resolution (i.e. use a smaller resolution value)")
    }

    fetch <- function(x1, y1, x2, y2, ncell.lon, ncell.lat) {
      ncell.lon <- floor(ncell.lon)
      ncell.lat <- floor(ncell.lat)
      x1 <- round(x1, 1)
      x2 <- round(x2, 1)
      y1 <- round(y1, 1)
      y2 <- round(y2, 1)
      WEB.REQUEST <- paste0("https://gis.ngdc.noaa.gov/arcgis/rest/services/DEM_mosaics/DEM_all/ImageServer/exportImage?bbox=", x1, ",", y1, ",", x2, ",", y2, "&bboxSR=4326&size=", ncell.lon, ",", ncell.lat,"&imageSR=4326&format=tiff&pixelType=F32&interpolation=+RSP_NearestNeighbor&compression=LZ77&renderingRule={%22rasterFunction%22:%22none%22}&mosaicRule={%22where%22:%22Name=%", database, "%27%22}&f=image")
      download.file(url = WEB.REQUEST, destfile = "tmp.tif", mode = "wb")
      dat <- suppressWarnings(try(raster::raster("tmp.tif"), silent = TRUE))
      dat <- as.xyz(as.bathy(dat))
      file.remove("tmp.tif")
      return(dat)
    }

    # Naming the file
    if (antimeridian) {
      FILE <- paste0("marmap_coord_", x1, ";", y1, ";", x2, ";", y2, "_res_", resolution, "_anti", ".csv")
    } else {
      FILE <- paste0("marmap_coord_", x1, ";", y1, ";", x2, ";", y2, "_res_", resolution, ".csv")
    }

    # If file exists in PATH, load it,
    if(FILE %in% list.files(path = path) ) {
      message("File already exists ; loading \'", FILE, "\'", sep = "")
      exisiting.bathy <- read.bathy(paste0(path, "/", FILE), header = TRUE)
      return(exisiting.bathy)
    } else { # otherwise, fetch it on NOAA server

      if (antimeridian) {

        message("Querying NOAA database ...")
        message("This may take seconds to minutes, depending on grid size\n")
        left  <- fetch(l1, y1, l2, y2, ncell.lon.left, ncell.lat)
        right <- fetch(l3, y1, l4, y2, ncell.lon.right, ncell.lat)

        if (is(left, "try-error") | is(right, "try-error")) {
          stop("The NOAA server cannot be reached\n")
        } else {
          message("Building bathy matrix ...\n")
          left <- as.bathy(left)
          left <- left[-nrow(left),]
          right <- as.bathy(right)
          rownames(right) <- as.numeric(rownames(right)) + 360
          bath2 <- rbind(left, right)
          class(bath2) <- "bathy"
          bath <- as.xyz(bath2)
        }

      } else {

        message("Querying NOAA database ...")
        message("This may take seconds to minutes, depending on grid size\n")
        bath <- fetch(x1, y1, x2, y2, ncell.lon, ncell.lat)
        if (is(bath,"try-error")) {
          stop("The NOAA server cannot be reached\n")
        } else {
          message("Building bathy matrix ...")
          bath2 <- as.bathy(bath)
        }
      }

      if (keep) {
        write.table(bath, file = paste0(path,"/",FILE), sep = ",", quote = FALSE, row.names = FALSE)
      }

      return(bath2)
    }
  }
