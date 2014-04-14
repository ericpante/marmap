fortify.bathy <- function(x, ...) {
  # Convert an object of class bathy into a data.frame, for ggplot
  # x   object of class bathy

  x <- as.xyz(x)
  names(x) <- c("x", "y", "z")
  return(x)
}

autoplot.bathy <- function(x, geom="contour", mapping=aes(), coast=TRUE, ...) {
  # plot an object of class bathy

  # expand geom argument
  geom <- match.arg(geom, choices=c("contour", "raster", "tile"), several.ok=TRUE)
  if ( all(c("tile", "raster") %in% geom) ) {
    warning("'raster' and 'tile' are two alternative ways to plot an image map. Specifying both does not seem wise since one will cover the other. Keeping only 'raster'")
    geom <- geom[-which(geom == "tile")]
  }

  # get a data.frame
  xdf <- fortify.bathy(x)

  # set default mapping and add user-specified mappings
  mapping <- c(aes(x=x, y=y), mapping)
  class(mapping) <- "uneval"

  # prepare the base plot
  p <- ggplot(xdf, mapping=mapping) +
    # remove extra space around the plot
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))

  # add layers
  if ("tile" %in% geom) {
    # "image" plot
    p <- p + geom_tile(aes(fill=z), ...)

  }
  
  if ("raster" %in% geom) {
    # "image" plot using geom_raster
    # NB: faster than geom_tile, gives smaller PDFs but:
    #     . is not resolution independent
    #     . does not work with non-square aspect ratios (i.e. coord_map())
    p <- p + geom_raster(aes(fill=z), ...)
  }
  
  if ("contour" %in% geom) {
    # bathy contours
    # (do not set colour if it is already specified in the mapping)
    if ("colour" %in% names(mapping) | "colour" %in% names(list(...))) {
      contours <- geom_contour(aes(z=z), ...)
    } else {
      contours <- geom_contour(aes(z=z), colour="grey30", ...)
    }
    p <- p + contours
  }

  if ( coast ) {
    # "coastline" contour = 0 isobath
    p <- p + geom_contour(aes(z=z), colour="black", linetype="solid", size=0.5, breaks=0, alpha=1)
    # NB: set all the aesthetics so that they are not affected by what is specified in the function call
  }

  # set mapping projection
  if ( any(c("tile", "raster") %in% geom) ) {
    # with tile/raster, compute approximate "projection" (compute correct aspect ratio between lat and lon at ce center of the plot) because using coord_map() is too slow (and does not work with geom_raster)
    # TODO replace with coord_quickmap included in the next released version of ggplot2
    x.range <- range(xdf$x, na.rm=TRUE)
    y.range <- range(xdf$y, na.rm=TRUE)
    x.center <- sum(x.range) / 2
    y.center <- sum(y.range) / 2
    x.dist <- ggplot2:::dist_central_angle(x.center + c(-0.5, 0.5), rep(y.center, 2))
    y.dist <- ggplot2:::dist_central_angle(rep(x.center, 2), y.center+c(-0.5, 0.5))
    ratio <- y.dist / x.dist
    coord <- coord_fixed(ratio=ratio)
  } else {
    # true map projection
    coord <- coord_map()
  }
  p <- p + coord

  return(p)
}

