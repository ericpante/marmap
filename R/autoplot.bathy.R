autoplot.bathy <- function(object, geom="contour", mapping=NULL, coast=TRUE, ...) {
  # plot an object of class bathy

  # expand geom argument
  geom <- match.arg(geom, choices=c("contour", "raster", "tile"), several.ok=TRUE)
  if ( all(c("tile", "raster") %in% geom) ) {
    warning("'raster' and 'tile' are two alternative ways to plot an image map. Specifying both does not seem wise since one will cover the other. Keeping only 'raster'")
    geom <- geom[-which(geom == "tile")]
  }

  # get a data.frame
  xdf <- fortify.bathy(object)

  # set default mapping and add user-specified mappings
  if ( is.null(mapping) ) {
    mapping <- ggplot2::aes()
  }
  mapping <- c(ggplot2::aes_string(x='x', y='y'), mapping)
  class(mapping) <- "uneval"

  # prepare the base plot
  p <- ggplot2::ggplot(xdf, mapping=mapping) +
    # remove extra space around the plot
    ggplot2::scale_x_continuous(expand=c(0,0)) +
    ggplot2::scale_y_continuous(expand=c(0,0))

  # add layers
  if ("tile" %in% geom) {
    # "image" plot
    p <- p + ggplot2::geom_tile(ggplot2::aes_string(fill='z'), ...)

  }

  if ("raster" %in% geom) {
    # "image" plot using geom_raster
    # NB: faster than geom_tile, gives smaller PDFs but:
    #     . is not resolution independent
    #     . does not work with non-square aspect ratios (i.e. coord_map())
    p <- p + ggplot2::geom_raster(ggplot2::aes_string(fill='z'), ...)
  }

  if ("contour" %in% geom) {
    # bathy contours
    # (do not set colour if it is already specified in the mapping)
    if ("colour" %in% names(mapping) | "colour" %in% names(list(...))) {
      contours <- ggplot2::geom_contour(ggplot2::aes_string(z='z'), ...)
    } else {
      contours <- ggplot2::geom_contour(ggplot2::aes_string(z='z'), colour="grey30", ...)
    }
    p <- p + contours
  }

  if ( coast ) {
    # "coastline" contour = 0 isobath
    p <- p + ggplot2::geom_contour(ggplot2::aes_string(z='z'), colour="black", linetype="solid", size=0.5, breaks=0, alpha=1)
    # NB: set all the aesthetics so that they are not affected by what is specified in the function call
  }

  # set mapping projection
  if ( any(c("tile", "raster") %in% geom) ) {
	  coord <- ggplot2::coord_quickmap()
  } else {
    # true map projection
    coord <- ggplot2::coord_map()
  }
  p <- p + coord

  return(p)
}

