\name{autoplot.bathy}
\alias{autoplot.bathy}
\title{
Ploting bathymetric data with ggplot
}
\description{
Plots contour or image map from bathymetric data matrix of class \code{bathy} with ggplot2
}
\usage{
\method{autoplot}{bathy}(x, geom="contour", mapping=NULL, coast=TRUE, \dots)
}
\arguments{
  \item{x}{
bathymetric data matrix of class \code{bathy}, imported using \code{\link{read.bathy}}
}
  \item{geom}{
geometry to use for the plot, i.e. type of plot; can be `contour', `tile' or `raster'. contour does a contour plot. tile and raster produce an image plot. tile allows true geographical projection through \code{\link{coord_map}}. raster only allows approximate projection but is faster to plot.
Names can be abbreviated. Geometries can be combined by specifying several in a vector.
}
  \item{mapping}{
additional mappings between the data obtained from calling \code{\link{fortify.bathy}} on x and the aesthetics for all geoms. When not NULL, this is a call to aes().
}
  \item{coast}{
boolean; wether to highlight the coast (isobath 0 m) as a black line
}
  \item{\dots}{
passed to the chosen geom(s)
}
}
\details{
\code{\link{fortify.bathy}} is called with argument \code{x} to produce a data.frame compatible with ggplot2. Then layers are added to the plot based om the argument \code{geom}. Finally, the whole plot is projected geographically using \code{\link{coord_map}} (for \code{geom="contour"}) or an approximation thereof.
}

\author{
Jean-Olivier Irisson
}
\seealso{
\code{\link{fortify.bathy}}, \code{\link{plot.bathy}}, \code{\link{read.bathy}}, \code{\link{summary.bathy}}
}
\examples{
# load NW Atlantic data and convert to class bathy
	data(nw.atlantic)
	atl <- as.bathy(nw.atlantic)

  # basic plot
\dontrun{
  library("ggplot2")
	autoplot.bathy(atl)

  # plot images
	autoplot.bathy(atl, geom=c("tile"))
	autoplot.bathy(atl, geom=c("raster")) # faster but not resolution independant

  # plot both!
	autoplot.bathy(atl, geom=c("raster", "contour"))

  # geom names can be abbreviated
	autoplot.bathy(atl, geom=c("r", "c"))

  # do not highlight the coastline
	autoplot.bathy(atl, coast=FALSE)
    
  # better colour scale
  	autoplot.bathy(atl, geom=c("r", "c")) +
    scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="darkgreen")

  # set aesthetics
	autoplot.bathy(atl, geom=c("r", "c"), colour="white", size=0.1)
  
  # topographical colour scale, see ?scale_fill_etopo
	autoplot.bathy(atl, geom=c("r", "c"), colour="white", size=0.1) + scale_fill_etopo()
  	
	# add sampling locations
	data(metallo)
  last_plot() + geom_point(aes(x=lon, y=lat), data=metallo, alpha=0.5)

  # an alternative contour map making use of additional mappings
  # see ?stat_contour in ggplot2 to understand the ..level.. argument
	autoplot.bathy(atl, geom="contour", mapping=aes(colour=..level..))
}
}