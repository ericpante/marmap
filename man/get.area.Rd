\name{get.area}
\alias{get.area}
\title{
Get projected surface area
}
\description{
Get projected surface area for specific depth layers
}
\usage{
get.area(mat, level.inf, level.sup=0, xlim=NULL, ylim=NULL)
}
\arguments{
  \item{mat}{
bathymetric data matrix of class \code{bathy}, imported using \code{read.bathy} (no default)
}
  \item{level.inf}{
lower depth limit for calculation of projected surface area (no default)
}
  \item{level.sup}{
upper depth limit for calculation of projected surface area (default is zero)
}
  \item{xlim}{
longitudinal range of the area of interest (default is \code{NULL})
}
  \item{ylim}{
latitudinal range of the area of interest (default is \code{NULL})
}
}
\details{
\code{get.area} calculates the projected surface area of specific depth layers (e.g. upper bathyal, lower bathyal), the projected plane being the ocean surface. The resolution of \code{get.area} depends on the resolution of the input bathymetric data. \code{xlim} and \code{ylim} can be used to restrict the area of interest. Area calculation is based on \code{areaPolygon} of package \code{geosphere} (using an average Earth radius of 6,371 km). 
}
\value{
A list of four objects: the projeced surface area in squared kilometers, a matrix with the cells used for calculating the projected surface area, the longitude and latitude of the matrix used for the calculations. 
}
\author{
Benoit Simon-Bouhet and Eric Pante
}
\seealso{
\code{\link{plotArea}}, \code{\link{plot.bathy}}, \code{\link{contour}}, \code{\link{areaPolygon}}
}
\examples{

## get area for the entire hawaii dataset:
	data(hawaii)
	plot(hawaii, lwd=0.2)
	
	mesopelagic <- get.area(hawaii, level.inf=-1000, level.sup=-200)
	bathyal <- get.area(hawaii, level.inf=-4000, level.sup=-1000)
	abyssal <- get.area(hawaii, level.inf=min(hawaii), level.sup=-4000)
	
	col.meso <- rgb(0.3, 0, 0.7, 0.3) 
	col.bath <- rgb(0.7, 0, 0, 0.3) 
	col.abys <- rgb(0.7, 0.7, 0.3, 0.3) 

	plotArea(mesopelagic, col = col.meso)
	plotArea(bathyal, col = col.bath)
	plotArea(abyssal, col = col.abys)
	
	me <- round(mesopelagic$Square.Km, 0)
	ba <- round(bathyal$Square.Km, 0)
	ab <- round(abyssal$Square.Km, 0)

	legend(x="bottomleft",
		legend=c(paste("mesopelagic:",me,"km2"),
		         paste("bathyal:",ba,"km2"), 
		         paste("abyssal:",ab,"km2")), 
		col="black", pch=21, 
		pt.bg=c(col.meso,col.bath,col.abys))

# Use of xlim and ylim
	data(hawaii)
	plot(hawaii, lwd=0.2)
	
	mesopelagic <- get.area(hawaii, xlim=c(-161.4,-159), ylim=c(21,23),
	                        level.inf=-1000, level.sup=-200)
	bathyal <- get.area(hawaii, xlim=c(-161.4,-159), ylim=c(21,23),
	                        level.inf=-4000, level.sup=-1000)
	abyssal <- get.area(hawaii, xlim=c(-161.4,-159), ylim=c(21,23),
	                        level.inf=min(hawaii), level.sup=-4000)
	
	col.meso <- rgb(0.3, 0, 0.7, 0.3) 
	col.bath <- rgb(0.7, 0, 0, 0.3) 
	col.abys <- rgb(0.7, 0.7, 0.3, 0.3) 
	
	plotArea(mesopelagic, col = col.meso)
	plotArea(bathyal, col = col.bath)
	plotArea(abyssal, col = col.abys)
	
	me <- round(mesopelagic$Square.Km, 0)
	ba <- round(bathyal$Square.Km, 0)
	ab <- round(abyssal$Square.Km, 0)
	
	legend(x="bottomleft",
		legend=c(paste("mesopelagic:",me,"km2"),
		         paste("bathyal:",ba,"km2"), 
		         paste("abyssal:",ab,"km2")), 
		col="black", pch=21, 
		pt.bg=c(col.meso,col.bath,col.abys))
}
