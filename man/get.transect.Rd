\name{get.transect}
\alias{get.transect}
\title{
Compute approximate cross section along a depth transect
}
\description{
Compute the depth along a linear transect which bounds are specified by the user.
}
\usage{
get.transect(mat, x1, y1, x2, y2, locator=FALSE, distance=FALSE, \dots)
}
\arguments{
  \item{mat}{
bathymetric data matrix of class \code{bathy}, imported using \code{read.bathy} (no default)
}
  \item{x1}{
start longitude of the transect (no default)
}
  \item{x2}{
stop longitude of the transect (no default)
}
  \item{y1}{
start latitude of the transect (no default)
}
  \item{y2}{
stop latitude of the transect (no default)
}
  \item{locator}{
whether to use locator to choose transect bounds interactively with a map (default is \code{FALSE})
}
  \item{distance}{
whether to compute the haversine distance (in km) from the start of the transect, along the transect (default is \code{FALSE})
}
  \item{\dots}{
other arguments to be passed to \code{locator()} to specify the characteristics of the points and lines to draw on the bathymetric map when \code{locator=TRUE}.
}
}
\details{
\code{get.transect} allows the user to compute an approximate linear depth cross section either by inputing start and stop coordinates, or by clicking on a map created with \code{plot.bathy}. In its interactive mode, this function uses the \code{locator} function (\code{graphics} package); after creating a map with \code{plot.bathy}, the user can click twice to delimit the bound of the transect of interest (for example, lower left and upper right corners of a rectangular area of interest), press Escape, and get a table with the transect information. 
}
\value{
A table with, at least, longitude, latitude and depth along the transect, and if specified (distance=\code{TRUE}), the distance in kilometers from the start of the transect. The number of elements in the resulting table depends on the resolution of the \code{bathy} object.
}
\note{
The distance option of \code{get.transect} is calculated based on the haversine formula for getting the great circle distance (takes into account the curvature of the Earth). \code{get.transect} uses an internal function called \code{diag.bathy} that extracts the approximate diagonal of a matrix, when that matrix has uneven dimentions (different numbers of columns and rows).
}
\author{
Eric Pante and Benoit Simon-Bouhet
}
\section{Warning }{
Clicking once or more than twice on the map will return a warning message: "Please choose only two points from the map". Manually entering coordinates that are outside the geographical range of the input \code{bathy} matrix will return a warning message. 
}
\seealso{
\code{\link{read.bathy}}, \code{\link{nw.atlantic}}, \code{\link{nw.atlantic.coast}}, \code{\link{get.depth}}, \code{\link{get.sample}}
}
\examples{
	
# load datasets
	data(nw.atlantic); as.bathy(nw.atlantic) -> atl
	data(nw.atlantic.coast)

# Example 1. get.transect(), without use of locator()
	get.transect(atl, -65, 43,-59,40) -> test ; plot(test[,3]~test[,2],type="l")
	get.transect(atl, -65, 43,-59,40, distance=TRUE) -> test ; plot(test[,4]~test[,3],type="l")

# Example 2. get.transect(), without use of locator(); pretty plot
	par(mfrow=c(2,1),mai=c(1.2, 1, 0.1, 0.1))
	plot(atl, deep=-6000, shallow=-10, step=1000, lwd=0.5, col="grey50",drawlabels=TRUE)
	lines(nw.atlantic.coast)

	get.transect(atl, -75, 44,-46,32, loc=FALSE, dis=TRUE) -> test
	points(test$lon,test$lat,type="l",col="blue",lwd=2,lty=2)
	plotProfile(test)

# Example 3. get.transect(), with use of locator(); pretty plot
\dontrun{
	par(mfrow=c(2,1),mai=c(1.2, 1, 0.1, 0.1))
	plot(atl, deep=-6000, shallow=-10, step=1000, lwd=0.5, col="grey50",drawlabels=TRUE)
	lines(nw.atlantic.coast)
	
	get.transect(atl, loc=TRUE, dis=TRUE, col=2, lty=2) -> test
	plotProfile(test)
	}
}
