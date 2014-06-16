\name{subsetBathy}
\alias{subsetBathy}
\title{
Creates bathy objects from larger bathy objects
}
\description{
Generates rectangular or non rectangular \code{bathy} objects by extracting bathymetric data from larger \code{bathy} objects.
}
\usage{
subsetBathy(mat, x, y=NULL, locator=TRUE, \dots)
}
\arguments{
  \item{mat}{
Bathymetric data matrix of class \code{bathy}, as imported with \code{read.bathy}.
}
  \item{x}{
Either a list of two elements (numeric vectors of longitude and latitude), a 2-column matrix or data.frame of longitudes and latitudes, or a numeric vector of longitudes.
}
  \item{y}{
Either \code{NULL} (default) or a numerical vector of latitudes. Ignored if \code{x} is not a numeric vector.
}
  \item{locator}{
Logical. Whether to choose data points interactively with a map or not. If \code{TRUE} (default), a bathymetric map must have been plotted and both \code{x} and \code{y} are both ignored.
}
  \item{...}{
Further arguments to be passed to \code{locator} when the interactive mode is used (\code{locator=TRUE}).
}
}
\details{
\code{subsetBathy} allows the user to generate new \code{bathy} objects by extracting data from larger \code{bathy} objects. The extraction of bathymetric data can be done interactively by clicking on a bathymetric map, or by providing longitudes and latitudes for the boundaries for the new \code{bathy} object. If two data points are provided, a rectangular area is selected. If more than two points are provided, a polygon is defined by linking the points and the bathymetic data is extracted within the polygon only. \code{subsetBathy} relies on the \code{point.in.polygon} function from package \code{sp} to identify which points of the initial bathy matrix lie witin the boundaries of the user-defined polygon.
}
\value{
A matrix of class \code{bathy}.
}
\references{
Pebesma, EJ, RS Bivand, (2005). Classes and methods for spatial data in R. R News 5 (2), \url{http://cran.r-project.org/doc/Rnews/.}

Bivand RS, Pebesma EJ, Gomez-Rubio V (2013). Applied spatial data analysis with R, Second edition. Springer, NY. \url{http://www.asdar-book.org/}
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{plot.bathy}}, \code{\link{get.depth}}, \code{\link{summary.bathy}}, \code{\link{aleutians}}
}
\examples{
# load aleutians dataset
data(aleutians)

# create vectors of latitude and longitude to define the boundary of a polygon
lon <- c(188.56, 189.71, 191, 193.18, 196.18, 196.32, 196.32, 194.34, 188.83)
lat <- c(54.33, 55.88, 56.06, 55.85, 55.23, 54.19, 52.01, 50.52, 51.71)


# plot the initial bathy and overlay the polygon
plot(aleutians, image=TRUE, land=TRUE, lwd=.2)
polygon(lon,lat)

# Use of subsetBathy to extract the new bathy object
zoomed <- subsetBathy(aleutians, x=lon, y=lat, locator=FALSE)

# plot the new bathy object
dev.new() ; plot(zoomed, land=TRUE, image=TRUE, lwd=.2)

# alternativeley once the map is plotted, use the interactive mode:
\dontrun{
plot(aleutians, image=TRUE, land=TRUE, lwd=.2)
zoomed2 <- subsetBathy(aleutians, pch=19, col=3)
dev.new() ; plot(zoomed2, land=TRUE, image=TRUE, lwd=.2)
}
# click several times and press Escape

}
