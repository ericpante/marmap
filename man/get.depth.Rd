\name{get.depth}
\alias{get.depth}
\title{
Get depth data by clicking on a map
}
\description{
Outputs depth information based on points selected by clicking on a map
}
\usage{
get.depth(mat, x, y=NULL, locator=TRUE, distance=FALSE, \dots)
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
  \item{distance}{
whether to compute the haversine distance (in km) from the first data point on (default is \code{FALSE}). Only available when at least two points are provided.
}
  \item{...}{
Further arguments to be passed to \code{locator} when the interactive mode is used (\code{locator=TRUE}).
}
}
\details{
\code{get.depth} allows the user to get depth data by clicking on a map created with \code{plot.bathy} or by providing coordinates of points of interest. This function uses the \code{locator} function (\code{graphics} package); after creating a map with \code{plot.bathy}, the user can click on the map once or several times (if \code{locator=TRUE}), press the Escape button, and get the depth of those locations in a three-coumn data.frame (longitude, latitude and depth). Alternatively, when \code{locator=FALSE}, the user can submit a list of longitudes and latitudes, a two-column matrix or data.frame of longitudes and latitudes (as input for \code{x}), or one vector of longitudes (\code{x}) and one vector of latitudes (\code{y}). The non-interactive mode is well suited to get depth information for each point provided by GPS tracking devices. While \code{\link{get.transect}} gets every single depth value available in the bathymetric matrix between two points along a user-defined transect, \code{get.depth} only provides depth data for the specific points provided as input by the user.
}
\value{
A data.frame with at least, longitude, latitude and depth with one line for each point of interest. If \code{distance=TRUE}, a fourth column containing the kilometric distance from the first point is added.
}
\author{
Benoit Simon-Bouhet and Eric Pante
}
\seealso{
\code{\link{path.profile}}, \code{\link{get.transect}}, \code{\link{read.bathy}}, \code{\link{summary.bathy}}, \code{\link{subsetBathy}}, \code{\link{nw.atlantic}}
}
\examples{
# load NW Atlantic data and convert to class bathy
data(nw.atlantic)
atl <- as.bathy(nw.atlantic)

# create vectors of latitude and longitude
lon <- c(-70, -65, -63, -55)
lat <- c(33, 35, 40, 37)

# a simple example
plot(atl, lwd=.5)
points(lon,lat,pch=19,col=2)

# Use get.depth to get the depth for each point
get.depth(atl, x=lon, y=lat, locator=FALSE)

# alternativeley once the map is plotted, use the iteractive mode:
\dontrun{
get.depth(atl, locator=TRUE, pch=19, col=3)
}
# click several times and press Escape

}
