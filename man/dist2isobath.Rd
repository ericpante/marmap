\name{dist2isobath}
\alias{dist2isobath}
\title{
Computes the shortest great circle distance between any point and a given isobath
}
\description{
Computes the shortest (great circle) distance between a set of points and an isoline of depth or altitude. Points can be selected interactively by clicking on a map.
}
\usage{
dist2isobath(mat, x, y=NULL, isobath=0, locator=FALSE, \dots)
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
  \item{isobath}{
A single numerical value indicating the isobath to which the shortest distance is to be computed (default is set to 0, \emph{i.e.} the coastline).
}
  \item{locator}{
Logical. Whether to choose data points interactively with a map or not. If \code{TRUE}, a bathymetric map must have been plotted and both \code{x} and \code{y} are both ignored.
}
  \item{...}{
Further arguments to be passed to \code{locator} when the interactive mode is used (\code{locator=TRUE}).
}
}
\details{
\code{dist2isobath} allows the user to compute the shortest great circle distance between a set of points (selected interactively on a map or not) and a user-defined isobath. \code{dist2isobath} takes advantage of functions from packages \code{sp} (\code{Lines()} and \code{SpatialLines()}) and \code{geosphere} (\code{dist2Line}) to compute the coordinates of the nearest location along a given isobath for each point provided by the user.
}
\value{
A 5-column data.frame. The first column contains the distance in meters between each point and the nearest point located on the given \code{isobath}. Columns 2 and 3 indicate the longitude and latitude of starting points (\emph{i.e.} either coordinates provided as \code{x} and \code{y} or coordinates of points selected interactively on a map when \code{locator=TRUE}) and columns 4 and 5 contains coordinates (longitudes and latitudes) arrival points \emph{i.e.} the nearest points on the \code{isobath}.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{linesGC}}, \code{\link{lc.dist}}
}
\examples{
# Load NW Atlantic data and convert to class bathy
data(nw.atlantic)
atl <- as.bathy(nw.atlantic)

# Create vectors of latitude and longitude
lon <- c(-70, -65, -63, -55, -48)
lat <- c(33, 35, 40, 37, 33)

# Compute distances between each point and the -200m isobath
d <- dist2isobath(atl, lon, lat, isobath = -200)
d

# Visualize the great circle distances
blues <- c("lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1")
plot(atl, image=TRUE, lwd=0.1, land=TRUE, bpal = list(c(0,max(atl),"grey"), c(min(atl),0,blues)))
plot(atl, deep=-200, shallow=-200, step=0, lwd=0.6, add=TRUE)
points(lon,lat, pch=21, col="orange4", bg="orange2", cex=.8)
linesGC(d[2:3],d[4:5])
}
