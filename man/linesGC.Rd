\name{linesGC}
\alias{linesGC}
\title{
Add Great Circle lines on a map
}
\description{
\code{linesGC} draws Great Circle lines between a set of start and end points on an existing map.
}
\usage{
linesGC(start.points, end.points, n = 10, antimeridian = FALSE, \dots)
}
\arguments{
  \item{start.points}{
Two-column data.frame or matrix of longitudes and latitudes for start points.
}
  \item{end.points}{
Two-column data.frame or matrix of longitudes and latitudes for end points. The dimensions of \code{start.points} and \code{end.points} must be compatible (\emph{i.e.} they must have the same number of rows).
}
  \item{n}{
Numeric. The number of intermediate points to add along the great circle line between the start end end points.
}
  \item{antimeridian}{
Logical indicating if the map on which the great circle lines will be plotted covers the antimeridian region. The antimeridian (or antemeridian) is the 180th meridian and is located in the middle of the Pacific Ocean, east of New Zealand and Fidji, west of Hawaii and Tonga.
}
  \item{...}{
Further arguments to be passed to \code{lines} to control the aspect of the lines to draw.
}
}
\details{
\code{linesGCD} takes advantage of the \code{gcIntermediate} function from package \code{geosphere} to plot lines following a great circle. When working with \code{marmap} maps encompassing the antimeridian, longitudes are numbered from 0 to 360 (as opposed to the classical numbering from -180 to +180). It is thus critical to set \code{antimeridian=TRUE} to avoid plotting incoherent great circle lines.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{dist2isobath}}, \code{\link{lc.dist}}
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

# Create a nice palette of bleus for the bathymetry
blues <- c("lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1")

# Visualize the great circle distances
plot(atl, image=TRUE, lwd=0.1, land=TRUE, 
	 bpal = list(c(0,max(atl),"grey"), c(min(atl),0,blues)))
points(lon,lat, pch=21, col="orange4", bg="orange2", cex=.8)
linesGC(d[2:3],d[4:5])

# Load aleutians data and plot the map
data(aleutians)
plot(aleutians, image=TRUE, lwd=0.1, land=TRUE, 
	 bpal = list(c(0,max(aleutians),"grey"), c(min(aleutians),0,blues)))

# define start and end points
start <- matrix(c(170,55, 190, 60), ncol=2, byrow=TRUE, dimnames=list(1:2, c("lon","lat")))
end <- matrix(c(200, 56, 201, 57), ncol=2, byrow=TRUE, dimnames=list(1:2, c("lon","lat")))
start
end

# Add points and great circle distances on the map
points(start, pch=21, col="orange4", bg="orange2", cex=.8)
points(end, pch=21, col="orange4", bg="orange2", cex=.8)
linesGC(start, end, antimeridian=TRUE)
}
