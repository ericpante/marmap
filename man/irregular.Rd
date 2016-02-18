\name{irregular}
\alias{irregular}
\title{
Irregularly spaced bathymetric data.
}
\description{
Three-column data.frame of irregularly-spaced longitudes, latitudes and depths.
}
\usage{
data(irregular)
}
\value{
A three-columns data.frame containing longitude, latitude and depth/elevation data.
}
\author{
Data modified form a dataset kindly provided by Noah Lottig from the university of Wisconsin \url{http://limnology.wisc.edu/personnel/lottig/} in the framework of the North Temperate Lakes Long Term Ecological Research program \url{http://lter.limnology.wisc.edu}
}
\seealso{
\code{\link{griddify}}
}
\examples{
# load data
data(irregular)

# use griddify
reg <- griddify(irregular, nlon = 40, nlat = 60)

# switch to class "bathy"
class(reg)
bat <- as.bathy(reg)
summary(bat)

# Plot the new bathy object along with the original data
plot(bat, image = TRUE, lwd = 0.1)
points(irregular$lon, irregular$lat, pch = 19, cex = 0.3, col = col2alpha(3))
}
