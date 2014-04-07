\name{as.SpatialGridDataFrame}
\alias{as.SpatialGridDataFrame}
\title{
Convert bathymetric data to a spatial grid}
\description{
Transforms an object of class \code{bathy} to a \code{SpatialGridDataFrame} object. 
}
\usage{
as.SpatialGridDataFrame(bathy)
}
\arguments{
  \item{bathy}{
an object of class \code{bathy}
}
}
\details{
\code{as.SpatialGridDataFrame} transforms \code{bathy} objects into objects of class \code{SpatialGridDataFrame} as defined in the \code{sp} package. All methods from the \code{sp} package are thus available for bathymetric data (e.g. rotations, projections...).
}
\value{
An object of class \code{SpatialGridDataFrame} with the same characteristics as the \code{bathy} object (same longitudinal and latitudinal ranges, same resolution).
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{as.xyz}}, \code{\link{as.bathy}}, \code{\link{as.raster}}
}
\examples{
# load Hawaii bathymetric data
data(hawaii)

# use as.SpatialGridDataFrame
sp.hawaii <- as.SpatialGridDataFrame(hawaii)

# Summaries
summary(hawaii)
summary(sp.hawaii)

# structure of the SpatialGridDataFrame object
str(sp.hawaii)

# Plots
plot(hawaii,image=TRUE,lwd=.2)
image(sp.hawaii)
}
