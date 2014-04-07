\name{as.raster}
\alias{as.raster}
\title{
Convert bathymetric data to a raster layer}
\description{
Transforms an object of class \code{bathy} to a raster layer. 
}
\usage{
as.raster(bathy)
}
\arguments{
  \item{bathy}{
an object of class \code{bathy}
}
}
\details{
\code{as.raster} transforms \code{bathy} objects into objects of class \code{RasterLayer} as defined in the \code{raster} package. All methods from the \code{raster} package are thus available for bathymetric data (e.g. rotations, projections...).
}
\value{
An object of class \code{RasterLayer} with the same characteristics as the \code{bathy} object (same longitudinal and latitudinal ranges, same resolution).
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{as.xyz}}, \code{\link{as.bathy}}, \code{\link{as.SpatialGridDataFrame}}
}
\examples{
# load Hawaii bathymetric data
data(hawaii)

# use as.raster
r.hawaii <- as.raster(hawaii)

# class "RasterLayer"
class(r.hawaii)

# Summaries
summary(hawaii)
summary(r.hawaii)

# structure of the RasterLayer object
str(r.hawaii)

\dontrun{
# Plots
#require(raster)
plot(hawaii,image=TRUE,lwd=.2)
plot(r.hawaii)
}
}
