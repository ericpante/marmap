\name{as.bathy}
\alias{as.bathy}
\title{
Convert to bathymetric data in an object of class bathy}
\description{
Reads either an object of class \code{RasterLayer}, \code{SpatialGridDataFrame} or a three-column data.frame containing longitude (x), latitude (y) and depth (z) data and converts it to a matrix of class bathy. 
}
\usage{
as.bathy(x)
}
\arguments{
  \item{x}{
Object of \code{RasterLayer} or \code{SpatialGridDataFrame}, or a three-column data.frame with longitude (x), latitude (y) and depth (z) (no default)
}
}
\details{
\code{x} can contain data downloaded from the NOAA GEODAS Grid Translator webpage (http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html) in the form of an xyz table. The function \code{as.bathy} can also be used to transform objects of class \code{raster} (see package \code{raster}) and \code{SpatialGridDataFrame} (see package \code{sp}).
}
\value{
The output of \code{as.bathy} is a matrix of class \code{bathy}, which dimensions and resolution are identical to the original object. The class \code{bathy} has its own methods for summarizing and ploting the data.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{summary.bathy}}, \code{\link{plot.bathy}}, \code{\link{read.bathy}}, \code{\link{as.xyz}}, \code{\link{as.raster}}, \code{\link{as.SpatialGridDataFrame}}.
}

\examples{
# load NW Atlantic data
data(nw.atlantic)

# use as.bathy
atl <- as.bathy(nw.atlantic)

# class "bathy"
class(atl)

# summarize data of class "bathy"
summary(atl)

}
