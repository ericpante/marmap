\name{as.xyz}
\alias{as.xyz}
\title{
Convert to xyz format}
\description{
Converts a matrix of class \code{bathy} into a three-column data.frame containing longitude, latitude and depth data.
}
\usage{
as.xyz(bathy)
}
\arguments{
  \item{bathy}{
matrix of class \code{bathy}.
}
}
\details{
The use of \code{as.bathy} and \code{as.xyz} allows to swicth back and forth between the long format (xyz) and the wide format of class \code{bathy} suitable for plotting bathymetric maps, computing least cost distances, etc. \code{as.xyz} is especially usefull for exporting xyz files when data are obtained using \code{subsetSQL}, i.e. bathymetric matrices of class \code{bathy}.
}
\value{
Three-column data.frame with a format similar to xyz files downloaded from the NOAA GEODAS Grid Translator webpage (\url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}). The first column contains longitude data, the second contains latitude data and the third contains depth/elevation data.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{as.bathy}}, \code{\link{summary.bathy}}
}
\examples{
# load celt data
data(celt)
dim(celt)
class(celt)
summary(celt)
plot(celt,deep= -300,shallow= -25,step=25)

# use as.xyz
celt2 <- as.xyz(celt)
dim(celt2)
class(celt2)
summary(celt2)

}
