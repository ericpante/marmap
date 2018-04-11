\name{celt}
\alias{celt}
\title{
Bathymetric data for the North Est Atlantic}
\description{
Bathymetric matrix of class \code{bathy} created from NOAA GEODAS data.
}
\usage{
data(celt)
}
\details{
Data imported from the NOAA GEODAS Grid Translator webpage (\url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}) and transformed into an object of class \code{bathy} by \code{as.bathy}. 
}
\value{
A text file.
}
\author{
see \url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}
}
\seealso{
\code{\link{as.bathy}}, \code{\link{read.bathy}}
}
\examples{
# load celt data
data(celt)

# class "bathy"
class(celt)
summary(celt)

# test plot.bathy
plot(celt, deep=-300, shallow=-50, step=25)

}
