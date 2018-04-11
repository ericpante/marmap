\name{florida}
\alias{florida}
\title{
Bathymetric data around Florida, USA}
\description{
Bathymetric object of class \code{bathy} created from NOAA GEODAS data.
}
\usage{
data(florida)
}
\details{
Data imported from the NOAA GEODAS Grid Translator webpage (\url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}) and transformed into an object of class \code{bathy} by \code{read.bathy}. 
}
\value{
A bathymetric object of class \code{bathy} with 539 rows and 659 columns.
}
\author{
see \url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}
}
\seealso{
\code{\link{plot.bathy}}, \code{\link{summary.bathy}}
}
\examples{
# load florida data
data(florida)

# class "bathy"
class(florida)
summary(florida)

# test plot.bathy
plot(florida,asp=1)
plot(florida,asp=1,image=TRUE,drawlabels=TRUE,land=TRUE,n=40)
}
