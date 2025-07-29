\name{aleutians}
\alias{aleutians}
\title{
Bathymetric data for the Aleutians (Alaska)}
\description{
Bathymetric matrix of class \code{bathy} created from NOAA GEODAS data.
}
\usage{
data(aleutians)
}
\details{
Data imported from the NOAA Grid Extract webpage (\url{https://www.ncei.noaa.gov/maps/grid-extract/}) and transformed into an object of class \code{bathy} by \code{as.bathy}.
}
\value{
A text file.
}
\author{
see \url{https://www.ncei.noaa.gov/maps/grid-extract/}
}
\seealso{
\code{\link{as.bathy}}, \code{\link{read.bathy}}, \code{\link{antimeridian.box}}
}
\examples{
# load celt data
data(aleutians)

# class "bathy"
class(aleutians)
summary(aleutians)

# test plot.bathy
plot(aleutians,image = TRUE,
     bpal = list(c(0,max(aleutians),"grey"),
                 c(min(aleutians),0,"darkblue","lightblue")),
     land = TRUE, lwd = 0.1, axes = FALSE)
antimeridian.box(aleutians, 10)
}
