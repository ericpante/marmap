\name{nw.atlantic.coast}
\alias{nw.atlantic.coast}
\title{
Coastline data for the North West Atlantic
}
\description{
Coastline data for the North West Atlantic, as downloaded using the NOAA Coastline Extractor tool.
}
\usage{
data(nw.atlantic.coast)
}
\details{
Coastline data for the NW Atlantic was obtained using the NOAA Coastline Extractor tool. To get more coastline data, go to \url{https://shoreline.noaa.gov/ccoast.html}.
}
\value{
A 2-column data frame
}
\references{
see \url{https://shoreline.noaa.gov/ccoast.html}
}
\seealso{
\code{\link{nw.atlantic}}
}
\examples{
# load NW Atlantic data and convert to class bathy
data(nw.atlantic,nw.atlantic.coast)
atl <- as.bathy(nw.atlantic)

## the function plot below plots only isobaths:
## - isobaths between 8000-4000 in light grey,
## - isobaths between 4000-500 in dark grey (to emphasize seamounts)

plot(atl, deep=c(-8000,-4000), shallow=c(-4000,-500), step=c(500,500),
 	 lwd=c(0.5,0.5,1.5),lty=c(1,1,1),
 	 col=c("grey80", "grey20", "blue"),
 	 drawlabels=c(FALSE,FALSE,FALSE) )

## the coastline can be added from a different source,
## and can therefore have a different resolution:
lines(nw.atlantic.coast)

## add a geographical reference on the coast:
points(-71.064,42.358, pch=19); text(-71.064,42.358,"Boston", adj=c(1.2,0))

}
