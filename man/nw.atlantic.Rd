\name{nw.atlantic}
\alias{nw.atlantic}
\title{
Bathymetric data for the North West Atlantic
}
\description{
Data imported from the NOAA GEODAS server
}
\usage{
data(nw.atlantic)
}
\details{
Data imported from the NOAA GEODAS Grid Translator webpage (\url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}). To prepare data from NOAA, fill the custom grid form, and choose "XYZ (lon,lat,depth)" as the "Output Grid Format", "No Header" as the "Output Grid Header", and either of the space, tab or comma as the column delimiter (either can be used, but "comma" is the default import format of \code{read.bathy}). Choose "omit empty grid cells" to reduce memory usage. 
}
\value{
A three-columns data.frame containing longitude, latitude and depth/elevation data.
}
\author{
see \url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}
}
\seealso{
\code{\link{plot.bathy}}, \code{\link{summary.bathy}}
}
\examples{
# load NW Atlantic data
data(nw.atlantic)

# use as.bathy
atl <- as.bathy(nw.atlantic)

# class "bathy"
class(atl)
summary(atl)

# test plot.bathy
plot(atl, deep=-8000, shallow=-1000, step=1000)

}
