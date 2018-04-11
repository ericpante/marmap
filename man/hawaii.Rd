\name{hawaii}
\alias{hawaii}
\alias{hawaii.sites}
\title{
Bathymetric data for Hawaii, USA
}
\description{
Bathymetric object of class \code{bathy} created from NOAA GEODAS data and arbitrary locations around the main Hawaiian islands.
}
\usage{
data(hawaii)
data(hawaii.sites)
}
\details{
\code{hawaii} contains data imported from the NOAA GEODAS Grid Translator webpage (\url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}) and transformed into an object of class \code{bathy} by \code{read.bathy}.
\code{hawaii.sites} is a 2-columns data.frame containing longitude and latitude of 6 locations spread at sea around Hawaii.
}
\value{
\code{hawaii}: a bathymetric object of class \code{bathy} with 539 rows and 659 columns.
\code{hawaii.sites}: data.frame (6 rows, 2 columns)
}
\author{
see \url{https://maps.ngdc.noaa.gov/viewers/wcs-client/}
}
\seealso{
\code{\link{plot.bathy}}, \code{\link{summary.bathy}}
}
\examples{
# load hawaii data
	data(hawaii)
	data(hawaii.sites)

# class "bathy"
	class(hawaii)
	summary(hawaii)

\dontrun{
## use of plot.bathy to produce a bathymetric map
# creation of a color palette
	pal <- colorRampPalette(c("black","darkblue","blue","lightblue"))

# Plotting the bathymetry
	plot(hawaii,image=TRUE,draw=TRUE,bpal=pal(100),asp=1,col="grey40",lwd=.7)

# Adding coastline
	require(mapdata)
	map("worldHires",res=0,fill=TRUE,col=rgb(.8,.95,.8,.7),add=TRUE)

# Adding hawaii.sites location on the map
	points(hawaii.sites,pch=21,col="yellow",bg=col2alpha("yellow",.9),cex=1.2)
}
}
