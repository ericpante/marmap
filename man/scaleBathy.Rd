\name{scaleBathy}
\alias{scaleBathy}
\title{
Adds a scale to a map
}
\description{
Uses geographic information from object of class \code{bathy} to calculate and plot a scale in kilometer.
}
\usage{
scaleBathy(mat, deg=1, x="bottomleft", y=NULL, inset=10, angle=90, ...)
}
\arguments{
  \item{mat}{
bathymetric data matrix of class \code{bathy}, imported using \code{read.bathy}
}
  \item{deg}{
the number of degrees of longitudes to convert into kilometers (default is 1)
}
  \item{x, y}{
the coordinates used to plot the scale on the map (see Details)
}
  \item{inset}{
when \code{x} is a keyword (e.g. \code{"bottomleft"}), \code{inset} is a percentage of the plotting space controlling the relative position of the plotted scale (see Examples)
}
  \item{angle}{
angle from the shaft of the arrow to the edge of the arrow head
}
  \item{...}{
further arguments to be passed to \code{text}
}

}
\details{
\code{scaleBathy} is a simple utility to add a scale to the lower left corner of a \code{bathy} plot. The distance in kilometers between two points separated by 1 degree longitude is calculated based on the minimum latitude of the \code{bathy} object used to plot the map. Option \code{deg} allows the user to plot the distance separating more than one degree (default is one). 

The plotting coordinates \code{x} and \code{y} either correspond to two points on the map (i.e. longitude and latitude of the point where the scale should be plotted), or correspond to a keyword (set with \code{x}, \code{y} being set to \code{NULL}) from the list "bottomright", "bottomleft", "topright", "topleft". When a keyword is used, the option \code{inset} controls how far the scale will be from the edges of the plot.
}
\value{
a scale added to the active graphical device
}
\author{
Eric Pante
}
\note{
The calculation formula is from function \code{map.scale} of package \code{maps}. 6372.798 km is used as the Earth radius. 
}
\seealso{
\code{\link{plot.bathy}}
}
\examples{
# load NW Atlantic data and convert to class bathy
	data(nw.atlantic)
	atl <- as.bathy(nw.atlantic)

# a simple example
	plot(atl, deep=-8000, shallow=-1000, step=1000, lwd=0.5, col="grey")
	scaleBathy(atl, deg=4)

# using keywords to place the scale with inset=10%
	par(mfrow=c(2,2))
	plot(atl, deep=-8000, shallow=-1000, step=1000, lwd=0.5, col="grey")
	scaleBathy(atl, deg=4, x="bottomleft", y=NULL)
	plot(atl, deep=-8000, shallow=-1000, step=1000, lwd=0.5, col="grey")
	scaleBathy(atl, deg=4, x="bottomright", y=NULL)

# using keywords to place the scale with inset=20%
	plot(atl, deep=-8000, shallow=-1000, step=1000, lwd=0.5, col="grey")
	scaleBathy(atl, deg=4, x="topleft", y=NULL, inset=20)
	plot(atl, deep=-8000, shallow=-1000, step=1000, lwd=0.5, col="grey")
	scaleBathy(atl, deg=4, x="topright", y=NULL, inset=20) 

}
