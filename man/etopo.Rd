\name{etopo}
\alias{etopo}
\alias{etopo.colors}
\alias{etopo.colours}
\alias{scale_colour_etopo}
\alias{scale_color_etopo}
\alias{scale_fill_etopo}
\title{
Etopo colours
}
\description{
Various ways to access the colours or on the etopo colour scale
}
\usage{
etopo.colors(n)

scale_fill_etopo(\dots)
scale_colour_etopo(\dots)
}
\arguments{
  \item{n}{
number of colours to get from the scale. Those are evenly spaced within the scale.
}
  \item{\dots}{
passed to \code{scale_fill_gradientn} or \code{scale_colour_gradientn}
}
}
\details{
\code{etopo.colors} is equivalent to all usual colours in R (\code{\link{heat.colors}}, \code{\link{cm.colors}}).

\code{scale_fill/contour_etopo} are meant to be used with ggplot2. They allow consistent plots in various subregions by setting the limits of the scale explicitly.
}

\author{
Jean-Olivier Irisson
}
\seealso{
\code{\link{autoplot.bathy}}, \code{\link{palette.bathy}}
}
\examples{
# load NW Atlantic data and convert to class bathy
	data(nw.atlantic)
	atl <- as.bathy(nw.atlantic)

  library("ggplot2")
  head(fortify(atl))

  # one can now use bathy objects with ggplot directly
  ggplot(atl) + geom_contour(aes(x=x, y=y, z=z))
  
  # which allows complete plot configuration
  atl.df <- fortify(atl)
  ggplot(atl.df, aes(x=x, y=y)) +
    geom_raster(aes(fill=z), data=atl.df[atl.df$z <= 0,]) +
    geom_contour(aes(z=z),
      breaks=c(-100, -200, -500, -1000, -2000, -4000),
      colour="white", size=0.1
    )
}
