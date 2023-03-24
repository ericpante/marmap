\name{fortify.bathy}
\alias{fortify.bathy}
\title{
Extract bathymetry data in a data.frame}
\description{
Extract bathymetry data in a data.frame
}
\usage{
\method{fortify}{bathy}(model, data, \dots)
}
\arguments{
  \item{model}{
bathymetric data matrix of class \code{bathy}, imported using \code{\link{read.bathy}}
}
  \item{data}{
ignored  
}
  \item{\dots}{
ignored
}
}
\details{
\code{fortify.bathy} is really just calling \code{\link{as.xyz}} and ensuring consistent names for the columns. It then allows to use ggplot2 functions directly.
}

\author{
Jean-Olivier Irisson, Benoit Simon-Bouhet
}
\seealso{
\code{\link{autoplot.bathy}}, \code{\link{as.xyz}}
}
\examples{
# load NW Atlantic data and convert to class bathy
data(nw.atlantic)
atl <- as.bathy(nw.atlantic)

library("ggplot2")
# convert bathy object into a data.frame
head(fortify(atl))

# one can now use bathy objects with ggplot directly
ggplot(atl) + geom_contour(aes(x=x, y=y, z=z)) + coord_map()

# which allows complete plot configuration
atl.df <- fortify(atl)
ggplot(atl.df, aes(x=x, y=y)) + coord_quickmap() +
  geom_raster(aes(fill=z), data=atl.df[atl.df$z <= 0,]) +
  geom_contour(aes(z=z),
    breaks=c(-100, -200, -500, -1000, -2000, -4000),
    colour="white", size=0.1
  ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))
}
