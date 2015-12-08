\name{plot.buffer}
\alias{plot.buffer}
\title{
Plots a circular buffer and or its outline
}
\description{
\code{plot.buffer} is a generic function that allows the plotting of objects of class \code{buffer}, either as new plots or as a new layer added on top of an existing one. The plotting of both the bathymetry/hypsometry as well as the outline of the buffer is possible.
}
\usage{
\method{plot}{buffer}(x, outline = TRUE, add = TRUE, ...)
}
\arguments{
  \item{x}{
an object of class \code{buffer} as produced by the \code{create.buffer()} function.
}
  \item{outline}{
Should the outline of the buffer be plotted (default) or the bathymetric/hypsometric data within the buffer.
}
  \item{add}{
Should the plot be added on top of an existing bathymetric/hypsometric plot (default) or as a new plot
}
  \item{...}{
Further arguments to be passed to the \code{symbols()} function from the \code{graphics} package when \code{outline = TRUE} (default) or to \code{plot.bathy()} when \code{outline = FALSE}.
}
}
\value{
Either a plot of the outline of a buffer (default) or a bathymetric map with isobaths of a buffer when \code{outline = FALSE}
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{create.buffer}}, \code{\link{combine.buffers}}, \code{\link{plot.bathy}}
}
\examples{
# load and plot a bathymetry
data(florida)
plot(florida, lwd = 0.2)
plot(florida, n = 0, lwd = 0.7, add = TRUE)

# add points around which a buffer will be computed
loc <- data.frame(-80, 26)
points(loc, pch = 19, col = "red")

# compute buffer
buf <- create.buffer(florida, loc, radius=1.5)

# plot buffer bathymetry 
plot(buf, outline=FALSE, n=10, lwd=.5, col=2)

# add buffer outline
plot(buf, lwd=.7, fg=2)
}
