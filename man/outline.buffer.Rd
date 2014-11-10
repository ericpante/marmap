\name{outline.buffer}
\alias{outline.buffer}
\title{
Get a buffer in a proper format to allow the plotting of its outline
}
\description{
Get a buffer in a format suitable for plotting its outline. \code{outline.buffer} replaces any \code{NA} values in \code{buffer} by 0 and non \code{NA} values by -1.
}
\usage{
outline.buffer(buffer)
}
\arguments{
  \item{buffer}{
a buffer object of class \code{bathy} (i.e. \code{bathy} matrix containing depth/altitude values within the buffer and \code{NA}s outside)
}
}
\details{
This function is essentially used to prepare a buffer or a combination of buffers for plotting its outline on a bathymetric map.
}
\value{
An object of class \code{bathy} of the same dimension as \code{buffer} containing only zeros (outside the buffer area) and -1 values (within the buffer).
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
loc <- data.frame(c(-80,-82), c(26,24))
points(loc, pch = 19, col = "red")

# compute buffer
buf <- create.buffer(florida, loc, radius=1.5)

# plot buffer outline
plot(outline.buffer(buf), add=TRUE, lwd=.7, col=2)
}
