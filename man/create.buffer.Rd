\name{create.buffer}
\alias{create.buffer}
\title{
Create a buffer of specified radius around one or several points
}
\description{
Create a circular buffer of user-defined radius around one or several points defined by their longitudes and latitudes.
}
\usage{
create.buffer(mat, loc, radius, km = FALSE)
}
\arguments{
  \item{mat}{
an object of class \code{bathy}
}
  \item{loc}{
a 2-column \code{data.frame} of longitudes and latitudes for points around which the buffer is to be created.
}
  \item{radius}{
\code{numeric}. Radius of the buffer in the same unit as the \code{bathy} object (i.e. usually decimal degrees) when \code{km=FALSE} (default) or in kilometers when \code{radius=TRUE}.
}
  \item{km}{
\code{logical}. If \code{TRUE}, the \code{radius} should be provided in kilometers. When \code{FALSE} (default) the radius is in the same unit as the \code{bathy} object (i.e. usually decimal degrees).
}

}
\details{
This function takes advantage of the \code{buffer} function from package \code{adehabitatMA} and several functions from packages \code{sp} to define the buffer around the points provided by the user.
}
\value{
An object of class \code{bathy} of the same size as \code{mat} containing only \code{NA}s outside of the buffer and values of depth/altitude (taken from \code{mat}) within the buffer.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{outline.buffer}}, \code{\link{combine.buffers}}, \code{\link{plot.bathy}}
}
\examples{
# load and plot a bathymetry
data(florida)
plot(florida, lwd = 0.2)
plot(florida, n = 1, lwd = 0.7, add = TRUE)

# add points around which a buffer will be computed
loc <- data.frame(c(-80,-82), c(26,24))
points(loc, pch = 19, col = "red")

# compute buffer
buf <- create.buffer(florida, loc, radius=1.5)

# highlight isobath with the buffer and add outline
plot(buf, add = TRUE, n = 10, col = 2, lwd=.4)
plot(outline.buffer(buf), add = TRUE, lwd = 0.7, col = 2)
}
