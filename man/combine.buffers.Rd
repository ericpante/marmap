\name{combine.buffers}
\alias{combine.buffers}
\title{
Create a new, (non circular) composite buffer from a list of existing buffers.
}
\description{
Creates a new bathy object from a list of existing buffers of compatible dimensions.
}
\usage{
combine.buffers(...)
}
\arguments{
  \item{...}{
2 or more buffer objects as produced by \code{\link{create.buffer}}. All \code{bathy} objects within the \code{buffer} objects must be compatible: they should have the same dimensions (same number of rows and columns) and cover the same area (same longitudes and latitudes).
}
}
\value{
An object of class \code{bathy} of the same dimensions as the original \code{bathy} objects contained within each \code{buffer} objects. The resulting \code{bathy} object contains only \code{NA}s outside of the combined buffer and values of depth/altitude inside the combined buffer.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{create.buffer}}, \code{\link{plot.buffer}}, \code{\link{plot.bathy}}
}
\examples{
# load and plot a bathymetry
data(florida)
plot(florida, lwd = 0.2)
plot(florida, n = 1, lwd = 0.7, add = TRUE)

# add points around which a buffer will be computed
loc <- data.frame(c(-80,-82), c(26,24))
points(loc, pch = 19, col = "red")

# create 2 distinct buffer objects with different radii
buf1 <- create.buffer(florida, loc[1,], radius=1.9)
buf2 <- create.buffer(florida, loc[2,], radius=1.2)

# combine both buffers
buf <- combine.buffers(buf1,buf2)

\dontrun{
# Add outline of the resulting buffer in red 
# and the outline of the original buffers in blue
plot(outline.buffer(buf), lwd = 3, col = 2, add=TRUE)
plot(buf1, lwd = 0.5, fg="blue")
plot(buf2, lwd = 0.5, fg="blue")
}
}
