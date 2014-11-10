\name{combine.buffers}
\alias{combine.buffers}
\title{
Create a new, composite buffer from a list of existing buffers.
}
\description{
Create a new buffer object from a list of existing buffers of compatible dimensions.
}
\usage{
combine.buffers(buf)
}
\arguments{
  \item{buf}{
a list of 2 or more buffer objects as produced by \code{\link{create.buffer}}. All buffer objects in the list must have the same number of rows and columns.
}
}
\value{
An object of class \code{bathy} of the same dimension as the original buffers containing only \code{NA}s outside of the combined buffer and values of depth/altitude (taken from the list of buffers \code{buf}) within the combined buffer.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{create.buffer}}, \code{\link{outline.buffer}}, \code{\link{plot.bathy}}
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
buf <- combine.buffers(list(buf1,buf2))

\dontrun{
# Add outline of the resulting buffer in red 
# and the outline of the original buffers in blue
plot(outline.buffer(buf), add = TRUE, lwd = 3, col = 2)
plot(outline.buffer(buf1), add = TRUE, lwd = 0.5, col="blue")
plot(outline.buffer(buf2), add = TRUE, lwd = 0.5, col="blue")
}
}
