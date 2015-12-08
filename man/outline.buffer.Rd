\name{outline.buffer}
\alias{outline.buffer}
\title{
Get a composite buffer in a format suitable for plotting its outline
}
\description{
Get a buffer (i.e. a non-circular buffer as produced by \code{combine.buffers()}) in a format suitable for plotting its outline. \code{outline.buffer()} replaces any \code{NA} values in a \code{buffer} or \code{bathy} object by 0 and non-\code{NA} values by -1.
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
This function is essentially used to prepare a composite buffer for plotting its outline on a bathymetric map. Plotting a single circular buffer should be done using the \code{plot.buffer()} function since it offers a more straightforward method for plotting and much smoother outlines, especially for low-resolution bathymetries.
}
\value{
An object of class \code{bathy} of the same dimensions as \code{buffer} containing only zeros (outside the buffer area) and -1 values (within the buffer).
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