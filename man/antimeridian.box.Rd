\name{antimeridian.box}
\alias{antimeridian.box}
\title{
Adds a box to maps including antimeridian
}
\description{
Adds a box on maps including the antimeridian (180)
}
\usage{
antimeridian.box(object, tick.spacing)
}
\arguments{
  \item{object}{
matrix of class bathy
}
  \item{tick.spacing}{
spacing between tick marks (in degrees, default=20)
}
}
\value{
The function adds a box and tick marks to an existing plot which contains the antimeridian line (180 degrees). 
}
\author{
Eric Pante & Benoit Simon-Bouhet
}
\seealso{
\code{\link{plot.bathy}}
}
\examples{
data(aleutians)

# default plot:
plot(aleutians,n=1)

# plot with corrected box and labels:
plot(aleutians,n=1,axes=FALSE)
antimeridian.box(aleutians, 10)
}
