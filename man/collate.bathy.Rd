\name{collate.bathy}
\alias{collate.bathy}
\title{
Collates two bathy matrices with data from either sides of the antimeridian
}
\description{
Collates two bathy matrices, one with longitude 0 to 180 degrees East, and the other with longitude 0 to 180 degrees West
}
\usage{
collate.bathy(east,west)
}
\arguments{
  \item{east}{
matrix of class \code{bathy} with eastern data (West of antimeridian)
}
  \item{west}{
matrix of class \code{bathy} with western data (East of antimeridian)
}
}
\details{
This function is meant to be used with \code{read.bathy()} or \code{readGEBCO.bathy()}, when data is downloaded from either sides of the antimeridian line (180 degrees longitude). If, for example, data is downloaded from GEBCO for longitudes of 170E-180 and 180-170W, \code{collate.bathy()} will create a single matrix of class \code{bathy} with a coordinate system going from 170 to 190 degrees longitude. 

\code{getNOAA.bathy()} deals with data from both sides of the antimeridian and does not need further processing with \code{collate.bathy()}.
}
\value{
A single matrix of class \code{bathy} that can be interpreted by \code{plot.bathy}. When plotting collated data (with longitudes 0 to 180 and 180 to 360 degrees), plots can be modified to display the conventional coordinate system (with longitudes 0 to 180 and -180 to 0 degrees) using function \code{antimeridian.box()}.
}
\author{
Eric Pante
}
\seealso{
\code{\link{getNOAA.bathy}}, \code{\link{summary.bathy}}, \code{\link{plot.bathy}}, \code{\link{antimeridian.box}}
}
\examples{
## faking two datasets using aleutians, for this example
## "a" and "b" simulate two datasets downloaded from GEBCO, for ex.
	data(aleutians)
	aleutians[1:181,] -> a ; "bathy" -> class(a)
	aleutians[182:601,] -> b ; "bathy" -> class(b)
	-(360-as.numeric(rownames(b))) -> rownames(b) 

## check these objects with summary(): pay attention of the Longitudinal range
	summary(aleutians)
	summary(a)
	summary(b)

## merge datasets:
	collate.bathy(a,b) -> collated
	summary(collated) # should be identical to summary(aleutians)
}
