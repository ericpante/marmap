\name{summary.bathy}
\alias{summary.bathy}
\title{
Summary of bathymetric data of class \code{bathy}
}
\description{
Summary of bathymetric data of class \code{bathy}. Provides geographic bounds and resolution (in minutes) of the dataset, statistics on depth data, and a preview of the bathymetric matrix.   
}
\usage{
\method{summary}{bathy}(object, \dots)
}
\arguments{
  \item{object}{
object of class \code{bathy}
}
  \item{\dots}{
additional arguments affecting the summary produced (see \code{base} function \code{summary}).  	
}
}
\value{
Information on the geographic bounds of the dataset (minimum and maximum latitude and longitude), resolution of the matrix in minutes, statistics on the depth data (e.g. min, max, median...), and a preview of the data. 
}
\author{
Eric Pante and Benoit Simon-Bouhet
}
\seealso{
\code{\link{read.bathy}}, \code{\link{plot.bathy}}
}
\examples{
# load NW Atlantic data
data(nw.atlantic)

# use as.bathy
atl <- as.bathy(nw.atlantic)

# class bathy
class(atl)

# summarize data of class bathy
summary(atl)

}
