\name{check.bathy}
\alias{check.bathy}
\title{
Sort bathymetric data matrix by increasing latitude and longitude}
\description{
Reads a bathymetric data matrix and orders its rows and columns by increasing latitude and longitude. 
}
\usage{
check.bathy(x)
}
\arguments{
  \item{x}{
a matrix
}
}
\details{
\code{check.bathy} allows to sort rows and columns by increasing latitude and longitude, which is necessary for ploting with the function \code{image} (package \code{graphics}). \code{check.bathy} is used within the \code{marmap} functions \code{read.bathy} and \code{as.bathy} (it is also used in \code{getNOAA.bathy} through \code{as.bathy}). 
}
\value{
The output of \code{check.bathy} is an ordered matrix.
}
\author{
Eric Pante
}
\seealso{
\code{\link{read.bathy}}, \code{\link{as.bathy}}, \code{\link{getNOAA.bathy}}
}
\examples{
matrix(1:100, ncol=5, dimnames=list(20:1, c(3,2,4,1,5))) -> a
check.bathy(a)
}
