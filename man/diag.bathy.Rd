\name{diag.bathy}
\alias{diag.bathy}
\title{
Finds matrix diagonal for non-square matrices
}
\description{
Finds either the values of the coordinates of the non-linear diagonal of non-square matrices. 
}
\usage{
diag.bathy(mat,coord=FALSE)
}
\arguments{
  \item{mat}{
a data matrix
}
  \item{coord}{
whether of not to output the coordinates of the diagonal (default is \code{FALSE})
}
}
\details{
diag.bathy gets the values or coordinates from the first element of a matrix to its last elements. If the matrix is non-square, that is, its number of rows and columns differ, diag.bathy computes an approximate diagonal.  
}
\value{
A vector of diagonal values is \code{coord} is \code{FALSE}, or a table of diagonal coordinates if\code{coord} is \code{FALSE}}
\author{
Eric Pante
}
\seealso{
\code{\link{get.transect}}, \code{\link{diag}}
}
\examples{
	
# a square matrix: diag.bathy behaves as diag
	matrix(1:25, 5, 5) -> a ; a
	diag(a)
	diag.bathy(a)

# a non-square matrix: diag.bathy does not behaves as diag
	matrix(1:15, 3, 5) -> b ; b
	diag(b)
	diag.bathy(b)
	
# output the diagonal or its coordinates: 
	rownames(b) <- seq(32,35, length.out=3)
	colnames(b) <- seq(-100,-95, length.out=5)
	diag.bathy(b, coord=FALSE)
	diag.bathy(b, coord=TRUE)
	
}
