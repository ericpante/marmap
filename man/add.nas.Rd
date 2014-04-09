\name{add.nas}
\alias{add.nas}
\title{
Adds NAs to a vector
}
\description{
Internal function to add NAs to a vector (used in \code{read.bathy} and \code{as.bathy})
}
\usage{
add.nas(vec, pos.missing)
}
\arguments{
  \item{vec}{
vector of data (in the context of marmap, depth/altitude values, ie 3rd column of xyz files)
}
  \item{pos.missing}{
vector, position of missing value in the future bathy object
}
}
\value{
The function returns an updated vector containing both the original data and NAs at the positions imposed by argument \code{pos.missing}. The length of the resulting vector equals the sum of lengths of \code{vec} and \code{pos.missing}. 
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{as.bathy}}, \code{\link{read.bathy}}
}
\examples{
add.nas(vec=c("-100", "-200", "-500"), pos.missing=3)
}
