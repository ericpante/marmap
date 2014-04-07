\name{is.bathy}
\alias{is.bathy}
\title{
Test whether an object is of class bathy
}
\description{
Test whether an object is of class bathy
}
\usage{
is.bathy(xyz)
}
\arguments{
  \item{xyz}{
three-column data.frame with longitude (x), latitude (y) and depth (z) (no default)
}
}
\value{
The function returns \code{TRUE} or \code{FALSE}
}
\author{
Eric Pante
}
\seealso{
\code{\link{as.bathy}}, \code{\link{summary.bathy}}, \code{\link{read.bathy}}
}
\examples{
# load NW Atlantic data
data(nw.atlantic)

# test class "bathy"
is.bathy(nw.atlantic)

# use as.bathy
atl <- as.bathy(nw.atlantic)

# class "bathy"
class(atl)
is.bathy(atl)

# summarize data of class "bathy"
summary(atl)

}
