\name{tectonic}
\alias{tectonic}
\title{
World tectonic plate boundaries
}
\description{
A table of coordinates for plate boundaries compiled by Peter Bird
}
\usage{
data(tectonic)
}
\details{
Data were imported from Peter Bird's website \url{http://peterbird.name/oldFTP/PB2002/PB2002_boundaries.dig.txt}. 
}
\value{
A 3-column table. First two columns are longitude and latitude. Third Column contains (1) the segment number, (2) the plate type (see Bird's website for details), and (3) the reference for the segment.
}
\author{
see \url{http://peterbird.name/oldFTP/PB2002/PB2002_boundaries.dig.txt}
made available in marmap by Eric Pante
}
\references{
Bird, P. (2003) An updated digital model of plate boundaries, Geochemistry Geophysics Geosystems, 4(3), 1027, doi:10.1029/2001GC000252.
	}
\seealso{
\code{\link{tecto.lines}}
}
\examples{
getNOAA.bathy(20,160,-60,60,res=20) -> ex
plot(ex,n=1, im=T)
tecto.lines(ex, col="white", lwd=4)
tecto.lines(ex, col="blue", lwd=2)
}
