\name{get.depth}
\alias{get.depth}
\title{
Get depth data by clicking on a map
}
\description{
Outputs depth information based on points selected by clicking on a map
}
\usage{
get.depth(mat)
}
\arguments{
  \item{mat}{
bathymetric data matrix of class \code{bathy}, imported using \code{read.bathy}
}
}
\details{
\code{get.depth} allows the user to get depth data by clicking on a map created with \code{plot.bathy}. This function uses the \code{locator} function (\code{graphics} package); after creating a map with \code{plot.bathy}, the user can either (a) click once on the map, press the Escape button, and get the depth of that location, or (b) click twice to delimit an area (for example, lower left and upper right corners of a rectangular area of interest), press Escape, and get summary information on that area from \code{summary.bathy}. 
}
\value{
a value if the user clicks once on the map, a summary if the user clicks twice on the map
}
\author{
Eric Pante
}
\section{Warning }{
clicking more than twice on the map will return a warning message: "Please choose only two points from the map"
}
\seealso{
\code{\link{read.bathy}}, \code{\link{summary.bathy}}, \code{\link{nw.atlantic}}, \code{\link{metallo}}
}
\examples{
# load NW Atlantic data and convert to class bathy
data(nw.atlantic)
atl <- as.bathy(nw.atlantic)

# a simple example
plot(atl, deep=-8000, shallow=-1000, step=1000)

# once the map is plotted, use get.depth to get depth!
\dontrun{
get.depth(atl)
}
# click once or twice, and press Escape 
}
