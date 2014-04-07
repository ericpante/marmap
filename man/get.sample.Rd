\name{get.sample}
\alias{get.sample}
\title{
Get sample data by clicking on a map
}
\description{
Outputs sample information based on points selected by clicking on a map
}
\usage{
get.sample(mat, sample, col.lon, col.lat, \dots)
}
\arguments{
  \item{mat}{
bathymetric data matrix of class \code{bathy}, imported using \code{read.bathy} (no default)
}
  \item{sample}{
data.frame containing sampling information (at least longitude and latitude) (no default)
}
  \item{col.lon}{
column number of data frame \code{sample} containing longitude information (no default)
}
  \item{col.lat}{
column number of data frame \code{sample} containing latitude information (no default)
}
  \item{\dots}{
further arguments to be passed to \code{\link{locator}}
}
}
\details{
\code{get.sample} allows the user to get sample data by clicking on a map created with \code{plot.bathy}. This function uses the \code{locator} function (\code{graphics} package). After creating a map with \code{plot.bathy}, the user can  click twice on the map to delimit an area (for example, lower left and upper right corners of a rectangular area of interest), press Escape, and get a dataframe corresponding to the \code{sample} points present within the selected area. 
}
\value{
a dataframe of the elements of \code{sample} present within the area selected
}
\author{
Eric Pante
}
\section{Warning }{
clicking once or more than twice on the map will return a warning message: "Please choose two points from the map"
}
\seealso{
\code{\link{read.bathy}}, \code{\link{summary.bathy}}, \code{\link{nw.atlantic}}, \code{\link{metallo}}
}
\examples{
\dontrun{
# load metallo sampling data and add a third field containing a specimen ID
data(metallo)
id = as.factor(paste("Metallo",seq(1,38,1)))
data.frame(metallo,id)-> metallo2

# load NW Atlantic data, convert to class bathy, and plot
data(nw.atlantic)
atl <- as.bathy(nw.atlantic)
plot(atl, deep=-8000, shallow=0, step=1000, col="grey")

# once the map is plotted, use get.sample to get sampling info!
get.sample(atl, metallo2, 1, 2)
# click twice and press Escape
}
}
