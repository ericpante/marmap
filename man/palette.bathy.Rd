\name{palette.bathy}
\alias{palette.bathy}
\title{
Build a bathymetry- and/or topography-constrained color palette}
\description{
Build a constrained color palette based on depth / altitude bounds and given colors. 
}
\usage{
palette.bathy(mat, layers, land=FALSE, default.col="white")
}
\arguments{
  \item{mat}{
a matrix of bathymetric data, class bathy not required.
}
  \item{layers}{
a list of depth bounds and colors (see below)
}
  \item{land}{
logical. Wether to consider land or not (\code{default} is \code{FALSE})
}
  \item{default.col}{
a color for the area of the matrix not bracketed by the list supplied to \code{layers}
}
}
\details{
\code{palette.bathy} allows the production of color palettes for specified bathymetric and/or topographic layers. The \code{layers} argument must be a list of vectors. Each vector corresponds to a bathymetry/topography layer (for example, one layer for bathymetry and one layer for topography). The first and second elements of the vector are the minimum and maximum bathymetry/topography, respectively. The other elements of the vector (3, onward) correspond to colors (see example below). \code{palette.bathy} is called internally by \code{plot.bathy} when the \code{image} argument is set to \code{TRUE}.
}
\value{
A vector of colors which size depends on the depth / altitude range of the \code{bathy} matrix.
}
\author{
Eric Pante and Benoit Simon-Bouhet
}
\seealso{
\code{\link{plot.bathy}}
}
\examples{
# load NW Atlantic data and convert to class bathy
	data(nw.atlantic)
	atl <- as.bathy(nw.atlantic)

# creating depth-constrained palette for the ocean only
    newcol <- palette.bathy(mat=atl, 
		layers = list(c(min(atl), 0, "purple", "blue", "lightblue")),
		land = FALSE, default.col = "grey" )
	plot(atl, land = FALSE, n = 10, lwd = 0.5, image = TRUE, 
		bpal = newcol, default.col = "grey")
	
# same:
	plot(atl, land = FALSE, n = 10, lwd = 0.5, image = TRUE,
		bpal = list(c(min(atl), 0, "purple", "blue", "lightblue")),
		default.col = "gray")

# creating depth-constrained palette for 3 ocean "layers"
	newcol <- palette.bathy(mat = atl, layers = list(
		c(min(atl), -3000, "purple", "blue", "grey"),
		c(-3000, -150, "white"),
		c(-150, 0, "yellow", "green", "brown")),
		land = FALSE, default.col = "grey")		
	plot(atl, land = FALSE, n = 10, lwd = 0.7, image = TRUE, 
		bpal = newcol, default.col = "grey")

# same
	plot(atl, land = FALSE, n = 10, lwd = 0.7, image = TRUE, 
		bpal = list(c(min(atl), -3000, "purple","blue","grey"),
					c(-3000, -150, "white"),
					c(-150, 0, "yellow", "green", "brown")),
		default.col = "grey")

# creating depth-constrained palette for land and ocean
	newcol <- palette.bathy(mat= atl, layers = list(
		c(min(atl),0,"purple","blue","lightblue"),
		c(0, max(atl), "gray90", "gray10")), 
		land = TRUE)
	plot(atl, land = TRUE, n = 10, lwd = 0.5, image = TRUE, bpal = newcol)

# same
	plot(atl, land = TRUE, n = 10, lwd = 0.7, image = TRUE,
		bpal = list(
			c(min(atl), 0, "purple", "blue", "lightblue"),
			c(0, max(atl), "gray90", "gray10")))
}
