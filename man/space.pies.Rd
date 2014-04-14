\name{space.pies}
\alias{space.pies}
\title{
Automatic placement of piecharts on maps}
\description{
Attemps to automatically place piecharts on maps, avoiding overlap. Work in progress...
}
\usage{
space.pies(x, y, pie.slices, pie.colors=NULL, pie.radius=1, pie.space=5, 
              link=TRUE, seg.lwd=1, seg.col=1, seg.lty=1, coord=NULL)
}
\arguments{
  \item{x}{
the longitude of the anchor point for the piechart
}
  \item{y}{
the latitude of the anchor point for the piechart
}
  \item{pie.slices}{
a table with the counts to draw pies (col: pie categories, or slices; rows: sites on the map)
}
  \item{pie.colors}{
a table with the colors to draw pies (col: pie categories, or slices; rows: sites on the map)
}
  \item{pie.radius}{
size of the piechart
}
  \item{pie.space}{
factor of spacing between the anchor and the pie (the larger, the farther the pie from the anchor)
}
  \item{link}{
logical; whether to add a segment to link pie and anchor
}
  \item{seg.lwd}{
the line width of the link
}
  \item{seg.col}{
the line color of the link
}
  \item{seg.lty}{
the line type of the link
}
  \item{coord}{
when coord = \code{NULL} (default), placement is automatic. Otherwise, a 2-col table of lon/lat for pies.
}
}
\details{
\code{space.pies} tries to position piecharts on a map while avoiding overlap between them. The function heavily relies on two other functions. \code{floating.pie} from package plotrix is used to draw individual piecharts. \code{floating.pie} treats one pie at a time; \code{space.pies} can handle one or multiple pies by looping \code{floating.pie}. \code{pointLabels} from package maptools was modified to find the best placement for the pies, given their size and distance from their anchor point. \code{pointLabels} was originally meant to automatically place text labels, not objects; the modified version contained in \code{space.pies} uses the coordinates chosen by \code{pointLabels} for text. The algorithm used is simulating annealing (SANN). You can get a different result each time you run \code{space.pies}, because \code{pointLabel} finds one good solution out of many. If you are not satisfied by the solution, you can try running the function again.

The argument \code{coord} allows to choose between the automatic placement outlined above, and a user-defined list of longitudes and latitudes (in a two-column table format) for plotting the piecharts. 

Anchor point: spatial location of the data corresponding to the piechart (e.g. a sampling point).
}
\value{
Piechart(s) added to a plot. 
}
\author{
Eric Pante, using functions \code{\link[plotrix]{floating.pie}} from package plotrix and \code{\link[maptools]{pointLabel}} from maptools. 
}
\references{
Bivand, R. and Lewin-Koh, N. (2013) maptools: Tools for reading and handling spatial objects. R package version 0.8-25. http://CRAN.R-project.org/package=maptools

Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12.

SANN code implemented in \code{pointLabel} based on: Jon Christensen, Joe Marks, and Stuart Shieber. Placing text labels on maps and diagrams. In Paul Heckbert, editor, Graphics Gems IV, pages 497-504. Academic Press, Boston, MA, 1994.
}
\seealso{
\code{\link{plot.bathy}}, \code{\link[plotrix]{floating.pie}}, \code{\link[maptools]{pointLabel}}
}
\examples{
# fake frequencies to feed to space.pies()
	sample(seq(10,90,5), 11)-> freq.a
	100-freq.a -> freq.b
	rep("lightblue",11) -> col.a
	rep("white",11) -> col.b
	
# some coordinates on the NW Atlantic coast, and on seamounts
	x = c(-74.28487,-73.92323,-73.80753,-72.51728,-71.12418,
		  -69.81176,-69.90715,-70.43201,-70.17135,-69.43912,-65.49608)
	y = c(39.36714,39.98515,40.40316,40.79654,41.49872,41.62076,
		  41.99805,42.68061,43.40714,43.81499,43.36471)
	pts.coast = data.frame(x,y, freq.a, freq.b, col.a, col.b)

	x = c(-66.01404,-65.47260,-63.75456,-63.26082,-62.12838,
	      -60.46885,-59.96952,-56.90925,-52.20397,-51.32288,-50.72461)
	y = c(39.70769,39.39064,38.83020,38.56479,38.01881,38.95405,
	      37.55675,34.62617,36.15592,36.38992,35.91779)
	pts.smt = data.frame(x,y, freq.a, freq.b, col.a, col.b)

# prepare the plot
	data(nw.atlantic) ; atl <- as.bathy(nw.atlantic)
	plot(atl, deep=-8000, shallow=0, step=1000,col="grey")
	points(pts.coast,pch=19,col="blue", cex=0.5)
	points(pts.smt,pch=19,col="blue", cex=0.5)

# automatic placement of piecharts with space.pies
	space.pies(pts.coast[,1], pts.coast[,2], 
	           pie.slices=pts.coast[,3:4], pie.colors=pts.coast[,5:6], pie.radius=0.5)
	space.pies(pts.smt[,1], pts.smt[,2], 
	           pie.slices=pts.smt[,3:4], pie.colors=pts.coast[,5:6], pie.radius=0.5)	
}
