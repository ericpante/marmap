\name{trans.mat}
\alias{trans.mat}
\title{
Transition matrix
}
\description{
Creates a transition object to be used by \code{lc.dist} to compute least cost distances between locations.
}
\usage{
trans.mat(bathy,min.depth=0,max.depth=NULL)
}
\arguments{
  \item{bathy}{
A matrix of class \code{bathy}.
}
  \item{min.depth, max.depth}{
Numeric. The range of depth between which the path will be possible. The default (\code{min.depth=0} and max.depth=NULL) indicates that the transition between cells of the grid is possible between 0 meters depth and the maximum depth of \code{bathy}. See details
}
}
\details{
\code{trans.mat} creates a transition object usable by \code{lc.dist} to computes least cost distances between a set of locations. \code{trans.mat} rely on the function \code{raster} from package \code{raster} (Hijmans & van Etten, 2012. \url{http://CRAN.R-project.org/package=raster}) and on \code{transition} from package \code{gdistance} (van Etten, 2011. \url{http://CRAN.R-project.org/package=gdistance}).

The transition object contains the probability of transition from one cell of a bathymetric grid to adjacent cells and depends on user defined parameters. \code{trans.mat} is especially usefull when least cost distances need to be calculated between several locations at sea. The default values for \code{min.depth} and \code{max.depth} ensure that the path computed by \code{lc.dist} will be the shortest path possible at sea avoiding land masses. The path can be constrained to a given depth range by setting manually \code{min.depth} and \code{max.depth}. For instance, it is possible to limit the possible paths to the continental shelf by setting \code{max.depth=-200}. Inaccuracies of the bathymetric data can occasionally result in paths crossing land masses. Setting \code{min.depth} to low negative values (e.g. -10 meters) can limit this problem.

\code{trans.mat} takes also advantage of the function \code{geoCorrection} from package \code{gdistance} (van Etten, 2012. \url{http://CRAN.R-project.org/package=gdistance}) to take into account map distortions over large areas.
}
\section{Warning }{
Please be aware that the use of \code{trans.mat} can be time consumming for large bathymetric datasets. The function takes about one minute to compute a transition matrix for the \code{hawaii} bathymetric data (bathymetric data of class \code{bathy} with 599 rows and 419 columns, see \code{\link{hawaii}}) on a MacBook Pro with a 2.66 GHz Intel Core i7 processor and 4 Go of RAM.
}
\value{
A transition object.
}
\references{
Jacob van Etten (2011). gdistance: distances and routes on geographical grids. R package version 1.1-2.  \url{http://CRAN.R-project.org/package=gdistance}
Robert J. Hijmans & Jacob van Etten (2012). raster: Geographic analysis and modeling with raster data. R package version 1.9-92. \url{http://CRAN.R-project.org/package=raster}
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{lc.dist}}, \code{\link{hawaii}}
}
\examples{
# Load and plot bathymetry
data(hawaii)
summary(hawaii)
plot(hawaii)

\dontrun{
# Compute transition object with no depth constraint
trans1 <- trans.mat(hawaii)

# Compute transition object with minimum depth constraint: 
# path impossible in waters shallower than -200 meters depth
trans2 <- trans.mat(hawaii,min.depth=-200)

# Visualizing results
par(mfrow=c(1,2))
plot(raster(trans1), main="No depth constraint")
plot(raster(trans2), main="Constraint in shallow waters")
}
}
