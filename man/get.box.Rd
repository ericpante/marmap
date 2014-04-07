\name{get.box}
\alias{get.box}
\title{
Get bathymetric information of a belt transect
}
\description{
\code{get.box} gets depth information of a belt transect of width \code{width} around a transect defined by two points on a bathymetric map.
}
\usage{
get.box(bathy,x1,x2,y1,y2,width,locator=FALSE,ratio=FALSE, \dots)
}
\arguments{
  \item{bathy}{
Bathymetric data matrix of class \code{bathy}.
}
  \item{x1}{
Numeric. Start longitude of the transect. Requested when \code{locator=FALSE}.
}
  \item{x2}{
Numeric. Stop longitude of the transect. Requested when \code{locator=FALSE}.
}
  \item{y1}{
Numeric. Start latitude of the transect. Requested when \code{locator=FALSE}.
}
  \item{y2}{
Numeric. Stop latitude of the transect. Requested when \code{locator=FALSE}.
}
  \item{width}{
Numeric. Width of the belt transect in degrees. 
}
  \item{locator}{
Logical. Whether to choose transect bounds interactively with a map or not. When \code{FALSE} (default), a bathymetric map  (\code{plot.bathy(bathy,image=TRUE)}) is automatically plotted and the position of the belt transect is added.
}
  \item{ratio}{
Logical. Should aspect ratio for the \code{wireframe} plotting function (package \code{lattice}) be computed (default is \code{FALSE}).
}
  \item{\dots}{
Other arguments to be passed to \code{\link{locator}} and \code{\link{lines}} to specify the characteristics of the points and lines to draw on the bathymetric map for both the transect and the bounding box of belt transect.
}
}
\details{
\code{get.box} allows the user to get depth data for a rectangle area of the map around an approximate linear transect (belt transect). Both the position and size of the belt transect are user defined. The position of the transect can be specified either by inputing start and stop coordinates, or by clicking on a map created with \code{plot.bathy}. In its interactive mode, this function uses the \code{locator} function (\code{\link{graphics}} package) to retrieve and plot the coordinates of the selected transect. The argument \code{width} allows the user to specify the width of the belt transect in degrees. 
}
\value{
A matrix containing depth values for the belt transect. \code{rownames} indicate the kilometric distance from the start of the transect and \code{colnames} indicate the distance form the central transect in degrees.
If \code{ratio=TRUE}, a list of two elements: \code{depth}, a matrix containing depth values for the belt transect similar to the description above and \code{ratios} a vector of length two specifying the ratio between (i) the width and length of the belt transect and (ii) the depth range and the length of the belt transect. These ratios can be used by the function \code{wireframe} to produce realistic 3D bathymetric plots of the selected belt transect.
}
\author{
Benoit Simon-Bouhet and Eric Pante
}
\seealso{
\code{\link{plot.bathy}}, \code{\link{get.transect}}, \code{\link{get.depth}}
}
\examples{

# load and plot bathymetry
	data(hawaii)
	plot(hawaii,im=TRUE)

# get the depth matrix for a belt transect
	depth <- get.box(hawaii,x1=-157,y1=20,x2=-155.5,y2=21,width=0.5,col=2)
	
# plotting a 3D bathymetric map of the belt transect
	require(lattice)
	wireframe(depth,shade=TRUE)
	
# get the depth matrix for a belt transect with realistic aspect ratios
	depth <- get.box(hawaii,x1=-157,y1=20,x2=-155.5,y2=21,width=0.5,col=2,ratio=TRUE)

# plotting a 3D bathymetric map of the belt transect with realistic aspect ratios
	require(lattice)
	wireframe(depth[[1]],shade=TRUE,aspect=depth[[2]])
}
