\name{lc.dist}
\alias{lc.dist}
\title{
Computes least cost distances between two or more locations
}
\description{
Computes least cost distances between two or more locations
}
\usage{
lc.dist(trans,loc,res=c("dist","path"))
}
\arguments{
  \item{trans}{
transition object as computed by \code{trans.mat}
}
  \item{loc}{
A two-columns matrix or data.frame containing latitude and longitude for 2 or more locations.
}
  \item{res}{
either \code{"dist"} or \code{"path"}. See details.
}
}
\details{
\code{lc.dist} computes least cost distances between 2 or more locations. This function relies on the package \code{gdistance} (van Etten, 2011. \url{http://CRAN.R-project.org/package=gdistance}) and on the \code{trans.mat} function to define a range of depths where the paths are possible.
}
\value{
Results can be presented either as a kilometric distance matrix between all possible pairs of locations (argument \code{res="dist"}) or as a list of paths (i.e. 2-columns matrices of routes) between pairs of locations (\code{res="path"}).
}
\references{
Jacob van Etten (2011). gdistance: distances and routes on geographical grids. R package version 1.1-2.  \url{http://CRAN.R-project.org/package=gdistance}
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{trans.mat}}
}
\examples{
# Load and plot bathymetry
	data(hawaii)
	pal <- colorRampPalette(c("black","darkblue","blue","lightblue"))
	plot(hawaii,image=TRUE,bpal=pal(100),asp=1,col="grey40",lwd=.7,
	     main="Bathymetric map of Hawaii")

# Load and plot several locations
	data(hawaii.sites)
	sites <- hawaii.sites[-c(1,4),]
	rownames(sites) <- 1:4
	points(sites,pch=21,col="yellow",bg=col2alpha("yellow",.9),cex=1.2)
	text(sites[,1],sites[,2],lab=rownames(sites),pos=c(3,4,1,2),col="yellow")

\dontrun{
# Compute transition object with no depth constraint
	trans1 <- trans.mat(hawaii)

# Compute transition object with minimum depth constraint: 
# path impossible in waters shallower than -200 meters depth
	trans2 <- trans.mat(hawaii,min.depth=-200)

# Computes least cost distances for both transition matrix and plots the results on the map
	out1 <- lc.dist(trans1,sites,res="path")
	out2 <- lc.dist(trans2,sites,res="path")
	lapply(out1,lines,col="yellow",lwd=4,lty=1) # No depth constraint (yellow paths)
	lapply(out2,lines,col="red",lwd=1,lty=1) # Min depth set to -200 meters (red paths)

# Computes and display distance matrices for both situations
	dist1 <- lc.dist(trans1,sites,res="dist")
	dist2 <- lc.dist(trans2,sites,res="dist")
	dist1
	dist2

# plots the depth profile between location 1 and 3 in the two situations
	dev.new()
	par(mfrow=c(2,1))
	path.profile(out1[[2]],hawaii,pl=TRUE,
                 main="Path between locations 1 & 3\nProfile with no depth constraint")
	path.profile(out2[[2]],hawaii,pl=TRUE,
                 main="Path between locations 1 & 3\nProfile with min depth set to -200m")
}
}
