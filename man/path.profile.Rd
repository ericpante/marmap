\name{path.profile}
\alias{path.profile}
\title{
Geographic coordinates, kilometric distance and depth along a path 
}
\description{
Computes and plots the depth/altitude along a transect or path
}
\usage{
path.profile(path,bathy,plot=FALSE, \dots)
}
\arguments{
  \item{path}{
2-columns matrix of longitude and latitude as obtained from \code{lc.dist} with argument \code{dist=TRUE}.
}
  \item{bathy}{
bathymetric data matrix of class \code{bathy}.
}
  \item{plot}{
logical. Should the depth profile be plotted?
}
  \item{\dots}{
when \code{plot=TRUE}, other arguments to be passed to \code{plotProfile}, such as \link{graphical parameters} (see \code{\link{par}} and \code{\link{plotProfile}}).
}
}
\value{
a four-columns matrix containing longitude, latitude, kilometric distance from the start of a route and depth for a set of points along a route. Optionally (i.e. when \code{plot=TRUE}) a bivariate plot of depth against the kilometric distance from the starting point of a transect or least cost path.
}
\author{
Benoit Simon-Bouhet
}
\seealso{
\code{\link{plotProfile}}
}
\examples{
# Loading an object of class bathy and a data.frame of locations
	require(mapdata)
	data(hawaii)
	data(hawaii.sites)

# Preparing a color palette for the bathymetric map
	pal <- colorRampPalette(c("black","darkblue","blue","lightblue"))

# Plotting the bathymetric data and the path between locations
# (the path starts on location 1)
	plot(hawaii,image=TRUE,bpal=pal(100),col="grey40",lwd=.7, 
	     main="Bathymetric map of Hawaii")
	map("worldHires",res=0,fill=TRUE,col=rgb(.8,.95,.8,.7),add=TRUE)
	lines(hawaii.sites,type="o",lty=2,lwd=2,pch=21,
	      col="yellow",bg=col2alpha("yellow",.9),cex=1.2)
	text(hawaii.sites[,1], hawaii.sites[,2], 
	     lab=rownames(hawaii.sites),pos=c(3,3,4,4,1,2),col="yellow")

# Computing and plotting the depth profile for this path
	profile <- path.profile(hawaii.sites,hawaii,plot=TRUE, 
	                        main="Depth profile along the path\nconnecting the 6 sites")
	summary(profile)

}
