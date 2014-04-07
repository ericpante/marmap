\name{plotProfile}
\alias{plotProfile}
\title{
Ploting bathymetric data along a transect or path 
}
\description{
Plots the depth/altitude along a transect or path
}
\usage{
plotProfile(profile,shadow=TRUE,xlim,ylim,col.sea,col.bottom,xlab,ylab, \dots)
}
\arguments{
  \item{profile}{
4-columns matrix obtained from \code{get.transect} with argument \code{dist=TRUE}, or from \code{path.profile}.
}
  \item{shadow}{
logical. Should the depth profile cast a shadow over the plot background?
}
  \item{xlim, ylim}{
numeric vectors of length 2, giving the x and y coordinates ranges. If unspecified, \code{xlim} values are based on the length of the transect or path and \code{ylim} values are based on the depth range of the bathymetric matrix \code{bathy}.
}
  \item{col.sea}{
color for the sea area of the plot. Defaults to \code{rgb(130/255,180/255,212/255)}
}
  \item{col.bottom}{
color for the bottom area of the plot. Defaults to \code{rgb(198/255,184/255,151/255)}
}
  \item{xlab, ylab}{
titles for the x and y axes. If unspecified, \code{xlab="Distance from start of transect (km)"} and \code{ylab="Depth (m)"}
}
  \item{\dots}{
arguments to be passed to methods, such as \link{graphical parameters} (see \code{\link{par}})
}
}
\value{
a bivariate plot of depth against the kilometric distance from the starting point of a transect or least cost path.
}
\author{
Benoit Simon-Bouhet
}
\note{
\code{path.profile} with argument \code{plot} set to \code{TRUE} plots depth profiles with default values for all arguments of \code{plotProfile}.
}
\seealso{
\code{\link{path.profile}}, \code{\link{plot.bathy}}
}
\examples{

# Example 1:
	data(celt)
	layout(matrix(1:2,nc=1),height=c(2,1))
	par(mar=c(4,4,1,1))
	plot(celt,n=40,draw=TRUE)
	points(c(-6.34,-5.52),c(52.14,50.29),type="o",col=2)
	
	tr <- get.transect(celt, x1 = -6.34, y1 = 52.14, x2 = -5.52, y2 = 50.29, distance = TRUE)
	plotProfile(tr)

# Example 2: 
	layout(matrix(1:2,nc=1),height=c(2,1))
	par(mar=c(4,4,1,1))
	plot(celt,n=40,draw=TRUE)
	points(c(-5,-6.34),c(49.8,52.14),type="o",col=2)
	
	tr2 <- get.transect(celt, x1 = -5, y1 = 49.8, x2 = -6.34, y2 = 52.14, distance = TRUE)
	plotProfile(tr2)

# Example 3: click several times on the map and press ESC
\dontrun{
	layout(matrix(1:2,nc=1),height=c(2,1))
	par(mar=c(4,4,1,1))
	data(florida)
	plot(florida,image=TRUE,dra=TRUE,land=TRUE,n=40)
	
	out <- path.profile(as.data.frame(locator(type="o",col=2,pch=19,cex=.8)),florida)
	plotProfile(out)
}
}

