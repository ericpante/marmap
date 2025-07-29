\name{metallo}
\alias{metallo}
\title{
Coral sampling information from the North West Atlantic
}
\description{
Coral sampling data from Thoma et al 2009 (MEPS)
}
\usage{
data(nw.atlantic)
}
\details{
Sampling locations (longitude, latitude, depth in meters) for the deep-sea octocoral species Metallogorgia melanotrichos (see Thoma et al 2009 for details, including cruise information)
}
\value{
A 3-column data frame
}
\references{
Thoma, J. N., E. Pante, M. R. Brugler, and S. C. France. 2009. Deep-sea octocorals and antipatharians show no evidence of seamount-scale endemism in the NW Atlantic. Marine Ecology Progress Series 397:25-35. \doi{https://doi.org/10.3354/meps08318}
}
\seealso{
\code{\link{nw.atlantic}}
}
\examples{
# load NW Atlantic data and convert to class bathy
data(nw.atlantic,metallo)
atl <- as.bathy(nw.atlantic)

## the function plot below plots:
## - the coastline in blue,
## - isobaths between 8000-4000 in light grey,
## - isobaths between 4000-500 in dark grey (to emphasize seamounts)

# 1st example: function points uses first two columns ; 3rd column contains depth info
plot(atl, deep=c(-8000,-4000,0), shallow=c(-4000,-500,0), step=c(500,500,0),
 	 lwd=c(0.5,0.5,1.5),lty=c(1,1,1),
 	 col=c("grey80", "grey20", "blue"),
 	 drawlabels=c(FALSE,FALSE,FALSE) )
points(metallo, cex=1.5, pch=19,col=rgb(0,0,1,0.5))

# 2nd example: plot points according to coordinates
plot(atl, deep=c(-8000,-4000,0), shallow=c(-4000,-500,0), step=c(500,500,0),
 	 lwd=c(0.5,0.5,1.5),lty=c(1,1,1),
 	 col=c("grey80", "grey20", "blue"),
 	 drawlabels=c(FALSE,FALSE,FALSE) )
subset(metallo, metallo$lon>-55) -> s # isolate points from the Corner Rise seamounts:
points(s, cex=1.5, pch=19,col=rgb(0,0,1,0.5)) # only plot those points

# 3rd example: point colors corresponding to a depth gradient:
par(mai=c(1,1,1,1.5))
plot(atl, deep=c(-6500,0), shallow=c(-50,0), step=c(500,0),
     lwd=c(0.3,1), lty=c(1,1),
     col=c("black","black"),
     drawlabels=c(FALSE,FALSE,FALSE))

max(metallo$depth, na.rm=TRUE) -> mx
colorRamp(c("white","lightyellow","lightgreen","blue","lightblue1","purple")) -> ramp
rgb( ramp(seq(0, 1, length = mx)), max = 255) -> blues

points(metallo, col="black", bg=blues[metallo$depth], pch=21,cex=1.5)
require(shape); colorlegend(zlim=c(-mx,0), col=rev(blues), main="depth (m)",posx=c(0.85,0.88))

}
