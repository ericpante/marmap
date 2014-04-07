\name{col2alpha}
\alias{col2alpha}
\title{
Adds alpha transparency to a (vector of) color(s)
}
\description{
Adds transparency to a color or a vector of colors by specifying one or several alpha values.
}
\usage{
col2alpha(color,alpha = 0.5)
}
\arguments{
  \item{color}{
a (vector of) color codes or names
}
  \item{alpha}{
a value (or vector of values) between 0 (full transparency) and 1 (no transparency).
}
}
\details{
When the size of \code{color} and \code{alpha} vectors are different, \code{alpha} values are recycled.
}
\value{
A (vector) of color code(s).
}
\author{
Benoit Simon-Bouhet
}
\examples{
# Generate random data
dat <- rnorm(4000)

# plot with plain color for points
plot(dat,pch=19,col="red")

# Add some transparency to get a better idea of density
plot(dat,pch=19,col=col2alpha("red",.3))

# Same color for all points but with increasing alpha (decreasing transparency)
plot(dat,pch=19,col=col2alpha(rep("red",4000),seq(0,1,len=4000)))

# Two colors, same alpha
plot(dat,pch=19,col=col2alpha(rep(c("red","purple"),each=2000),.2))

# Four colors, gradient of transparency for each color
plot(dat,pch=19,col=col2alpha(rep(c("blue","purple","red","orange"),each=1000),seq(.1,.6,len=1000)))

# Alpha transparency applied to a gradient of colors
plot(dat,pch=19,col=col2alpha(rainbow(4000),.5))

}
