\name{palette.bathy}
\alias{palette.bathy}
\title{
Build a bathymetry- and/or topography-constrained color palette}
\description{
Build a constrained color palette based on depth / altitude bounds and given colors. 
}
\usage{
palette.bathy(mat, deep, shallow, palcol, land=FALSE, default.col="white")
}
\arguments{
  \item{mat}{
a matrix of bathymetric data, class bathy not required.
}
  \item{deep}{
a vector of the deepest depth(s) to assign colors to.
}
  \item{shallow}{
a vector of the shallowest depth(s) to assign colors to.
}
  \item{palcol}{
a list of color groups.
}
  \item{land}{
logical. Wether to consider land or not (\code{default} is \code{FALSE})
}
  \item{default.col}{
a color for the area of the matrix not bracketed by \code{deep} and \code{shallow}
}
}
\details{
\code{palette.bathy} allows the production of color palettes for specified bathymetric and/or topographic layers. The \code{deep} and \code{shallow} arguments work as in \code{plot.bathy}: these are vectors of one or more depth / altitude values. \code{deep=c(-1000,-100)} and \code{shallow=c(-200,0)} will create a palette with colors within the -1000 to -200 range, and within the -100 and 0 range. Colors are specified with the \code{palcol} argument; this is a list object containing the color vectors to be used for each specified depth region (see exampples below). 
}
\value{
A vector of colors which size depends on the depth / altitude range of the \code{bathy} matrix.
}
\author{
Eric Pante
}
\seealso{
\code{\link{plot.bathy}}
}
\examples{
# load NW Atlantic data and convert to class bathy
	data(nw.atlantic)
	atl <- as.bathy(nw.atlantic)

# creating depth-constrained palette for the ocean only
	palette.bathy(mat=atl, deep=min(atl), shallow=0, land=FALSE, default.col="gray",
     	         palcol=list(grp1=c("purple","blue","lightblue")) ) -> newcol
	plot(atl, land=F,n=10, lwd=0.5, im=T, bpal=newcol)

# creating depth-constrained palette for 3 ocean "layers"
	palette.bathy(mat= atl, deep=c(min(atl),-3000,-150), 
				  shallow=c(-3000,-150,0), land=F, default.col="grey",
             	  palcol=list(grp1=c("purple","blue","grey"), 
                       	      grp2=c("white"),
                              grp3=c("yellow","green","brown")) ) -> newcol
	plot(atl, land=F,n=10, lwd=0.7, im=T, bpal=newcol)

# creating depth-constrained palette for land and ocean
	palette.bathy(mat= atl, deep=c(min(atl),0), shallow=c(0,max(atl)), 
				  land=T, default.col="gray",
    	          palcol=list(grp1=c("purple","blue","lightblue"), 
       		                  grp2=c("gray90","gray10")) ) -> newcol
	plot(atl, land=T,n=10, lwd=0.5, im=T, bpal=newcol)
	
}
