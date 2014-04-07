\name{readGEBCO.bathy}
\alias{readGEBCO.bathy}
\title{
Read bathymetric data from a GEBCO file}
\description{
Imports 30-sec and 1-min bathymetric data from a .nc file downloaded on the GEBCO website. 
}
\usage{
readGEBCO.bathy(file, db = "GEBCO_1min", resolution = 1)
}
\arguments{
  \item{file}{
name of the \code{.nc} file 
}
  \item{db}{
type of GEBCO database queried, either GEBCO_1min or GEBCO_08 (default is GEBCO_1min) 
}
  \item{resolution}{
resolution of the grid, in units of the selected database (default is 1; see details)
}
}
\details{
\code{readGEBCO.bathy} reads a 30 arcseconds or 1 arcminute bathymetry file downloaded from the GEBCO (General Bathymetric Chart of the Oceans) website (British Oceanographic Data Center). The website allows the download of bathymetric data in the netCDF format. \code{readGEBCO.bathy} uses the \code{ncdf} package to load the data into R, and parses it into an object of class \code{bathy}. 

The argument \code{db} specifies whether data was downloaded from the 30 arcseconds database (GEBCO_08) or the 1 arcminute database (GEBCO_1min, the default). A third database type, GEBCO_08 SID, is available from the website. This database includes a source identifier specifying which grid cells have depth information based on soundings ; it does not include bathymetry or topography data. \code{readGEBCO.bathy} can read this type of database with \code{db = "GEBCO_08"}, and only the SID information will be included in the object of class \code{bathy}. Therefore, to display a map with both the bathymetry and the SID information, you will have to download both datasets from GEBCO, and import and plot both independently. 

The argument \code{resolution} specifies the resolution of the object of class \code{bathy}. Because the resolution of GEBCO data is rather fine, we offer the possibility of downsizing the dataset with \code{resolution}. \code{resolution} is in units of the selected database: in "GEBCO_1min", \code{resolution} is in minutes; in "GEBCO_08", \code{resolution} is in 30 arcseconds (that is, \code{resolution = 3} corresponds to 3x30sec, or 1.5 arcminute).

About warnings: sometimes, a GEBCO-generated nc file will have fewer z values than expected given the dimention queried on the server. For example, if we asked for a region including 281 latitude values and 391 longitude values, there should be 281x391=109871 cells, but might in reality get fewer cells, for example 280x390=109200 cells. We think that this is due to the way the GEBCO server extracts the data based on your query. To deal with it, we remove the last latitude and longitude values of the matrix so that the number of cells and the number of latitude x longitude values match exactly. When this happens, \code{readGEBCO.bathy} sends a warning message to let you know of the situation. 
}
\value{
The output of \code{readGEBCO.bathy} is a matrix of class \code{bathy}, which dimensions depends on the resolution specified (one-minute, the original GEBCO resolution, is the default). The class \code{bathy} has its own methods for summarizing and ploting the data. 
}
\references{
British Oceanographic Data Center: General Bathymetric Chart of the Oceans gridded bathymetric data sets (accessed Oct 5, 2013) \url{http://www.bodc.ac.uk/data/online_delivery/gebco/}

General Bathymetric Chart of the Oceans website (accessed Oct 5, 2013) \url{http://www.gebco.net}

David Pierce (2011). ncdf: Interface to Unidata netCDF data files. R package version 1.6.6. http://CRAN.R-project.org/package=ncdf
}
\author{
Eric Pante and Benoit Simon-Bouhet
}
\seealso{
\code{\link{getNOAA.bathy}}, \code{\link{read.bathy}}, \code{\link{plot.bathy}}
}
\examples{
\dontrun{
# This example will not run, and we do not provide the dummy "gebco_file.nc" file, 
# because a copyright license must be signed on the GEBCO website before the data can be 
# downloaded and used. We just provide this line as an example for synthax. 
  readGEBCO.bathy(file="gebco_file.nc", db="GEBCO_1min", resolution=1) -> nw.atl

# Second not-run example, with GEBCO_08 and SID: 
  readGEBCO.bathy("gebco_08_7_38_10_43_corsica.nc", db="GEBCO_08") -> med
  summary(med) # the bathymetry data

  readGEBCO.bathy("gebco_SID_7_38_10_43_corsica.nc", db="GEBCO_08")-> sid
  summary(sid) # the SID data

  colorRampPalette(c("lightblue","cadetblue1","white")) -> blues # custom col palette
  plot(med, n=1, im=T, bpal=blues(100)) # bathymetry

  as.numeric(rownames(sid)) -> x.sid
  as.numeric(colnames(sid)) -> y.sid
  contour(x.sid, y.sid, sid, drawlabels=FALSE, lwd=.1, add=TRUE) # SID
}
}