\name{getBSHC.bathy}
\alias{getBSHC.bathy}
\title{
Import bathymetric data from the Baltic Sea Bathymetry Database (BSHC) server}
\description{
Imports bathymetric data from the BSHC server, given coordinate bounds and resolution. 
}
\usage{
getBSHC.bathy(lon1,lon2,lat1,lat2, keep=FALSE)
}
\arguments{
  \item{lon1}{
first longitude of the area for which bathymetric data will be downloaded 
}
  \item{lon2}{
second longitude of the area for which bathymetric data will be downloaded 
}
  \item{lat1}{
first latitude of the area for which bathymetric data will be downloaded 
}
  \item{lat2}{
second latitude of the area for which bathymetric data will be downloaded 
}
  \item{keep}{
whether to write the data downloaded from NOAA into a file (default is FALSE)
}
}
\details{
\code{getBSHC.bathy} queries the Baltic Sea Bathymetry Database, given the coordinates of the area of interest, and within the bounds of the database. Information about the database can be found here: \url{http://data.bshc.pro/about/}; terms and conditions are here: \url{http://data.bshc.pro/legal/}. 

Users have the option of directly writing the downloaded data into a file (\code{keep=TRUE} argument ; see below). If an identical query is performed (i.e. using identical lat-long and resolution), \code{getBSHC.bathy} will load data from the file previously written to the disk instead of querying the BSHC database. This behavior should be used preferentially (1) to reduce the number of uncessary queries to the BSHC website, and (2) to reduce data load time. If the user wants to make multiple, identical queries to the BSHC website without loading the data written to disk, the data file name must be modified by the user. Alternatively, the data file can be moved outside of the present working directory.

Credits (copied from the BSHC website): "The shaded bathymetry relief outside the Baltic Sea is based on data from the General Bathymetric Chart of the Oceans:
'The GEBCO_08 Grid, version 20100927, \url{http://www.gebco.net/}'. The land elevations displayed on this web site are derived from the US Geological Survey GMTED2010 dataset (Danielson and Gesch 2011). The BSBD project is co-financed by the European Union Trans-European Transport Network (TEN-T)."

}
\value{
The output of \code{getBSHC.bathy} is a matrix of class \code{bathy}. The class \code{bathy} has its own methods for summarizing and ploting the data. If \code{keep=TRUE}, a csv file containing the downloaded data is produced. This file is named using the following format: 'marmap_BSHCcoord_COORDINATES.csv' (COORDINATES separated by semicolons). 
}
\references{
Per the BSHC website: "When you use or re-distribute BSBD data, or products derived from it, please use the following attribution: 'Baltic Sea Hydrographic Commission, 2013, Baltic Sea Bathymetry Database version 0.9.3. Downloaded from http://data.bshc.pro/ on download date'. When including BSBD data in a web map, please put the copyright notice "© BSHC" in the map, linked to http://data.bshc.pro/."

Danielson, J.J., and Gesch, D.B., 2011, Global multi-resolution terrain elevation data 2010 (GMTED2010): U.S. Geological Survey Open-File Report 2011–1073, 26 p., \url{http://pubs.usgs.gov/of/2011/1073/pdf/of2011-1073.pdf}

Hell B, Broman B, Jakobsson L, Jakobsson M, Magnusson A, Wiberg P. 2012. The use of bathymetric data in society and science: a review from the baltic sea. Ambio. 41:138–50. \url{http://data.bshc.pro/media/Hell_et_al_2011_Data_Use.pdf} 
}
\author{
Eric Pante and Benoit Simon-Bouhet
}
\seealso{
\code{\link{getNOAA.bathy}}, \code{\link{read.bathy}}, \code{\link{readGEBCO.bathy}}, \code{\link{plot.bathy}}
}
\examples{
\dontrun{
# you must have an internet connection.
getBSHC.bathy(lon1=-20,lon2=-90,lat1=50,lat2=20)
}
}