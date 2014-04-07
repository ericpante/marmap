\name{getNOAA.bathy}
\alias{getNOAA.bathy}
\title{
Import bathymetric data from the NOAA server}
\description{
Imports bathymetric data from the NOAA server, given coordinate bounds and resolution. 
}
\usage{
getNOAA.bathy(lon1,lon2,lat1,lat2, resolution = 4, keep=FALSE, antimeridian=FALSE)
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
  \item{resolution}{
resolution of the grid, in minutes (default is 4)
}
  \item{keep}{
whether to write the data downloaded from NOAA into a file (default is FALSE)
}
  \item{antimeridian}{
whether the area should include the antimeridian (longitude 180 or -180). See details.
}
}
\details{
\code{getNOAA.bathy} queries the ETOPO1 database hosted on the NOAA website, given the coordinates of the area of interest and desired resolution. Users have the option of directly writing the downloaded data into a file (\code{keep=TRUE} argument ; see below). If an identical query is performed (i.e. using identical lat-long and resolution), \code{getNOAA.bathy} will load data from the file previously written to the disk instead of querying the NOAA database. This behavior should be used preferentially (1) to reduce the number of uncessary queries to the NOAA website, and (2) to reduce data load time. If the user wants to make multiple, identical queries to the NOAA website without loading the data written to disk, the data file name must be modified by the user. Alternatively, the data file can be moved outside of the present working directory.

\code{getNOAA.bathy} allows users to download bathymetric data in the antimeridian region when \code{antimeridian=TRUE}. The antimeridian is the 180th meridian and is located about in the middle of the Pacific Ocean, east of New Zealand and Fidji, west of Hawaii and Tonga. For a given pair of longitude values, e.g. -150 (150 degrees West) and 150 (degrees East), you have the possibility to get data for 2 distinct regions: the area centered on the antimeridian (60 degrees wide, when \code{antimeridian=TRUE}) or the area centered on the prime meridian (300 degrees wide, when \code{antimeridian=FALSE}). It is recommended to use \code{keep=TRUE} in combination with \code{antimeridian=TRUE} since gathering data for an area around the antimeridian requires two distinct queries to NOAA servers.
}
\value{
The output of \code{getNOAA.bathy} is a matrix of class \code{bathy}, which dimensions depends on the resolution of the grid uploaded from the NOAA server. The class \code{bathy} has its own methods for summarizing and ploting the data. If \code{keep=TRUE}, a csv file containing the downloaded data is produced. This file is named using the following format: 'marmap_coord_COORDINATES_res_RESOLUTION.csv' (COORDINATES separated by semicolons, and the RESOLUTION in degrees). 
}
\references{
Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009. \url{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/docs/ETOPO1.pdf}
}
\author{
Eric Pante and Benoit Simon-Bouhet
}
\seealso{
\code{\link{read.bathy}}, \code{\link{readGEBCO.bathy}}, \code{\link{plot.bathy}}
}
\examples{
\dontrun{
# you must have an internet connection. This line queries the NOAA ETOPO1 database 
# for data from North Atlantic, for a resolution of 10 minutes. 

getNOAA.bathy(lon1=-20,lon2=-90,lat1=50,lat2=20, resolution=10) -> a
plot(a, image=TRUE, deep=-6000, shallow=0, step=1000)

# download speed for a matrix of 10 degrees x 10 degrees x 30 minutes
system.time(getNOAA.bathy(lon1=0,lon2=10,lat1=0,lat2=10, resolution=30))
}
}