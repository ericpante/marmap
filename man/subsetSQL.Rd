\name{subsetSQL}
\alias{subsetSQL}
\alias{setSQL}
\title{
Creating and querying local SQL database for bathymetric data
}
\description{
\code{subsetSQL} queries the local SQL database created with \code{setSQL} to extract smaller data subsets.
}
\usage{
setSQL(bathy, sep="\t", header=TRUE)
subsetSQL(min_lon, max_lon, min_lat, max_lat)
}
\arguments{
  \item{bathy}{
A text file containthree-column table with longitude (x), latitude (y) and depth (z) (no default)
}
  \item{sep}{
character separating columns
}
\item{header}{
whether this table has a row of column names (default = TRUE)
}
  \item{min_lon}{
minimum longitude of the data to be extracted from the local SQL database
}
  \item{max_lon}{
maximum longitude of the data to be extracted from the local SQL database
}
  \item{min_lat}{
minimum latitude of the data to be extracted from the local SQL database
}
  \item{max_lat}{
maximum latitude of the data to be extracted from the local SQL database
}
}
\details{
Functions \code{setSQL} and \code{subsetSQL} were built to work together. \code{setSQL} builds an SQL database and saves it on disk. \code{subsetSQL} queries that local database and the fields \code{min_lon}, \code{max_lon}, etc, are used to extract a subset of the database. The functions were built as two entities so that multiple queries can be done multiple times, without re-building the database each time. These functions were designed to access the very large (>5Go) ETOPO1 file that can be downloaded from the NOAA website (\url{http://www.ngdc.noaa.gov/mgg/global/global.html})
}
\value{
\code{setSQL} returns \code{TRUE} is the database was successfully created. \code{subsetSQL} returns a matrix of class \code{bathy} that can directly be used with \code{plot.bathy}. 
}
\references{
Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009. \url{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/docs/ETOPO1.pdf}
}
\author{
Eric Pante
}
\note{
There must be no database file called \code{bathy_db} in the working directory prior to running \code{setSQL}. 
}
\examples{
# load NW Atlantic data
data(nw.atlantic)

# write example file to disk
write.table(nw.atlantic, "NW_Atlantic.csv", sep=",", quote=FALSE, row.names=FALSE)

\dontrun{
# prepare SQL database
setSQL(bathy="NW_Atlantic.csv", sep=",")

# uses data from the newly-created SQL database:
subsetSQL(min_lon=-70,max_lon=-50,
             min_lat=35, max_lat=41) -> test

# visualize the results (of class bathy)
summary(test)

# remove temporary database and CSV files
system("rm bathy_db") # remove file, for unix-like systems
system("rm NW_Atlantic.csv") # remove file, for unix-like systems
}
}
