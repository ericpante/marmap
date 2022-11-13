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
setSQL(bathy, header = TRUE, sep = ",", db.name = "bathy_db")
subsetSQL(min_lon, max_lon, min_lat, max_lat, db.name = "bathy_db")
}
\arguments{
  \item{bathy}{
A text file containing a comma-separated, three-column table with longitude, latitude and depth data (no default)
}
  \item{header}{
does the xyz file contains a row of column names (default = TRUE)
}
  \item{sep}{
character separating columns in the xyz file, (default=",")
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
  \item{db.name}{
The name of (or path to) the SQL database to be created on disk by \code{setSQL} or from which \code{subsetSQL} will extract data ("bathy_db" by default)
}
}
\details{
Functions \code{setSQL} and \code{subsetSQL} were built to work together. \code{setSQL} builds an SQL database and saves it on disk. \code{subsetSQL} queries that local database and the fields \code{min_lon}, \code{max_lon}, etc, are used to extract a subset of the database. The functions were built as two entities so that multiple queries can be done multiple times, without re-building the database each time. These functions were designed to access the very large (>5Go) ETOPO 2022 file that can be downloaded from the NOAA website (\url{https://www.ngdc.noaa.gov/mgg/global/global.html})
}
\value{
\code{setSQL} returns \code{TRUE} if the database was successfully created. \code{subsetSQL} returns a matrix of class \code{bathy} that can directly be used with \code{plot.bathy}.
}
\references{
NOAA National Centers for Environmental Information. 2022: ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information. \doi{doi.org/10.25921/fd45-gt74}
}
\author{
Eric Pante
}
\note{
If unspecified, \code{db.name} is set to "bathy_db" by default. Thus, theere must be no database file called \code{bathy_db} in the working directory prior to running \code{setSQL} unless a different name is used for the new database.
Make sure that your "bathy" input is a xyz text file (for function \code{setSQL}) with 3 columns containing longitude, latitude and depth data, in that order.
\code{setSQL} and \code{subsetSQL} were modified on Nov. 2, 2014 to comply with RSQLite 1.0.0.
}
\examples{
\dontrun{
# load NW Atlantic data
data(nw.atlantic)

# write data to disk as a comma-separated text file
write.table(nw.atlantic, "NW_Atlantic.csv", sep=",", quote=FALSE, row.names=FALSE)

# prepare SQL database
setSQL(bathy="NW_Atlantic.csv")

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
