setSQL = function(bathy, sep="\t", header=TRUE){

	# prepare SQL database
	# require(RSQLite)
	con <- RSQLite::dbConnect("SQLite", dbname = "bathy_db")
	RSQLite::dbWriteTable(con, name="bathy_data", value=bathy, 
             row.names=FALSE, col.names=c("lon","lat","depth"), 
             header=header, sep = sep)

}