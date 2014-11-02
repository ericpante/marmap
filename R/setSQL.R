setSQL = function(bathy, sep="\t", header=TRUE){

	# prepare ("connect") SQL database
	con <- DBI::dbConnect(SQLite(), dbname = "bathy_db")
	# data frame -> database table.
	DBI::dbWriteTable(con, name="bathy_db", value=bathy, header=T)

}