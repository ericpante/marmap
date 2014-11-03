setSQL = function(bathy){

	# prepare ("connect") SQL database
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "bathy_db")
	# data frame -> database table.
	DBI::dbWriteTable(con, name="bathy_db", value=bathy, header=TRUE)

}