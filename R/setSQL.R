setSQL = function(bathy, header = TRUE, sep = ","){

	# prepare ("connect") SQL database
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "bathy_db")
	# data frame -> database table.
	DBI::dbWriteTable(con, name="bathy_db", value=bathy, header=header, sep=sep)

}