subsetSQL = function(min_lon, max_lon, min_lat, max_lat){

	# prepare ("connect") SQL database
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "bathy_db")

	# build SQL request: 
    paste("SELECT * from bathy_db where lon >",min_lon,
    	  "and lon <",max_lon," and lat >",min_lat," and lat <",max_lat) -> REQUEST
    
    # send request and retrieve results
    res <- DBI::dbSendQuery(con, REQUEST)
    data <- DBI::fetch(res, n = -1)
	DBI::dbClearResult(res)
	DBI::dbDisconnect(con)
    return(as.bathy(data))
}