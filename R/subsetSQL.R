subsetSQL = function(min_lon, max_lon, min_lat, max_lat){

	# prepare ("connect") SQL database
	con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "bathy_db")

	cn <- DBI::dbListFields(con,"bathy_db")

	# build SQL request: 
    paste("SELECT * from bathy_db where ", cn[1], " >", min_lon,
    	  "and ", cn[1], " <", max_lon," and ", cn[2], " >",min_lat," and ", cn[2], " <",max_lat) -> REQUEST
    
    # send request and retrieve results
    res <- DBI::dbSendQuery(con, REQUEST)
    data <- DBI::fetch(res, n = -1)
	DBI::dbClearResult(res)
	DBI::dbDisconnect(con)
    return(as.bathy(data))
}