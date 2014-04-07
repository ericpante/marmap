subsetSQL = function(min_lon, max_lon, min_lat, max_lat){

	# prepare SQL database
	# require(RSQLite)
	con <- RSQLite::dbConnect("SQLite", dbname = "bathy_db")

    # build SQL request: 
    # paste("SELECT * from bathy_data where lon >",min_lon,
    # 	  "and lon <",max_lon," and lat>",min_lat," and lat <",max_lat,
    # 	  "and depth >",min_depth,"and depth <",max_depth) -> REQUEST       

    paste("SELECT * from bathy_data where lon >",min_lon,
    	  "and lon <",max_lon," and lat>",min_lat," and lat <",max_lat) -> REQUEST
    
    # send request and retrieve results
    res <- RSQLite::dbSendQuery(con, REQUEST)
    data <- RSQLite::fetch(res, n = -1)
	RSQLite::dbClearResult(res)
	RSQLite::dbDisconnect(con)
    return(as.bathy(data))
}