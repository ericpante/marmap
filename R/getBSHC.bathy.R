getBSHC.bathy <- function (lon1, lon2, lat1, lat2, keep = FALSE) 
{
    x1 = x2 = y1 = y2 = NULL
    if (lon1 < lon2) {
        x1 <- lon1
        x2 <- lon2
    }
    else {
        x2 <- lon1
        x1 <- lon2
    }
    if (lat1 < lat2) {
        y1 <- lat1
        y2 <- lat2
    }
    else {
        y2 <- lat1
        y1 <- lat2
    }

    fetch <- function(x1, y1, x2, y2, res) {
        WEB.REQUEST <- paste("http://data.bshc.pro/ogc/bsbd-0.9.3?SERVICE=wcs&VERSION=1.0.0&REQUEST=GetCoverage&coverage=bsbd&CRS=EPSG:4326&bbox=", y1, ",", x1, ",", y2, ",", x2,"&width=478&height=315&format=XYZ", sep = "")
        dat <- suppressWarnings(try(read.table(WEB.REQUEST, sep=" "), 
            silent = TRUE))
        return(dat)
    }
    
        FILE <- paste("marmap_ BSHCcoord_", x1, ":", y1, ":", x2, 
            ":", y2,".csv", sep = "")
    
    if (FILE %in% list.files()) {
        cat("File already exists ; loading '", FILE, "'", sep = "")
        exisiting.bathy <- read.bathy(FILE, header = T)
        return(exisiting.bathy)
    }
    else {
		cat("Querying BSHC database ...\n")
		cat("This may take seconds to minutes, depending on grid size\n")
		bath <- fetch(x1, y1, x2, y2, res)
		if (is(bath, "try-error")) {
		    stop("The BSHC server cannot be reached\n")
		}
		else {
		    cat("Building bathy matrix ...\n")
		    bath2 <- as.bathy(bath)
		}
		
	if (keep) {
	    write.table(bath, file = FILE, sep = ",", quote = FALSE, 
	        row.names = FALSE)
	}
	return(bath2)
    }
}