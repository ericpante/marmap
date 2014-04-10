tecto.lines <- function(mat, ...){
	data(tectonic)
	range(as.numeric(rownames(mat))) -> lon.rg
	range(as.numeric(colnames(mat))) -> lat.rg

	subset(tectonic, tectonic[,1]> lon.rg[1] & tectonic[,1]< lon.rg[2] & 
	                 tectonic[,2]> lat.rg[1] & tectonic[,2]< lat.rg[2] ) -> sub.tec
	unique(sub.tec[,3]) -> plates
	
	for(i in plates){
		lines(sub.tec[sub.tec$type.source ==i, 1:2], ...)
		}
}


