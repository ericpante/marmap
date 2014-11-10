combine.buffers <- function(buf) {
  if (!is.list(buf)) 	stop("'buf' must be a list of length 2 or more")
  if (length(buf) < 2) 	stop("'buf' must be a list of length 2 or more")
  if (any(apply(sapply(buf,dim),1,function(x) length(table(x))) != 1)) stop("all elements of 'buf' must have the same dimensions")
  
  lon <- rownames(buf[[1]])
  lat <- colnames(buf[[1]])
  
  # Stack buffers in a new array
  buf <- array(as.vector(sapply(buf,as.vector)),dim=c(dim(buf[[1]]),length(buf)))
  
  # Apply element wise function : if all elements in a position are NA, leave it that way, otherwise leave the depth/altitude value
  res <- apply(buf, 1:2, function(x) ifelse(any(!is.na(x)),na.omit(x)[1],NA))
  rownames(res) <- lon
  colnames(res) <- lat
  
  class(res) <- "bathy"
  
  return(res)
}