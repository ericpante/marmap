fortify.bathy <- function(model, data, ...) {
  # Convert an object of class bathy into a data.frame, for ggplot
  # x   object of class bathy

  x <- as.xyz(model)
  names(x) <- c("x", "y", "z")
  return(x)
}