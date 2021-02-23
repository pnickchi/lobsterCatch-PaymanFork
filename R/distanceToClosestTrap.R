#' This function returns id of the closest trap and calculates its distance to ? (Note: Complete later)
#' @param Lobster location of lobster in x and y coordinates
#' @param Trap location of trap in x and y coordinates
#' @return Returns distance to closest trap and the trap ID
#' @export
distanceToClosestTrap <- function(Lobster, Trap){
  ds = unlist(apply(Trap,1,distanceToTrapCalculator,Lobster))
  dmin = which.min(ds)
  return( c(distance = ds[dmin], trapId = dmin) )
}