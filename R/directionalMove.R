#' This function moves  lobsters toward the trap (+some randomness). The closer a lobster gets to the trap the smaller the random component of movement. This function will 
#' a lobster is within the radius of influece of a trap.
#' @param Lobster location of lobster in x and y coordinates
#' @param dStep is how much a lobster moves in each time step
#' @param minDistoTrap is the distance to trap
#' @param Trap location of trap in x and y coordinates
#' @param radiusOfInfluence trap's radius of influence
#' @param currentZoI  initial zone of influence
#' @return Returns the new coordinates of each lobster
#' @export
directionalMove <- function(Lobster, dStep, minDistoTrap, Trap, radiusOfInfluence, currentZoI){
  
  # Get x and y coordinate of lobster
  xLobster = Lobster[1] 
  yLobster = Lobster[2]
  
  # Get x and y coordinate of trap
  xtrap = Trap[1]
  ytrap = Trap[2]
  
  thetaT =  atan2(ytrap-yLobster,xtrap-xLobster)*180/pi
  b = 1 + 0.9 * (minDistoTrap - currentZoI) / radiusOfInfluence
  thetaR = -180:180
  P = 1/(180^b) * abs(thetaR) ^ b
  Prtheta_R = (1-P) / sum(1-P)
  theta_r = sample(thetaR,size = 1, prob = Prtheta_R)
  theta   <- thetaT + theta_r
  xNew   <- dStep * cos(theta * pi / 180) + xLobster
  yNew   <- dStep * sin(theta * pi / 180) + yLobster
  
  return( list(EASTING = xNew, NORTHING = yNew) )
  
}