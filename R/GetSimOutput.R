#' This function extracts two pieces of information at the end of simulation: maximum numbr of trapped
#' lobster and the time when that occurred. 
#' @param x is an object that contains information from function SimulateLobsterMovement
#' @return Returns  maximum catch and time to maximum catch
#' @export
GetSimOutput = function(x){
  
  time.to.max <- list()
  max.catch   <- list()
  
  for( i in 1:length(x) ){
    time.to.max[[i]] = apply(x[[i]]$traps, 2, which.max)
    max.catch[[i]]   = apply(x[[i]]$traps, 2, max)
  }
  
  
  time.to.max = do.call(rbind,time.to.max)
  max.catch   = do.call(rbind,max.catch)
  
  return( list(TimeToMax = time.to.max, MaxCatch = max.catch ) )
}
