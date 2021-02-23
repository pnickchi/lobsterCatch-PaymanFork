#' This function simulates an arena and a number of lobsters to start with.
#' @param nrowgrids is a numeric value which defines number of rows of the arena
#' @param ncolgrids is a numeric value which defines number of columns of the arena
#' @param initlambda is the density of lobsters
#' @param initD is the dispersion index of lobsters 
#' @param losbterSizeFile is a csv file that contains the size frequency of lobsters
#' @return Returns  x and y coordinates of simulated lobsters at the beginning 

initialLobsterGrid = function(nrowgrids, ncolgrids, initlambda, initD, losbterSizeFile){
  
  ngrids <- nrowgrids * ncolgrids
  
  stop.condition = FALSE # this is to take care of instances when no lobster is simulated(due to small density)
  
  while(stop.condition == FALSE){
    initialLobster  <- rpoisD(n = ngrids,lambda = initlambda, D = initD)
    LobsterStart    <- data.frame(EASTING = rep(1:ncolgrids,times=nrowgrids), 
                                  NORTHING = rep(1:nrowgrids,each=ncolgrids), 
                                  Lobs = initialLobster)
    LobsterStart <- subset(LobsterStart,Lobs>0)
    tt <- unlist( apply(X = LobsterStart, MARGIN = 1, FUN = replicateCoordinates) )
    tt <- matrix(tt, ncol = 2, byrow = TRUE)
    colnames(tt)<- c("EASTING","NORTHING")
    initialxyCoordinate  = as.data.frame(tt)
  
    if( nrow(initialxyCoordinate) > 0 ){
      stop.condition = TRUE
    }

  }
  
  initialxyCoordinate$trapped <- 0 
  initialxyCoordinate$lobLength <- NA 
  
  if( losbterSizeFile != '' ){
    lobsterSizeFreq <- read.csv(file = losbterSizeFile, header = TRUE, stringsAsFactors = FALSE)
    lobsterSizeFreq$prob <- lobsterSizeFreq$freq / sum(lobsterSizeFreq$freq )
    labels   <- lobsterSizeFreq$bins
    lobProb <- lobsterSizeFreq$prob
    initialxyCoordinate$lobLength <- rep(NA, nrow(initialxyCoordinate) )
    initialxyCoordinate$lobLength<-sample(x = labels, size = sum(initialLobster), replace = TRUE, prob = lobProb)
  }
  
  return(initialxyCoordinate)
  
}
  
  
  