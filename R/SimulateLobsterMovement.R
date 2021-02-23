#' A function to run the simulation based on defined parameters 
#' @param p is a list which contains all input variables
#' @return Returns a list
#' @export


SimulateLobsterMovement = function(p){
  #To be used for debugging purposes only 
  # nrowgrids  <- p$nrowgrids
  # ncolgrids  <- p$ncolgrids
  # initlambda <- p$initlambda
  # initD      <- p$initD
  # ntraps     <- nrow(p$Trap)
  # lobLengthThreshold <- p$lobLengthThreshold
  # currentZoI <- p$currentZoI
  # shrinkage <- p$shrinkage
  # dStep <- p$dStep
  # tSteps <- p$tSteps
  # howClose <- p$howClose
  # q0 <- p$q0 
  # qmin <- p$qmin 
  # saturationThreshold <- p$saturationThreshold
  # trapSaturation <- p$trapSaturation
  # lengthBased <- p$lengthBased
  # lobLengthThreshold <- p$lobLengthThreshold
  # Trap <- p$Trap
  # radiusOfInfluence <- p$radiusOfInfluence
  
  with(p,{
    
  if( (p$lengthBased == TRUE) & (p$losbterSizeFile == '') ){
      losbterSizeFile   <- file.choose()
      p$losbterSizeFile <- losbterSizeFile
  }
    
  CatchSimulationOutput = list()
  for(k in 1:p$realizations){
    
    start     <- Sys.time()
    outputs   <- list()
    outputs$traps    = rep(0, times = ntraps)
    outputs$lobsters = data.frame(EASTING = 0, NORTHING = 0, trapped=0, T = 0, I = 0, lobLength = 0)
    
    
    coordinatesOverTime      <- list()
    coordinatesOverTime[[1]] <- initialLobsterGrid(nrowgrids, ncolgrids, initlambda, initD, losbterSizeFile)
    
    
    trapCatch           <- list()
    lobSize             <- list() 
    trapCatch[[1]]      <- rep(0, length=ntraps)
    lobSize[[1]]        <- rep('',length=ntraps)
    
      
    for(t in 2:tSteps){
      
      if(t>2){currentZoI<- currentZoI * shrinkage}
        
      tempUpdateGrid = updateGrid(Lobster = coordinatesOverTime[[t-1]], 
                                  Trap = Trap, 
                                  trapCatch = trapCatch[[t-1]], 
                                  lobSize = lobSize[[t-1]], 
                                  radiusOfInfluence = radiusOfInfluence,
                                  currentZoI = currentZoI, dStep = dStep, howClose = howClose, q0 = q0, qmin = qmin, 
                                  saturationThreshold = saturationThreshold, trapSaturation = trapSaturation, lengthBased = lengthBased,
                                  lobLengthThreshold = lobLengthThreshold )
      # Adam: DO you think, this would help?
      #In case we ran out of space (memory)  the following line can be moved out of 
      #the loop so that we simply just Just keep the last iteration? To make things faster 
      #and less memory demanding?
      coordinatesOverTime[[t]] <- tempUpdateGrid[[1]]
      trapCatch[[t]]           <- tempUpdateGrid[[2]]
      lobSize[[t]]             <- tempUpdateGrid[[3]]
    }
    
    
    outmove   = do.call(rbind, coordinatesOverTime)
    outmove$TimeStep = rep(0:(tSteps-1), each = nrow(coordinatesOverTime[[1]]) )
    outmove$LobIndx = rep(1:nrow(coordinatesOverTime[[1]]), times=tSteps)
    
    outtraps   = as.data.frame(do.call(rbind, trapCatch))
    outlobsize = as.data.frame(do.call(rbind, lobSize) )
    colnames(outtraps)   = paste0( 'Trap', 1:ncol(outtraps) )
    colnames(outlobsize) = paste0( 'Trap', 1:ncol(outtraps) )
    
    outputs$traps    = outtraps
    outputs$lobsters = outmove
    outputs$lobSize  = outlobsize 
    
    CatchSimulationOutput[[k]] = outputs
    print(paste('Timing', Sys.time()-start, 'for iteration #',k,sep=" "))
  }
  return(CatchSimulationOutput)
  })
  
}