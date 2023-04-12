# Load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(usethis)
library(devtools)

load_all()
document()
#check()
#build()

# Define function
GetdfComplete = function(x, extp){

  temp           <- bind_cols(x)

  densitylambda       <- rep.int(p$initlambda,          p$realizations)
  dstepmov            <- rep.int(p$dStep,               p$realizations)
  saturationThreshold <- rep.int(p$saturationThreshold, p$realizations)
  baitShrinkage       <- rep.int(p$shrinkage,           p$realizations)

  res <- bind_cols(temp, densitylambda, dstepmov, saturationThreshold, baitShrinkage)

  clNames <- c(
    paste0('TimeToMax_Trap',    1:ncol(x$TimeToMax)),
    paste0('MaxCatch_Trap',     1:ncol(x$MaxCatch)),
    paste0('LegalCatchWt_Trap', 1:ncol(x$LegalCatchWt)),
    paste0('TotalCatchWt_Trap', 1:ncol(x$TotalCatchWt))
  )
  colnames(res) <- c(clNames,'densitylambda','dstepmov','saturationThreshold','baitShrinkage')

  return(res)

}


nrowgrids           <- c(200,  200)
ncolgrids           <- c(200,  200)
unitarea            <- c(100, 100)
initlambda          <- c(0.5, 0.5)
initD               <- c(3,  3)
shrinkage           <- c(0.993, 0.993)
currentZoI          <- c(15, 15)
radiusOfInfluence   <- c(15, 15)
Trap                <- list(data.frame( x = c(100), y = c(100) ), data.frame( x = c(100), y = c(100) ))
ntraps              <- unlist( lapply(X = Trap, nrow) )
saturationThreshold <- c(5,   5)
howClose            <- c(0.5, 0.5)
dStep               <- c(1,  10)
lengthBased         <- c(TRUE,TRUE)
lobsterSizeFile     <- 'D:/Personal/Vahab/Grid/package_on_github/lobsterCatch-PaymanFork/inst/extdata/LobsterSizeFreqs.csv'
lobLengthThreshold  <- c(115, 115)
trapSaturation      <- c(FALSE, FALSE)
q0                  <- c(0.5, 0.5)
qmin                <- c(0,   0)
realizations        <- c(10,  10)
tSteps              <- c(50,  50)
sexBased            <- c(TRUE,TRUE)
lobsterSexDist      <- list(labels = c('M','F','MM','BF'),
                            prob1 = c(0.55,0.35,0.05,0.05),
                            prob2 = c(0.5,0.50,0,0),
                            lobsterMatThreshold = 100)

param <- list( nrowgrids=nrowgrids,
               ncolgrids=ncolgrids,
               unitarea=unitarea,
               initlambda=initlambda,
               initD=initD,
               shrinkage=shrinkage,
               currentZoI=currentZoI,
               radiusOfInfluence=radiusOfInfluence,
               Trap=Trap,
               ntraps=ntraps,
               saturationThreshold=saturationThreshold,
               howClose=howClose,
               dStep=dStep,
               lengthBased=lengthBased,
               lobsterSizeFile=lobsterSizeFile,
               lobLengthThreshold=lobLengthThreshold,
               trapSaturation=trapSaturation,
               q0=q0,
               qmin=qmin,
               realizations=realizations,
               tSteps=tSteps,
               sexBased=sexBased,
               lobsterSexDist=lobsterSexDist)

nsettings <- length(param$nrowgrids)


#
# Loop over list and initialize a parameter list and execute the simulations
#
Simrun  <- list()
Results <- list()
resultdfcomplete <- list()
for(i in 1:nsettings){

  p <- list()
  p$nrowgrids            <- param$nrowgrids[i]
  p$ncolgrids            <- param$ncolgrids[i]
  p$ngrids               <- p$nrowgrids[i] * p$ncolgrids[i]
  p$unitarea             <- param$unitarea[i]
  p$initlambda           <- param$initlambda[i]
  p$initD                <- param$initD[i]
  p$shrinkage            <- param$shrinkage[i]
  p$currentZoI           <- param$currentZoI[i]
  p$radiusOfInfluence    <- param$radiusOfInfluence[i]
  p$Trap                 <- as.data.frame(param$Trap[i])
  p$ntraps               <- param$ntraps[i]
  p$saturationThreshold  <- param$saturationThreshold[i]
  p$howClose             <- param$howClose[i]
  p$dStep                <- param$dStep[i]
  p$lengthBased          <- param$lengthBased[i]
  p$lobsterSizeFile      <- param$lobsterSizeFile
  p$lobLengthThreshold   <- param$lobLengthThreshold[i]
  p$trapSaturation       <- param$trapSaturation[i]
  p$q0                   <- param$q0[i]
  p$qmin                 <- param$qmin[i]
  p$realizations         <- param$realizations[i]
  p$tSteps               <- param$tSteps[i]
  p$sexBased             <- param$sexBased[i]
  p$lobsterSexDist       <- param$lobsterSexDist

  print( paste0('Run the simulation for parameter setting = ', i) )

  Simrun[[i]]            <- SimulateLobsterMovement(p)
  Results[[i]]           <- GetSimOutput(Simrun[[i]])
  resultdfcomplete[[i]]  <- GetdfComplete(x = Results[[i]], extp = p)

  # Uncomment the following lines to save the output as a RDS file
  saveRDS(object = Simrun[[i]],  file = paste0('results_for_debug/Set_', i, '_Simrun', '.rds') )
  saveRDS(object = Results[[i]], file = paste0('results_for_debug/Set_', i, '_Results', '.rds'))
  saveRDS(object = resultdfcomplete[[i]], file = paste0('results_for_debug/Set_', i, '_resultdfcomplete', '.rds'))
}

