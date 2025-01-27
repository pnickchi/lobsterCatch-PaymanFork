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

  density             <- rep.int(p$initlambda,          p$realizations)
  dstepmov            <- rep.int(p$dStep,               p$realizations)
  saturationThreshold <- rep.int(p$saturationThreshold, p$realizations)
  baitShrinkage       <- rep.int(p$shrinkage,           p$realizations)
  howClose            <- rep.int(p$howClose,            p$realizations)
  trapSaturation      <- rep.int(p$trapSaturation,      p$realizations)

  res <- bind_cols(temp, density, dstepmov, saturationThreshold, baitShrinkage, howClose, trapSaturation)

  clNames <- c(
    paste0('TimeToMax_Trap',    1:ncol(x$TimeToMax)),
    paste0('MaxCatch_Trap',     1:ncol(x$MaxCatch)),
    paste0('LegalCatchWt_Trap', 1:ncol(x$LegalCatchWt)),
    paste0('TotalCatchWt_Trap', 1:ncol(x$TotalCatchWt))
  )
  colnames(res) <- c(clNames,'densitylambda','dstepmov','saturationThreshold','baitShrinkage', 'howClose', 'trapSaturation')

  return(res)

}

# Variables that have different values
initlambda          <- c(0.1,   0.5, 1, 1.6)
howClose            <- c(0.5,   1,   1.5)
shrinkage           <- c(0.993, 0.995)
dStep               <- 1:10

nsettings <- length(initlambda) * length(howClose) * length(shrinkage) * length(dStep)


# Variables that have fixed value
nrowgrids           <- rep(200, nsettings)
ncolgrids           <- rep(200, nsettings)
unitarea            <- rep(100, nsettings)
initD               <- rep(3,   nsettings)
currentZoI          <- rep(15,  nsettings)
radiusOfInfluence   <- rep(15,  nsettings)
saturationThreshold <- rep(5,   nsettings)
Trap                <- rep(list( data.frame(x = c(100), y = c(100)) ), nsettings)
ntraps              <- unlist( lapply(X = Trap, nrow) )
lobLengthThreshold  <- rep(115, nsettings)
q0                  <- rep(0.5, nsettings)
qmin                <- rep(0.5, nsettings)
realizations        <- rep(50,  nsettings)
tSteps              <- rep(50, nsettings)
sexBased            <- rep(TRUE, nsettings)
lengthBased         <- rep(TRUE, nsettings)
trapSaturation      <- rep(FALSE, nsettings)

lobsterSizeFile     <- 'https://raw.githubusercontent.com/vpourfaraj/lobsterCatch/main/inst/extdata/LobsterSizeFreqs.csv'
lobsterSexDist      <- list(labels = c('M','F','MM','BF'),
                            prob1 = c(0.55,0.35,0.05,0.05),
                            prob2 = c(0.5,0.50,0,0),
                            lobsterMatThreshold = 100)


temp <- expand.grid(initlambda, howClose, shrinkage, dStep)
initlambda <- temp$Var1
howClose   <- temp$Var2
shrinkage  <- temp$Var3
dStep      <- temp$Var4


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




# nrowgrids           <- rep(200, length(initlambda) * length(dStep))
# ncolgrids           <- rep(200, length(initlambda) * length(dStep))
# unitarea            <- rep(100, length(initlambda) * length(dStep))
# initD               <- rep(3, length(initlambda) * length(dStep))
# shrinkage           <- rep(0.993, length(initlambda) * length(dStep))
# currentZoI          <- rep(15, length(initlambda) * length(dStep))
# radiusOfInfluence   <- rep(15, length(initlambda) * length(dStep))
# saturationThreshold <- rep(5, length(initlambda) * length(dStep))
# howClose            <- rep(0.5, length(initlambda) * length(dStep))
# Trap                <- rep(list(data.frame( x = c(100), y = c(100))),  length(initlambda) * length(dStep))
# ntraps              <- unlist( lapply(X = Trap, nrow) )
# lobLengthThreshold  <- rep(115, length(initlambda) * length(dStep))
# q0                  <- rep(0.5, length(initlambda) * length(dStep))
# qmin                <- rep(0.5, length(initlambda) * length(dStep))
# realizations        <- rep(50, length(initlambda) * length(dStep))
# tSteps              <- rep(50, length(initlambda) * length(dStep))
# sexBased            <- rep(TRUE, length(initlambda) * length(dStep))
# lengthBased         <- rep(TRUE, length(initlambda) * length(dStep))
# trapSaturation      <- rep(FALSE, length(initlambda) * length(dStep))

# lobsterSizeFile     <- 'https://raw.githubusercontent.com/vpourfaraj/lobsterCatch/main/inst/extdata/LobsterSizeFreqs.csv'
# lobsterSexDist      <- list(labels = c('M','F','MM','BF'),
#                             prob1 = c(0.55,0.35,0.05,0.05),
#                             prob2 = c(0.5,0.50,0,0),
#                             lobsterMatThreshold = 100)
# initlambda          <- rep(0.5, 10)
# dStep               <- 1:10
#dStep               <- c(1,1,1,5,5,5,10,10,10)

# nsettings <- length(param$nrowgrids)


# param <- list( nrowgrids=nrowgrids,
#                ncolgrids=ncolgrids,
#                unitarea=unitarea,
#                initlambda=initlambda,
#                initD=initD,
#                shrinkage=shrinkage,
#                currentZoI=currentZoI,
#                radiusOfInfluence=radiusOfInfluence,
#                Trap=Trap,
#                ntraps=ntraps,
#                saturationThreshold=saturationThreshold,
#                howClose=howClose,
#                dStep=dStep,
#                lengthBased=lengthBased,
#                lobsterSizeFile=lobsterSizeFile,
#                lobLengthThreshold=lobLengthThreshold,
#                trapSaturation=trapSaturation,
#                q0=q0,
#                qmin=qmin,
#                realizations=realizations,
#                tSteps=tSteps,
#                sexBased=sexBased,
#                lobsterSexDist=lobsterSexDist)



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
  p$ngrids               <- param$nrowgrids[i] * param$ncolgrids[i]
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
  #saveRDS(object = Simrun[[i]],  file = paste0('results_for_debug/Set_', i, '_Simrun', '.rds') )
  #saveRDS(object = Results[[i]], file = paste0('results_for_debug/Set_', i, '_Results', '.rds'))
  saveRDS(object = resultdfcomplete[[i]], file = paste0('results_for_debug/Set_', i, '_resultdfcomplete', '.rds'))
}

