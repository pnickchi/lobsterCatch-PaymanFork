library(lobsterCatch)
library(tidyverse)
library(ggplot2)

#initialize a parameter file to pass info into the code and then put all into a function
p = list()

p$nrowgrids = 10
p$ncolgrids = 10
p$ngrids = p$nrowgrids * p$ncolgrids
p$initlambda = 0.4 # Initial density of lobster
p$initD = 3  #Initial Dispersion of lobster (initlambda and initD go into rpoissonD to randomly allocation lobster across the grid space)
p$shrinkage = 0.993
#p$initlambda = 0.2 #is the density of lobsters at the beginning of simulation
#p$initD = 3 #is the dispersion index of lobsters on seabed at the beginning of the simulation
p$currentZoI = 15
p$radiusOfInfluence = 15
p$Trap = data.frame( x = c(3,5,6), y = c(3,5,6) )
p$ntraps = nrow(p$Trap)
p$saturationThreshold = 5
p$howClose = 0.5
p$dStep = 5
p$lengthBased = TRUE
p$lobsterSizeFile <- 'C:/Users/pourfarajv/Desktop/Kumu_R_Visulization/AgentbasedModeling/lobsterCatch/inst/extdata/LobsterSizeFreqs.csv'
#p$lobsterSizeFile <- 'LobsterSizeFreqs.csv'
p$lobLengthThreshold = 115
p$trapSaturation = TRUE
p$q0 = 0.5
p$qmin = 0
p$realizations = 20 #number of iterations/simulations
p$tSteps = 5       #timesteps per iteration
p$sexBased <- TRUE
# The following lines creates a sex distribution
p$lobsterSexDist <- list(labels = c('M','F','MM','BF'), #male, female, mature male, berried female
                         prob1 = c(0.55,0.35,0.05,0.05), #their prob in population
                         prob2 = c(0.5,0.50,0,0), # prob of small males and females that are under lobsterMatThreshold
                         lobsterMatThreshold = 100  # The average size of mature lobsters
)
# p$lobsterSexDist <- ''  # in case of p$sexBased = FALSE

Simrun <- SimulateLobsterMovement(p)

Results  <- GetSimOutput(Simrun)

#unlisting the result to add parameters columns

resultsdf<- data.frame(unlist(Results, FALSE, TRUE))

#Converting to long format ( wasn't able to use pivot_longer!)
timetomax <- c(resultsdf$TimeToMax.Trap1, resultsdf$TimeToMax.Trap2, resultsdf$TimeToMax.Trap3)
maxcatchno  <- c(resultsdf$MaxCatch.Trap1, resultsdf$MaxCatch.Trap2, resultsdf$MaxCatch.Trap3)
legalcatchwt  <- c(resultsdf$LegalCatchWt.Trap1, resultsdf$LegalCatchWt.Trap2, resultsdf$LegalCatchWt.Trap3)
totalcatchwt  <- c(resultsdf$TotalCatchWt.Trap1, resultsdf$TotalCatchWt.Trap2, resultsdf$TotalCatchWt.Trap3)
#taking the params used for naming purpose
densitylambda<- rep.int(p$initlambda, p$realizations)
dstepmov<- rep.int(p$dStep,p$realizations)


resultdfcomplete <- data.frame(timetomax = timetomax,
                               maxcatchno = maxcatchno,
                               legalcatchwt = legalcatchwt,
                               totalcatchwt = totalcatchwt,
                               densitylambda = densitylambda,
                               dstepmov= dstepmov)


#export the result as RDS

saveRDS(resultdfcomplete, file = 'resultlambda0.4dstep5.rds')




