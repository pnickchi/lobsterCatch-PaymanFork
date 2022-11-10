#' This function extracts two pieces of information at the end of simulation: maximum number of trapped
#' lobster and the time when that occurred.
#' @param x is an object that contains information from function SimulateLobsterMovement
#' @param mls is the minimum legal size for calculating catch weight
#' @return Returns  maximum catch in number total weight, and legal wt and length of time to reach maximum catch
#' @export
GetSimOutput = function(x,mls=82.5){

  time.to.max <- list()
  max.catch   <- list()
  legwts      <- list()
  totwts      <- list()
  for( i in 1:length(x) ){
    time.to.max[[i]] = apply(x[[i]]$traps, 2, which.max)
    max.catch[[i]]   = apply(x[[i]]$traps, 2, max)

        legwtpertrap =  c()
        wtpertrap = c()
          s = x[[i]]$lobSize
          s = s[nrow(s),]
          j= length(s)
          for(k in 1:j){
                l = s[k]
                l = na.omit(as.numeric(unlist(strsplit(l[[1]],"-CL"))))
                n = which(l>mls)
                wtpertrap = c(wtpertrap,sum(bio.lobster::lobLW(l)))
                legwtpertrap = c(legwtpertrap,sum(bio.lobster::lobLW(l[n])))
              }
        legwts[[i]] = legwtpertrap
        totwts[[i]] = wtpertrap
  }


  time.to.max = as.data.frame(do.call(rbind,time.to.max))
  max.catch   = as.data.frame(do.call(rbind,max.catch))
  legwts      = as.data.frame(do.call(rbind,legwts))
  totwts      = as.data.frame(do.call(rbind,totwts))

  names(legwts) = names(totwts) = names(time.to.max)

  return( list(TimeToMax = time.to.max, MaxCatch = max.catch, LegalCatchWt = legwts, TotalCatchWt = totwts ) )
}
