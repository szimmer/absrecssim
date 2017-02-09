chooseRandDist <- function(rdist, n, args=NULL){
#' Generates a random distribution from rdist (character) with n observations
#'  and passes other arguments. Note, if the random distribution is rmultinomial,
#'  it is assumed that size=1 and is retruned as a vector of length n
#'  
#'  @param rdist random generation function (as a character) 
#'  @param n number of observations
#'  @param args is optional arguments to the random generation function
#'  
#'  
#'  @return Format of return depends on rdist (rnorm, rbinom, rmultinom return a vector of length n)
#'  
#'  @examples
#'  chooseRandDist("rnorm", 10, list(mean=5, sd=1))
#'  chooseRandDist("rmultinom", 100, list(p=c(.7,.2,.1),size=1))
#'  chooseRandDist("rbinom", 100, list(p=c(.8),size=1))
#'  

  
  j <- do.call(rdist,c(list(n=n), args))
  
  if (rdist =="rmultinom"){
    j2 <- factor(apply(j,2, which.max), levels=1:length(args$p))
    return(j2)
  }else{
    return(j)
  }
}

AltDist <- function(dist, pch){
  k <- length(dist)
  distout <- rep(NA, length(dist))
  
  distout[1] <- dist[1]*pch
  distout[2:k] <- dist[2:k]+dist[1]*(1-pch)/(k-1)
  return(distout)
}
#chooseRandDist("rnorm", 10, list(mean=5, sd=1))
#chooseRandDist("rmultinom", 100, list(p=c(.8,.2),size=1))
#chooseRandDist("rbinom", 100, list(p=c(.8),size=1))

simPop <- function(N, pC, DistCovered, DistNotCovered){
#' This function creates 1 realization of a population that may have undercoverage
#' The population has different geographic units with sizes specified in the vector N
#' The coverage rates for each area are specified in the vector pC
#' The outcome variables distriubtions for the covered pop are specified as well 
#' as the non-covered pop
#' 
#' @param N is a vector which has the population sizes for each geographic area
#' @param pC is a vector of coverage rates for each geographic area, same length as N
#' @param DistCovered is a list to specify inputs for chooseRandDist, length is number of outcome variables
#' @param DistNotCovered is a list to specify inputs for chooseRandDist, length is number of outcome variables
#' 
#' 
#' @return Returns a data_frame 
#' 

  require(dplyr)
  require(tidyr)
  require(lazyeval)
  
  
  PopFrame <- data_frame(Geos=rep(factor(1:length(N)), N), probCovered=rep(pC, N)) 
  PopFrame  <- PopFrame %>% mutate(Covered=rbinom(sum(N),1,probCovered))
  PopFrame <- PopFrame %>% select(-probCovered)
  #PopFrame <- mutate(PopFrame, chooseRandDist("rbinom", 100, list(p=c(.8),size=1)))
  PopNC <- PopFrame %>% filter(Covered==0)
  PopC <- PopFrame %>% filter(Covered==1)
  
  NNC <- nrow(PopNC)
  NC <- nrow(PopC)
  
  for (idx in 1:length(DistCovered)){
    PopNC <- PopNC %>% mutate(xi=chooseRandDist(rdist=DistNotCovered[[idx]][[1]], n=NNC, args=DistNotCovered[[idx]][[2]]))
    PopC <- PopC %>% mutate(xi=chooseRandDist(rdist=DistCovered[[idx]][[1]], n=NC, args=DistCovered[[idx]][[2]]))
    colnames(PopNC)[ncol(PopNC)] <- paste("V",idx,sep="_")
    colnames(PopC)[ncol(PopC)] <- paste("V",idx,sep="_")
  }
  
  PopFrame2 <- bind_rows(PopC, PopNC)
  
  return(PopFrame2)
}


# ip1 <- c(5,8,3,2,2)*50000
# ip2 <- c(.8,.9,1,1,.7)
# ip3 <- list(list(rdist="rnorm",args=list(mean=5,sd=5)),
#                     list(rdist="rbinom",args=list(p=.7,size=1)),
#                     list(rdist="rmultinom",args=list(p=c(.7,.2,.1), size=1)))
# 
# ip4 <- list(list(rdist="rnorm",args=list(mean=10,sd=5)),
#                     list(rdist="rbinom",args=list(p=.4,size=1)),
#                     list(rdist="rmultinom",args=list(p=c(.6,.3,.1), size=1)))
# 
# j <- simPop(ip1, ip2, ip3,ip4)