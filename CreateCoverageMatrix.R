library(readr)
PUMAcov <- read_csv("//rtints27/csds/CENTER/Zimmer/ABS_IRD/Data/PUMACoverage.csv")

N <- PUMAcov$ACSCount
pC <- PUMAcov$CoverageRate


pCmeans <- seq(0,1,by=.01)
pCorigmean <- mean(pC)
pCratios <- pCmeans/pCorigmean

pcMat <- matrix(NA, nrow=length(pC), ncol=length(pCmeans))

for (i in 1:length(pCmeans)){
  rati <- pCratios[i]
  if (i==1 | i==length(pCmeans)){
    pCi <- rep(rati, length(pC))
  } else {
    pCiraw <- pC*rnorm(200,rati,.005)
    pCiraw[pCiraw < 0] <- 0
    pCiraw[pCiraw >1 ] <- 1
    pCi <- pCiraw/sum(pCiraw)*pCmeans[i]*200
  }
  pcMat[,i] <- pCi
}

apply(pcMat,2,mean)
apply(pcMat,2,sd)
apply(pcMat,2,sd)/apply(pcMat,2,mean)

rm(PUMAcov, i, pC, pCi, pCiraw, pCmeans, pCorigmean, pCratios, rati)
