source("//rtpnfil02/rtpnfil02_vol6/CSDS/CENTER/Zimmer/ABS_IRD/Programs/Simulation/SimulationFunctions.r")
source("//rtpnfil02/rtpnfil02_vol6/CSDS/CENTER/Zimmer/ABS_IRD/Programs/Simulation/CreateCoverageMatrix.R")

library(haven)
library(dplyr)
#/rtpnfil02/rtpnfil02_vol6/CSDS/CENTER/Zimmer/ABS_IRD/Data/CHUM
estsRECSorig <- read_sas("//rtpnfil02/rtpnfil02_vol6/CSDS/CENTER/Zimmer/ABS_IRD/Data/CHUM/release1_all.sas7bdat")
estsRECS <- zap_labels(estsRECSorig)
estsRECS <- estsRECS %>% select(var, value, mean_chum, mean_notchum) %>%
  filter(var != "sqftest") %>% mutate(varnum=as.numeric(factor(var))) %>% 
  group_by(varnum) %>% mutate(ncats=length(var))

Groups <- length(unique(estsRECS$varnum))
altDist <- nullDist <- vector("list", Groups)

for (i in 1:Groups){
  dati <- estsRECS %>% filter(varnum==i)
  nullDist[[i]] <- list(rdist="rmultinom",args=list(p=c(dati$mean_chum), size=1))
  altDist[[i]] <- list(rdist="rmultinom",args=list(p=c(dati$mean_notchum), size=1))
}
rm(dati, Groups, i)

i <- 2

pC <- pcMat[,i]
j <- simPop(round(N/1000), pC, nullDist,altDist)
#j <- j %>% mutate(V_3=factor(V_3), V_4=factor(V_4), V_5=factor(V_5),V_6=factor(V_6), V_7=factor(V_7))
lapply(1:14,function(x){c(table(j[,x+2]))/nrow(j)})
jsamp <- sample_n(j, size=1000)
