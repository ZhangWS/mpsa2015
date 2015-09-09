setwd("C:/Users/Boris/Desktop/Sync me/888 Other academic work/MPSA 2015/data/run 2")
library(foreign)
library(data.table)

##first, read in the big file, one for each type.
#comment out those not needed

##nb it stripped the run number out. May have to infer or re read and force it not to ##strip headers
smallworldparam <- read.csv("smallworld2.csv", header= FALSE, skip=6, nrows = 10)
colno <- seq(2,ncol(smallworldparam), 40) #calculate all row numbers
tmp <- smallworldparam[, colno]
tmp <- as.data.table(t(tmp)) #all hail Hadley Wickham
names(tmp) <- c("runnum", "noden", "spreadern","ntwktype", "ptrustavg", "passlimit", "accpthres", "itrustavg", "stepsn", "")
###SAVE BEFORE CONTINUING
write.csv(tmp, file="smallworld2settings.csv")



#had to hack together a solution as read.csv refused to work on this section
tmp <- readLines("smallworld2.csv")
tmp <- strsplit(tmp, "\n")
tmp1 <- as.character(tmp[18])


tmp1 <- gsub("\"", "", tmp1)
tmp1 <- gsub("\\\\", "", tmp1)
tmp2 <- strsplit(tmp1, ",")
tmp3 <- tmp2[[1]]
tmp3 <- as.numeric(tmp3[-1])

tmp4 <- matrix(tmp3, nrow=7290, ncol = 40, byrow=T)
tmp5 <- as.data.frame(tmp4)

#links-transmitted is count of links that has pass-it-on > 0
names(tmp5) <- c("agents-n", "links-n", "links-used", "spreaders-avglinks", "links-mean-trust", "links-mean-inst-trust","ambiv-n", "ambiv-avglinks", "ambiv-cluster-coeff", "ambiv-betweenness", "ambiv-eigencent", "ambiv-pgrank", "ambiv-closeness", "ambiv-mean-inst-trust", "rej-n", "rej-avglinks", "rej-coeff", "rej-betweenness", "rej-eigencent", "rej-pgrank", "rej-closeness", "rej-mean-inst-trust", "aa-n", "aa-avglinks", "aa-coeff", "aa-betweenness", "aa-eigencent", "aa-pgrank", "aa-closeness", "aa-mean-inst-trust","as-n", "as-avglinks", "as-coeff", "as-betweenness", "as-eigencent", "as-pgrank", "as-closeness", "as-mean-inst-trust","believers-n", "misinfo-avgexpose-n")
write.csv(tmp5, file="smallworld2readings.csv")

#and now to reconcile both bits

swparam <-read.csv("smallworld2settings.csv")
swreadings <- read.csv("smallworld2readings.csv")

swall <- cbind(swparam,swreadings)
write.csv(swall, file="smallworld2data.csv")





#################################################################################
###SCALE FREE STUFF

scalefreeparam <- read.csv("scalefree2.csv", header= FALSE, skip=6, nrows = 10)
tmp1 <- scalefreeparam[, colno]
tmp1 <- as.data.table(t(tmp1)) #all hail Hadley Wickham
names(tmp1) <- c("runnum", "noden", "spreadern","ntwktype", "ptrustavg", "passlimit", "accpthres", "itrustavg", "stepsn", "")
write.csv(tmp1, file="scalefree2settings.csv")


#for some reason, read.csv refused to work here, so I hacked this lovely thing together.
tmp <- readLines("scalefree2.csv")
tmp <- strsplit(tmp, "\n")
tmp1 <- as.character(tmp[18])


tmp1 <- gsub("\"", "", tmp1)
tmp1 <- gsub("\\\\", "", tmp1)
tmp2 <- strsplit(tmp1, ",")
tmp3 <- tmp2[[1]]
tmp3 <- as.numeric(tmp3[-1])

tmp4 <- matrix(tmp3, nrow=7290, ncol = 40, byrow=T)
tmp5 <- as.data.frame(tmp4)
names(tmp5) <- c("agents-n", "links-n", "links-used", "spreaders-avglinks", "links-mean-trust", "links-mean-inst-trust","ambiv-n", "ambiv-avglinks", "ambiv-cluster-coeff", "ambiv-betweenness", "ambiv-eigencent", "ambiv-pgrank", "ambiv-closeness", "ambiv-mean-inst-trust", "rej-n", "rej-avglinks", "rej-coeff", "rej-betweenness", "rej-eigencent", "rej-pgrank", "rej-closeness", "rej-mean-inst-trust", "aa-n", "aa-avglinks", "aa-coeff", "aa-betweenness", "aa-eigencent", "aa-pgrank", "aa-closeness", "aa-mean-inst-trust","as-n", "as-avglinks", "as-coeff", "as-betweenness", "as-eigencent", "as-pgrank", "as-closeness", "as-mean-inst-trust","believers-n", "misinfo-avgexpose-n")
write.csv(tmp5, file="scalefree2readings.csv")

#and now to reconcile both bits

sfparam <-read.csv("scalefree2settings.csv")
sfreadings <- read.csv("scalefree2readings.csv")

sfall <- cbind(sfparam,sfreadings)
write.csv(sfall, file="scalefree2data.csv")













#################################################################################
#Random Networks was planned but never completed, as it turns out that the paper didn't need this. However, it's still available here in the code.

#randomparam <- read.csv("random.csv")
tmp1 <- randomparam[, colno]
tmp1 <- as.data.table(t(tmp1)) #all hail Hadley Wickham
names(tmp1) <- c("runnum", "noden", "spreadern","ntwktype", "ptrustavg", "passlimit", "accpthres", "itrustavg", "stepsn", "")
write.csv(tmp, file="randomsettings.csv")


#for some reason, read.csv refused to work here, so I hacked this lovely thing together.
tmp <- readLines("random.csv")
tmp <- strsplit(tmp, "\n")
tmp1 <- as.character(tmp[18])


tmp1 <- gsub("\"", "", tmp1)
tmp1 <- gsub("\\\\", "", tmp1)
tmp2 <- strsplit(tmp1, ",")
tmp3 <- tmp2[[1]]
tmp3 <- as.numeric(tmp3[-1])

tmp4 <- matrix(tmp3, nrow=7290, ncol = 33, byrow=T)
tmp5 <- as.data.frame(tmp4)
names(tmp5) <- c("agents-n", "links-n", "links-used", "spreaders-avglinks", "links-mean-trust", "links-mean-inst-trust","ambiv-n", "ambiv-avglinks", "ambiv-cluster-coeff", "ambiv-betweenness", "ambiv-eigencent", "ambiv-pgrank", "ambiv-closeness", "ambiv-mean-inst-trust", "rej-n", "rej-avglinks", "rej-coeff", "rej-betweenness", "rej-eigencent", "rej-pgrank", "rej-closeness", "rej-mean-inst-trust", "aa-n", "aa-avglinks", "aa-coeff", "aa-betweenness", "aa-eigencent", "aa-pgrank", "aa-closeness", "aa-mean-inst-trust","as-n", "as-avglinks", "as-coeff", "as-betweenness", "as-eigencent", "as-pgrank", "as-closeness", "as-mean-inst-trust","believers-n", "misinfo-avgexpose-n")
write.csv(tmp5, file="randomreadings.csv")

#and now to reconcile both bits

rparam <-read.csv("randomsettings.csv")
rreadings <- read.csv("randomreadings.csv")

rall <- cbind(rparam,rreadings)
write.csv(rall, file="rdata.csv")