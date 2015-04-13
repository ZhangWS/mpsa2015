setwd("C:/Users/Boris/Desktop/Sync me/888 Other academic work/MPSA 2015/data")
library(foreign)
library(data.table)

##first, read in the big file, one for each type.
#comment out those not needed

##nb it stripped the run number out. May have to infer or re read and force it not to ##strip headers
smallworldparam <- read.csv("smallworld.csv", header= FALSE, skip=6, nrows = 10)
colno <- seq(2,ncol(smallworldparam), 33) #calculate all row numbers
tmp <- smallworldparam[, colno]
tmp <- as.data.table(t(tmp)) #all hail Hadley Wickham
names(tmp) <- c("n", "spreadern", "ntype", "ptrustavg", "passlimit", "accpthres", "itrustavg", "stepsn", "fininitval", "whoknows")
###SAVE BEFORE CONTINUING
write.csv(tmp, file="smallworldsettings.csv")



#had to hack together a solution as read.csv refused to work on this section
tmp <- readLines("smallworld.csv")
tmp <- strsplit(tmp, "\n")
tmp1 <- as.character(tmp[18])


tmp1 <- gsub("\"", "", tmp1)
tmp1 <- gsub("\\\\", "", tmp1)
tmp2 <- strsplit(tmp1, ",")
tmp3 <- tmp2[[1]]
tmp3 <- as.numeric(tmp3[-1])

tmp4 <- matrix(tmp3, nrow=7290, ncol = 33, byrow=T)
tmp5 <- as.data.frame(tmp4)
names(tmp5) <- c("agents-n", "links-n", "spreaders-avglinks", "ambiv-n", "ambiv-avglinks", "ambiv-cluster-coeff", "ambiv-betweenness", "ambiv-eigencent", "ambiv-pgrank", "ambiv-closeness", "rej-n", "rej-avglinks", "rej-coeff", "rej-betweenness", "rej-eigencent", "rej-pgrank", "rej-closeness", "aa-n", "aa-avglinks", "aa-coeff", "aa-betweenness", "aa-eigencent", "aa-pgrank", "aa-closeness", "as-n", "as-avglinks", "as-coeff", "as-betweenness", "as-eigencent", "as-pgrank", "as-closeness", "believers-n", "misinfo-avgexpose-n")
write.csv(tmp5, file="smallworldreadings.csv")

#and now to reconcile both bits

swparam <-read.csv("smallworldsettings.csv")
swreadings <- read.csv("smallworldreadings.csv")

swall <- cbind(swparam,swreadings)
write.csv(swall, file="smallworlddata.csv")





#################################################################################
###SCALE FREE STUFF

scalefreeparam <- read.csv("scalefree.csv", header= FALSE, skip=6, nrows = 10)
tmp1 <- scalefreeparam[, colno]
tmp1 <- as.data.table(t(tmp1)) #all hail Hadley Wickham
names(tmp1) <- c("n", "spreadern", "ntype", "ptrustavg", "passlimit", "accpthres", "itrustavg", "stepsn", "fininitval", "whoknows")
write.csv(tmp, file="scalefreesettings.csv")


#for some reason, read.csv refused to work here, so I hacked this lovely thing together.
tmp <- readLines("scalefree.csv")
tmp <- strsplit(tmp, "\n")
tmp1 <- as.character(tmp[18])


tmp1 <- gsub("\"", "", tmp1)
tmp1 <- gsub("\\\\", "", tmp1)
tmp2 <- strsplit(tmp1, ",")
tmp3 <- tmp2[[1]]
tmp3 <- as.numeric(tmp3[-1])

tmp4 <- matrix(tmp3, nrow=7290, ncol = 33, byrow=T)
tmp5 <- as.data.frame(tmp4)
names(tmp5) <- c("agents-n", "links-n", "spreaders-avglinks", "ambiv-n", "ambiv-avglinks", "ambiv-cluster-coeff", "ambiv-betweenness", "ambiv-eigencent", "ambiv-pgrank", "ambiv-closeness", "rej-n", "rej-avglinks", "rej-coeff", "rej-betweenness", "rej-eigencent", "rej-pgrank", "rej-closeness", "aa-n", "aa-avglinks", "aa-coeff", "aa-betweenness", "aa-eigencent", "aa-pgrank", "aa-closeness", "as-n", "as-avglinks", "as-coeff", "as-betweenness", "as-eigencent", "as-pgrank", "as-closeness", "believers-n", "misinfo-avgexpose-n")
write.csv(tmp5, file="scalefreereadings.csv")

#and now to reconcile both bits

sfparam <-read.csv("scalefreesettings.csv")
sfreadings <- read.csv("scalefreereadings.csv")

sfall <- cbind(sfparam,sfreadings)
write.csv(sfall, file="scalefreedata.csv")

#################################################################################
#Random Networks still not done yet
#randomparam <- read.csv("random.csv")
tmp1 <- randomparam[, colno]
tmp1 <- as.data.table(t(tmp1)) #all hail Hadley Wickham
names(tmp1) <- c("n", "spreadern", "ntype", "ptrustavg", "passlimit", "accpthres", "itrustavg", "stepsn", "fininitval", "whoknows")
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
names(tmp5) <- c("agents-n", "links-n", "spreaders-avglinks", "ambiv-n", "ambiv-avglinks", "ambiv-cluster-coeff", "ambiv-betweenness", "ambiv-eigencent", "ambiv-pgrank", "ambiv-closeness", "rej-n", "rej-avglinks", "rej-coeff", "rej-betweenness", "rej-eigencent", "rej-pgrank", "rej-closeness", "aa-n", "aa-avglinks", "aa-coeff", "aa-betweenness", "aa-eigencent", "aa-pgrank", "aa-closeness", "as-n", "as-avglinks", "as-coeff", "as-betweenness", "as-eigencent", "as-pgrank", "as-closeness", "believers-n", "misinfo-avgexpose-n")
write.csv(tmp5, file="randomreadings.csv")

#and now to reconcile both bits

rparam <-read.csv("randomsettings.csv")
rreadings <- read.csv("randomreadings.csv")

rall <- cbind(rparam,rreadings)
write.csv(rall, file="rdata.csv")