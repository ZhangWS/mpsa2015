setwd("C:/Users/Boris/Desktop/Sync me/888 Other academic work/MPSA 2015/data")
library(foreign)
library(data.table)
library(dplyr)
library(ggplot2)

#these two work. We even have a codebook! See Codebook.txt
sw <- read.csv(file="smallworlddata.csv")
sf <- read.csv(file="scalefreedata.csv")

#######ADD COLUMNS
#percentage belief?
sw <- mutate(sw, pctbelief = believers.n / n )
sf <- mutate(sf, pctbelief = believers.n / n )

#total number of susceptible links (accept-some + accept-all)
sw <- mutate(sw, suscept.links = ((aa.avglinks * aa.n) + (as.avglinks * as.n)) / (aa.n + as.n))
sf <- mutate(sf, suscept.links = ((aa.avglinks * aa.n) + (as.avglinks * as.n)) / (aa.n + as.n))

#inverted institutional trust
sw<- mutate(sw, mistrust = 100 - itrustavg)
sf<- mutate(sf, mistrust = 100 - itrustavg)

#total trust (institutional + personal)
sw <- mutate(sw, totaltrust = ptrustavg + itrustavg )
sf <- mutate(sf, totaltrust = ptrustavg + itrustavg )

sw$n <- as.factor(sw$n)
sf$n <- as.factor(sf$n)

#grouped by n
swn <- group_by(sw, n)
sfn <- group_by(sf, n)

#as a function of agent n

qplot(sw$n, sw$steps , colour=factor(sw$spreadern)) + geom_point() + labs(title = "Speed of Rumor Penetration in Small World Network", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,25))

qplot(sf$n, sf$steps , colour=factor(sf$spreadern)) + geom_point() + labs(title = "Speed of Rumor Penetration in Scale Free Network", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,50))

qplot(sf$n, sf$steps , colour=factor(sf$spreadern)) + geom_point() + labs(title = "Speed of Rumor Penetration in Scale Free Network", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,50))

qplot(sf$n, sf$steps , colour=factor(sf$spreadern)) + geom_point() + labs(title = "Speed of Rumor Penetration in Scale Free Network", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,510))

#as a function of rejector connectedness

qplot(data=sw, x=rej.avglinks, y=stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function of Rejector Connectedness in Small World Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,25))

qplot(data=sw, x=rej.avglinks, y=stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function of Rejector Connectedness in Small World Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))

qplot(data=sf, x=rej.avglinks, y=stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function of Rejector Connectedness in Small World Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))
qplot(sf$rej.avglinks, sfn$stepsn, colour=factor(sfn$n))+geom_point()+ labs(title = "Speed of Rumor Penetration as a Function of Rejector Connectedness in Scale Free Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,510))

#as a function of spreader n

qplot(swn$spreadern, swn$stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function of # Initial Spreaders in Small World Network", x="# Initial Spreaders", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))

qplot(sfn$spreadern, sfn$stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function of # Initial Spreaders in Scale Free Network", x="# of Initial Spreaders", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))

qplot(sfn$spreadern, sfn$stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function of # Initial Spreaders in Scale Free Network", x="# of Initial Spreaders", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,510))

#as a function of avg susceptible links

qplot(data=sw, x=suscept.links, y=stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function \nof Susceptible Agent Connectedness in Small World Network", x="Avg # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))

qplot(sfn$suscept.links, sfn$stepsn, colour=factor(sfn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a function of \nof Susceptible Agent Connectedness in Scale Free Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))


qplot(sfn$suscept.links, sfn$stepsn, colour=factor(sfn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a function of \nof Susceptible Agent Connectedness in Scale Free Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,510))


#summaries requested by Leslie and the gang
swnsum <- summarise(swn, min(ambiv.avglinks), mean(ambiv.avglinks), max(ambiv.avglinks), min(rej.avglinks), mean(rej.avglinks), max(rej.avglinks), min(aa.avglinks), mean(aa.avglinks), max(aa.avglinks), min(as.avglinks), mean(as.avglinks), max(as.avglinks)) 

sfnsum <- summarise(sfn, min(ambiv.avglinks), mean(ambiv.avglinks), max(ambiv.avglinks), min(rej.avglinks), mean(rej.avglinks), max(rej.avglinks), min(aa.avglinks), mean(aa.avglinks), max(aa.avglinks), min(as.avglinks), mean(as.avglinks), max(as.avglinks))

#write out to CSV
write.csv(swnsum, "swnsum.csv")
write.csv(sfnsum, "sfnsum.csv")


######################################
#grouped by n and spreadern
swnsprn <- group_by(sw, n, spreadern)
sfnsprn <- group_by(sf, n, spreadern)

#yet more summaries for Leslie and the Cool gang
swnsprnsum <- summarise(swnsprn, min(spreaders.avglinks), mean(spreaders.avglinks), max(spreaders.avglinks))
sfnsprnsum <- summarise(sfnsprn, min(spreaders.avglinks), mean(spreaders.avglinks), max(spreaders.avglinks))

#write out more more more!

write.csv(swnsprnsum, "swnsprnsum.csv")
write.csv(sfnsprnsum, "sfnsprnsum.csv")

#summarize % belief 
swnsprnsum2 <- summarise(swnsprn, min(pctbelief), mean(pctbelief), max(pctbelief))
sfnsprnsum2 <- summarise(sfnsprn, min(pctbelief), mean(pctbelief), max(pctbelief))

write.csv(swnsprnsum2, "swnsprnbeliefsum.csv")
write.csv(sfnsprnsum2, "sfnsprnbeliefsum.csv")


###############################################################
#subset to smaller numbers to check distributional graphs


# avg links of rejectors
p <- ggplot(data=swn, aes(x=rej.avglinks, y=pctbelief, colour=n)) + geom_point(alpha=0.5)
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness in Small World Network ", y="% Believers", x="Avg # Links")+ labs(colour="Total # agents\nin simulation")

p <- ggplot(data=sfn, aes(x=rej.avglinks, y=pctbelief, colour=n)) + geom_point(alpha=0.5)
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness in Scale Free Network", y="% Believers", x="Avg # Links")+ labs(colour="Total # agents\nin simulation")


#avg links of susceptible

p <- ggplot(data=swn, aes(x=suscept.links, y=pctbelief, colour=n)) + geom_point(alpha=0.5)
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness in Small World Network Distributed by Initial Spreader #", y="% Believers", x="Avg # Links") + labs(colour="Total Agents")

p <- ggplot(data=sfn, aes(x=suscept.links, y=pctbelief, colour=n)) + geom_point(alpha=0.5)
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness in Scale Free Network", y="% Believers", x="Avg # Links") + labs(colour="Total # agents\nin simulation")

#check % belief as 

p <- ggplot(data=swn, aes(x=totaltrust, y=pctbelief, colour = swn125$mistrust)) + geom_point(alpha=0.5)
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Institutional and Interpersonal Trust in Small World Network", y="% Believers", x="Avg # Links", colour="Trust Level")+ labs(colour="Total # agents\nin simulation")

p <- ggplot(data=sfn, aes(x=totaltrust, y=pctbelief, colour = sfn125$mistrust)) + geom_point(alpha=0.5)
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Scale Free Network", y="% Believers", x="Avg # Links", colour="Trust Level") + labs(colour="Total # agents\nin simulation")




############################################################################
#giant sums of doom



swg <- group_by(sw, n, spreadern, ptrustavg, itrustavg, accpthres)
sfg <- group_by(sf, n, spreadern, ptrustavg, itrustavg, accpthres)

swmeans <- summarise(swg, mean(stepsn), mean(links.n), mean(spreaders.avglinks), mean(ambiv.n), mean(ambiv.avglinks), mean(ambiv.cluster.coeff), mean(ambiv.betweenness), mean(ambiv.eigencent), mean(ambiv.pgrank), mean(ambiv.closeness),mean(rej.n), mean(rej.avglinks), mean(rej.coeff), mean(rej.betweenness), mean(rej.eigencent), mean(rej.pgrank), mean(rej.closeness),mean(aa.n), mean(aa.avglinks), mean(aa.coeff), mean(aa.betweenness), mean(aa.eigencent), mean(aa.pgrank), mean(aa.closeness), mean(as.n), mean(as.avglinks), mean(as.coeff), mean(as.betweenness), mean(as.eigencent), mean(as.pgrank), mean(as.closeness), mean(believers.n), mean(misinfo.avgexpose.n), mean(pctbelief), mean(suscept.links), mean(mistrust), mean(totaltrust))

sfmeans <- summarise(sfg, mean(stepsn), mean(links.n), mean(spreaders.avglinks), mean(ambiv.n), mean(ambiv.avglinks), mean(ambiv.cluster.coeff), mean(ambiv.betweenness), mean(ambiv.eigencent), mean(ambiv.pgrank), mean(ambiv.closeness),mean(rej.n), mean(rej.avglinks), mean(rej.coeff), mean(rej.betweenness), mean(rej.eigencent), mean(rej.pgrank), mean(rej.closeness),mean(aa.n), mean(aa.avglinks), mean(aa.coeff), mean(aa.betweenness), mean(aa.eigencent), mean(aa.pgrank), mean(aa.closeness), mean(as.n), mean(as.avglinks), mean(as.coeff), mean(as.betweenness), mean(as.eigencent), mean(as.pgrank), mean(as.closeness), mean(believers.n), mean(misinfo.avgexpose.n), mean(pctbelief), mean(suscept.links), mean(mistrust), mean(totaltrust))

qplot(swmeans$'mean(spreaders.avglinks)', swmeans$'mean(pctbelief)', colour=factor(swmeans$spreadern)) +geom_point()
qplot(sfmeans$'mean(spreaders.avglinks)', sfmeans$'mean(pctbelief)', colour=factor(swmeans$spreadern)) +geom_point()

qplot(swmeans$'mean(rej.avglinks)', swmeans$'mean(pctbelief)', colour=factor(swmeans$spreadern)) +geom_point()
qplot(sfmeans$'mean(rej.avglinks)', sfmeans$'mean(pctbelief)', colour=factor(swmeans$spreadern)) +geom_point()


write.csv(swmeans, "swmeans.csv")
write.csv(sfmeans, "sfmeans.csv")




###########################################################

#not currently working
r <- read.csv(file="randomdata.csv")