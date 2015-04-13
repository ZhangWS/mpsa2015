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

qplot(data=sw, x=rej.avglinks, y=stepsn, colour=factor(swn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a Function of Rejector Connectedness in Small World Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))
qplot(sfn$rej.avglinks, sfn$stepsn, colour=factor(sfn$n))+geom_point()+ labs(title = "Speed of Rumor Penetration as a Function of Rejector Connectedness in Scale Free Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,510))

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

###subset to n = 125, 1-5-10 spreaders
swn125 <- filter(swn, n == 125)
sfn125 <- filter(sfn, n == 125)



# avg links of rejectors
p <- ggplot(data=swn125, aes(x=rej.avglinks, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Small World Network Distributed by Initial Spreader #, N = 125", y="% Believers", x="Avg # Links")

p <- ggplot(data=sfn125, aes(x=rej.avglinks, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 125", y="% Believers", x="Avg # Links")


#avg links of susceptible

p <- ggplot(data=swn125, aes(x=suscept.links, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness \nin Small World Network Distributed by Initial Spreader #, N = 125", y="% Believers", x="Avg # Links")

p <- ggplot(data=sfn125, aes(x=suscept.links, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 125", y="% Believers", x="Avg # Links")

swn125$mistrust <- as.factor(swn125$mistrust)
sfn125$mistrust <- as.factor(sfn125$mistrust)
#check % belief as 

p <- ggplot(data=swn125, aes(x=totaltrust, y=pctbelief, colour = swn125$mistrust)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Institutional and Interpersonal Trust \nin Small World Network Distributed by Initial Spreader #, N = 125", y="% Believers", x="Avg # Links", colour="Trust Level")

p <- ggplot(data=sfn125, aes(x=totaltrust, y=pctbelief, colour = sfn125$mistrust)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 125", y="% Believers", x="Avg # Links", colour="Trust Level")


#subset N= 500
swn500 <- filter(swn, n == 500)
sfn500 <- filter(sfn, n == 500)


# avg links of rejectors
p <- ggplot(data=swn500, aes(x=rej.avglinks, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Small World Network Distributed by Initial Spreader #, N = 500", y="% Believers", x="Avg # Links")

p <- ggplot(data=sfn500, aes(x=rej.avglinks, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 500", y="% Believers", x="Avg # Links")



#avg links of susceptible

p <- ggplot(data=swn500, aes(x=suscept.links, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness \nin Small World Network Distributed by Initial Spreader #, N = 500", y="% Believers", x="Avg # Links")

p <- ggplot(data=sfn500, aes(x=suscept.links, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 500", y="% Believers", x="Avg # Links")

swn500$mistrust <- as.factor(swn500$mistrust)
sfn500$mistrust <- as.factor(sfn500$mistrust)

#check % belief as 

p <- ggplot(data=swn500, aes(x=totaltrust, y=pctbelief, colour = swn500$mistrust)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Institutional and Interpersonal Trust \nin Small World Network Distributed by Initial Spreader #, N = 500", y="% Believers", x="Avg # Links", colour="Trust Level")

p <- ggplot(data=sfn500, aes(x=totaltrust, y=pctbelief, colour = sfn500$mistrust)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 500", y="% Believers", x="Avg # Links", colour="Trust Level")



#N=1500


swn1500 <- filter(swn, n == 1500)
sfn1500 <- filter(sfn, n == 1500)

# avg links of rejectors
p <- ggplot(data=swn1500, aes(x=rej.avglinks, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Small World Network Distributed by Initial Spreader #, N = 1500", y="% Believers", x="Avg # Links")

p <- ggplot(data=sfn1500, aes(x=rej.avglinks, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 1500", y="% Believers", x="Avg # Links")


sf-pctbelief-rejlinks-1500



#avg links of susceptible

p <- ggplot(data=swn1500, aes(x=suscept.links, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness \nin Small World Network Distributed by Initial Spreader #, N = 1500", y="% Believers", x="Avg # Links")

p <- ggplot(data=sfn1500, aes(x=suscept.links, y=pctbelief)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Susceptible Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 1500", y="% Believers", x="Avg # Links")

swn1500$mistrust <- as.factor(swn1500$mistrust)
sfn1500$mistrust <- as.factor(sfn1500$mistrust)
#check % belief as 

p <- ggplot(data=swn1500, aes(x=totaltrust, y=pctbelief, colour = swn1500$mistrust)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Institutional and Interpersonal Trust \nin Small World Network Distributed by Initial Spreader #, N = 1500", y="% Believers", x="Avg # Links", colour="Trust Level")

p <- ggplot(data=sfn1500, aes(x=totaltrust, y=pctbelief, colour = sfn1500$mistrust)) + geom_point()
p + facet_grid(.~spreadern) + labs(title="Proportion of Believers as a Function of Rejector Connectedness \nin Scale Free Network Distributed by Initial Spreader #, N = 1500", y="% Believers", x="Avg # Links", colour="Trust Level")


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