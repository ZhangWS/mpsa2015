#TO REPLICATE OUR ANALYSIS
#RUN DATACLEANING.R FIRST
#RUN DATAREADING.R SECOND
#RUN RUN2PROCESSING.R THIRD

####################################
#TABLE 2
####################################


#let's extract some subsetted data tables...

#first, let's subset by N and spreader n
swn125sp1 <- filter(sw, agents.n == 120 & spreadern == 1)
sfn125sp1 <- filter(sf, agents.n == 125 & spreadern == 1)

#by acceptance threshold
summary(swn125sp1$pctbelief[swn125sp1$accpthres == 25])
summary(swn125sp1$pctbelief[swn125sp1$accpthres == 50])
summary(swn125sp1$pctbelief[swn125sp1$accpthres == 75])



summary(sfn125sp1$pctbelief[sfn125sp1$accpthres == 25])
summary(sfn125sp1$pctbelief[sfn125sp1$accpthres == 50])
summary(sfn125sp1$pctbelief[sfn125sp1$accpthres == 75])


#average size
swn500sp1 <- filter(sw, agents.n == 500 & spreadern == 1)
sfn500sp1 <- filter(sf, agents.n == 500 & spreadern == 1)

#by acceptance threshold
summary(swn500sp1$pctbelief[swn500sp1$accpthres == 25])
summary(swn500sp1$pctbelief[swn500sp1$accpthres == 50])
summary(swn500sp1$pctbelief[swn500sp1$accpthres == 75])



summary(sfn500sp1$pctbelief[sfn500sp1$accpthres == 25])
summary(sfn500sp1$pctbelief[sfn500sp1$accpthres == 50])
summary(sfn500sp1$pctbelief[sfn500sp1$accpthres == 75])


#big size
swn1500sp1 <- filter(sw, agents.n == 1500 & spreadern == 1)
sfn1500sp1 <- filter(sf, agents.n == 1500 & spreadern == 1)


############################################
#Table 3
############################################

#by acceptance threshold
summary(swn1500sp1$pctbelief[swn1500sp1$accpthres == 25])
summary(swn1500sp1$pctbelief[swn1500sp1$accpthres == 50])
summary(swn1500sp1$pctbelief[swn1500sp1$accpthres == 75])


summary(sfn1500sp1$pctbelief[sfn1500sp1$accpthres == 25])
summary(sfn1500sp1$pctbelief[sfn1500sp1$accpthres == 50])
summary(sfn1500sp1$pctbelief[sfn1500sp1$accpthres == 75])



###########################################
#initial conditions set for following figures
swn <- filter(sw, spreadern==1 & ptrustavg == 75)
sfn <- filter(sf, spreadern==1 & ptrustavg == 75)



###########################################
#Figure 1a: Speed of Rumor Penetration in Small World Network
###########################################

qplot(swn$n, swn$steps, colour=factor(swn$accpthres)) + geom_point(alpha=0.5) + labs(title = "Speed of Rumor Penetration in Small World Network", x="# Agents", y="# Iterations", colour="Acceptance \n Threshold") + coord_cartesian(ylim = c(0,25))

###########################################
#Figure 1b: Speed of Rumor Penetration in Scale Free Network
###########################################
qplot(sf$n, sf$steps , colour=factor(sf$spreadern)) + geom_point(alpha = 0.5) + labs(title = "Speed of Rumor Penetration in Scale Free Network", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,50))



########################################################
# Figure 2a & 2b Proportion of Believers as a Function of Rejector Connectedness
########################################################


p <- ggplot(data=swn, aes(x=rej.avglinks, y=pctbelief, colour=factor(accpthres))) + geom_point(alpha=0.5)
p + facet_grid(.~noden) + labs(title="Proportion of Believers as a Function of Rejector Connectedness\n in Small World Network", y="% Believers", x="Avg # Links")+ labs(colour="Acceptance\nThreshold")+coord_cartesian(xlim = c(0,7))

p <- ggplot(data=sfn, aes(x=rej.avglinks, y=pctbelief, colour=factor(accpthres))) + geom_point(alpha=0.5)
p + facet_grid(.~noden) + labs(title="% Belief as a Function of Rejector Connectedness\n in Scale Free Network, Initial Spreader = 1", y="% Believers", x="Avg # Links")+ labs(colour="Acceptance\nThreshold")+coord_cartesian(xlim = c(0,7))


###############
#Figure 3a & 3b & 3c Speed of Rumor Penetration as a Function \nof Susceptible Agent Connectedness
###############

qplot(data=swn, x=suscept.links, y=stepsn, colour=factor(swn$n))+geom_point(alpha=0.5) + labs(title = "Speed of Rumor Penetration as a Function \nof Susceptible Agent Connectedness in Small World Network", x="Avg # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,20)) + facet_grid(.~n)

qplot(sfn$suscept.links, sfn$stepsn, colour=factor(sfn$n))+geom_point() + labs(title = "Speed of Rumor Penetration as a function of \nof Susceptible Agent Connectedness in Scale Free Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,50))


p <- ggplot(data = sfn, aes(x=suscept.links, y=stepsn, colour=factor(accpthres))) + geom_point(alpha=0.5)
p + facet_grid(.~noden) + labs(title = "Speed of Rumor Penetration as a function of \nof Susceptible Agent Connectedness in Scale Free Network", x="Average # of Links", y="# Iterations", colour="# Agents") + coord_cartesian(ylim = c(0,510)) 

##########################
#Figure 4a, 4b, 4c Speed of Rumor Penetration
##########################

qplot(sw$n, sw$steps , colour=factor(sw$spreadern)) + geom_point() + labs(title = "Speed of Rumor Penetration in Small World Network", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,25))

qplot(sf$n, sf$steps , colour=factor(sf$spreadern)) + geom_point(alpha = 0.5) + labs(title = "Speed of Rumor Penetration in Scale Free Network, Enlarged", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,50))

qplot(sf$n, sf$steps , colour=factor(sf$spreadern)) + geom_point(alpha = 0.5) + labs(title = "Speed of Rumor Penetration in Scale Free Network", x="# Agents", y="# Iterations", colour="# Initial Spreaders") + coord_cartesian(ylim = c(0,510))
