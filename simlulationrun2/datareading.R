setwd("C:/Users/Boris/Desktop/Sync me/888 Other academic work/MPSA 2015/data/run2")
library(foreign)
library(data.table)
library(dplyr)
library(ggplot2)

#these two work. We even have a codebook! See Codebook.txt
sw <- as.data.table(read.csv(file="smallworld2data.csv"))
sf <- as.data.table(read.csv(file="scalefree2data.csv"))

#percentage belief?
sw <- mutate(sw, pctbelief = believers.n / (aa.n + as.n) )
sf <- mutate(sf, pctbelief = believers.n / (aa.n + as.n) )

#total number of susceptible links (accept-some + accept-all)
sw <- mutate(sw, suscept.links = ((aa.avglinks * aa.n) + (as.avglinks * as.n)) / (aa.n + as.n))
sf <- mutate(sf, suscept.links = ((aa.avglinks * aa.n) + (as.avglinks * as.n)) / (aa.n + as.n))

#inverted institutional trust
sw<- mutate(sw, mistrust = 100 - itrustavg)
sf<- mutate(sf, mistrust = 100 - itrustavg)

#total trust (institutional + personal)
sw <- mutate(sw, totaltrust = ptrustavg + itrustavg )
sf <- mutate(sf, totaltrust = ptrustavg + itrustavg )

sw$n <- as.factor(sw$agents.n)
sf$n <- as.factor(sf$agents.n)


#calculate connectedness number of susceptible links (accept-some + accept-all)
sw <- mutate(sw, suscept.coeff = ((aa.coeff * aa.n) + (as.coeff * as.n)) / (aa.n + as.n))
sf <- mutate(sf, suscept.coeff = ((aa.coeff * aa.n) + (as.coeff * as.n)) / (aa.n + as.n))

#grouped by n
#swn <- group_by(sw, agents.n)
#sfn <- group_by(sf, agents.n)
