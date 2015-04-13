setwd("C:/Users/Boris/Desktop/Sync me/888 Other academic work/MPSA 2015/data")
library(foreign)
library(data.table)
library(dplyr)

#these two work. We even have a codebook! See Codebook.txt
sw <- read.csv(file="smallworlddata.csv")
sf <- read.csv(file="scalefreedata.csv")



#basic plots?
plot(sw$spreadern, sw$stepsn)
plot(sf$spreadern, sf$stepsn)



groupa <- groupby(sw, )











#not currently working
r <- read.csv(file="randomdata.csv")