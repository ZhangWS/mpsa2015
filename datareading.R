setwd("C:/Users/Boris/Desktop/Sync me/888 Other academic work/MPSA 2015/data")
library(foreign)
library(data.table)

#these two work. We even have a codebook! See Codebook.txt
sw <- read.csv(file="smallworlddata.csv")
sf <- read.csv(file="scalefreedata.csv")

#not currently working
r <- read.csv(file="randomdata.csv")