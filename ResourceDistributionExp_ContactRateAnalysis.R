#Skylar Hopkins and Rowan Bermnan 2020
#This script analyzes the contact rate data from the 2016 resource distribution exp

#####################################################################################
########################Load packages and data#######################################
#####################################################################################
library(ggplot2)
library(tidyverse)

Data<-read.csv(paste(getwd(), "/ResourceDistributionExperiment_ContactData.csv", sep=""))
View(Data)

#Quick data checking
table(Data$Date, exclude = NULL)
table(Data$Trial, exclude = NULL)
table(Data$Tank, exclude = NULL)
table(Data$Density, exclude = NULL)
#The agg index fraction form got turned into a date. Don't worry about it. Just use
#the AggregationIndex column
table(Data$AggregationIndex, exclude = NULL)
#We might need to turn B=BB, C=CC, etc.
table(Data$Snail1, exclude=NULL)
table(Data$Snail2, exclude=NULL)
table(Data$FacetoFaceYN, exclude=NULL)
#If we need to work with times, we'll need to put them in a recognizable format 
hist(Data$StartTime) #error
hist(Data$Duration)
abline(v=45*60) #this would be the longest possible duration

#####################################################################################
############Convert observed contacts into contacts per snail########################
#####################################################################################

