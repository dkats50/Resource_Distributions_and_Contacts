#Skylar Hopkins and Rowan Bermnan 2020
#This script analyzes the contact rate data from the 2016 resource distribution exp

#HELLO

#####################################################################################
########################Load packages and data#######################################
#####################################################################################
library(ggplot2)
library(tidyverse)

#This has the observations for each contact - rows are "dyads" or contacts
Dyads<-read.csv(paste(getwd(), "/ResourceDistributionExperiment_ContactData.csv", sep=""))
View(Dyads)

#Quick Dyads checking
table(Dyads$Date, exclude = NULL)
table(Dyads$Trial, exclude = NULL)
table(Dyads$Tank, exclude = NULL)
table(Dyads$Density, exclude = NULL)
#The agg index fraction form got turned into a date. Don't worry about it. Just use
#the AggregationIndex column
table(Dyads$AggregationIndex, exclude = NULL)
#We might need to turn B=BB, C=CC, etc.
table(Dyads$Snail1, exclude=NULL)
table(Dyads$Snail2, exclude=NULL)
table(Dyads$FacetoFaceYN, exclude=NULL)
#If we need to work with times, we'll need to put them in a recognizable format 
hist(Dyads$StartTime) #error
hist(Dyads$Duration)
abline(v=45*60) #this would be the longest possible duration

#This has the info for each snail EXCEPT for per capita contacts
Snails<-read.csv(paste(getwd(), "/ResourceDistributionExp_IndividualSnailData.csv", sep=""))
#View(Snails)
Snails$ColorID<-as.character(Snails$ColorID)

#Some quick checking and cleaning up color codes
table(Snails$Trial)
table(Snails$Tank)
table(Snails$SnailDensity)
table(Snails$AggregationIndex)
table(Snails$TrialDay)
#Agh we'll need to do some clean up here. Wtf is lost, YG1, and YG2?
table(Snails$ColorID)
#Tank 24 (density = 16) Trial 2 had one snail called "lost" and two PB snails. One should be 
#YP and one should be RG. YP had contacts and was big, whereas RG was small and lost.
Snails$ColorID[Snails$ColorID=="lost" & Snails$Tank==24]<-"RG"
Snails$ColorID[Snails$ColorID=="PB" & Snails$Tank==24 & Snails$Trial==2 & Snails$Length>16]<-"YP"
#Tank 14 (density=12) Trial 2 was "not painted" when dissected - called "naked" on the map
#It didn't have any contacts. Prob should be charcoal, so let's call it that
Snails$ColorID[Snails$ColorID=="not painted"]<-"C"
#One time we said BG instead of GB 
Snails$ColorID[Snails$ColorID=="BG"]<-"GB"
#Twice we said BP instead of PB 
Snails$ColorID[Snails$ColorID=="BP"]<-"PB"
unique(Dyads$Snail1) %in% unique(Snails$ColorID) #OK
unique(Dyads$Snail2) %in% unique(Snails$ColorID) #OK
hist(Snails$Length)

#####################################################################################
############Convert observed contacts into contacts per snail########################
#####################################################################################

