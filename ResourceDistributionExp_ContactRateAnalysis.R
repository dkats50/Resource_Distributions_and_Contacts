#Skylar Hopkins 2020
#This script analyzes the contact rate data from the 2016 resource distribution exp

#####################################################################################
########################Load packages and data#######################################
#####################################################################################
library(ggplot2)
library(tidyverse)

#This has the observations for each contact - rows are "dyads" or contacts between two snails
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
#needed to know what letter combos exist
dyads_letters <- unique(Dyads$Snail1) 
summary(dyads_letters)

#Make columns for each snail color
Dyads$RY <- rowSums(Dyads == "RY")
Dyads$Y <- rowSums(Dyads == "Y")
Dyads$B <- rowSums(Dyads == "B")
Dyads$C <- rowSums(Dyads == "C")
Dyads$G <- rowSums(Dyads == "G")
Dyads$GB <- rowSums(Dyads == "GB")
Dyads$GP <- rowSums(Dyads == "GP")
Dyads$P <- rowSums(Dyads == "P")
Dyads$PB <- rowSums(Dyads == "PB")
Dyads$R <- rowSums(Dyads == "R")
Dyads$RB <- rowSums(Dyads == "RB")
Dyads$RG <- rowSums(Dyads == "RG")
Dyads$YB <- rowSums(Dyads == "YB")
Dyads$YG <- rowSums(Dyads == "YG")
Dyads$YP <- rowSums(Dyads == "YP")
Dyads$RP <- rowSums(Dyads == "RP")
#Conver wide data into long data grouping by density, tank, and trial
dyads_long <- Dyads %>% group_by(Tank,Trial) %>% summarise_at(vars(c(Y,B,C,G,GB,GP,P,PB,R,RB,RG,YB,YG,YP, RY, RP)), sum) %>% pivot_longer(cols = c(Y,B,C,G,GB,GP,P,PB,R,RB,RG,YB,YG,YP, RY, RP), names_to = "ColorID", values_to = "Count") 
#Removing rows with 0 from the new dataframe
dyads_long <- dyads_long[dyads_long$Count != 0,]

#Merging long data and individual snail data
Snails <- merge(Snails, dyads_long, by = c("Trial", "Tank", "ColorID"), all = TRUE)
Snails$Count[is.na(Snails$Count)] <- 0 

############################################################################
##################Plot mean contacts per tank###############################
############################################################################
#Calculate means and 95% CIs based on a Poisson distribution: http://stats.stackexchange.com/questions/15371/how-to-calculate-a-confidence-level-for-a-poisson-distribution
width<-function (z=1.96, lambda, N) {
  W<-z*sqrt(lambda/N)
  return(W)
}

Means <- Snails %>% 
  group_by(Trial,Tank,SnailDensity, AggregationIndex) %>% 
  summarise(mean = mean(Count), numsnails = n()) %>%
  mutate(highCI = (mean + width(lambda=mean, N=numsnails)), lowCI = (mean - width(lambda=mean, N=numsnails)))
Means$lowCI[Means$lowCI<0]<-0

##Colorblind palette
cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#Plot density vs mean contact rate with aggregation index treatmenst as groups
contacts_plot <- ggplot(Means, aes(SnailDensity, mean))+
  geom_point(aes(color = factor(AggregationIndex)))+
  xlab("Snail Density")+
  ylab("Per Capita Contact Rate")+
  theme_classic()+
  scale_x_continuous(limits = c(0,16))+
  scale_color_manual(values = cb, name = "Aggregation Index")+
  facet_wrap(~AggregationIndex, nrow = 4)+
  geom_errorbar(aes(ymin = highCI, ymax = lowCI), width = 0.1) +
  theme(legend.position = "none")
contacts_plot

ggplot(data=Means, aes(y=mean, x=AggregationIndex, color=SnailDensity)) +
  geom_jitter()

ggplot(data=Means, aes(y=mean, color=AggregationIndex, x=SnailDensity)) +
  geom_jitter()
