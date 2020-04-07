#This is code from a prior experiment that turned "dyads" or paired contact observations
#into contacts per snail
library(readr)

Data <- read_delim(paste(getwd(), "/CodeFromPriorExp/Intraspecific Contact Exp Trials1to4 RawDyadData Dec2017.txt", sep=""), "\t", escape_double = FALSE, trim_ws = TRUE)
View(Data)

TOTALTIME<-45

Colors<-c("B", "C", "G", "GB", "O", "P", "PG", "R", "RB", "RG", "Y", "YB", "YG")

#Apparently this makes a new column where the snail color codes are pasted together with the 
#one earlier in the alphabet first. There's obviously a faster way to do this. I think I did it this
#way because the colors weren't in order or didn't have consistent names or something.
##Make list of unique pair names
Ind1Dummy<-NULL; Ind2Dummy<-NULL
Data$Ind1Alph<-NULL; Data$Ind2Alph<-NULL
Data$Pair<-NULL
for(i in 1:length(Data$Individual1)) {
  for(j in 1:length(Colors)) {
    if(Data$Individual1[i]==Colors[j]) {Ind1Dummy[i]<-which(Colors==Colors[j])}
    if(Data$Individual2[i]==Colors[j]) {Ind2Dummy[i]<-which(Colors==Colors[j])}
  }
  Data$Ind1Alph[i]<-min(c(Ind1Dummy[i], Ind2Dummy[i]))
  Data$Ind2Alph[i]<-max(c(Ind1Dummy[i], Ind2Dummy[i]))
  Data$Pair[i]<-paste(Colors[Data$Ind1Alph[i]], ".", Colors[Data$Ind2Alph[i]],sep="")
}
head(Data$Individual1)
head(Data$Individual2)
head(Data$Pair) ##Good - earlier in alphabet always listed first, so don't have both B-Y and Y-B

##Import the inividual attribute data so we can add contacts to it
##I didn't give you the dataset to load this in - it has one column for snail ID/color and one for size
ContactCounts <- read_delim("~/Documents/DD FD NL/Revision/Intraspecific Contact Exp Trials1to4 DissectionDataBySnail.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
ContactCounts$Contacts<-0

##For each snail color ID in each tank in each trial, count number of contacts it participated in
##Contacts between same dyad at different time points count as separate contacts
#In this code, I used a for loop to add each count to the row for each snail in the second dataset
#with the snail attributes in it. It probably makes more sense to do this quickly using dplyr and then 
#merge the contact count dataset to the snail attribute dataset using the snail name/color/ID
for(i in 1:length(ContactCounts$Contacts)) {
  ContactCounts$Contacts[i]<-sum(Data$Ind1Alph[Data$UniqueTank==ContactCounts$UniqueTank[i]]==which(Colors==ContactCounts$Color[i]))+sum(Data$Ind2Alph[Data$UniqueTank==ContactCounts$UniqueTank[i]]==which(Colors==ContactCounts$Color[i]))
}
ContactCounts$Contacts

##Averages for each tank for each trial (just use dplyr when you do it)
Averages<-aggregate(ContactCounts$Contacts, by=list(ContactCounts$Trial, ContactCounts$Tank, ContactCounts$Treatment), FUN=mean)
names(Averages)<-c("Trial", "Tank", "Treatment", "AveContacts")

##Make a color vector to plot snals from diff trials as diff colors
TrialColors2<-rep("black",length=length(Averages$Trial))
TrialColors2[Averages$Trial=="2"]<-"green"
TrialColors2[Averages$Trial=="3"]<-"purple"
TrialColors2[Averages$Trial=="4"]<-"orange"

par(mfrow=c(1,1), mar=c(3,3,1,1))
plot(Averages$AveContacts~jitter(Averages$Treatment, 0.5), ylim=c(0, 10), pch=21, bg=TrialColors2)
mtext("Helisoma density", side=1, line=2); mtext("Per capita intraspecific contacts", side=2, line=2) 

TankMeans<-aggregate(ContactCounts[,c(6,14)], by=list(ContactCounts$UniqueTank), FUN="mean")
names(TankMeans)<-c("UniqueTank", "Treatment", "Contacts")
plot(TankMeans$Contacts~jitter(TankMeans$Treatment), xlim=c(0, 13))
