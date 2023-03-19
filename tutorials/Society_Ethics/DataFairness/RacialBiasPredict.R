#Load Libraries
library(survival)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(gplots)

#Filtering Data Taken from Original Github
data <- filter(filter(read.csv("~/Documents/GitHub/compas-analysis-master/cox-parsed.csv"), score_text != "N/A"), end > start)  %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))

grp <- data[!duplicated(data$id),]
nrow(grp)

summary(grp$score_factor)

summary(grp$race_factor)

#Counting White Figures
WhiteHit=sum(grp$decile_score>5 & grp$event==1 & grp$race_factor=='Caucasian')
WhiteMiss=sum(grp$decile_score<6 & grp$event==1 & grp$race_factor=='Caucasian')
WhiteFA=sum(grp$decile_score>5 & grp$event==0 & grp$race_factor=='Caucasian')
WhiteCR=sum(grp$decile_score<6 & grp$event==0 & grp$race_factor=='Caucasian')

#Create White Data Frame
Whiteframe= data.frame(name=c("WhiteCR","WhiteHit","WhiteMiss","WhiteFA"),value=c(WhiteCR,WhiteHit,WhiteMiss,WhiteFA))

#Plot White
ggplot(Whiteframe, aes(x=name,y=value))+ geom_bar(stat="identity")+ggtitle("White Statistics")

#Counting Black Figures
BlackMiss=sum(grp$decile_score<6 & grp$event==1 & grp$race_factor=='African-American')
BlackFA=sum(grp$decile_score>5 & grp$event==0 & grp$race_factor=='African-American')
BlackHit=sum(grp$decile_score>5 & grp$event==1 & grp$race_factor=='African-American')
BlackCR=sum(grp$decile_score<6 & grp$event==0 & grp$race_factor=='African-American')

#Creating Black DataFrame
Blackframe= data.frame(name=c("BlackFA","BlackMiss","BlackCR","BlackHit"),value=c(BlackFA,BlackMiss,BlackCR,BlackHit))

#Plot Black
ggplot(Blackframe, aes(x=name,y=value))+ geom_bar(stat="identity") + ggtitle("Black Statistics")
 
#Create White and Black Data Frame for False alarms and misses
WhiteBlackframe  = data.frame(name=c("FalseAlarm","Miss","FalseAlarm","Miss"),value=c(WhiteFA,WhiteMiss,BlackFA,BlackMiss),race=c("white","white","black","black"))

#Plot
ggplot(WhiteBlackframe, aes(x=name,y=value,fill=race))+ geom_bar(stat="identity") +ggtitle("True White/Black Statistics")

#Count number of White and Black people in the study
Black=sum(grp$race_factor=='African-American')
White=sum(grp$race_factor=='Caucasian')

#Find Sum of all participants
ComboWhiteBlack=Black+White

#Find total  amount of false alarms and misses
ComboFA=BlackFA+WhiteFA
ComboMiss=BlackMiss+WhiteMiss

#Find  numbers if there  was no bias
NonbiasedWhiteFA=(White/ComboWhiteBlack)*ComboFA
NonbiasedBlackFA=(Black/ComboWhiteBlack)*ComboFA
NonbiasedWhiteMiss=(White/ComboWhiteBlack)*ComboMiss
NonbiasedBlackMiss=(Black/ComboWhiteBlack)*ComboMiss

#create data frame of unbiased numbers
WhiteBlackframeUnbiased  = data.frame(name=c("FalseAlarm","Miss","FalseAlarm","Miss"),value=c(NonbiasedWhiteFA,NonbiasedWhiteMiss,NonbiasedBlackFA,NonbiasedBlackMiss),race=c("white","white","black","black"))

#plot unbiased numbers
ggplot(WhiteBlackframeUnbiased, aes(x=name,y=value,fill=race))+ geom_bar(stat="identity") + ggtitle("Unbiased White/Black Statistics")


#Counting White Figures
MaleHit=sum(grp$decile_score>5 & grp$event==1 & grp$sex=='Male')
MaleMiss=sum(grp$decile_score<6 & grp$event==1 & grp$sex=='Male')
MaleFA=sum(grp$decile_score>5 & grp$event==0 & grp$sex=='Male')
MaleCR=sum(grp$decile_score<6 & grp$event==0 & grp$sex=='Male')


#Create White Data Frame
Maleframe = data.frame(name=c("MaleFA","MaleMiss","MaleCR","MaleHit"),value=c(MaleFA,MaleMiss,MaleCR,MaleHit))


#Plot Male
ggplot(Maleframe, aes(x=name,y=value))+ geom_bar(stat="identity")+ggtitle("Male Statistics")

#Counting Female Figures
FemaleMiss=sum(grp$decile_score<6 & grp$event==1 & grp$sex=='Female')
FemaleFA=sum(grp$decile_score>5 & grp$event==0 & grp$sex=='Female')
FemaleHit=sum(grp$decile_score>5 & grp$event==1 & grp$sex=='Female')
FemaleCR=sum(grp$decile_score<6 & grp$event==0 & grp$sex=='Female')

#Creating Female DataFrame
Femaleframe = data.frame(name=c("FemaleFA","FemaleMiss","FemaleCR","FemaleHit"),value=c(FemaleFA,FemaleMiss,FemaleCR,FemaleHit))

#Plot
ggplot(Femaleframe,aes(x=name,y=value))+ geom_bar(stat="identity")+ggtitle("Female Statistics")

#Create White and Black Data Frame
MaleFemaleframe  = data.frame(name=c("FalseAlarm","Miss","FalseAlarm","Miss"),value=c(MaleFA,MaleMiss,FemaleFA,FemaleMiss),race=c("Male","Male","Female","Female"))

#Plot
ggplot(MaleFemaleframe, aes(x=name,y=value,fill=race))+ geom_bar(stat="identity")+ggtitle("True Male/Female Statistics")

#Find numbers to find unbiased numbers similar to race calculations
Male=sum(grp$sex=='Male')
Female=sum(grp$sex=='Female')
ComboMaleFemale=Male+Female
ComboFASex=MaleFA+FemaleFA
ComboMissSex=MaleMiss+FemaleMiss
NonbiasedMaleFA=(Male/ComboMaleFemale)*ComboFASex
NonbiasedFemaleFA=(Female/ComboMaleFemale)*ComboFASex
NonbiasedMaleMiss=(Male/ComboMaleFemale)*ComboMissSex
NonbiasedFemaleMiss=(Female/ComboMaleFemale)*ComboMissSex

#Crete dataframe
MaleFemaleframeNonbiased  = data.frame(name=c("FalseAlarm","Miss","FalseAlarm","Miss"),value=c(NonbiasedMaleFA,NonbiasedMaleMiss,NonbiasedFemaleFA,NonbiasedFemaleMiss),sex=c("Male","Male","Female","Female"))

#Plot
ggplot(MaleFemaleframeNonbiased, aes(x=name,y=value,fill=sex))+ geom_bar(stat="identity")+ggtitle("Unbiased Male/Female Statistics")


#Sex and Race

#False Alarm 

#Finding False Alarm Rate for Each Group
WhiteMaleFA=sum(grp$decile_score>5 & grp$event==0 & grp$race_factor=='Caucasian' & grp$sex=='Male')
BlackMaleFA=sum(grp$decile_score>5 & grp$event==0 & grp$race_factor=='African-American' & grp$sex=='Male')
WhiteFemaleFA=sum(grp$decile_score>5 & grp$event==0 & grp$race_factor=='Caucasian' & grp$sex=='Female')
BlackFemaleFA=sum(grp$decile_score>5 & grp$event==0 & grp$race_factor=='African-American' &  grp$sex=='Female')

#Finding amount of each group in the sample
WhiteMales=sum(grp$race=='Caucasian' & grp$sex=='Male')
BlackMales=sum(grp$race=='African-American' & grp$sex=='Male')
WhiteFemales=sum(grp$race=='Caucasian' & grp$sex=='Female')
BlackFemales=sum(grp$race=='African-American' & grp$sex=='Female')

#Find Total sample population
ComboRaceSex=WhiteMales+BlackMales+WhiteFemales+BlackFemales

#Find Total amount of false alarms
ComboFAAll=WhiteMaleFA+BlackMaleFA+WhiteFemaleFA+BlackFemaleFA

#Calculate number of false alarms per group if there was no bias
UBWhiteMaleFA=(WhiteMales/ComboRaceSex)*ComboFAAll
UBBlackMaleFA=(BlackMales/ComboRaceSex)*ComboFAAll
UBWhiteFemaleFA=(WhiteFemales/ComboRaceSex)*ComboFAAll
UBBlackFemaleFA=(BlackFemales/ComboRaceSex)*ComboFAAll

#Create dataframe
RaceMaleframe=data.frame(Name=c("WM","WM-","BM","BM-","WF","WF-","BF","BF-"),Values=c(WhiteMaleFA,UBWhiteMaleFA,BlackMaleFA,UBBlackMaleFA,WhiteMaleFA,UBWhiteFemaleFA,BlackFemale,UBBlackFemaleFA),At=c("True","Unbiased","True","Unbiased","True","Unbiased","True","Unbiased"))

#Plot graph
ggplot(RaceMaleframe, aes(x=Name,y=Values,fill=At))+ geom_col(stat="identity")+ggtitle("False  Alarm Rates based on Race and Sex")


#Miss

#Find the miss rate for each group
WhiteMaleMiss=sum(grp$decile_score<6 & grp$event==1 & grp$race_factor=='Caucasian' & grp$sex=='Male')
BlackMaleMiss=sum(grp$decile_score<6 & grp$event==1 & grp$race_factor=='African-American' & grp$sex=='Male')
WhiteFemaleMiss=sum(grp$decile_score<5 & grp$event==1 & grp$race_factor=='Caucasian' & grp$sex=='Female')
BlackFemaleMiss=sum(grp$decile_score<5 & grp$event==1 & grp$race_factor=='African-American' &  grp$sex=='Female')

#Find the Total misses  over every group
ComboMissAll=WhiteMaleMiss+BlackMaleMiss+WhiteFemaleMiss+BlackFemaleMiss

#Calculate number of misses per group if there was no bias 
UBWhiteMaleMiss=(WhiteMales/ComboRaceSex)*ComboMissAll
UBBlackMaleMiss=(BlackMales/ComboRaceSex)*ComboMissAll
UBWhiteFemaleMiss=(WhiteFemales/ComboRaceSex)*ComboMissAll
UBBlackFemaleMiss=(BlackFemales/ComboRaceSex)*ComboMissAll

#Create Data Frame
RaceMaleMiss=data.frame(Name=c("WM","WM-","BM","BM-","WF","WF-","BF","BF-"),Values=c(WhiteMaleMiss,UBWhiteMaleMiss,BlackMaleMiss,UBBlackMaleMiss,WhiteMaleMiss,UBWhiteFemaleMiss,BlackFemaleMiss,UBBlackFemaleMiss),At=c("True","Unbiased","True","Unbiased","True","Unbiased","True","Unbiased"))

#Plot results
ggplot(RaceMaleMiss, aes(x=Name,y=Values,fill=At))+ geom_col(stat="identity")+ggtitle("Miss Rates based on Race and Sex")




