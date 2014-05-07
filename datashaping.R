library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("Hmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("xtable", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

# setwd("~/Dropbox/CrimeData/Manchester")
filenames<-list.files("~/Dropbox/CrimeData/Manchester",full.names=TRUE)
dataset <- ldply(filenames, read.csv, header=TRUE)
Manchester<-dataset
Manchester<-Manchester[Manchester$Crime.type!="Drugs",]
 Manchester<-Manchester[Manchester$Crime.type!="Other crime",]
 Manchester<-Manchester[Manchester$Crime.type!="Anti-social behaviour",]
Manchester$Crime.type<-factor(Manchester$Crime.type)
 crime.f<-factor(Manchester$Crime.type)
 dummies <- dummyVars(~Crime.type,data = Manchester)
 dummies2<-predict(dummies, newdata=Manchester)
 dummies2<-as.data.frame(dummies2)
Manchester$Serious<-dummies2$Crime.type.Burglary + dummies2$Crime.type.Robbery + dummies2$"Crime.type.Criminal damage and arson" + dummies2$"Crime.type.Violent crime" + dummies2$"Crime.type.Violence and sexual offences"
 Manchester$Minor<-  dummies2$Crime.type.Shoplifting + dummies2$"Crime.type.Vehicle crime" + dummies2$"Crime.type.Other theft" + dummies2$"Crime.type.Public disorder and weapons" + dummies2$"Crime.type.Theft from the person" + dummies2$"Crime.type.Public order" + dummies2$"Crime.type.Possession of weapons" + dummies2$"Crime.type.Bicycle theft"
aggdata<-aggregate(Serious ~ Month, Manchester, FUN=sum)
 aggdata2<-aggregate(Minor ~ Month, Manchester, FUN=sum)
ManchesterMerged<-data.frame(Month=aggdata$Month, Serious=aggdata$Serious, Minor=aggdata2$Minor, Location="Manchester")
ManchesterMerged<-ManchesterMerged[-(1:9),]
for(i in 1:(length(filenames)-10))
  ManchesterMerged$DS[i]<-ManchesterMerged$Serious[i+1]-ManchesterMerged$Serious[i] 
for(i in 1:(length(filenames)-10))
  ManchesterMerged$DM[i]<-ManchesterMerged$Minor[i+1]-ManchesterMerged$Minor[i]
Manchester1<-data.frame(Month=ManchesterMerged$Month, Amount=ManchesterMerged$Serious, isin="Serious", Location="Manchester")
Manchester2<-data.frame(Month=ManchesterMerged$Month, Amount=ManchesterMerged$Minor, isin="Minor", Location="Manchester")
Manchester3<-rbind(Manchester1, Manchester2)

plotManchester<- ggplot(Manchester3, aes(Month,Amount,group=isin, color=isin)) + geom_line() +xlab("Months") + ylab("Criminal activites") + ggtitle("Manchester") + geom_vline(xintercept=c(7,13,19,25), linetype="dotted")  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(values=c("red", "orange"),name="Crime type")

 print(plotManchester)
#  ggsave("~/Dropbox/CrimeData/Pics/test.png", height=6.44, width=8.00,units=c("in"), dpi=100)
#  dev.off()
#   print(plotManchester)
#  ggsave("~/Dropbox/CrimeData/Pics/test.eps", height=6.44, width=8.00, units=c("in"), dpi=100)

