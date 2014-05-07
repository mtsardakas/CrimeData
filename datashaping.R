library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
setwd("~/Dropbox/CrimeData/Merseyside")

filenames<-list.files("~/Dropbox/CrimeData/Merseyside")
dataset <- ldply(filenames, read.csv, header=TRUE)
Southampton<-dataset
Southampton<-Southampton[Southampton$Crime.type!="Drugs",]
 Southampton<-Southampton[Southampton$Crime.type!="Other crime",]
 Southampton<-Southampton[Southampton$Crime.type!="Anti-social behaviour",]
Southampton$Crime.type<-factor(Southampton$Crime.type)
 crime.f<-factor(Southampton$Crime.type)
 dummies <- dummyVars(~Crime.type,data = Southampton)
 dummies2<-predict(dummies, newdata=Southampton)
 dummies2<-as.data.frame(dummies2)
Southampton$Serious<-dummies2$Crime.type.Burglary + dummies2$Crime.type.Robbery + dummies2$"Crime.type.Criminal damage and arson" + dummies2$"Crime.type.Violent crime" + dummies2$"Crime.type.Violence and sexual offences"
 Southampton$Minor<-  dummies2$Crime.type.Shoplifting + dummies2$"Crime.type.Vehicle crime" + dummies2$"Crime.type.Other theft" + dummies2$"Crime.type.Public disorder and weapons" + dummies2$"Crime.type.Theft from the person" + dummies2$"Crime.type.Public order" + dummies2$"Crime.type.Possession of weapons" + dummies2$"Crime.type.Bicycle theft"
aggdata<-aggregate(Serious ~ Month, Southampton, FUN=sum)
 aggdata2<-aggregate(Minor ~ Month, Southampton, FUN=sum)
SouthamptonMerged<-data.frame(Month=aggdata$Month, Serious=aggdata$Serious, Minor=aggdata2$Minor, Location="Southampton")
SouthamptonMerged<-SouthamptonMerged[-(1:9),]
for(i in 1:27)
  SouthamptonMerged$DS[i]<-SouthamptonMerged$Serious[i+1]-SouthamptonMerged$Serious[i] 
for(i in 1:27)
  SouthamptonMerged$DM[i]<-SouthamptonMerged$Minor[i+1]-SouthamptonMerged$Minor[i]
Southampton1<-data.frame(Month=SouthamptonMerged$Month, Amount=SouthamptonMerged$Serious, isin="Serious", Location="Southampton")
Southampton2<-data.frame(Month=SouthamptonMerged$Month, Amount=SouthamptonMerged$Minor, isin="Minor", Location="Southampton")
Southampton3<-rbind(Southampton1, Southampton2)

plotSouthampton<- ggplot(Southampton3, aes(Month,Amount,group=isin, color=isin)) + geom_line() +xlab("Months") + ylab("Criminal activites") + ggtitle("Southampton") + geom_vline(xintercept=c(7,13,19,25), linetype="dotted")  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(values=c("red", "orange"),name="Crime type")

 print(plotSouthampton)
 ggsave("~/Dropbox/CrimeData/Pics/Southampton4.png", height=6.44, width=8.00,units=c("in"), dpi=100)
 dev.off()
  print(plotSouthampton)
 ggsave("~/Dropbox/CrimeData/Pics/Southampton4.eps", height=6.44, width=8.00, units=c("in"), dpi=100)

