library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("Hmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("xtable", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
filenamestest<-(length(list.files("~/Dropbox/CrimeData/South Hampshire")) + length(list.files("~/Dropbox/CrimeData/Merseyside")))/2
agg<- matrix(0, ncol = 37, nrow =length(filenames)) 
agg2<- matrix(0, ncol = 37, nrow = length(filenames)) 

pb <- txtProgressBar(min = 0, max = 37, style = 3)
for(i in 1:37){
  Sys.sleep(0.1)
setTxtProgressBar(pb, i)
setwd(paste0("~/Dropbox/CrimeData/All/", i))
filenames<-list.files(paste0("~/Dropbox/CrimeData/All/", i),full.names=TRUE)
dataset <- ldply(filenames, read.csv, header=TRUE)
All<-dataset
All<-All[All$Crime.type!="Drugs",]
All<-All[All$Crime.type!="Other crime",]
All<-All[All$Crime.type!="Anti-social behaviour",]
All$Crime.type<-factor(All$Crime.type)
crime.f<-factor(All$Crime.type)
dummies <- dummyVars(~Crime.type,data = All)
dummies2<-predict(dummies, newdata=All)
dummies2<-as.data.frame(dummies2)
All$Serious<-dummies2$Crime.type.Burglary + dummies2$Crime.type.Robbery + dummies2$"Crime.type.Criminal damage and arson" + dummies2$"Crime.type.Violent crime" + dummies2$"Crime.type.Violence and sexual offences"
All$Minor<-  dummies2$Crime.type.Shoplifting + dummies2$"Crime.type.Vehicle crime" + dummies2$"Crime.type.Other theft" + dummies2$"Crime.type.Public disorder and weapons" + dummies2$"Crime.type.Theft from the person" + dummies2$"Crime.type.Public order" + dummies2$"Crime.type.Possession of weapons" + dummies2$"Crime.type.Bicycle theft"
aggdata<-aggregate(Serious ~ Month, All, FUN=sum)
aggdata<-as.data.frame(aggdata)
aggdata2<-aggregate(Minor ~ Month, All, FUN=sum)
aggdata2<-as.data.frame(aggdata2)
agg[,i]<-aggdata$Serious
agg2[,i]<-aggdata2$Minor

}
# aggMerged<-data.frame(Amount=rowSums(agg), Month=aggdata$Month)
# aggMerged2<-data.frame(Amount=rowSums(agg), Month=aggdata2$Month)
agg<-as.data.frame(agg)
agg2<-as.data.frame(agg2)
 AllMerged<-data.frame(Month=aggdata$Month, Serious=rowSums(agg), Minor=rowSums(agg2))
 AllMerged<-AllMerged[-(1:9),]
for(j in 1:(length(filenames)-10))
  AllMerged$DS[j]<-AllMerged$Serious[j+1]-AllMerged$Serious[j] 
for(j in 1:(length(filenames)-10))
  AllMerged$DM[j]<-AllMerged$Minor[j+1]-AllMerged$Minor[j]
All1<-data.frame(Month=AllMerged$Month, Amount=AllMerged$Serious, isin="Serious", Location="All")
All2<-data.frame(Month=AllMerged$Month, Amount=AllMerged$Minor, isin="Minor", Location="All")
All3<-rbind(All1, All2)

plotAll<- ggplot(All3, aes(Month,Amount,group=isin, color=isin)) + geom_line() +xlab("Months") + ylab("Criminal activites") + ggtitle("All") + geom_vline(xintercept=c(7,13,19), linetype="dotted")  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(values=c("red", "orange"),name="Crime type")
close(pb)
# print(plotAll)
# ggsave("~/Dropbox/CrimeData/Pics/All3.png", height=6.44, width=8.00,units=c("in"), dpi=100)
# dev.off()
#  print(plotAll)
# ggsave("~/Dropbox/CrimeData/Pics/All3.eps", height=6.44, width=8.00, units=c("in"), dpi=100)