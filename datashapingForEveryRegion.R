names=c("Birmingham", "Manchester", "Leeds - Bradford", "Liverpool - Merseyside", "Southampton - Portsmouth", "Newcastle - Sunderland", "Nottingham - Derby","Sheffield", "Suffolk", "Norfolk", "Cambridgeshire","Lincolnshire", "Cumbria", "Durham", "Sussex", "Devon - Cornwall")
# names=c("Birmingham","Manchester","Leeds - Bradford")
setwd("/Users/Michalis/Dropbox/CrimeData")
for(i in names){
  filenames<-list.files(paste0("/Users/Michalis/Dropbox/CrimeData/",i),full.names=TRUE)
  areaName<-i
  dataset <- ldply(filenames, read.csv, header=TRUE)
  areaName<-dataset
  areaName<-areaName[areaName$Crime.type!="Drugs",]
  areaName<-areaName[areaName$Crime.type!="Other crime",]
  areaName<-areaName[areaName$Crime.type!="Anti-social behaviour",]
  areaName$Crime.type<-factor(areaName$Crime.type)
  crime.f<-factor(areaName$Crime.type)
  dummies <- dummyVars(~Crime.type,data = areaName)
  dummies2<-predict(dummies, newdata=areaName)
  dummies2<-as.data.frame(dummies2)
  areaName$Serious<-dummies2$Crime.type.Burglary + dummies2$Crime.type.Robbery + dummies2$"Crime.type.Criminal damage and arson" + dummies2$"Crime.type.Violent crime" + dummies2$"Crime.type.Violence and sexual offences"
  areaName$Minor<-  dummies2$Crime.type.Shoplifting + dummies2$"Crime.type.Vehicle crime" + dummies2$"Crime.type.Other theft" + dummies2$"Crime.type.Public disorder and weapons" + dummies2$"Crime.type.Theft from the person" + dummies2$"Crime.type.Public order" + dummies2$"Crime.type.Possession of weapons" + dummies2$"Crime.type.Bicycle theft"
  aggdata<-aggregate(Serious ~ Month, areaName, FUN=sum)
  aggdata2<-aggregate(Minor ~ Month, areaName, FUN=sum)
  areaNameMerged<-data.frame(Month=aggdata$Month, Serious=aggdata$Serious, Minor=aggdata2$Minor, Location=i)
  areaNameMerged<-areaNameMerged[-(1:9),]
  for(j in 1:(length(filenames)-10)){
    areaNameMerged$DS[j]<-areaNameMerged$Serious[j+1]-areaNameMerged$Serious[j] 
    areaNameMerged$DM[j]<-areaNameMerged$Minor[j+1]-areaNameMerged$Minor[j]
  }
  areaNameMerged$DS[length(filenames)-9]<-NA
  areaNameMerged$DM[length(filenames)-9]<-NA

  assign(paste0(i,"Merged"),areaNameMerged)
  areaName1<-data.frame(Month=areaNameMerged$Month, Amount=areaNameMerged$Serious, isin="Serious", Location=i)
  areaName2<-data.frame(Month=areaNameMerged$Month, Amount=areaNameMerged$Minor, isin="Minor", Location=i)
  areaName3<-rbind(areaName1, areaName2)
  
  plotareaName<- ggplot(areaName3, aes(Month,Amount,group=isin, color=isin)) + geom_line() +xlab("Months") + ylab("Criminal activites") + ggtitle(i) + geom_vline(xintercept=c(7,13,19,25), linetype="dotted")  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(values=c("red", "orange"),name="Crime type")
  assign(paste0("plot",i),plotareaName)
  print(plotareaName)
  ggsave(paste0("~/Dropbox/CrimeData/Cors/test",i,".png"), height=6.44, width=8.00,units=c("in"), dpi=100)
  dev.off()
}
LeedsMerged<-`Leeds - BradfordMerged`
LiverpoolMerged<-`Liverpool - MerseysideMerged`
SouthamptonMerged<-`Southampton - PortsmouthMerged`
NewcastleMerged<-`Newcastle - SunderlandMerged`
NottinghamMerged<-`Nottingham - DerbyMerged`
DevonMerged<-`Devon - CornwallMerged`