#Load packages 
library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("data.table", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggmap", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("Hmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("xtable", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("maps", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("maptools", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("RgoogleMaps", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("rworldmap", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("spatial", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("descr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("corrplot", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#Create data frame and discard unwanted crime categories 
setwd("~/Dropbox/CrimeData/Manchester")
filenames<-list.files("~/Dropbox/CrimeData/Manchester")
dataset <- ldply(filenames, read.csv, header=TRUE)
ManchesterTest<-dataset
ManchesterTest<-ManchesterTest[ManchesterTest$Crime.type!="Drugs",]
ManchesterTest<-ManchesterTest[ManchesterTest$Crime.type!="Other crime",]
ManchesterTest<-ManchesterTest[ManchesterTest$Crime.type!="Anti-social behaviour",]
ManchesterTest$Crime.type<-factor(ManchesterTest$Crime.type)
crime.f<-factor(ManchesterTest$Crime.type)
dummies <- dummyVars(~Crime.type,data = ManchesterTest)
dummies2<-predict(dummies, newdata=ManchesterTest)
dummies2<-as.data.frame(dummies2)
ManchesterTest$Serious<-dummies2$Crime.type.Burglary + dummies2$Crime.type.Robbery + dummies2$"Crime.type.Criminal damage and arson" + dummies2$"Crime.type.Violent crime" + dummies2$"Crime.type.Violence and sexual offences"
ManchesterTest$Minor<-  dummies2$Crime.type.Shoplifting + dummies2$"Crime.type.Vehicle crime" + dummies2$"Crime.type.Other theft" + dummies2$"Crime.type.Public disorder and weapons" + dummies2$"Crime.type.Theft from the person" + dummies2$"Crime.type.Public order" + dummies2$"Crime.type.Possession of weapons" + dummies2$"Crime.type.Bicycle theft"

#Split in blocks geographically and check frequencies
ManchesterTest$Lon <- as.numeric(cut(ManchesterTest$Longitude,10))
ManchesterTest$Lat <- as.numeric(cut(ManchesterTest$Latitude,10))
freqLon <- freq(ManchesterTest$Lon,plot=FALSE)
freqLat <- freq(ManchesterTest$Lat,plot=FALSE)
freqLon <- as.data.frame(freqLon)
freqLat <- as.data.frame(freqLat)
ManchesterTest2<-ManchesterTest
ManchesterTest3<-subset(ManchesterTest2, ManchesterTest2$Lon==5 |ManchesterTest2$Lon==6 | ManchesterTest2$Lon==7 | ManchesterTest2$Lon==8 | ManchesterTest2$Lon==9)
ManchesterTest4<-subset(ManchesterTest3, ManchesterTest3$Lat==4 |ManchesterTest3$Lat==5 | ManchesterTest3$Lat==6 | ManchesterTest3$Lat==7 | ManchesterTest3$Lat==8)
ManchesterTestFinal<-ManchesterTest4
df<-data.frame(x=ManchesterTestFinal$Longitude,y=ManchesterTestFinal$Latitude,z=ManchesterTestFinal$Serious)

#Plot map 

# ManchesterMap=get_map(location=c(min(ManchesterTestFinal$Longitude),min(ManchesterTestFinal$Latitude),max(ManchesterTestFinal$Longitude),max(ManchesterTestFinal$Latitude)),color="bw",crop=TRUE,)
# ManchesterMap2=get_map(location=c(min(ManchesterTestFinal$Longitude),min(ManchesterTestFinal$Latitude),max(ManchesterTestFinal$Longitude),max(ManchesterTestFinal$Latitude)),source="osm",messaging=TRUE,crop=TRUE,color="bw",filename="ManchesterMap2.png",zoom=10)
# ManchesterMap3=get_map(location=centroid(p),color="bw",crop=TRUE,zoom=11)
# p<-rbind(c(attr(ManchesterMap2, "bb")$ll.lon,attr(ManchesterMap2, "bb")$ll.lat),c(attr(ManchesterMap2, "bb")$ur.lon,attr(ManchesterMap2, "bb")$ll.lat),c(attr(ManchesterMap2, "bb")$ur.lon,attr(ManchesterMap2, "bb")$ur.lat),c(attr(ManchesterMap2, "bb")$ll.lon,attr(ManchesterMap2, "bb")$ur.lat),c(attr(ManchesterMap2, "bb")$ll.lon,attr(ManchesterMap2, "bb")$ll.lat))

# ManMap<-qmap(location = 'Manchester', zoom = 11, maptype = 'toner', source = 'stamen',legend="topleft",extent="device")
# overlay<-stat_density2d(aes(x = Longitude, y = Latitude,fill = ..level..,alpha=..level..), bins = 20, size=5,data = df,geom = "polygon")
# MapTest<-ManMap + overlay + inset(grob = ggplotGrob(ggplot() + overlay + theme_inset()),xmin = (-2.479837 + abs((-2.023770+2.479837)*3/4)), xmax =-2.023770 , ymin = 53.34309, ymax =(53.34309+ (53.61781-53.34309)/4)) + scale_fill_gradient(low="lightgreen",high="darkgreen",name="Crime\nDensity") +scale_alpha_continuous(guide="none",range = c(0.25, 0.75)) 


#Split data frame in blocks, aggregate S-M crime, remove unwanted data
# stepLon<-abs(max(ManchesterTestFinal$Longitude) -min(ManchesterTestFinal$Longitude) )/4
# stepLat<-abs(max(ManchesterTestFinal$Longitude) -min(ManchesterTestFinal$Longitude) )/4
# cutLon<-c(min(ManchesterTestFinal$Longitude),min(ManchesterTestFinal$Longitude)+stepLon,min(ManchesterTestFinal$Longitude) + 2*stepLon, min(ManchesterTestFinal$Longitude)+ 3*stepLon,min(ManchesterTestFinal$Longitude)+ 4*stepLon)
# cutLat<-c(min(ManchesterTestFinal$Latitude),min(ManchesterTestFinal$Latitude)+stepLat,min(ManchesterTestFinal$Latitude) + 2*stepLat, min(ManchesterTestFinal$Latitude)+ 3*stepLat,min(ManchesterTestFinal$Latitude)+ 4*stepLat)
# ManchesterTestFinal$Lon2 <- as.numeric(cut(ManchesterTestFinal$Longitude,cutLon,dig.lab=9,include.lowest=TRUE))
# ManchesterTestFinal$Lat2 <- as.numeric(cut(ManchesterTestFinal$Latitude,cutLat,dig.lab=9,include.lowest=TRUE))
ManchesterTestFinal$Lon2 <- as.numeric(cut(ManchesterTestFinal$Longitude,20,dig.lab=9))
ManchesterTestFinal$Lat2 <- as.numeric(cut(ManchesterTestFinal$Latitude,25,dig.lab=9))

aggdata<-aggregate(ManchesterTestFinal$Serious,by=list(ManchesterTestFinal$Lon2,ManchesterTestFinal$Lat2,ManchesterTestFinal$Month), FUN=sum)
aggdata2<-aggregate(ManchesterTestFinal$Minor,by=list(ManchesterTestFinal$Lon2,ManchesterTestFinal$Lat2,ManchesterTestFinal$Month), FUN=sum)
ManchesterMergedFocus<-data.frame(Month=aggdata$Group.3, Serious=aggdata$x, Minor=aggdata2$x, BlockLon=aggdata$Group.1,BlockLat=aggdata$Group.2)
ManchesterMergedFocus$BlockLon<-as.factor(ManchesterMergedFocus$BlockLon)
ManchesterMergedFocus$BlockLat<-as.factor(ManchesterMergedFocus$BlockLat)
sqLon=length(levels(ManchesterMergedFocus$BlockLon))
sqLat=length(levels(ManchesterMergedFocus$BlockLat))
firstRowsToRemove<- sqLon*sqLat*9
rownames(ManchesterMergedFocus) <- NULL
ManchesterMergedFocus<-ManchesterMergedFocus[-(1:firstRowsToRemove),]
for(i in 1:(length(ManchesterMergedFocus$Serious)-sqLon*sqLat*1))
  ManchesterMergedFocus$DS[i]<-ManchesterMergedFocus$Serious[i+1]-ManchesterMergedFocus$Serious[i] 
for(i in 1:(length(ManchesterMergedFocus$Serious)-sqLon*sqLat*1))
  ManchesterMergedFocus$DM[i]<-ManchesterMergedFocus$Minor[i+1]-ManchesterMergedFocus$Minor[i]

ManchesterMergedFocus$Month<-factor(ManchesterMergedFocus$Month)
ManchesterMergedFocus$BlockLon <- factor(ManchesterMergedFocus$BlockLon, labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T"))
cols<-c("BlockLon","BlockLat")
ManchesterMergedFocus$Block<-apply( ManchesterMergedFocus[ , cols ] , 1 , paste , collapse = "" )
ManchesterMergedFocus$Block<-as.factor(ManchesterMergedFocus$Block)
df<-ManchesterMergedFocus
ManchesterTestFinal$Lon2 <- factor(ManchesterTestFinal$Lon2, labels = c("A", "B", "C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T"))
vals <- expand.grid(Month = unique(ManchesterMergedFocus$Month), BlockLon=unique(ManchesterTestFinal$Lon2),BlockLat=unique(ManchesterTestFinal$Lat2))
df2<-rbind.fill(df,vals)
dups<-duplicated(df2[,c(1,4:5)])
test<-df2[!dups,]
test$Block<-apply( test[ , cols ] , 1 , paste , collapse = "" )
test$Block<-as.factor(test$Block)
test[is.na(test)]<-0
# Fix final date!!!!!

ManchesterMergedFocus<-test

#Create seperate data frames for each block 
for(i in unique(ManchesterMergedFocus$Block)) {
  assign(paste0("ManchesterMergedFocus", i), data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i)))
#   rownames(as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))) <- NULL
  Manchester1<-data.table(Month=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Month, Amount=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Serious, isin=factor("Serious"), Block=factor(i), Location="Manchester")
  Manchester2<-data.table(Month=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Month, Amount=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Minor, isin=factor("Minor"), Block=factor(i), Location="Manchester")
  assign(paste0("ManchesterMergedFocusGG", i),data.table(rbind(Manchester1, Manchester2)))
  ManPlot<-ggplot(as.data.table(rbind(Manchester1, Manchester2)), aes(Month, Amount, group=as.factor(isin), color=as.factor(isin))) + geom_line() +xlab("Months") + ylab("Criminal activites") + ggtitle(paste0("Manchester - Block ",i )) + geom_vline(xintercept=c(7,13,19,25), linetype="dotted")  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(values=c("red", "orange"),name="Crime type")
  assign(paste0("/Users/Michalis/Dropbox/CrimeData/Cors/plotMan",i),ManPlot)
}
#Plot correlation matrices 
for(i in unique(ManchesterMergedFocus$Block)) {
  df<-data.frame(Serious=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$Serious,Minor=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$Minor,DS=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$DS,DM=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$DM)
  png(filename=paste0("/Users/Michalis/Dropbox/CrimeData/Cors/ManplotCor",i),height=6.44, width=8.00,units="in",res=200)
  rupp<-corrplot(rcorr(as.matrix(df),type="pearson")$r,method="number",diag=TRUE,outline=TRUE,p.mat=rcorr(as.matrix(df),type="pearson")$P,sig.level=0.05,insig="pch",type="upper",title=paste0("\n",i),is.corr=TRUE,addCoef.col="black",tl.pos="d",tl.col="black",tl.cex=1,tl.offset=0.25,cl.pos="n",col="black")
  rdown<-corrplot(rcorr(as.matrix(df),type="spearman")$r,method="number",diag=FALSE,outline=TRUE,p.mat=rcorr(as.matrix(df),type="spearman")$P,sig.level=0.05,insig="pch",type="lower",is.corr=TRUE,addCoef.col="black",tl.col="black",tl.pos="n",cl.pos="n",add=TRUE,col="black")
  dev.off()
}

#Plot correlation matrix for all blocks 
png(filename="/Users/Michalis/Dropbox/CrimeData/Cors/ManplotCorAll",res=200,height=6*6.44, width=7*8.00, units="in",)
layout(matrix(seq(1,500), 25, 20))
for(i in sort(unique(ManchesterMergedFocus$BlockLon))) {
  df<-data.frame(Serious=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$Serious,Minor=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$Minor,DS=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$DS,DM=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$DM,BlockLat=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$BlockLat)
  for(j in sort(unique(ManchesterMergedFocus$BlockLat))) {
  
  ddf<-data.frame(Serious=subset(df,df$BlockLat==j)$Serious,Minor=subset(df,df$BlockLat==j)$Minor,DS=subset(df,df$BlockLat==j)$DS,DM=subset(df,df$BlockLat==j)$DM)  
  if(!is.na(rcorr(as.matrix(ddf))$r[1,2])){
    rupp<-corrplot(rcorr(as.matrix(ddf),type="pearson")$r,method="number",diag=TRUE,outline=TRUE,p.mat=rcorr(as.matrix(ddf),type="pearson")$P,sig.level=0.05,insig="pch",type="upper",title=paste0("\n",i,j),is.corr=TRUE,addCoef.col="black",tl.pos="d",tl.col="black",tl.cex=1,tl.offset=0.25,cl.pos="n",col="black")
    
    rdown<-corrplot(rcorr(as.matrix(ddf),type="spearman")$r,method="number",diag=FALSE,outline=TRUE,p.mat=rcorr(as.matrix(ddf),type="spearman")$P,sig.level=0.05,insig="pch",type="lower",is.corr=TRUE,addCoef.col="black",tl.col="black",tl.pos="n",cl.pos="n",add=TRUE,col="black")  
  }
  else{ 
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
  }
  
}
}
dev.off()
#Focus on high density area(s)
#calculate boundary coords using gglocator(4) or centre with gglocator(1) 
  focusPoints<-data.frame(Lon=cbind(c(-2.267745,-2.266359,-2.219228,-2.217841)),Lat=cbind(c(53.47014,53.49076,53.49159,53.47014)))
#calculate area,perimeter,centroid and sides
areaManFocus<-areaPolygon(focusPoints)
centroidManFocus<-centroid(focusPoints)
perimeterManFocus<-perimeter(focusPoints)
leftSideManFocus<-distVincentyEllipsoid(focusPoints[1,],focusPoints[2,])
topSideManFocus<-distVincentyEllipsoid(focusPoints[2,],focusPoints[3,])
rightSideManFocus<-distVincentyEllipsoid(focusPoints[3,],focusPoints[4,])
bottomSideManFocus<-distVincentyEllipsoid(focusPoints[1,],focusPoints[4,])
sidesManFocus<-cbind(leftSideManFocus,topSideManFocus,rightSideManFocus,bottomSideManFocus)

#restrict dataframe inside boundary and split into blocks
ManchesterTestFocus<-subset(ManchesterTest,Longitude>=min(focusPoints[1,1],focusPoints[2,1]) &Longitude<=max(focusPoints[3,1],focusPoints[4,1]) & Latitude>=min(focusPoints[1,2],focusPoints[4,2]) & Latitude<=max(focusPoints[2,2],focusPoints[3,2]))
ManchesterTestFocus$Lon2 <- as.numeric(cut(ManchesterTestFocus$Longitude,5,dig.lab=9))
ManchesterTestFocus$Lat2 <- as.numeric(cut(ManchesterTestFocus$Latitude,5,dig.lab=9))
freqLon2 <- freq(ManchesterTestFocus$Lon2,plot=FALSE)
freqLat2 <- freq(ManchesterTestFocus$Lat2,plot=FALSE)
freqLon2 <- as.data.frame(freqLon2)
freqLat2 <- as.data.frame(freqLat2)

aggdataFocus<-aggregate(ManchesterTestFocus$Serious,by=list(ManchesterTestFocus$Lon2,ManchesterTestFocus$Lat2,ManchesterTestFocus$Month), FUN=sum)
aggdataFocus2<-aggregate(ManchesterTestFocus$Minor,by=list(ManchesterTestFocus$Lon2,ManchesterTestFocus$Lat2,ManchesterTestFocus$Month), FUN=sum)
ManchesterMergedFocus<-data.frame(Month=aggdataFocus$Group.3, Serious=aggdataFocus$x, Minor=aggdataFocus2$x, BlockLon=aggdataFocus$Group.1,BlockLat=aggdataFocus$Group.2)
ManchesterMergedFocus$BlockLon<-as.factor(ManchesterMergedFocus$BlockLon)
ManchesterMergedFocus$BlockLat<-as.factor(ManchesterMergedFocus$BlockLat)
sqLonFocus=length(levels(ManchesterMergedFocus$BlockLon))
sqLatFocus=length(levels(ManchesterMergedFocus$BlockLat))
firstRowsToRemove<- sqLonFocus*sqLatFocus*9
rownames(ManchesterMergedFocus) <- NULL
ManchesterMergedFocus<-ManchesterMergedFocus[-(1:firstRowsToRemove),]
for(i in 1:(length(ManchesterMergedFocus$Serious)-sqLon*sqLat*1)){
  ManchesterMergedFocus$DS[i]<-ManchesterMergedFocus$Serious[i+1]-ManchesterMergedFocus$Serious[i] 
  ManchesterMergedFocus$DM[i]<-ManchesterMergedFocus$Minor[i+1]-ManchesterMergedFocus$Minor[i]
}
ManchesterMergedFocus$Month<-factor(ManchesterMergedFocus$Month)
ManchesterMergedFocus$BlockLon <- factor(ManchesterMergedFocus$BlockLon, labels = c("A", "B", "C","D","E"))
cols<-c("BlockLon","BlockLat")
ManchesterMergedFocus$Block<-apply( ManchesterMergedFocus[ , cols ] , 1 , paste , collapse = "" )
ManchesterMergedFocus$Block<-as.factor(ManchesterMergedFocus$Block)
df<-ManchesterMergedFocus
ManchesterTestFocus$Lon2 <- factor(ManchesterTestFocus$Lon2, labels = c("A", "B", "C","D","E"))
vals <- expand.grid(Month = unique(ManchesterMergedFocus$Month), BlockLon=unique(ManchesterTestFocus$Lon2),BlockLat=unique(ManchesterTestFocus$Lat2))
df2<-rbind.fill(df,vals)
dups<-duplicated(df2[,c(1,4:5)])
test<-df2[!dups,]
test$Block<-apply( test[ , cols ] , 1 , paste , collapse = "" )
test$Block<-as.factor(test$Block)
test[is.na(test)]<-0
# Fix final date!!!!!
ManchesterMergedFocus<-test


#Create seperate data frames for each block and plot time series
for(i in unique(ManchesterMergedFocus$Block)) {
  assign(paste0("ManchesterMergedFocus", i), data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i)))
  #   rownames(as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))) <- NULL
  Manchester1<-data.table(Month=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Month, Amount=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Serious, isin=factor("Serious"), Block=factor(i), Location="Manchester")
  Manchester2<-data.table(Month=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Month, Amount=as.data.table(subset(ManchesterMergedFocus, ManchesterMergedFocus$Block==i))$Minor, isin=factor("Minor"), Block=factor(i), Location="Manchester")
  assign(paste0("ManchesterMergedFocusGG", i),data.table(rbind(Manchester1, Manchester2)))
  ManPlot<-ggplot(as.data.frame(rbind(Manchester1, Manchester2)), aes(Month, Amount, group=as.factor(isin), color=as.factor(isin))) + geom_line() +xlab("Months") + ylab("Criminal activites") + ggtitle(paste0("Manchester - Block ",i," (focus)")) + geom_vline(xintercept=c(7,13,19,25), linetype="dotted")  + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_color_manual(values=c("red", "orange"),name="Crime type")
  assign(paste0("ManPlotFocus",i),ManPlot)
  print(ManPlot)
  ggsave(paste0("/Users/Michalis/Dropbox/CrimeData/Cors/ManPlotFocus",i,".png"),height=6.44, width=8.00, units=c("in"), dpi=100)
#   dev.off()
}
#Plot correlation matrices 
for(i in unique(ManchesterMergedFocus$Block)) {
  df<-data.frame(Serious=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$Serious,Minor=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$Minor,DS=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$DS,DM=subset(ManchesterMergedFocus,ManchesterMergedFocus$Block==i)$DM)
  png(filename=paste0("/Users/Michalis/Dropbox/CrimeData/Cors/ManplotCorFocus",i),height=6.44, width=8.00,units="in",res=200)
  rupp<-corrplot(rcorr(as.matrix(df),type="pearson")$r,method="number",diag=TRUE,outline=TRUE,p.mat=rcorr(as.matrix(df),type="pearson")$P,sig.level=0.05,insig="pch",type="upper",title=paste0("\n",i),is.corr=TRUE,addCoef.col="black",tl.pos="d",tl.col="black",tl.cex=1,tl.offset=0.25,cl.pos="n",col="black")
  rdown<-corrplot(rcorr(as.matrix(df),type="spearman")$r,method="number",diag=FALSE,outline=TRUE,p.mat=rcorr(as.matrix(df),type="spearman")$P,sig.level=0.05,insig="pch",type="lower",is.corr=TRUE,addCoef.col="black",tl.col="black",tl.pos="n",cl.pos="n",add=TRUE,col="black")
  dev.off()
}

#Plot correlation matrix for all blocks 
png(filename="/Users/Michalis/Dropbox/CrimeData/Cors/ManplotCorAllFocus",res=200,height=2*6.44, width=2*8.00, units="in",)
layout(matrix(seq(1,25), 5, 5))
for(i in sort(unique(ManchesterMergedFocus$BlockLon))) {
  df<-data.frame(Serious=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$Serious,Minor=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$Minor,DS=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$DS,DM=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$DM,BlockLat=subset(ManchesterMergedFocus,ManchesterMergedFocus$BlockLon==i)$BlockLat)
  for(j in sort(unique(ManchesterMergedFocus$BlockLat),decreasing=TRUE)) {
    
    ddf<-data.frame(Serious=subset(df,df$BlockLat==j)$Serious,Minor=subset(df,df$BlockLat==j)$Minor,DS=subset(df,df$BlockLat==j)$DS,DM=subset(df,df$BlockLat==j)$DM)  
    if(!is.na(rcorr(as.matrix(ddf))$r[1,2])){
      rupp<-corrplot(rcorr(as.matrix(ddf),type="pearson")$r,method="number",diag=TRUE,outline=TRUE,p.mat=rcorr(as.matrix(ddf),type="pearson")$P,sig.level=0.05,insig="pch",type="upper",title=paste0("\n",i,j),is.corr=TRUE,addCoef.col="black",tl.pos="d",tl.col="black",tl.cex=1,tl.offset=0.25,cl.pos="n",col="black")
      
      rdown<-corrplot(rcorr(as.matrix(ddf),type="spearman")$r,method="number",diag=FALSE,outline=TRUE,p.mat=rcorr(as.matrix(ddf),type="spearman")$P,sig.level=0.05,insig="pch",type="lower",is.corr=TRUE,addCoef.col="black",tl.col="black",tl.pos="n",cl.pos="n",add=TRUE,col="black")  
    }
    else{ 
      plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    }
    
  }
}
dev.off()