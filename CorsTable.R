library("plyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("reshape", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("caret", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("Hmisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
library("xtable", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

AllSDS<-cbind(AllMerged$Serious,AllMerged$DS)
AllMDM<-cbind(AllMerged$Minor,AllMerged$DM)
AllSM<-cbind(AllMerged$Serious,AllMerged$Minor)
AllDSDM<-cbind(AllMerged$DS,AllMerged$DM)
AllSDM<-cbind(AllMerged$Serious,AllMerged$DM)
AllMDS<-cbind(AllMerged$Minor,AllMerged$DS)

ManchesterSDS<-cbind(ManchesterMerged$Serious,ManchesterMerged$DS)
ManchesterMDM<-cbind(ManchesterMerged$Minor,ManchesterMerged$DM)
ManchesterSM<-cbind(ManchesterMerged$Serious,ManchesterMerged$Minor)
ManchesterDSDM<-cbind(ManchesterMerged$DS,ManchesterMerged$DM)
ManchesterSDM<-cbind(ManchesterMerged$Serious,ManchesterMerged$DM)
ManchesterMDS<-cbind(ManchesterMerged$Minor,ManchesterMerged$DS)

LeedsSDS<-cbind(LeedsMerged$Serious,LeedsMerged$DS)
LeedsMDM<-cbind(LeedsMerged$Minor,LeedsMerged$DM)
LeedsSM<-cbind(LeedsMerged$Serious,LeedsMerged$Minor)
LeedsDSDM<-cbind(LeedsMerged$DS,LeedsMerged$DM)
LeedsSDM<-cbind(LeedsMerged$Serious,LeedsMerged$DM)
LeedsMDS<-cbind(LeedsMerged$Minor,LeedsMerged$DS)

LiverpoolSDS<-cbind(LiverpoolMerged$Serious,LiverpoolMerged$DS)
LiverpoolMDM<-cbind(LiverpoolMerged$Minor,LiverpoolMerged$DM)
LiverpoolSM<-cbind(LiverpoolMerged$Serious,LiverpoolMerged$Minor)
LiverpoolDSDM<-cbind(LiverpoolMerged$DS,LiverpoolMerged$DM)
LiverpoolSDM<-cbind(LiverpoolMerged$Serious,LiverpoolMerged$DM)
LiverpoolMDS<-cbind(LiverpoolMerged$Minor,LiverpoolMerged$DS)

SouthamptonSDS<-cbind(SouthamptonMerged$Serious,SouthamptonMerged$DS)
SouthamptonMDM<-cbind(SouthamptonMerged$Minor,SouthamptonMerged$DM)
SouthamptonSM<-cbind(SouthamptonMerged$Serious,SouthamptonMerged$Minor)
SouthamptonDSDM<-cbind(SouthamptonMerged$DS,SouthamptonMerged$DM)
SouthamptonSDM<-cbind(SouthamptonMerged$Serious,SouthamptonMerged$DM)
SouthamptonMDS<-cbind(SouthamptonMerged$Minor,SouthamptonMerged$DS)

NewcastleSDS<-cbind(NewcastleMerged$Serious,NewcastleMerged$DS)
NewcastleMDM<-cbind(NewcastleMerged$Minor,NewcastleMerged$DM)
NewcastleSM<-cbind(NewcastleMerged$Serious,NewcastleMerged$Minor)
NewcastleDSDM<-cbind(NewcastleMerged$DS,NewcastleMerged$DM)
NewcastleSDM<-cbind(NewcastleMerged$Serious,NewcastleMerged$DM)
NewcastleMDS<-cbind(NewcastleMerged$Minor,NewcastleMerged$DS)

NottinghamSDS<-cbind(NottinghamMerged$Serious,NottinghamMerged$DS)
NottinghamMDM<-cbind(NottinghamMerged$Minor,NottinghamMerged$DM)
NottinghamSM<-cbind(NottinghamMerged$Serious,NottinghamMerged$Minor)
NottinghamDSDM<-cbind(NottinghamMerged$DS,NottinghamMerged$DM)
NottinghamSDM<-cbind(NottinghamMerged$Serious,NottinghamMerged$DM)
NottinghamMDS<-cbind(NottinghamMerged$Minor,NottinghamMerged$DS)

SheffieldSDS<-cbind(SheffieldMerged$Serious,SheffieldMerged$DS)
SheffieldMDM<-cbind(SheffieldMerged$Minor,SheffieldMerged$DM)
SheffieldSM<-cbind(SheffieldMerged$Serious,SheffieldMerged$Minor)
SheffieldDSDM<-cbind(SheffieldMerged$DS,SheffieldMerged$DM)
SheffieldSDM<-cbind(SheffieldMerged$Serious,SheffieldMerged$DM)
SheffieldMDS<-cbind(SheffieldMerged$Minor,SheffieldMerged$DS)
SuffolkSDS<-cbind(SuffolkMerged$Serious,SuffolkMerged$DS)
SuffolkMDM<-cbind(SuffolkMerged$Minor,SuffolkMerged$DM)
SuffolkSM<-cbind(SuffolkMerged$Serious,SuffolkMerged$Minor)
SuffolkDSDM<-cbind(SuffolkMerged$DS,SuffolkMerged$DM)
SuffolkSDM<-cbind(SuffolkMerged$Serious,SuffolkMerged$DM)
SuffolkMDS<-cbind(SuffolkMerged$Minor,SuffolkMerged$DS)

NorfolkSDS<-cbind(NorfolkMerged$Serious,NorfolkMerged$DS)
NorfolkMDM<-cbind(NorfolkMerged$Minor,NorfolkMerged$DM)
NorfolkSM<-cbind(NorfolkMerged$Serious,NorfolkMerged$Minor)
NorfolkDSDM<-cbind(NorfolkMerged$DS,NorfolkMerged$DM)
NorfolkSDM<-cbind(NorfolkMerged$Serious,NorfolkMerged$DM)
NorfolkMDS<-cbind(NorfolkMerged$Minor,NorfolkMerged$DS)

CambridgeshireSDS<-cbind(CambridgeshireMerged$Serious,CambridgeshireMerged$DS)
CambridgeshireMDM<-cbind(CambridgeshireMerged$Minor,CambridgeshireMerged$DM)
CambridgeshireSM<-cbind(CambridgeshireMerged$Serious,CambridgeshireMerged$Minor)
CambridgeshireDSDM<-cbind(CambridgeshireMerged$DS,CambridgeshireMerged$DM)
CambridgeshireSDM<-cbind(CambridgeshireMerged$Serious,CambridgeshireMerged$DM)
CambridgeshireMDS<-cbind(CambridgeshireMerged$Minor,CambridgeshireMerged$DS)

LincolnshireSDS<-cbind(LincolnshireMerged$Serious,LincolnshireMerged$DS)
LincolnshireMDM<-cbind(LincolnshireMerged$Minor,LincolnshireMerged$DM)
LincolnshireSM<-cbind(LincolnshireMerged$Serious,LincolnshireMerged$Minor)
LincolnshireDSDM<-cbind(LincolnshireMerged$DS,LincolnshireMerged$DM)
LincolnshireSDM<-cbind(LincolnshireMerged$Serious,LincolnshireMerged$DM)
LincolnshireMDS<-cbind(LincolnshireMerged$Minor,LincolnshireMerged$DS)

CumbriaSDS<-cbind(CumbriaMerged$Serious,CumbriaMerged$DS)
CumbriaMDM<-cbind(CumbriaMerged$Minor,CumbriaMerged$DM)
CumbriaSM<-cbind(CumbriaMerged$Serious,CumbriaMerged$Minor)
CumbriaDSDM<-cbind(CumbriaMerged$DS,CumbriaMerged$DM)
CumbriaSDM<-cbind(CumbriaMerged$Serious,CumbriaMerged$DM)
CumbriaMDS<-cbind(CumbriaMerged$Minor,CumbriaMerged$DS)

DurhamSDS<-cbind(DurhamMerged$Serious,DurhamMerged$DS)
DurhamMDM<-cbind(DurhamMerged$Minor,DurhamMerged$DM)
DurhamSM<-cbind(DurhamMerged$Serious,DurhamMerged$Minor)
DurhamDSDM<-cbind(DurhamMerged$DS,DurhamMerged$DM)
DurhamSDM<-cbind(DurhamMerged$Serious,DurhamMerged$DM)
DurhamMDS<-cbind(DurhamMerged$Minor,DurhamMerged$DS)

SussexSDS<-cbind(SussexMerged$Serious,SussexMerged$DS)
SussexMDM<-cbind(SussexMerged$Minor,SussexMerged$DM)
SussexSM<-cbind(SussexMerged$Serious,SussexMerged$Minor)
SussexDSDM<-cbind(SussexMerged$DS,SussexMerged$DM)
SussexSDM<-cbind(SussexMerged$Serious,SussexMerged$DM)
SussexMDS<-cbind(SussexMerged$Minor,SussexMerged$DS)

DevonSDS<-cbind(DevonMerged$Serious,DevonMerged$DS)
DevonMDM<-cbind(DevonMerged$Minor,DevonMerged$DM)
DevonSM<-cbind(DevonMerged$Serious,DevonMerged$Minor)
DevonDSDM<-cbind(DevonMerged$DS,DevonMerged$DM)
DevonSDM<-cbind(DevonMerged$Serious,DevonMerged$DM)
DevonMDS<-cbind(DevonMerged$Minor,DevonMerged$DS)

CorelsPearson<-data.frame(SDS=rbind(rcorr(BirminghamSDS,type="pearson")$r[2],rcorr(ManchesterSDS,type="pearson")$r[2],rcorr(LeedsSDS,type="pearson")$r[2],rcorr(LiverpoolSDS,type="pearson")$r[2],rcorr(SouthamptonSDS,type="pearson")$r[2],rcorr(NewcastleSDS,type="pearson")$r[2],rcorr(NottinghamSDS,type="pearson")$r[2],rcorr(SheffieldSDS,type="pearson")$r[2],rcorr(SuffolkSDS,type="pearson")$r[2],rcorr(NorfolkSDS,type="pearson")$r[2],rcorr(CambridgeshireSDS,type="pearson")$r[2],rcorr(LincolnshireSDS,type="pearson")$r[2],rcorr(CumbriaSDS,type="pearson")$r[2],rcorr(DurhamSDS,type="pearson")$r[2],rcorr(SussexSDS,type="pearson")$r[2],rcorr(DevonSDS,type="pearson")$r[2],rcorr(AllSDS,type="pearson")$r[2]),
                          SDSPV=rbind(rcorr(BirminghamSDS,type="pearson")$P[2],rcorr(ManchesterSDS,type="pearson")$P[2],rcorr(LeedsSDS,type="pearson")$P[2],rcorr(LiverpoolSDS,type="pearson")$P[2],rcorr(SouthamptonSDS,type="pearson")$P[2],rcorr(NewcastleSDS,type="pearson")$P[2],rcorr(NottinghamSDS,type="pearson")$P[2],rcorr(SheffieldSDS,type="pearson")$P[2],rcorr(SuffolkSDS,type="pearson")$P[2],rcorr(NorfolkSDS,type="pearson")$P[2],rcorr(CambridgeshireSDS,type="pearson")$P[2],rcorr(LincolnshireSDS,type="pearson")$P[2],rcorr(CumbriaSDS,type="pearson")$P[2],rcorr(DurhamSDS,type="pearson")$P[2],rcorr(SussexSDS,type="pearson")$P[2],rcorr(DevonSDS,type="pearson")$P[2],rcorr(AllSDS,type="pearson")$P[2]),
                          MDM=rbind(rcorr(BirminghamMDM,type="pearson")$r[2],rcorr(ManchesterMDM,type="pearson")$r[2],rcorr(LeedsMDM,type="pearson")$r[2],rcorr(LiverpoolMDM,type="pearson")$r[2],rcorr(SouthamptonMDM,type="pearson")$r[2],rcorr(NewcastleMDM,type="pearson")$r[2],rcorr(NottinghamMDM,type="pearson")$r[2],rcorr(SheffieldMDM,type="pearson")$r[2],rcorr(SuffolkMDM,type="pearson")$r[2],rcorr(NorfolkMDM,type="pearson")$r[2],rcorr(CambridgeshireMDM,type="pearson")$r[2],rcorr(LincolnshireMDM,type="pearson")$r[2],rcorr(CumbriaMDM,type="pearson")$r[2],rcorr(DurhamMDM,type="pearson")$r[2],rcorr(SussexMDM,type="pearson")$r[2],rcorr(DevonMDM,type="pearson")$r[2],rcorr(AllMDM,type="pearson")$r[2]),
                          MDMPV=rbind(rcorr(BirminghamMDM,type="pearson")$P[2],rcorr(ManchesterMDM,type="pearson")$P[2],rcorr(LeedsMDM,type="pearson")$P[2],rcorr(LiverpoolMDM,type="pearson")$P[2],rcorr(SouthamptonMDM,type="pearson")$P[2],rcorr(NewcastleMDM,type="pearson")$P[2],rcorr(NottinghamMDM,type="pearson")$P[2],rcorr(SheffieldMDM,type="pearson")$P[2],rcorr(SuffolkMDM,type="pearson")$P[2],rcorr(NorfolkMDM,type="pearson")$P[2],rcorr(CambridgeshireMDM,type="pearson")$P[2],rcorr(LincolnshireMDM,type="pearson")$P[2],rcorr(CumbriaMDM,type="pearson")$P[2],rcorr(DurhamMDM,type="pearson")$P[2],rcorr(SussexMDM,type="pearson")$P[2],rcorr(DevonMDM,type="pearson")$P[2],rcorr(AllMDM,type="pearson")$P[2]),
                          SM=rbind(rcorr(BirminghamSM,type="pearson")$r[2],rcorr(ManchesterSM,type="pearson")$r[2],rcorr(LeedsSM,type="pearson")$r[2],rcorr(LiverpoolSM,type="pearson")$r[2],rcorr(SouthamptonSM,type="pearson")$r[2],rcorr(NewcastleSM,type="pearson")$r[2],rcorr(NottinghamSM,type="pearson")$r[2],rcorr(SheffieldSM,type="pearson")$r[2],rcorr(SuffolkSM,type="pearson")$r[2],rcorr(NorfolkSM,type="pearson")$r[2],rcorr(CambridgeshireSM,type="pearson")$r[2],rcorr(LincolnshireSM,type="pearson")$r[2],rcorr(CumbriaSM,type="pearson")$r[2],rcorr(DurhamSM,type="pearson")$r[2],rcorr(SussexSM,type="pearson")$r[2],rcorr(DevonSM,type="pearson")$r[2],rcorr(AllSM,type="pearson")$r[2]),
                          SMPV=rbind(rcorr(BirminghamSM,type="pearson")$P[2],rcorr(ManchesterSM,type="pearson")$P[2],rcorr(LeedsSM,type="pearson")$P[2],rcorr(LiverpoolSM,type="pearson")$P[2],rcorr(SouthamptonSM,type="pearson")$P[2],rcorr(NewcastleSM,type="pearson")$P[2],rcorr(NottinghamSM,type="pearson")$P[2],rcorr(SheffieldSM,type="pearson")$P[2],rcorr(SuffolkSM,type="pearson")$P[2],rcorr(NorfolkSM,type="pearson")$P[2],rcorr(CambridgeshireSM,type="pearson")$P[2],rcorr(LincolnshireSM,type="pearson")$P[2],rcorr(CumbriaSM,type="pearson")$P[2],rcorr(DurhamSM,type="pearson")$P[2],rcorr(SussexSM,type="pearson")$P[2],rcorr(DevonSM,type="pearson")$P[2],rcorr(AllSM,type="pearson")$P[2]),
                          DSDM=rbind(rcorr(BirminghamDSDM,type="pearson")$r[2],rcorr(ManchesterDSDM,type="pearson")$r[2],rcorr(LeedsDSDM,type="pearson")$r[2],rcorr(LiverpoolDSDM,type="pearson")$r[2],rcorr(SouthamptonDSDM,type="pearson")$r[2],rcorr(NewcastleDSDM,type="pearson")$r[2],rcorr(NottinghamDSDM,type="pearson")$r[2],rcorr(SheffieldDSDM,type="pearson")$r[2],rcorr(SuffolkDSDM,type="pearson")$r[2],rcorr(NorfolkDSDM,type="pearson")$r[2],rcorr(CambridgeshireDSDM,type="pearson")$r[2],rcorr(LincolnshireDSDM,type="pearson")$r[2],rcorr(CumbriaDSDM,type="pearson")$r[2],rcorr(DurhamDSDM,type="pearson")$r[2],rcorr(SussexDSDM,type="pearson")$r[2],rcorr(DevonDSDM,type="pearson")$r[2],rcorr(AllDSDM,type="pearson")$r[2]),
                          DSDMPV=rbind(rcorr(BirminghamDSDM,type="pearson")$P[2],rcorr(ManchesterDSDM,type="pearson")$P[2],rcorr(LeedsDSDM,type="pearson")$P[2],rcorr(LiverpoolDSDM,type="pearson")$P[2],rcorr(SouthamptonDSDM,type="pearson")$P[2],rcorr(NewcastleDSDM,type="pearson")$P[2],rcorr(NottinghamDSDM,type="pearson")$P[2],rcorr(SheffieldDSDM,type="pearson")$P[2],rcorr(SuffolkDSDM,type="pearson")$P[2],rcorr(NorfolkDSDM,type="pearson")$P[2],rcorr(CambridgeshireDSDM,type="pearson")$P[2],rcorr(LincolnshireDSDM,type="pearson")$P[2],rcorr(CumbriaDSDM,type="pearson")$P[2],rcorr(DurhamDSDM,type="pearson")$P[2],rcorr(SussexDSDM,type="pearson")$P[2],rcorr(DevonDSDM,type="pearson")$P[2],rcorr(AllDSDM,type="pearson")$P[2]),
                          SDM=rbind(rcorr(BirminghamSDM,type="pearson")$r[2],rcorr(ManchesterSDM,type="pearson")$r[2],rcorr(LeedsSDM,type="pearson")$r[2],rcorr(LiverpoolSDM,type="pearson")$r[2],rcorr(SouthamptonSDM,type="pearson")$r[2],rcorr(NewcastleSDM,type="pearson")$r[2],rcorr(NottinghamSDM,type="pearson")$r[2],rcorr(SheffieldSDM,type="pearson")$r[2],rcorr(SuffolkSDM,type="pearson")$r[2],rcorr(NorfolkSDM,type="pearson")$r[2],rcorr(CambridgeshireSDM,type="pearson")$r[2],rcorr(LincolnshireSDM,type="pearson")$r[2],rcorr(CumbriaSDM,type="pearson")$r[2],rcorr(DurhamSDM,type="pearson")$r[2],rcorr(SussexSDM,type="pearson")$r[2],rcorr(DevonSDM,type="pearson")$r[2],rcorr(AllSDM,type="pearson")$r[2]),
                          SDMPV=rbind(rcorr(BirminghamSDM,type="pearson")$P[2],rcorr(ManchesterSDM,type="pearson")$P[2],rcorr(LeedsSDM,type="pearson")$P[2],rcorr(LiverpoolSDM,type="pearson")$P[2],rcorr(SouthamptonSDM,type="pearson")$P[2],rcorr(NewcastleSDM,type="pearson")$P[2],rcorr(NottinghamSDM,type="pearson")$P[2],rcorr(SheffieldSDM,type="pearson")$P[2],rcorr(SuffolkSDM,type="pearson")$P[2],rcorr(NorfolkSDM,type="pearson")$P[2],rcorr(CambridgeshireSDM,type="pearson")$P[2],rcorr(LincolnshireSDM,type="pearson")$P[2],rcorr(CumbriaSDM,type="pearson")$P[2],rcorr(DurhamSDM,type="pearson")$P[2],rcorr(SussexSDM,type="pearson")$P[2],rcorr(DevonSDM,type="pearson")$P[2],rcorr(AllSDM,type="pearson")$P[2]),
                          MDS=rbind(rcorr(BirminghamMDS,type="pearson")$r[2],rcorr(ManchesterMDS,type="pearson")$r[2],rcorr(LeedsMDS,type="pearson")$r[2],rcorr(LiverpoolMDS,type="pearson")$r[2],rcorr(SouthamptonMDS,type="pearson")$r[2],rcorr(NewcastleMDS,type="pearson")$r[2],rcorr(NottinghamMDS,type="pearson")$r[2],rcorr(SheffieldMDS,type="pearson")$r[2],rcorr(SuffolkMDS,type="pearson")$r[2],rcorr(NorfolkMDS,type="pearson")$r[2],rcorr(CambridgeshireMDS,type="pearson")$r[2],rcorr(LincolnshireMDS,type="pearson")$r[2],rcorr(CumbriaMDS,type="pearson")$r[2],rcorr(DurhamMDS,type="pearson")$r[2],rcorr(SussexMDS,type="pearson")$r[2],rcorr(DevonMDS,type="pearson")$r[2],rcorr(AllMDS,type="pearson")$r[2]),
                          MDSPV=rbind(rcorr(BirminghamMDS,type="pearson")$P[2],rcorr(ManchesterMDS,type="pearson")$P[2],rcorr(LeedsMDS,type="pearson")$P[2],rcorr(LiverpoolMDS,type="pearson")$P[2],rcorr(SouthamptonMDS,type="pearson")$P[2],rcorr(NewcastleMDS,type="pearson")$P[2],rcorr(NottinghamMDS,type="pearson")$P[2],rcorr(SheffieldMDS,type="pearson")$P[2],rcorr(SuffolkMDS,type="pearson")$P[2],rcorr(NorfolkMDS,type="pearson")$P[2],rcorr(CambridgeshireMDS,type="pearson")$P[2],rcorr(LincolnshireMDS,type="pearson")$P[2],rcorr(CumbriaMDS,type="pearson")$P[2],rcorr(DurhamMDS,type="pearson")$P[2],rcorr(SussexMDS,type="pearson")$P[2],rcorr(DevonMDS,type="pearson")$P[2],rcorr(AllMDS,type="pearson")$P[2]))

CorelsSpearman<-data.frame(SDS=rbind(rcorr(BirminghamSDS,type="spearman")$r[2],rcorr(ManchesterSDS,type="spearman")$r[2],rcorr(LeedsSDS,type="spearman")$r[2],rcorr(LiverpoolSDS,type="spearman")$r[2],rcorr(SouthamptonSDS,type="spearman")$r[2],rcorr(NewcastleSDS,type="spearman")$r[2],rcorr(NottinghamSDS,type="spearman")$r[2],rcorr(SheffieldSDS,type="spearman")$r[2],rcorr(SuffolkSDS,type="spearman")$r[2],rcorr(NorfolkSDS,type="spearman")$r[2],rcorr(CambridgeshireSDS,type="spearman")$r[2],rcorr(LincolnshireSDS,type="spearman")$r[2],rcorr(CumbriaSDS,type="spearman")$r[2],rcorr(DurhamSDS,type="spearman")$r[2],rcorr(SussexSDS,type="spearman")$r[2],rcorr(DevonSDS,type="spearman")$r[2],rcorr(AllSDS,type="spearman")$r[2]),
                          SDSPV=rbind(rcorr(BirminghamSDS,type="spearman")$P[2],rcorr(ManchesterSDS,type="spearman")$P[2],rcorr(LeedsSDS,type="spearman")$P[2],rcorr(LiverpoolSDS,type="spearman")$P[2],rcorr(SouthamptonSDS,type="spearman")$P[2],rcorr(NewcastleSDS,type="spearman")$P[2],rcorr(NottinghamSDS,type="spearman")$P[2],rcorr(SheffieldSDS,type="spearman")$P[2],rcorr(SuffolkSDS,type="spearman")$P[2],rcorr(NorfolkSDS,type="spearman")$P[2],rcorr(CambridgeshireSDS,type="spearman")$P[2],rcorr(LincolnshireSDS,type="spearman")$P[2],rcorr(CumbriaSDS,type="spearman")$P[2],rcorr(DurhamSDS,type="spearman")$P[2],rcorr(SussexSDS,type="spearman")$P[2],rcorr(DevonSDS,type="spearman")$P[2],rcorr(AllSDS,type="spearman")$P[2]),
                          MDM=rbind(rcorr(BirminghamMDM,type="spearman")$r[2],rcorr(ManchesterMDM,type="spearman")$r[2],rcorr(LeedsMDM,type="spearman")$r[2],rcorr(LiverpoolMDM,type="spearman")$r[2],rcorr(SouthamptonMDM,type="spearman")$r[2],rcorr(NewcastleMDM,type="spearman")$r[2],rcorr(NottinghamMDM,type="spearman")$r[2],rcorr(SheffieldMDM,type="spearman")$r[2],rcorr(SuffolkMDM,type="spearman")$r[2],rcorr(NorfolkMDM,type="spearman")$r[2],rcorr(CambridgeshireMDM,type="spearman")$r[2],rcorr(LincolnshireMDM,type="spearman")$r[2],rcorr(CumbriaMDM,type="spearman")$r[2],rcorr(DurhamMDM,type="spearman")$r[2],rcorr(SussexMDM,type="spearman")$r[2],rcorr(DevonMDM,type="spearman")$r[2],rcorr(AllMDM,type="spearman")$r[2]),
                          MDMPV=rbind(rcorr(BirminghamMDM,type="spearman")$P[2],rcorr(ManchesterMDM,type="spearman")$P[2],rcorr(LeedsMDM,type="spearman")$P[2],rcorr(LiverpoolMDM,type="spearman")$P[2],rcorr(SouthamptonMDM,type="spearman")$P[2],rcorr(NewcastleMDM,type="spearman")$P[2],rcorr(NottinghamMDM,type="spearman")$P[2],rcorr(SheffieldMDM,type="spearman")$P[2],rcorr(SuffolkMDM,type="spearman")$P[2],rcorr(NorfolkMDM,type="spearman")$P[2],rcorr(CambridgeshireMDM,type="spearman")$P[2],rcorr(LincolnshireMDM,type="spearman")$P[2],rcorr(CumbriaMDM,type="spearman")$P[2],rcorr(DurhamMDM,type="spearman")$P[2],rcorr(SussexMDM,type="spearman")$P[2],rcorr(DevonMDM,type="spearman")$P[2],rcorr(AllMDM,type="spearman")$P[2]),
                          SM=rbind(rcorr(BirminghamSM,type="spearman")$r[2],rcorr(ManchesterSM,type="spearman")$r[2],rcorr(LeedsSM,type="spearman")$r[2],rcorr(LiverpoolSM,type="spearman")$r[2],rcorr(SouthamptonSM,type="spearman")$r[2],rcorr(NewcastleSM,type="spearman")$r[2],rcorr(NottinghamSM,type="spearman")$r[2],rcorr(SheffieldSM,type="spearman")$r[2],rcorr(SuffolkSM,type="spearman")$r[2],rcorr(NorfolkSM,type="spearman")$r[2],rcorr(CambridgeshireSM,type="spearman")$r[2],rcorr(LincolnshireSM,type="spearman")$r[2],rcorr(CumbriaSM,type="spearman")$r[2],rcorr(DurhamSM,type="spearman")$r[2],rcorr(SussexSM,type="spearman")$r[2],rcorr(DevonSM,type="spearman")$r[2],rcorr(AllSM,type="spearman")$r[2]),
                          SMPV=rbind(rcorr(BirminghamSM,type="spearman")$P[2],rcorr(ManchesterSM,type="spearman")$P[2],rcorr(LeedsSM,type="spearman")$P[2],rcorr(LiverpoolSM,type="spearman")$P[2],rcorr(SouthamptonSM,type="spearman")$P[2],rcorr(NewcastleSM,type="spearman")$P[2],rcorr(NottinghamSM,type="spearman")$P[2],rcorr(SheffieldSM,type="spearman")$P[2],rcorr(SuffolkSM,type="spearman")$P[2],rcorr(NorfolkSM,type="spearman")$P[2],rcorr(CambridgeshireSM,type="spearman")$P[2],rcorr(LincolnshireSM,type="spearman")$P[2],rcorr(CumbriaSM,type="spearman")$P[2],rcorr(DurhamSM,type="spearman")$P[2],rcorr(SussexSM,type="spearman")$P[2],rcorr(DevonSM,type="spearman")$P[2],rcorr(AllSM,type="spearman")$P[2]),
                          DSDM=rbind(rcorr(BirminghamDSDM,type="spearman")$r[2],rcorr(ManchesterDSDM,type="spearman")$r[2],rcorr(LeedsDSDM,type="spearman")$r[2],rcorr(LiverpoolDSDM,type="spearman")$r[2],rcorr(SouthamptonDSDM,type="spearman")$r[2],rcorr(NewcastleDSDM,type="spearman")$r[2],rcorr(NottinghamDSDM,type="spearman")$r[2],rcorr(SheffieldDSDM,type="spearman")$r[2],rcorr(SuffolkDSDM,type="spearman")$r[2],rcorr(NorfolkDSDM,type="spearman")$r[2],rcorr(CambridgeshireDSDM,type="spearman")$r[2],rcorr(LincolnshireDSDM,type="spearman")$r[2],rcorr(CumbriaDSDM,type="spearman")$r[2],rcorr(DurhamDSDM,type="spearman")$r[2],rcorr(SussexDSDM,type="spearman")$r[2],rcorr(DevonDSDM,type="spearman")$r[2],rcorr(AllDSDM,type="spearman")$r[2]),
                          DSDMPV=rbind(rcorr(BirminghamDSDM,type="spearman")$P[2],rcorr(ManchesterDSDM,type="spearman")$P[2],rcorr(LeedsDSDM,type="spearman")$P[2],rcorr(LiverpoolDSDM,type="spearman")$P[2],rcorr(SouthamptonDSDM,type="spearman")$P[2],rcorr(NewcastleDSDM,type="spearman")$P[2],rcorr(NottinghamDSDM,type="spearman")$P[2],rcorr(SheffieldDSDM,type="spearman")$P[2],rcorr(SuffolkDSDM,type="spearman")$P[2],rcorr(NorfolkDSDM,type="spearman")$P[2],rcorr(CambridgeshireDSDM,type="spearman")$P[2],rcorr(LincolnshireDSDM,type="spearman")$P[2],rcorr(CumbriaDSDM,type="spearman")$P[2],rcorr(DurhamDSDM,type="spearman")$P[2],rcorr(SussexDSDM,type="spearman")$P[2],rcorr(DevonDSDM,type="spearman")$P[2],rcorr(AllDSDM,type="spearman")$P[2]),
                          SDM=rbind(rcorr(BirminghamSDM,type="spearman")$r[2],rcorr(ManchesterSDM,type="spearman")$r[2],rcorr(LeedsSDM,type="spearman")$r[2],rcorr(LiverpoolSDM,type="spearman")$r[2],rcorr(SouthamptonSDM,type="spearman")$r[2],rcorr(NewcastleSDM,type="spearman")$r[2],rcorr(NottinghamSDM,type="spearman")$r[2],rcorr(SheffieldSDM,type="spearman")$r[2],rcorr(SuffolkSDM,type="spearman")$r[2],rcorr(NorfolkSDM,type="spearman")$r[2],rcorr(CambridgeshireSDM,type="spearman")$r[2],rcorr(LincolnshireSDM,type="spearman")$r[2],rcorr(CumbriaSDM,type="spearman")$r[2],rcorr(DurhamSDM,type="spearman")$r[2],rcorr(SussexSDM,type="spearman")$r[2],rcorr(DevonSDM,type="spearman")$r[2],rcorr(AllSDM,type="spearman")$r[2]),
                          SDMPV=rbind(rcorr(BirminghamSDM,type="spearman")$P[2],rcorr(ManchesterSDM,type="spearman")$P[2],rcorr(LeedsSDM,type="spearman")$P[2],rcorr(LiverpoolSDM,type="spearman")$P[2],rcorr(SouthamptonSDM,type="spearman")$P[2],rcorr(NewcastleSDM,type="spearman")$P[2],rcorr(NottinghamSDM,type="spearman")$P[2],rcorr(SheffieldSDM,type="spearman")$P[2],rcorr(SuffolkSDM,type="spearman")$P[2],rcorr(NorfolkSDM,type="spearman")$P[2],rcorr(CambridgeshireSDM,type="spearman")$P[2],rcorr(LincolnshireSDM,type="spearman")$P[2],rcorr(CumbriaSDM,type="spearman")$P[2],rcorr(DurhamSDM,type="spearman")$P[2],rcorr(SussexSDM,type="spearman")$P[2],rcorr(DevonSDM,type="spearman")$P[2],rcorr(AllSDM,type="spearman")$P[2]),
                          MDS=rbind(rcorr(BirminghamMDS,type="spearman")$r[2],rcorr(ManchesterMDS,type="spearman")$r[2],rcorr(LeedsMDS,type="spearman")$r[2],rcorr(LiverpoolMDS,type="spearman")$r[2],rcorr(SouthamptonMDS,type="spearman")$r[2],rcorr(NewcastleMDS,type="spearman")$r[2],rcorr(NottinghamMDS,type="spearman")$r[2],rcorr(SheffieldMDS,type="spearman")$r[2],rcorr(SuffolkMDS,type="spearman")$r[2],rcorr(NorfolkMDS,type="spearman")$r[2],rcorr(CambridgeshireMDS,type="spearman")$r[2],rcorr(LincolnshireMDS,type="spearman")$r[2],rcorr(CumbriaMDS,type="spearman")$r[2],rcorr(DurhamMDS,type="spearman")$r[2],rcorr(SussexMDS,type="spearman")$r[2],rcorr(DevonMDS,type="spearman")$r[2],rcorr(AllMDS,type="spearman")$r[2]),
                          MDSPV=rbind(rcorr(BirminghamMDS,type="spearman")$P[2],rcorr(ManchesterMDS,type="spearman")$P[2],rcorr(LeedsMDS,type="spearman")$P[2],rcorr(LiverpoolMDS,type="spearman")$P[2],rcorr(SouthamptonMDS,type="spearman")$P[2],rcorr(NewcastleMDS,type="spearman")$P[2],rcorr(NottinghamMDS,type="spearman")$P[2],rcorr(SheffieldMDS,type="spearman")$P[2],rcorr(SuffolkMDS,type="spearman")$P[2],rcorr(NorfolkMDS,type="spearman")$P[2],rcorr(CambridgeshireMDS,type="spearman")$P[2],rcorr(LincolnshireMDS,type="spearman")$P[2],rcorr(CumbriaMDS,type="spearman")$P[2],rcorr(DurhamMDS,type="spearman")$P[2],rcorr(SussexMDS,type="spearman")$P[2],rcorr(DevonMDS,type="spearman")$P[2],rcorr(AllMDS,type="spearman")$P[2]))

names=c("Birmingham", "Manchester", "Leeds -\n Bradford", "Liverpool -\n Merseyside", "Southampton -\n Portsmouth", "Newcastle -\n Sunderland", "Nottingham -\n Derby","Sheffield", "Suffolk", "Norfolk", "Cambridgeshire","Lincolnshire", "Cumbria", "Durham", "Sussex", "Devon - Cornwall", "Entire dataset")
rownames(CorelsPearson)<-names
Corels<-data.frame(cbind(CorelsPearson,CorelsSpearman))
tablecaption<-"Pearson's $r$ (left half of table) and Spearman's $ρ$ (right half) are calculated using data from September 2011 to Januay 2014 downloaded from the police website. The coefficients are in the left columns whereas the p-values are on the right. $S$ and $M$ represent serious and minor crime respectively and $\\Delta S$ and $\\Delta M$ are forward differences, e.g. $\\Delta S_{\\text{June}}=$ (Total amount of serious crime in July) - (Total amount of serious crime in June)."
print(xtable(Corels,digits=4,caption=tablecaption),include.rownames=TRUE,include.colnames=FALSE,booktabs=TRUE)
