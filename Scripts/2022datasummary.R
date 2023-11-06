library(glfishr)
library(dplyr)
library(ggplot2)
library(gfsR)
library(readxl)
library(tidyverse)

FN121<-get_FN121(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN123<-get_FN123(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN124<-get_FN124(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN125<-get_FN125(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))


setissues<-filter(FN121, EFFST>1)%>%select(PRJ_CD,SAM,SITE,EFFST,COMMENT1)
FN121<-FN121%>%filter(!EFFST>1)
netsset<-FN121%>%group_by(PRJ_CD)%>%dplyr::summarise(N=n())

#totalfish caught
FN123<-inner_join(FN121,FN123)
fishcaught<-FN123%>%group_by(PRJ_CD)%>%dplyr::summarise(totalfish=sum(CATCNT))

#catches by net
#examine # total fish caught in each net & mean
catch_sumbynet<-FN123%>%group_by(PRJ_CD,SAM)%>%dplyr::summarize(catch=sum(CATCNT))%>%group_by(PRJ_CD)%>%dplyr::summarise(meancatch=mean(catch))
catch_sumbynet<-left_join(catch_sumbynet,FN121)

#catches by species
catch_sumbyspc<-FN123%>%group_by(PRJ_CD,SPC)%>%dplyr::summarize(catch=sum(CATCNT),subcnt=sum(SUBCNT),biocnt=sum(BIOCNT,na.rm=T))



##CUEs
species <- unique(FN123$SPC)

spcsam <- merge(FN121, data.frame(SPC = species))

catcheswithzeros <- left_join(spcsam, FN123, by=c("SAM", "PRJ_CD", "SPC"))
catcheswithzeros$CATCNT[is.na(catcheswithzeros$CATCNT)]<-0
catcheswithzeros <- append.spc.names(catcheswithzeros)

CUE<-catcheswithzeros %>% group_by(PRJ_CD,SPC,SPC_NM) %>% 
  dplyr::summarise(ArithmeticmeanCATCNT = (round(mean(CATCNT),3)),GeometricmeanCATCNT = (round(exp(mean(log(CATCNT[CATCNT>0]))),3)), SE=sd(CATCNT)/sqrt(n()), SD=sd(CATCNT), RSE=round((round(SE,2)/ArithmeticmeanCATCNT)*100,0))%>%  arrange(desc(ArithmeticmeanCATCNT))
#remove 0 CUEs
CUE<-CUE%>%filter(!ArithmeticmeanCATCNT==0)%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NST","Toronto Islands","Upper"),Year=2022)%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NSB","Weller's Bay",EMB))%>%dplyr::rename(AMEAN=ArithmeticmeanCATCNT,GMEAN=GeometricmeanCATCNT,ASE=SE,ASD=SD,Species=SPC,COMMON_NAM=SPC_NM)
CUE<-left_join(CUE,netsset)


#add to historical time series
historicalCUE<-read_excel("./Data/CUESummary2001to2021_Sarahedit.xlsx")%>%mutate(Species = ifelse(nchar(Species)==2,as.character(paste("0",Species,sep="")),as.character(Species)))
allcues<-full_join(historicalCUE,CUE)

#cue by each EMB - not including timeframe
cuesummarybyEMBs<-allcues%>%group_by(EMB,Species,Year)%>%dplyr::summarize(meanCUE=round(mean(AMEAN),2),SD=round(mean(ASD),2))%>%mutate_if(is.numeric,~replace_na(.,0))%>%dplyr::rename(SPC=Species)
cuesummarybyEMBs<-append.spc.names(cuesummarybyEMBs)
loc<-data.frame(EMB=unique(cuesummarybyEMBs$EMB));Year<-data.frame(Year=c(2006:2022));locandyear<-merge(loc,Year)
cuesummarybyEMBs<-left_join(locandyear,cuesummarybyEMBs)#add NA values for years not visited

ggplot(filter(cuesummarybyEMBs,Year>2005,EMB%in%c("Toronto Islands","Weller's Bay","Upper Bay")), aes(Year,meanCUE,fill=EMB))+geom_bar(color="black",stat="identity",width=0.6)+
  theme_classic()+
  theme(axis.text=element_text(size=20,color="black"),axis.title=element_text(size=26)) +labs(x="Year class",y="Mean Catch")+
  scale_y_continuous(breaks = seq(0,100,by=5),limits=c(0,18))+scale_x_continuous(breaks=seq(2006,2022,by=2))

## now do by EMB type - not including timeframe (Hamilton is seperate)
#allcues<-allcues%>%dplyr::mutate(EMBtype=ifelse(EMB%in%c("Upper","Middle","West Lake","East Lake","Weller's Bay"),"Sheltered","Exposed"),EMBtype=ifelse(EMB%in%c("Lower","North Channel Kingston"),"Transitional",EMBtype),EMBtype=ifelse(EMB=="Hamilton Harbour","Hamilton",EMBtype))#%>%filter(!EMB%in%c("Lake St. Francis","Thousand Islands"))
#cuesummarybyEMBtypes<-allcues%>%group_by(EMBtype,Species,Year)%>%dplyr::summarize(meanCUE=round(mean(AMEAN),2),SD=round(mean(ASD),2))%>%mutate_if(is.numeric,~replace_na(.,0))%>%dplyr::rename(SPC=Species)%>%mutate(MeanandSD=paste(meanCUE, SD, sep=" ("),MeanandSD=paste(MeanandSD, ")",sep=""))


Species<-cuesummarybyEMBs%>%filter(EMB%in%c("Toronto Islands","Hamilton Harbour"),SPC=="334")%>%group_by(Year,EMB)%>%dplyr::summarise(meancue=mean(meanCUE,na.rm=F))
loc<-data.frame(EMB=unique(Species$EMB));Year<-data.frame(Year=c(2006:2022));locandyear<-merge(loc,Year)
Species<-left_join(locandyear,Species)#add NA values for years not visited
ggplot(Species,aes(Year,meancue,color=EMB))+geom_line(size=1.3)+geom_point(size=2)+theme_classic()+geom_line(data = filter(Species, is.na(meancue)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.8,0.95))+labs(x="Year",y="Mean CUE")+ylim(0,8)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue2","brown2"))
PiscUB<-cuesummarybyEMBs%>%filter(EMB=="Upper",SPC%in%c("334","316","317","131","041","051","251"),Year>2005)%>%group_by(Year,SPC)%>%dplyr::summarise(meancue=mean(meanCUE),meansd=mean(SD))
ggplot(PiscUB,aes(Year,meancue,fill=SPC))+geom_bar(stat="identity")+theme_classic()

#check length dist of piscivores since biomass is very decreased but catches are not
historicfn124<-read_excel("./Data/FN124_2001to2021_AllEmbs.xlsx")%>%filter(PRJ_CD=="LOA_IA19_NS1",Spc=="314")
ggplot(historicfn124,aes(as.factor(SIZ),SIZCNT))+geom_bar(stat="identity")
FN124UB<-FN124%>%filter(PRJ_CD=="LOA_IA22_NS1",SPC=="314")
ggplot(FN124UB,aes(as.factor(SIZ),SIZCNT))+geom_bar(stat="identity")

###CUEs for AGM for sunfish, crappie, pike
allcuesfiltered<-allcues%>%filter(Species%in%c("313","314","319","131"),EMB=="Upper")
#split out species dataframes
spc<-split(allcuesfiltered,allcuesfiltered$Species)
names(spc)<-c("S131","S313","S314","S319")
lapply(names(spc), function(x) assign(x,spc[[x]],envir = .GlobalEnv))
sunfish<-full_join(S313,S314)

Year<-data.frame(c(2001:2022));colnames(Year)<-"Year"
S319<-full_join(S319,Year)
ggplot(sunfish, aes(Year,AMEAN))+geom_line(colour="black",size=0.7)+geom_point(size=2)+
  labs(y="Mean Catch Count")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),plot.title = element_text(size=31,hjust=0.5))+labs(x="Year",y="CPUE (Catch per Trapnet)")+ylim(0,20)

ggplot(sunfish, aes(Year,AMEAN,colour=Species))+geom_line(size=0.8)+geom_point(size=2.8)+
  labs(y="Mean Catch Count")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),plot.title = element_text(size=31,hjust=0.5),legend.title = element_blank(),legend.position = "top",legend.text = element_text(size=18))+labs(x="Year",y="CPUE (Catch per Trapnet)")+
  ylim(0,180)+scale_color_manual(values=c("black","grey50"),labels=c("Pumpkinseed","Bluegill"))
