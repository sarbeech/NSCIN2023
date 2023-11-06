###update IBIB
library(glfishr)
library(dplyr)

FN121<-get_FN121(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN123<-get_FN123(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN124<-get_FN124(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN125<-get_FN125(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))


setissues<-filter(FN121, EFFST>1)%>%select(PRJ_CD,SAM,SITE,EFFST,COMMENT1)
FN121<-FN121%>%filter(!EFFST>1)
FN123<-inner_join(FN121,FN123)
FN124<-inner_join(FN121,FN124)


#calculate catch weight 
lenwtreg<-readxl::read_xlsx("./Data/RWTREG.xlsx")%>%mutate(SPC = ifelse(nchar(SPC)==2,as.character(paste("0",SPC,sep="")),as.character(SPC)))

lenCUE<-merge(FN124,FN123)
lenCUE<-lenCUE%>%mutate(SIZCUE=ifelse(CATCNT==SUBCNT, SIZCNT, CATCNT*(SIZCNT/SUBCNT))) #CUE by length bin

wtcalc<-left_join(lenCUE,lenwtreg)

wtcalc$SIZWT<-exp((log(wtcalc$SIZ)*wtcalc$SLOPE + wtcalc$`Y-INTERCEP`)) /1000 * wtcalc$SIZCUE # get weight per bin
#missing 061 and 702 - need to update RWTREG fole

catwtcalc<-wtcalc%>%group_by(PRJ_CD,SAM,SPC)%>%dplyr::summarise(CATWTcue=sum(SIZWT),LENCOUNT=sum(SIZCUE)) # get catch weight per species and sam

fn123revised<-left_join(FN123,catwtcalc)%>%mutate(averagewt=CATWTcue/CATCNT) #get average weight for all catches

#missing  251 from 124, missing some SAMs of 180, 313, 194
#194,313,702 take average weight 
avewt313<-fn123revised%>%filter(SPC=="313")%>%summarise(meanwt=mean(averagewt,na.rm=T));fn123revised<-fn123revised%>%mutate(CATWTcue=ifelse(SPC=="313" & SAM=="6"& PRJ_CD=="LOA_IA22_NSB",avewt313$meanwt,CATWTcue),LENCOUNT=(ifelse(SPC=="313" & SAM=="6" &  PRJ_CD=="LOA_IA22_NSB",1,LENCOUNT)))
avewt194<-fn123revised%>%filter(SPC=="194")%>%summarise(meanwt=mean(averagewt,na.rm=T));fn123revised<-fn123revised%>%mutate(CATWTcue=ifelse(SPC=="194" & SAM=="22", avewt194$meanwt,CATWTcue),LENCOUNT=(ifelse(SPC=="194" & SAM=="22",1,LENCOUNT)))
avewt702<-fn123revised%>%filter(SPC=="702")%>%summarise(meanwt=mean(averagewt,na.rm=T));fn123revised<-fn123revised%>%mutate(CATWTcue=ifelse(SPC=="702" & SAM=="18", avewt702$meanwt,CATWTcue),LENCOUNT=(ifelse(SPC=="702" & SAM=="18",1,LENCOUNT)))

#remove 180 (minow fam) because no way of knowing weight
fn123revised<-fn123revised%>%filter(!SPC=="180")
#251 has no wt or len info... mean length from annual report 913 in toronto, 832 in upper bay
eel<-lenwtreg%>%filter(SPC=="251")
exp((log(913)*eel$SLOPE + eel$`Y-INTERCEP`)) /1000 
exp((log(832)*eel$SLOPE + eel$`Y-INTERCEP`)) /1000 
fn123revised<-fn123revised%>%mutate(CATWTcue=ifelse(SPC=="251" & PRJ_CD=="LOA_IA22_NST", 1.714549*CATCNT,CATWTcue)) 
fn123revised<-fn123revised%>%mutate(CATWTcue=ifelse(SPC=="251" & PRJ_CD%in%c("LOA_IA22_NSB",PRJ_CD=="LOA_IA22_NS1"), 1.27255*CATCNT,CATWTcue)) 


#groupby sam and spc
fn123cond<-fn123revised%>%select(PRJ_CD,SAM,SPC,SUBSPACE,CATCNT,CATWTcue)%>%dplyr::rename(CATWT=CATWTcue)

SPCgroups<-readxl::read_xlsx("./Data/Groups_1.xlsx")%>%mutate(SPC = ifelse(nchar(SPC)==2,as.character(paste("0",SPC,sep="")),as.character(SPC)))
IBIprep<-left_join(fn123cond,SPCgroups,by="SPC")


IBImetrics<-IBIprep%>%group_by(PRJ_CD,SAM,SUBSPACE)%>%dplyr::summarize(SNAT=length(NATIVE[NATIVE=="Yes"]),SNIN=length(`NON-INDIGE`[`NON-INDIGE`=="Yes"]),SCEN=length(CENTRARCHI[CENTRARCHI=="Yes"]),SPIS=length(PISCIVORE[PISCIVORE=="Yes"]),PPIS=sum(CATWT[PISCIVORE=="Yes"])/sum(CATWT)*100, PGEN=sum(CATWT[GENERALIST=="Yes"]/sum(CATWT)*100), PSPE=sum(CATWT[SPECIALIST=="Yes"])/sum(CATWT)*100, BNAT= sum(CATWT[NATIVE=="Yes"]), NNAT=sum(CATCNT[NATIVE=="Yes"]),PNNI=sum(CATCNT[`NON-INDIGE`=="Yes"])/sum(CATCNT)*100, PBNI=sum(CATWT[`NON-INDIGE`=="Yes"])/sum(CATWT)*100)

#metrics standardization
IBImetrics<-IBImetrics%>%mutate(SNATS=ifelse(SNAT>=12,10,SNAT*0.83))
IBImetrics<-IBImetrics%>%mutate(SNINS=ifelse(SNIN>=2,0,(SNIN*-5)+10),SNINS=ifelse(SNIN==0,10,SNINS))
IBImetrics<-IBImetrics%>%mutate(SCENS=ifelse(SCEN>=5,10,SCEN*2))
IBImetrics<-IBImetrics%>%mutate(SPISS=ifelse(SPIS>=4,10,SPIS*2.5))
IBImetrics<-IBImetrics%>%mutate(PPISS=ifelse(PPIS>33.3,10,PPIS*0.3))
IBImetrics<-IBImetrics%>%mutate(PGENS=ifelse(PGEN==100,0,(PGEN*-0.15)+15),PGENS=ifelse(PGEN<=33.3,10,PGENS))
IBImetrics<-IBImetrics%>%mutate(PSPES=ifelse(PSPE>=33.3,10,PSPE*0.3))
IBImetrics<-IBImetrics%>%mutate(NNATS=ifelse(NNAT>=500,10,NNAT*0.02))
IBImetrics<-IBImetrics%>%mutate(BNATS=ifelse(BNAT>=120,10,BNAT*0.08))
IBImetrics<-IBImetrics%>%mutate(PNNIS=ifelse(PNNI==100,0,(PNNI*-0.1)+10),PNNIS=ifelse(PNNI==0,10,PNNIS))
IBImetrics<-IBImetrics%>%mutate(PBNIS=ifelse(PBNI==100,0,(PBNI*-0.1)+10),PBNIS=ifelse(PBNI==0,10,PBNIS))

#final IBI calc
IBI<-IBImetrics%>%group_by(PRJ_CD,SAM,SUBSPACE)%>%mutate(IBI=((SNATS+SNINS+SCENS+SPISS+PPISS+PGENS+PSPES+NNATS+BNATS+PNNIS+PBNIS)*(10/11)))

#calculate the mean IBI and means of metrics for 2022
IBI%>%group_by(PRJ_CD)%>%summarise(mSNAT=mean(SNAT),mSNIN=mean(SNIN),mSCEN=mean(SCEN), mSPIS=mean(SPIS),mPPIS=mean(PPIS),mPGEN=mean(PGEN),mPSPE=mean(PSPE),mNNAT=mean(NNAT),mBNAT=mean(BNAT),mPNNI=mean(PNNI),mPBNI=mean(PBNI),mIBI=mean(IBI),sSNAT=sd(SNAT),sSNIN=sd(SNIN),sSCEN=sd(SCEN), sSPIS=sd(SPIS),sPPIS=sd(PPIS),sPGEN=sd(PGEN),sPSPE=sd(PSPE),sNNAT=sd(NNAT),sBNAT=sd(BNAT),sPNNI=sd(PNNI),sPBNI=sd(PBNI),sIBI=sd(IBI),mSNATS=mean(SNATS),mSNINS=mean(SNINS),mSCENS=mean(SCENS),mSPISS=mean(SPISS),mPPISS=mean(PPISS),mPGENS=mean(PGENS),mPSPES=mean(PSPES),mNNATS=mean(NNATS),mBNATS=mean(BNATS),mPNNIS=mean(PNNIS),mPBNIS=mean(PBNIS))


#compare to other years
IBIthroughtime<-readxl::read_xlsx("./Data/Exports/IBI_FinalSB_2001to2021.xlsx")%>%mutate(SITE=as.character(SITE))

IBI<-IBI%>%mutate(Year=2022,EMBTYPE=ifelse(PRJ_CD=="LOA_IA22_NST","Exposed","Sheltered"),EMB=ifelse(PRJ_CD=="LOA_IA22_NST","Toronto Islands","Upper"),AREA=NA,WATERBODY=NA,REGION=NA)%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NSB","Weller's Bay",EMB))%>%dplyr::rename(SITE=SUBSPACE)

IBIall<-full_join(IBIthroughtime,IBI)%>%filter(!is.na(IBI))
IBIall<-IBIall%>%mutate(EMB=ifelse(EMB=="Lower","Lower BOQ",EMB))
IBIall<-IBIall%>%mutate(EMB=ifelse(EMB=="Middle","Middle BOQ",EMB))
IBIall<-IBIall%>%mutate(EMB=ifelse(EMB=="Upper","Upper BOQ",EMB))
#write.csv(IBIall,"./Data/Exports/IBI2001to2022_SBupdated.csv",row.names = F)

library(ggplot2)

IBIallEMB<-IBIall%>%group_by(EMB,EMBTYPE)%>%dplyr::summarise(mSNAT=mean(SNAT),mSNIN=mean(SNIN),mSCEN=mean(SCEN), mSPIS=mean(SPIS),mPPIS=mean(PPIS),mPGEN=mean(PGEN),mPSPE=mean(PSPE),mNNAT=mean(NNAT),mBNAT=mean(BNAT),mPNNI=mean(PNNI),mPBNI=mean(PBNI),mIBI=mean(IBI))
ggplot(IBIallEMB, aes(reorder(EMB,mIBI),mIBI,fill=EMBTYPE))+geom_bar(stat="identity",color="black")+
  theme_classic()+
  theme(axis.text=element_text(size=17,color="black"),axis.title=element_text(size=26),axis.text.x =element_text(angle=45,hjust = 1),panel.border = element_rect(color="black",fill=NA,size=1.5),panel.grid.major.y = element_line(color="grey30"),legend.title=element_blank(),legend.text = element_text(size=20),legend.position=c(0.20,0.85))+
  labs(y="IBI",x="")+ scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=20))+scale_fill_manual(values=c("skyblue2","black","grey50","firebrick3"))#+scale_pattern_manual(values = c(Sheltered = "stripe", Exposed = "none"))

IBIEMBtype<-IBIall%>%mutate(EMBTYPE=ifelse(EMB=="Upper BOQ","Upper BOQ",EMBTYPE))%>%mutate(EMBTYPE=ifelse(EMB=="Hamilton Harbour","Hamilton",EMBTYPE))%>%mutate(EMBTYPE=ifelse(EMB=="Toronto Islands","Toronto",EMBTYPE))%>%filter(!EMBTYPE%in%c("River Reaches","Transitional"))
IBIEMBtypesum<-IBIEMBtype%>%group_by(EMBTYPE)%>%dplyr::summarise(mSNAT=mean(SNAT),mSNIN=mean(SNIN),mSCEN=mean(SCEN), mSPIS=mean(SPIS),mPPIS=mean(PPIS),mPGEN=mean(PGEN),mPSPE=mean(PSPE),mNNAT=mean(NNAT),mBNAT=mean(BNAT),mPNNI=mean(PNNI),mPBNI=mean(PBNI),mIBI=mean(IBI))%>%mutate(embgroup=c("B","G","G","B","G"))
ggplot(IBIEMBtypesum, aes(reorder(EMBTYPE,mIBI),mIBI,fill=embgroup))+geom_bar(stat="identity",color="black")+
  theme_classic()+
  theme(axis.text=element_text(size=17,color="black"),axis.title=element_text(size=26),axis.text.x =element_text(angle=45,hjust = 1),panel.border = element_rect(color="black",fill=NA,size=1.5),panel.grid.major.y = element_line(color="grey30"),legend.title=element_blank(),legend.text = element_text(size=20),legend.position=c(0.20,0.85))+
  labs(y="IBI",x="")+ scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=20))+scale_fill_manual(values=c("skyblue2","grey50"),labels=c("Exposed","Sheltered"))


#IBI line plot UB, TO, Ham
IBIEMBtypethrutime<-IBIEMBtype%>%group_by(EMBTYPE,Year)%>%dplyr::summarise(mSNAT=mean(SNAT),mSNIN=mean(SNIN),mSCEN=mean(SCEN), mSPIS=mean(SPIS),mPPIS=mean(PPIS),mPGEN=mean(PGEN),mPSPE=mean(PSPE),mNNAT=mean(NNAT),mBNAT=mean(BNAT),mPNNI=mean(PNNI),mPBNI=mean(PBNI),mIBI=mean(IBI))#%>%filter(EMB%in%c("Upper BOQ","Hamilton Harbour","Toronto Islands"))
loc<-data.frame(EMBTYPE=unique(IBIEMBtypethrutime$EMBTYPE));Year<-data.frame(Year=c(2006:2022));locandyear<-merge(loc,Year)
IBIEMBtypethrutime<-left_join(locandyear,IBIEMBtypethrutime)
ggplot(IBIEMBtypethrutime,aes(Year,mIBI,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean IBI")+ylim(30,90)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deeppink3","turquoise","dodgerblue1","brown1","mediumblue"))#scale_color_manual(values=c("brown2","deepskyblue1","mediumblue"))

#PPB
IBIEMBtypethrutime<-IBIEMBtypethrutime%>%filter(!EMBTYPE%in%c("Sheltered","Exposed"))
ggplot(IBIEMBtypethrutime,aes(Year,mPPIS,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean PPB")+ylim(0,55)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue1","brown2","mediumblue")))
# number of pisc
ggplot(IBIEMBtypethrutime,aes(Year,mSPIS,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean # pisc spc")+ylim(0,5)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue1","brown2","mediumblue"))
#PCEN
ggplot(IBIEMBtypethrutime,aes(Year,mSCEN,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean PCEN")+ylim(0,7)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue1","brown2","mediumblue"))
#PGEN
ggplot(IBIEMBtypethrutime,aes(Year,mPGEN,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean PGEN")+ylim(0,95)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue1","brown2","mediumblue"))
#PSPE
ggplot(IBIEMBtypethrutime,aes(Year,mPSPE,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean PSPE")+ylim(0,90)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue1","brown2","mediumblue"))
#PBNI
ggplot(IBIEMBtypethrutime,aes(Year,mPBNI,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean PBNI")+ylim(0,50)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue1","brown2","mediumblue"))
#BNAT
ggplot(IBIEMBtypethrutime,aes(Year,mBNAT,color=EMBTYPE))+geom_line(size=1.3)+geom_point(size=3)+theme_classic()+geom_line(data = filter(IBIEMBtypethrutime, is.na(mIBI)==FALSE), linetype = "dashed",size=1.1)+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21),legend.position = c(0.9,0.9))+labs(x="Year",y="Mean BNAT")+ylim(0,300)+scale_x_continuous(breaks=seq(2006,2022,by=2))+scale_color_manual(values=c("deepskyblue1","brown2","mediumblue"))


#load historical catwt data
library(readxl)
historicalCATWT<-read_excel("./Data/CATWT.xlsx")%>%dplyr::rename(SPC=Spc,CATCNT=CATCNTCUE,CATWT=CATWTCUE)%>%mutate(SPC=as.character(SPC))%>%mutate(SPC = ifelse(nchar(SPC)==2,as.character(paste("0",SPC,sep="")),as.character(SPC)))
historicalCATWT<-left_join(historicalCATWT,SPCgroups)
Ham2021CATWT<-read.csv("./Data/CATWT2021Hamilton.csv")%>%mutate(SPC=as.character(SPC),SAM=as.character(SAM))%>%mutate(SPC = ifelse(nchar(SPC)==2,as.character(paste("0",SPC,sep="")),as.character(SPC)))
allCATWT<-full_join(historicalCATWT,Ham2021CATWT)
allCATWT<-full_join(allCATWT,IBIprep)%>%mutate(Year=paste("20", substr(PRJ_CD,7,8),sep=""))%>%mutate(Year=as.numeric(Year),SPC=ifelse(SPC%in%c("168","171","172","173","177","160"),"170",SPC))%>%filter(!SPC%in%c("010","013","303","702","091","312","198","231","366","180","201","042"))
allCATWT<-append.spc.names(allCATWT)%>%mutate(SPC_NM=ifelse(SPC=="170","Redhorses",SPC_NM))
library(stringr)
wallUB<-allCATWT%>%filter(SPC=="051",str_detect(PRJ_CD,"NST"))
wallUB<-wallUB%>%group_by(Year)%>%dplyr::summarise(sumCATWT=sum(CATWT))
ggplot(wallUB,aes(Year,sumCATWT))+geom_line(size=1.3)+geom_point(size=2)+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28))+labs(x="Year",y="Mean CATWT")+ylim(0,200)+scale_x_continuous(breaks=seq(2006,2022,by=2))

PiscUB<-allCATWT%>%filter(str_detect(PRJ_CD,"NS1"),SPC%in%c("334","316","317","131","041","051","251"),Year>2005)%>%group_by(Year,SPC,SPC_NM)%>%dplyr::summarise(sumCATWT=sum(CATWT))
ggplot(PiscUB,aes(Year,sumCATWT,fill=SPC))+geom_bar(stat="identity")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21))+labs(y="Mean Catch Weight (kg)")+scale_fill_discrete(labels=PiscUB$SPC_NM)

ggplot(filter(allCATWT,PISCIVORE=="Yes"),aes(Year,CATWT,fill=SPC_NM))+geom_bar(stat="identity")+theme_classic()+facet_wrap(~EMB)

CentraTO<-allCATWT%>%filter(str_detect(PRJ_CD,"NST"),CENTRARCHI=="Yes")%>%group_by(Year,SPC,SPC_NM)%>%dplyr::summarise(sumCATWT=sum(CATWT))
CentraTO$SPC_NM<-as.factor()
ggplot(CentraTO,aes(Year,sumCATWT,fill=as.factor(SPC)))+geom_bar(stat="identity")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21))+labs(y="Mean Catch Weight (kg)")+
  scale_fill_discrete(labels=CentraTO$SPC_NM)

SpecUB<-allCATWT%>%filter(str_detect(PRJ_CD,"NS1"),SPECIALIST=="Yes")%>%group_by(Year,SPC,SPC_NM)%>%dplyr::summarise(sumCATWT=sum(CATWT))
ggplot(SpecUB,aes(Year,sumCATWT,fill=SPC))+geom_bar(stat="identity")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21))+labs(y="Mean Catch Weight (kg)")+scale_fill_discrete(labels=SpecUB$SPC_NM)

NonnatTO<-allCATWT%>%filter(str_detect(PRJ_CD,"NST"),NATIVE=="No")%>%group_by(Year,SPC,SPC_NM)%>%dplyr::summarise(sumCATWT=sum(CATWT))
ggplot(NonnatTO,aes(Year,sumCATWT,fill=SPC))+geom_bar(stat="identity")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21))+labs(y="Mean Catch Weight (kg)")+scale_fill_discrete(labels=NonnatTO$SPC_NM)

NatTO<-allCATWT%>%filter(str_detect(PRJ_CD,"NST"),NATIVE=="Yes")%>%group_by(Year,SPC_NM,SPC)%>%dplyr::summarise(sumCATWT=sum(CATWT))
ggplot(NatTO,aes(Year,sumCATWT,fill=SPC))+geom_bar(stat="identity")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21))+labs(y="Mean Catch Weight (kg)")+scale_fill_discrete(labels=NatTO$SPC_NM)
NatHM<-allCATWT%>%filter(str_detect(PRJ_CD,"NSH"),NATIVE=="Yes")%>%group_by(Year,SPC,SPC_NM)%>%dplyr::summarise(sumCATWT=sum(CATWT))
ggplot(NatHM,aes(Year,sumCATWT,fill=SPC))+geom_bar(stat="identity")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21))+labs(y="Mean Catch Weight (kg)")+scale_fill_discrete(labels=NatHM$SPC_NM)

GenTO<-allCATWT%>%filter(str_detect(PRJ_CD,"NST"),GENERALIST=="Yes")%>%group_by(Year,SPC,SPC_NM)%>%dplyr::summarise(sumCATWT=sum(CATWT))
ggplot(GenTO,aes(Year,sumCATWT,fill=SPC_NM))+geom_bar(stat="identity")+theme_classic()+
  theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28),legend.title = element_blank(),legend.text = element_text(size=21))+labs(y="Mean Catch Weight (kg)")+scale_fill_discrete(labels=CentraTO$SPC_NM)
