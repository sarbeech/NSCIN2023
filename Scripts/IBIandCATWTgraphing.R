
IBIall<-read.csv("./Data/IBI2001to2022_SBupdated.csv")

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
