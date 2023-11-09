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
