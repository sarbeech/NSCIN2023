library(glfishr)
library(dplyr)
library(ggplot2)
library(gfsR)
library(readxl)
library(tidyverse)
library(lakeontario)
library(rgdal)
library(sp)
library(maptools)
library(raster)


FN121<-get_FN121(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN121trap<-get_FN121_Trapnet(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1"))); FN121<-full_join(FN121,FN121trap)
FN123<-get_FN123(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN124<-get_FN124(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))
FN125<-get_FN125(list(prj_cd=c("LOA_IA22_NSB","LOA_IA22_NST","LOA_IA22_NS1")))


#summarize site information
setissues<-filter(FN121, EFFST>1)%>%dplyr::select(PRJ_CD,SAM,SUBSPACE,EFFST,COMMENT1)
FN121<-FN121%>%filter(!EFFST>1) #remove sets with set issues
netsset<-FN121%>%group_by(PRJ_CD)%>%dplyr::summarise(N=n())

FN121$deptarget<-ifelse(FN121$GRDEPMAX > 1.9 & FN121$GRDEPMAX < 2.6 , "ontarget", "above")
FN121$deptarget<-ifelse(FN121$GRDEPMAX < 2, "below", FN121$deptarget)
FN121<-FN121%>%separate(SITP,into = c("substrate","cover"),1);FN121<-FN121%>%mutate(subtype=ifelse(substrate==3|substrate==4,"Soft","Hard"))


siteinfo<-FN121%>%group_by(PRJ_CD)%>%dplyr::summarize(
  SurveyDates = paste(min(EFFDT1),max(EFFDT1),sep="-"),
  WaterTemperature = paste(min(SITEM1),max(SITEM1), sep="-"),
  Numoflifts = n(),
  Hardsubstrate=sum(subtype=="Hard",na.rm = T),
  Softsubstrate=sum(subtype=="Soft",na.rm = T),
  Nocover =sum(cover=="1",na.rm = T),
  Lowcover=sum(cover=="2",na.rm = T),
  Medcover=sum(cover=="3",na.rm = T),
  Highcover=sum(cover=="1",na.rm = T),
  AverageGapdepth=mean(GRDEPMAX),
  Numoflifts_targetdepth = sum(deptarget=="ontarget"),
  Numoflifts_belowtarget = sum(deptarget=="below"),
  Numoflifts_abovetarget = sum(deptarget=="above")
)
write.csv(siteinfo,"./Data/Exports/SiteSummary2022.csv",row.names =F)

#make maps
# identify coordinates
coordinates(df) <- c("LONGITUDE", "LATITUDE")


BOQ <- readOGR("./Data/pec_detail.kml", verbose = FALSE)
Toronto<-readOGR("./Data/torontoharbour_detail.kml", verbose = FALSE)

#crs(shoreline)
#zone <- spTransform(zone, CRS = crs(shoreline))
tiff('./Figures/UBQWellersmap.tiff',units="in",width = 14,height=7,res=250,compression="lzw")
plot(BOQ, xlim = c(-77.30631,-77.29917), ylim = c(43.92400, 44.2300))
points(FN121$DD_LON0,FN121$DD_LAT0, pch =19, cex=1, col="red", bg="white", lwd=3)
text(-77.30631,44.2320,"Upper Bay of Quinte / Weller's Bay",cex=2)
text(-77.30631,44.2150,"Sites = 33 / 24",cex=1.3)
sp::degAxis(side = 1)
sp::degAxis(side = 2,las = 2)
sb <- layout.scale.bar(height = 1)
raster::scalebar(d = 10, # distance in km
                 xy =c(-77.780,44.17500),
                 type = "bar", 
                 divs = 2, 
                 below = "km", 
                 lonlat = TRUE,
                 label = c(0,5,10), 
                 adj=c(0, -1), 
                 lwd = 2)
dev.off()

tiff('./Figures/Torontomap.tiff',units="in",width = 14,height=7,res=250,compression="lzw")
plot(Toronto, xlim = c(-79.47731,-79.31917), ylim = c(43.60000, 43.66184))
sp::degAxis(side = 1)
sp::degAxis(side = 2,las = 2)
points(FN121$DD_LON0,FN121$DD_LAT0, pch =19, cex=1, col="red", bg="white", lwd=3)
text(-79.40731,43.66184,"Toronto Harbour",cex=2)
text(-79.40731,43.65800,"Sites = 22",cex=1.3)
sb <- layout.scale.bar(height = 1)
raster::scalebar(d = 2, # distance in km
                 xy =c(-79.33917,43.60200),
                 type = "bar", 
                 divs = 2, 
                 below = "km", 
                 lonlat = TRUE,
                 label = c(0,1,2), 
                 adj=c(0, -1), 
                 lwd = 2)
dev.off()


#totalfish caught
FN123<-inner_join(FN121,FN123)
fishcaught<-FN123%>%group_by(PRJ_CD)%>%dplyr::summarise(totalfish=sum(CATCNT),species=length(unique(SPC)))

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
  dplyr::summarise(ArithmeticmeanCATCNT = (round(mean(CATCNT),3)),GeometricmeanCATCNT = (round(exp(mean(log(CATCNT[CATCNT>0]))),3)), SE=sd(CATCNT)/sqrt(n()), SD=sd(CATCNT), RSE=round((round(SE,2)/ArithmeticmeanCATCNT)*100,0))
#remove 0 CUEs
CUE<-CUE%>%filter(!ArithmeticmeanCATCNT==0)%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NST","Toronto Islands","Upper"),Year=2022)%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NSB","Weller's Bay",EMB))
CUE<-left_join(CUE,netsset)

#get average length per species
Avelength=FN124%>%group_by(PRJ_CD,SPC)%>%dplyr::summarize(meanlength=sum(SIZ*SIZCNT)/sum(SIZCNT))

CUE<-left_join(CUE,Avelength)
write.csv(CUE,"./Data/Exports/Catchsummary2022.csv",row.names=F)




#add to historical time series
CUE<-CUE%>%dplyr::rename(AMEAN=ArithmeticmeanCATCNT,GMEAN=GeometricmeanCATCNT,ASE=SE,ASD=SD,Species=SPC,COMMON_NAM=SPC_NM)%>%select(! meanlength)
historicalCUE<-read_excel("./Data/Exports/CUESummary2001to2021_Sarahedit.xlsx")%>%mutate(Species = ifelse(nchar(Species)==2,as.character(paste("0",Species,sep="")),as.character(Species)))
allcues<-full_join(historicalCUE,CUE)

#cue by each EMB 
cuesummarybyEMBs<-allcues%>%group_by(EMB,Species,Year)%>%dplyr::summarize(meanCUE=round(mean(AMEAN),2),SD=round(mean(ASD),2))%>%mutate_if(is.numeric,~replace_na(.,0))%>%dplyr::rename(SPC=Species)
cuesummarybyEMBs<-append.spc.names(cuesummarybyEMBs)
loc<-data.frame(EMB=unique(cuesummarybyEMBs$EMB));Year<-data.frame(Year=c(2006:2022));locandyear<-merge(loc,Year)
cuesummarybyEMBs<-left_join(locandyear,cuesummarybyEMBs)#add NA values for years not visited

filteredcue<-cuesummarybyEMBs%>%filter(SPC%in%c("233","234","186","181","301","251","163","041","051"))%>%mutate(EMB=ifelse(EMB=="Upper","Upper Bay of Quinte",EMB))
tiff('./Figures/CUE2.tiff',units="in",width = 8,height=8.8,res=250,compression="lzw")
ggplot(filter(filteredcue,Year>2005,EMB%in%c("Toronto Islands","Weller's Bay","Upper Bay of Quinte")), aes(Year,meanCUE,fill=EMB))+geom_bar(color="black",stat="identity",width=0.6)+
  facet_grid(SPC_NM~EMB,switch="y",scales = "free_y")+theme_classic()+labs(y="Catch per Trapnet",x="Year")+
  theme(axis.text=element_text(size=10.5,color="black"),axis.title=element_text(size=15),legend.position="none",strip.text.x=element_text(size=15),strip.text.y=element_text(size=10),strip.text.y.left=element_text(angle=0),strip.placement="outside",panel.border = element_rect(color="black",fill=NA),strip.background = element_blank())+
  scale_fill_manual(values=c("red2","skyblue2","black"))
dev.off()

#now do second set of species - cue graph #2
filteredcue<-cuesummarybyEMBs%>%filter(SPC%in%c("313","311","314","316","317","319","331","334","131"))%>%mutate(EMB=ifelse(EMB=="Upper","Upper Bay of Quinte",EMB))
tiff('./Figures/CUE1.tiff',units="in",width = 8,height=8.8,res=250,compression="lzw")
ggplot(filter(filteredcue,Year>2005,EMB%in%c("Toronto Islands","Weller's Bay","Upper Bay of Quinte")), aes(Year,meanCUE,fill=EMB))+geom_bar(color="black",stat="identity",width=0.6)+
  facet_grid(SPC_NM~EMB,switch="y",scales = "free_y")+theme_classic()+labs(y="Catch per Trapnet",x="Year")+
  theme(axis.text=element_text(size=10.5,color="black"),axis.title=element_text(size=15),legend.position="none",strip.text.x=element_text(size=15),strip.text.y=element_text(size=10),strip.text.y.left=element_text(angle=0),strip.placement="outside",panel.border = element_rect(color="black",fill=NA),strip.background = element_blank())+
  scale_fill_manual(values=c("red2","skyblue2","black"))
dev.off()


#plot one species at a time
allcuesfiltered<-allcues%>%filter(Species%in%c("233","234","186","181","301","251","311","313","314","319","131","334","331","163"),EMB=="Upper")
#split out species dataframes
spc<-split(allcuesfiltered,allcuesfiltered$Species)
names(spc)<-c("S131","S313","S314","S319")#NEED TO UPDATE FOR SPECIES LIST ABOVE
lapply(names(spc), function(x) assign(x,spc[[x]],envir = .GlobalEnv))
#Year<-data.frame(c(2001:2022));colnames(Year)<-"Year"
#S319<-full_join(S319,Year)
ggplot(filter(filteredcue,Year>2005,EMB%in%c("Toronto Islands","Weller's Bay","Upper")), aes(Year,meanCUE))+geom_bar(color="black",stat="identity",width=0.6)+
    facet_wrap(~EMB)+labs(y="CUE")+
    theme_classic()+theme(axis.text=element_text(size=19,color="black"),axis.title=element_text(size=28))
#scale_y_continuous(breaks = seq(0,100,by=5),limits=c(0,18))+scale_x_continuous(breaks=seq(2006,2022,by=2))




##get age distributions
Ages<-FN125%>%filter(!is.na(AGE))%>%filter(SPC%in%c("131","311","313","314","316","317","334","331","319"))%>%mutate(Yearclass=2022-AGE,EMB=ifelse(PRJ_CD=="LOA_IA22_NST","Toronto Islands","Upper Bay of Quinte"))%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NSB","Weller's Bay",EMB))
Ages<-append.spc.names(Ages)
Agesummary<-Ages%>%group_by(PRJ_CD,EMB,SPC,SPC_NM,AGE,Yearclass)%>%dplyr::summarise(SumAges=n())

ggplot(Agesummary,aes(as.factor(AGE),SumAges))+geom_bar(stat="identity",col="black",fill="black")+facet_grid(SPC_NM~EMB,switch="y")+theme_classic()+labs(y="Count",x="Age")+
  theme(axis.text=element_text(size=25,color="black"),axis.title=element_text(size=28),strip.text.x=element_text(size=29),strip.text.y=element_text(size=23),strip.text.y.left=element_text(angle=0),strip.placement="outside",panel.border = element_rect(color="black",fill=NA),strip.background = element_blank() )

# get length distributions - export as 1800x 1100
lengthdistpreds<-FN124%>%filter(SPC%in%c("334","131","317","316"))%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NST","Toronto Islands","Upper Bay of Quinte"))%>%mutate(EMB=ifelse(PRJ_CD=="LOA_IA22_NSB","Weller's Bay",EMB));lengthdistpreds<-append.spc.names(lengthdistpreds)
ggplot(lengthdistpreds,aes(SIZ,SIZCNT))+geom_bar(stat="identity",col="black",fill="black")+facet_grid(SPC_NM~EMB,switch="y",scales="free_y")+theme_classic()+labs(y="Count",x="Length")+
  theme(axis.text=element_text(size=25,color="black"),axis.title=element_text(size=2),strip.text.x=element_text(size=31),strip.text.y=element_text(size=23),strip.text.y.left=element_text(angle=0),strip.placement="outside",panel.border = element_rect(color="black",fill=NA),strip.background = element_blank() )



#get IBI figures (assume IBI script has aleady been run and the data file updated)
#remove transitional areas - just show sheltered vs exposed
IBImeanthroughtime<-read.csv("./Data/Exports/IBI2001to2022_SBupdated.csv")
IBImeanthroughtime<-IBImeanthroughtime%>%filter(EMBTYPE%in%c("Sheltered","Exposed"))

library(ggpattern)
IBIallEMB<-IBImeanthroughtime%>%group_by(EMB,EMBTYPE)%>%dplyr::summarise(mSNAT=mean(SNAT),mSNIN=mean(SNIN),mSCEN=mean(SCEN), mSPIS=mean(SPIS),mPPIS=mean(PPIS),mPGEN=mean(PGEN),mPSPE=mean(PSPE),mNNAT=mean(NNAT),mBNAT=mean(BNAT),mPNNI=mean(PNNI),mPBNI=mean(PBNI),mIBI=mean(IBI),sdIBI=sd(IBI),sdPPIS=sd(PPIS),sdPSPE=sd(PSPE))%>%filter(EMBTYPE%in%c("Sheltered","Exposed"))

tiff('./Figures/IBI_AR.tiff',units="in",width = 16,height=8.5,res=250,compression="lzw")
ggplot(IBIallEMB, aes(reorder(EMB,mIBI),mIBI,fill=EMBTYPE))+geom_bar(stat="identity",color="black")+geom_errorbar(aes(x=EMB,ymin=mIBI-sdIBI,ymax=mIBI+sdIBI),color="grey40")+
  theme_classic()+
  theme(axis.text=element_text(size=17,color="black"),axis.title=element_text(size=26),axis.text.x =element_text(angle=45,hjust = 1),panel.border = element_rect(color="black",fill=NA,size=1.5),panel.grid.major.y = element_line(color="grey30"),legend.title=element_blank(),legend.text = element_text(size=20),legend.position=c(0.20,0.85))+labs(y="IBI",x="")+ scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=20))+scale_fill_manual(values = c(Sheltered = "black", Exposed = "grey"))+geom_hline(yintercept = 55,col="red",linetype="dotted",linewidth=1.2)
dev.off()

tiff('./Figures/PPB_AR.tiff',units="in",width = 12,height=10,res=250,compression="lzw")
ggplot(IBIallEMB, aes(reorder(EMB,mPPIS),mPPIS,fill=EMBTYPE))+geom_bar(stat="identity",color="black")+geom_errorbar(aes(x=EMB,ymin=mPPIS-sdPPIS,ymax=mPPIS+sdPPIS),color="grey40")+
  theme_classic()+  
  theme(axis.text=element_text(size=22,color="black"),axis.title=element_text(size=30),axis.text.x =element_text(angle=45,hjust = 1),panel.border = element_rect(color="black",fill=NA,size=1.5),legend.title=element_blank(),legend.text = element_text(size=28),legend.position=c(20,85))+labs(y="PPB",x="")+ scale_y_continuous(limits=c(0,80),breaks = seq(0,90,by=20))+scale_fill_manual(values = c(Sheltered = "black", Exposed = "grey"))+geom_hline(yintercept = 20,col="red",linetype="dotted",linewidth=1.6)
dev.off()

tiff('./Figures/PSPE_AR.tiff',units="in",width = 12,height=10,res=250,compression="lzw")
ggplot(IBIallEMB, aes(reorder(EMB,mPSPE),mPSPE,fill=EMBTYPE))+geom_bar(stat="identity",color="black")+geom_errorbar(aes(x=EMB,ymin=mPSPE-sdPSPE,ymax=mPSPE+sdPSPE),color="grey50")+
  theme_classic()+  
  theme(axis.text=element_text(size=22,color="black"),axis.title=element_text(size=30),axis.text.x =element_text(angle=45,hjust = 1),panel.border = element_rect(color="black",fill=NA,size=1.5),legend.title=element_blank(),legend.text = element_text(size=28),legend.position=c(20,85))+labs(y="PSPE",x="")+ scale_y_continuous(limits=c(0,85),breaks = seq(0,90,by=20))+scale_fill_manual(values = c(Sheltered = "black", Exposed = "grey"))+geom_hline(yintercept = 40,col="red",linetype="dotted",linewidth=1.6)
dev.off()


#get values for 2022
IBIbyyear<-IBImeanthroughtime%>%group_by(EMB,EMBTYPE,PRJ_CD,Year)%>%dplyr::summarise(mSNAT=mean(SNAT),mSNIN=mean(SNIN),mSCEN=mean(SCEN), mSPIS=mean(SPIS),mPPIS=mean(PPIS),mPGEN=mean(PGEN),mPSPE=mean(PSPE),mNNAT=mean(NNAT),mBNAT=mean(BNAT),mPNNI=mean(PNNI),mPBNI=mean(PBNI),mIBI=mean(IBI),sdIBI=sd(IBI),sdPPIS=sd(PPIS),sdPSPE=sd(PSPE))%>%filter(EMBTYPE%in%c("Sheltered","Exposed"))
