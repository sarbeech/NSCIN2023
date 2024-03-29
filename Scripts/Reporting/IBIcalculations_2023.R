###update IBI
library(glfishr)
library(dplyr)

FN121<-get_FN121(list(prj_cd=c("LOA_IA23_NSF", "LOA_IA23_NSI", "LOA_IA23_NSK")))
FN123<-get_FN123(list(prj_cd=c("LOA_IA23_NSF", "LOA_IA23_NSI", "LOA_IA23_NSK")))
FN124<-get_FN124(list(prj_cd=c("LOA_IA23_NSF", "LOA_IA23_NSI", "LOA_IA23_NSK")))
FN125<-get_FN125(list(prj_cd=c("LOA_IA23_NSF", "LOA_IA23_NSI", "LOA_IA23_NSK")))

#remove any compromised nets based on your judgment - look to comment field
setissues<-filter(FN121, EFFST>1)%>%
  select(PRJ_CD,SAM,SUBSPACE,EFFST,COMMENT1);setissues
FN124 %>% 
  filter(PRJ_CD == "LOA_IA23_NSI" & SAM == "20") #visually inspect the efforts above 5

FN121<-FN121 %>%
  filter(!(PRJ_CD %in% "LOA_IA23_NSF" & SAM %in% c("22", "18", "26"))) #decided to keep the other 3 effort status of 5 since they had considerable catches and I remember these sets as fishing properly

FN123<-inner_join(FN121,FN123)#add site info to FN123 and removes EFFST>1 from FN123
FN124<-inner_join(FN121,FN124)#add site info to FN124 and removes SAMs with EFFST>1


###calculate catch weight 
#apply length distribution from length tally info to the whole catch
#for SAMs where not all fish were length tallied (SUBCNT), get the proportion of fish lengthed in each size bin (i.e. SUBCNT=28, 8 fish lengthed in the 130 sizbin, prop = 8/28=0.28), then multiple by the total CATCNT
sizbinweight<-merge(FN124,FN123) #synonymous with innerjoin()
sizbinweight<-sizbinweight %>% 
  mutate(SIZCNTnew = ifelse(CATCNT == SUBCNT, SIZCNT, CATCNT*(SIZCNT/SUBCNT))) 

#import length weight regression file with equations for species
lenwtreg <- readxl::read_xlsx("./Data/RWTREG.xlsx") %>% 
  mutate(SPC = ifelse(nchar(SPC)==2,as.character(paste("0",SPC,sep="")),as.character(SPC)))

wtcalc<-left_join(sizbinweight,lenwtreg)

#identify species without any formula or non accurate formula
#should revisit thislist when NSI and NSK are completed 
missing_spc_len_Wt <- wtcalc %>%
  filter(NAME == "Estimate" | is.na(NAME)) %>%
  distinct(SPC) %>%
  left_join(select(wtcalc, SPC, PRJ_CD), by = "SPC") %>% 
  distinct(PRJ_CD, SPC)
#171 172 173 177 186 168 194 132 051 483 213 502 015
#171 172 173 177 168 - should have more accurate biomass estimates because redhorse are a sig catch and sizable fish
#186 - should have a more accurate biomass estimate because carp are a signigiant biomass and catch
#194 015 - minimal effect - possibly remove
#213 no lenwt formula -  should get more accurate estimate or leave with the "Estimate" formula
#132 - use the weight of the fish caught since there was only 1
#051 no lenwt formular - using the actual weight for LSF since catches were low. Use the average for NSK and NSI as catches were higher. Should get a more accurate biomass estimate since catches were signifigant
#502 no lenwt formula - use the 3 individuals that were caught (i beleive only one was weighed - use this weight)
#483 no lenwt formula - use the weight of the individuals caught (make sure to remove 1 indivudal caught in wing)
#320 no lenwt formula - use weight of bluegill
#232 no lenwt formula - use weight of brown bullehad

#get catch weight per size bin solely using length 
wtcalc$SIZWT <- exp((log(wtcalc$SIZ)*wtcalc$SLOPE + wtcalc$`Y-INTERCEP`)) /1000 * wtcalc$SIZCNTnew 

#information required to add weight for 213
fn125_213 <- get_FN125(list(prj_cd = c("SLR_IA22_LSF", "SLR_IA22_THI", "SLR_IA21_LSF", "SLR_IA21_THI")))
spc213_wt <- mean(filter(fn125_213, SPC == "213")$RWT)/1000
#add weights to fish without any formula 
wtcalc <- wtcalc %>% 
  mutate(SIZWT=ifelse(SPC=="502" & PRJ_CD=="LOA_IA23_NSF", filter(FN125, SPC == "502")$RWT/1000 , SIZWT), #add weight to 502
         SIZWT=ifelse(SPC=="186" , mean(filter(FN125, SPC == "186")$RWT,na.rm = T)/1000 , SIZWT), #add weight to 186 based on all weights of carp
         SIZWT=ifelse(SPC=="132" & PRJ_CD=="LOA_IA23_NSF", mean(filter(FN125, SPC == "132"& PRJ_CD=="LOA_IA23_NSF")$RWT,na.rm = T)/1000 , SIZWT), #add weight to 132
         SIZWT=ifelse(SPC=="051" & PRJ_CD=="LOA_IA23_NSF" & SAM == "7", filter(FN125, SPC == "051" & PRJ_CD=="LOA_IA23_NSF" & SAM == "7")$RWT/1000 , SIZWT),#adding specific weight of the exact fish 
         SIZWT=ifelse(SPC=="051" & PRJ_CD=="LOA_IA23_NSF" & SAM == "33", filter(FN125, SPC == "051"& PRJ_CD=="LOA_IA23_NSF" & SAM == "33")$RWT/1000 , SIZWT),#adding specific weight of the exact fish 
         SIZWT=ifelse(SPC=="051" & PRJ_CD=="LOA_IA23_NSF" & SAM == "38", filter(FN125, SPC == "051"& PRJ_CD=="LOA_IA23_NSF" & SAM == "38")$RWT/1000 , SIZWT),#adding specific weight of the exact fish 
         SIZWT=ifelse(SPC=="051" & PRJ_CD=="LOA_IA23_NSF" & SAM == "21", mean(filter(FN125, SPC == "051"& PRJ_CD=="LOA_IA23_NSF")$RWT, na.rm = T)/1000 , SIZWT), #add weight to 051 - was not weighed
         SIZWT=ifelse(SPC=="051" & PRJ_CD=="LOA_IA23_NSI", mean(filter(FN125, SPC == "051" & PRJ_CD=="LOA_IA23_NSI")$RWT)/1000 , SIZWT),
         SIZWT=ifelse(SPC=="051" & PRJ_CD=="LOA_IA23_NSK", mean(filter(FN125, SPC == "051" & PRJ_CD=="LOA_IA23_NSK")$RWT)/1000 , SIZWT),
         SIZWT=ifelse(SPC=="483" & PRJ_CD=="LOA_IA23_NSF" & SAM == "1", filter(FN125, SPC == "483" & PRJ_CD=="LOA_IA23_NSF" & SAM == "1")$RWT/1000 , SIZWT), #adding specific weight of fish
         SIZWT=ifelse(SPC=="483" & PRJ_CD=="LOA_IA23_NSF" & SAM == "13", mean(filter(FN125, SPC == "483"& PRJ_CD=="LOA_IA23_NSF" & SAM == "13")$RWT, na.rm = T)/1000 , SIZWT), #adding mean weight of the two individuals caught
         SIZWT=ifelse(SPC=="320" , mean(filter(FN125, SPC == "314")$RWT, na.rm = T)/1000, SIZWT), #adding weight to the sunfish based on average bluegill weight
         SIZWT=ifelse(SPC=="232" , mean(filter(FN125, SPC == "233")$RWT, na.rm = T)/1000, SIZWT), #adding weight to yellow bullhead based on brown bullhead
         SIZWT=ifelse(SPC=="213" , spc213_wt, SIZWT), #add weight for 213
         SIZWT=ifelse(SPC %in% c("168", "171", "172", "173", "177"), mean(filter(FN125, SPC %in% c("168", "171", "172", "173", "177"))$RWT, na.rm = TRUE)/1000 , SIZWT)) # add weight for redhorse species


#now get total catch weight (biomass) per SAM and SPC
catwtcalc <- wtcalc %>% 
  group_by(PRJ_CD, SAM, SPC) %>% 
  dplyr::summarise(CATWTnew=sum(SIZWT),
                   LENCOUNT=sum(SIZCNTnew)) 

#join back to the FN123
fn123revised <- left_join(FN123,catwtcalc) %>% 
  mutate(averagewt=CATWTnew/CATCNT) #get average weight of each species
#remove 194 and 015 
fn123revised <- fn123revised %>% 
  filter(!SPC == "194",
         !SPC == "015",
         !grepl("CAUGHT IN WING", COMMENT3, ignore.case = TRUE))


#for any species where BIOCNT = CATCNT we have summed the weight to get the actual CATWT
for (i in 1:nrow(fn123revised)) {
  if (fn123revised$BIOCNT[i] == fn123revised$CATCNT[i]) {
    filtered_data <- filter(FN125, SAM == fn123revised$SAM[i] & SPC == fn123revised$SPC[i])
    
    # Check if there are any NA values in the filtered data
    if (!any(is.na(filtered_data$RWT))) {
      fn123revised$CATWTnew[i] <- sum(filtered_data$RWT, na.rm = TRUE)/1000
    }
  }
}

fn123revised <- fn123revised %>% 
  mutate(CATWTnew = ifelse(PRJ_CD == "LOA_IA23_NSK" & SPC == "233" & SAM == "5", (mean(filter(FN125, SPC == "233")$RWT, na.rm = T)/1000)*filter(fn123revised, PRJ_CD == "LOA_IA23_NSK" & SPC == "233" & SAM == "5")$CATCNT, CATWTnew), 
         CATWTnew = ifelse(PRJ_CD == "LOA_IA23_NSK" & SPC == "131" & SAM == "16",(mean(filter(FN125, SPC == "131")$RWT, na.rm = T)/1000)*filter(fn123revised, PRJ_CD == "LOA_IA23_NSK" & SPC == "131" & SAM == "16")$CATCNT, CATWTnew), 
         CATWTnew = ifelse(PRJ_CD == "LOA_IA23_NSI" & SPC == "163" & SAM == "11",(mean(filter(FN125, SPC == "163")$RWT, na.rm = T)/1000)*filter(fn123revised, PRJ_CD == "LOA_IA23_NSI" & SPC == "163" & SAM == "11")$CATCNT, CATWTnew), 
         averagewt = ifelse(PRJ_CD == "LOA_IA23_NSI" & SPC == "163" & SAM == "11", (mean(filter(FN125, SPC == "163")$RWT, na.rm = T)/1000), averagewt), 
         averagewt = ifelse(PRJ_CD == "LOA_IA23_NSK" & SPC == "131" & SAM == "16",(mean(filter(FN125, SPC == "131")$RWT, na.rm = T)/1000), averagewt), 
         averagewt = ifelse(PRJ_CD == "LOA_IA23_NSK" & SPC == "233" & SAM == "5", (mean(filter(FN125, SPC == "233")$RWT, na.rm = T)/1000), averagewt))


#condense and add in species groupings 
fn123cond<-fn123revised %>% 
  select(PRJ_CD,SAM,SPC,SUBSPACE,CATCNT,CATWTnew) %>% 
  dplyr::rename(CATWT=CATWTnew)

SPCgroups <- readxl::read_xlsx("./Data/Groups_1.xlsx") %>% 
  mutate(SPC = ifelse(nchar(SPC)==2,as.character(paste("0",SPC,sep="")),as.character(SPC)))
SPCgroups <- SPCgroups[,1:16] #remove added column containing a comment

IBIprep<-left_join(fn123cond,SPCgroups,by="SPC")

#calculate ibi metrics
IBImetrics<-IBIprep %>% 
  group_by(PRJ_CD,SAM,SUBSPACE) %>%
  dplyr::summarize(SNAT=length(NATIVE[NATIVE=="Yes"]),
                   SNIN=length(`NON-INDIGE`[`NON-INDIGE`=="Yes"]),
                   SCEN=length(CENTRARCHI[CENTRARCHI=="Yes"]),
                   SPIS=length(PISCIVORE[PISCIVORE=="Yes"]),
                   PPIS=sum(CATWT[PISCIVORE=="Yes"])/sum(CATWT)*100, 
                   PGEN=sum(CATWT[GENERALIST=="Yes"]/sum(CATWT)*100), 
                   PSPE=sum(CATWT[SPECIALIST=="Yes"])/sum(CATWT)*100, 
                   BNAT= sum(CATWT[NATIVE=="Yes"]), 
                   NNAT=sum(CATCNT[NATIVE=="Yes"]),
                   PNNI=sum(CATCNT[`NON-INDIGE`=="Yes"])/sum(CATCNT)*100, 
                   PBNI=sum(CATWT[`NON-INDIGE`=="Yes"])/sum(CATWT)*100)

#metrics standardization
IBImetrics<-IBImetrics%>%
  mutate(SNATS=ifelse(SNAT>=12,10,SNAT*0.83),
         SNINS=ifelse(SNIN>=2,0,(SNIN*-5)+10),
         SNINS=ifelse(SNIN==0,10,SNINS),
         SCENS=ifelse(SCEN>=5,10,SCEN*2),
         SPISS=ifelse(SPIS>=4,10,SPIS*2.5),
         PPISS=ifelse(PPIS>33.3,10,PPIS*0.3),
         PGENS=ifelse(PGEN==100,0,(PGEN*-0.15)+15),
         PGENS=ifelse(PGEN<=33.3,10,PGENS))%>%
  mutate(PSPES=ifelse(PSPE>=33.3,10,PSPE*0.3),
         NNATS=ifelse(NNAT>=500,10,NNAT*0.02),
         BNATS=ifelse(BNAT>=120,10,BNAT*0.08),
         PNNIS=ifelse(PNNI==100,0,(PNNI*-0.1)+10),
         PNNIS=ifelse(PNNI==0,10,PNNIS),
         PBNIS=ifelse(PBNI==100,0,(PBNI*-0.1)+10),
         PBNIS=ifelse(PBNI==0,10,PBNIS))

#final IBI calc
IBI <- IBImetrics %>% 
  group_by(PRJ_CD,SAM,SUBSPACE) %>%
  mutate(IBI=((SNATS+SNINS+SCENS+SPISS+PPISS+PGENS+PSPES+NNATS+BNATS+PNNIS+PBNIS)*(10/11)))

#calculate the mean IBI and means of metrics for current year
IBIcurrentyr<-IBI %>% 
  group_by(PRJ_CD)%>%
  summarise(mSNAT=mean(SNAT),
            mSNIN=mean(SNIN),
            mSCEN=mean(SCEN), 
            mSPIS=mean(SPIS),
            mPPIS=mean(PPIS),
            mPGEN=mean(PGEN),
            mPSPE=mean(PSPE),
            mNNAT=mean(NNAT),
            mBNAT=mean(BNAT),
            mPNNI=mean(PNNI),
            mPBNI=mean(PBNI),
            mIBI=mean(IBI),
            sSNAT=sd(SNAT),
            sSNIN=sd(SNIN),
            sSCEN=sd(SCEN), 
            sSPIS=sd(SPIS),
            sPPIS=sd(PPIS),
            sPGEN=sd(PGEN),
            sPSPE=sd(PSPE),
            sNNAT=sd(NNAT),
            sBNAT=sd(BNAT),
            sPNNI=sd(PNNI),
            sPBNI=sd(PBNI),
            sIBI=sd(IBI),
            mSNATS=mean(SNATS),
            mSNINS=mean(SNINS),
            mSCENS=mean(SCENS),
            mSPISS=mean(SPISS),
            mPPISS=mean(PPISS),
            mPGENS=mean(PGENS),
            mPSPES=mean(PSPES),
            mNNATS=mean(NNATS),
            mBNATS=mean(BNATS),
            mPNNIS=mean(PNNIS),
            mPBNIS=mean(PBNIS))


#compare to other years
IBIthroughtime<-read.csv("./Data/IBI2001to2022_SBupdated.csv") %>% 
  mutate(SITE=as.character(SITE))
#add extra fields 
IBI <- IBI %>% 
  mutate(Year=2023,
         EMBTYPE=ifelse(PRJ_CD %in% c("LOA_IA23_NSF", "LOA_IA23_NSI"),"River Reaches","Transitional"), #transitional will be assigned to NSK
         EMB= ifelse(PRJ_CD == "LOA_IA23_NSF", "Lake St. Francis", 
                     ifelse(PRJ_CD == "LOA_IA23_NSI", "Thousand Islands", "North Channel Kingston")),
         WATERBODY = ifelse(PRJ_CD == "LOA_IA23_NSF", "Lake St. Francis", 
                            ifelse(PRJ_CD == "LOA_IA23_NSI", "Thousand Islands", "North Channel Kingston")),
         AREA=NA,
         REGION=NA) %>% 
  dplyr::rename(SITE=SUBSPACE)
#join current year to historical file
IBIthroughtime$SAM <- as.character(IBIthroughtime$SAM) #making classes compatible
IBIall <- full_join(IBIthroughtime,IBI) %>% 
  filter(!is.na(IBI))


write.csv(IBIall,"./Data/Exports/IBI2001to2023_ARupdated.csv",row.names = F)


