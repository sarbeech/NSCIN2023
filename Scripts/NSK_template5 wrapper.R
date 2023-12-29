library(gfsR)
#devtools::install_github("AdamCottrill/glfishr")
library(glfishr)
library(lubridate)
library(dplyr)
library(RODBC)
library(tidyr)
library(readxl)

#pull in from access database
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSK.accdb"

fetch_data <- function(table, prj_cd, src_db){
  DBConnection <- odbcConnectAccess2007(src_db, uid = "", pwd = "")
  dat <- sqlFetch(DBConnection, table, as.is=TRUE, stringsAsFactors=FALSE)
  odbcClose(DBConnection)
  return(dat)
}

#load all tables
fn011 <- fetch_data("FN011", params$prj_cd, src_db)
fn012 <- fetch_data("FN012", params$prj_cd, src_db) 
fn022 <- fetch_data("FN022", params$prj_cd, src_db)
fn026 <- fetch_data("FN026", params$prj_cd, src_db)
fn026_subspace <- fetch_data("FN026_SUBSPACE", params$prj_cd, src_db)
fn028 <- fetch_data("FN028", params$prj_cd, src_db)
fn121 <- fetch_data("FN121", params$prj_cd, src_db)
fn122 <- fetch_data("FN122", params$prj_cd, src_db)
fn123 <- fetch_data("FN123", params$prj_cd, src_db)
fn123_nonfish <- fetch_data("FN123_NonFish", params$prj_cd, src_db)
fn124 <- fetch_data("FN124", params$prj_cd, src_db)
fn125 <- fetch_data("FN125", params$prj_cd, src_db)
fn125_lamprey <- fetch_data("FN125_lamprey", params$prj_cd, src_db)
fn125_tags <- fetch_data("FN125_tags", params$prj_cd, src_db)
fn126 <- fetch_data("FN126", params$prj_cd, src_db)
fn127 <- fetch_data("FN127", params$prj_cd, src_db)
Gear_Effort_Process_Types<- fetch_data("Gear_Effort_Process_Types", params$prj_cd, src_db)


FN011=fn011%>%mutate(PRJ_DATE0=as.Date(PRJ_DATE0),PRJ_DATE1=as.Date(PRJ_DATE1)) #remove times

FN012<-fn012
#FN012NSB<-read_xlsx("./FN012NSB2023Mar.xlsx")
#FN012NSBfilt<-FN012NSB%>%select(!c(TLEN_MIN,FLEN_MIN,TLEN_MAX,FLEN_MAX,RWT_MIN,RWT_MAX,K_MIN_ERROR,K_MAX_ERROR,K_MIN_WARN,K_MAX_WARN))
#fn012update<-read_xlsx("./FN012NS12023Mar.xlsx")%>%select(c(SPC,TLEN_MIN,FLEN_MIN,TLEN_MAX,FLEN_MAX,RWT_MIN,RWT_MAX,K_MIN_ERROR,K_MAX_ERROR,K_MIN_WARN,K_MAX_WARN)) #add updated min/max values
#FN012<-full_join(FN012NSBfilt,fn012update)%>%filter(!is.na(PRJ_CD))

FN022=fn022%>%mutate(SSN_DATE0=as.Date(SSN_DATE0),SSN_DATE1=as.Date(SSN_DATE1)) #remove times

#FN026
FN026=fn026

#FN026 subspace
FN026_Subspace=fn026_subspace

FN028=fn028 #remove dates
FN028$EFFTM0_GE=format(as.POSIXct(FN028$EFFTM0_GE), format = "%H:%M:%S")
FN028$EFFTM0_LT=format(as.POSIXct(FN028$EFFTM0_LT), format = "%H:%M:%S")

#FN121
fn121<-fn121%>%select(! c(ENTRY_STATUS, LAT0,LAT1,LON0,LON1,EFF0,EFF1,EFFDURCALC,GR))%>%
  mutate(EFFDT0=as.Date(EFFDT0),EFFDT1=as.Date(EFFDT1),EFFTM0=format(as.POSIXct(fn121$EFFTM0), format = "%H:%M:%S"),EFFTM1=format(as.POSIXct(fn121$EFFTM1), format = "%H:%M:%S"))

#FN121
FN121 <- data.frame(
  PRJ_CD = fn121$PRJ_CD,
  SAM = fn121$SAM,
  PROCESS_TYPE = fn121$PROCESS_TYPE,
  SSN = fn121$SSN,
  SUBSPACE = fn121$SUBSPACE, 
  MODE = fn121$MODE, 
  GRID5=NA,
  EFFDT0 = fn121$EFFDT0,
  EFFTM0 = fn121$EFFTM0,
  EFFDT1 = (fn121$EFFDT1),
  EFFTM1 = fn121$EFFTM1,
  EFFDUR = fn121$EFFDUR,
  EFFST = fn121$EFFST, 
  SITP = fn121$SITP,
  DD_LAT0 = fn121$DD_LAT0,
  DD_LON0 = fn121$DD_LON0,
  DD_LON1 = fn121$DD_LON1,
  DD_LAT1 = fn121$DD_LAT1, 
  SITEM1 = fn121$SITEM1,
  SITEM0 = fn121$SITEM0,
  SIDEP0 = fn121$SIDEP0,
  SIDEP1=NA,
  GRDEPMAX = fn121$GRDEPMAX, 
  GRDEPMIN = fn121$GRDEPMIN,
  GRDEPMID=fn121$GRDEPMID,
  SECCHI0 = fn121$SECCHI0,
  SECCHI1=fn121$SECCHI1, 
  SLIME = fn121$SLIME,
  CREW = fn121$CREW,
  COMMENT1 = fn121$COMMENT1,
  VESSEL = fn121$VESSEL,
  VESSEL_DIRECTION=NA,
  VESSEL_SPEED=NA,
  WARP=NA,
  BOTTOM=fn121$BOTTOM,
  COVER=fn121$COVER,
  LEADUSE=fn121$LEADUSE,
  DISTOFF=fn121$DISTOFF,
  LEAD_ANGLE=fn121$LEAD_ANGLE,
  O2BOT0=NA,
  O2BOT1=NA,
  O2SURF0= NA,
  O2SURF1=NA,
  O2GR0=NA,
  O2GR1=NA,
  AIRTEM0=NA,
  AIRTEM1=NA,
  WIND0=NA,
  WIND1=fn121$WIND1,
  PRECIP0=NA,
  PRECIP1=NA,
  CLOUD_PC0=NA,
  CLOUD_PC1=NA,
  WAVEHT0=NA,
  WAVEHT1=fn121$WAVEHT1,
  XWEATHER=fn121$XWEATHER,
  VEGETATION=fn121$VEGETATION
)
library(leaflet)
library(rgdal)
leaflet()%>%addCircles(data=FN121,lng=~DD_LON0,lat=~DD_LAT0)
BOQ <- readOGR("./Data/pec_detail.kml", verbose = FALSE)
plot(BOQ)
points(FN121$DD_LON0,FN121$DD_LAT0, pch =19, cex=1, col="red", bg="white", lwd=3)




FN122=fn122



#FN123
FN123=fn123%>%select(!ENTRY_STATUS)



#FN123nonfish
FN123_NonFish=fn123_nonfish



#FN124
FN124=fn124%>%select(!ENTRY_STATUS)



# fn125
FN125 <- data.frame(
PRJ_CD = fn125$PRJ_CD,
SAM = fn125$SAM,
EFF = fn125$EFF,
SPC = fn125$SPC,
GRP =  fn125$GRP,
FISH = fn125$FISH,
FLEN = fn125$FLEN,
TLEN = fn125$TLEN,
GIRTH = NA, 
RWT = fn125$RWT, 
EVISWT=fn125$EVISWT,
SEX = fn125$SEX,
MAT = fn125$MAT, 
GON = as.character(fn125$GON),
GONWT=fn125$GONWT,
CLIPC = fn125$CLIPC,
CLIPA = fn125$CLIPA,
NODC = fn125$NODC,
NODA = fn125$NODA,
TISSUE = fn125$TISSUE,
AGEST = fn125$AGEST,
FATE = fn125$FATE,# some fish were just weighed, it is REQUIRED,
STOM_CONTENTS_WT=fn125$STOM_CONTENTS_WT,
FDSAM=fn125$FDSAM,
COMMENT5 = fn125$COMMENT5
)



# fn125_lamprey 
FN125_lamprey<-data.frame(
  PRJ_CD = fn125_lamprey$PRJ_CD,
  SAM = fn125_lamprey$SAM,
  EFF = fn125_lamprey$EFF,
  SPC = fn125_lamprey$SPC,
  GRP =  fn125_lamprey$GRP,
  FISH = fn125_lamprey$FISH,
  LAMID =fn125_lamprey$LAMID,
  XLAM =fn125_lamprey$XLAM,
  LAMIJC_TYPE= fn125_lamprey$LAMIJC_TYPE,
  LAMIJC_SIZE = fn125_lamprey$LAMIJC_SIZE, 
  COMMENT_LAM = fn125_lamprey$COMMENT_LAM
)



#fn125_tag
#FN125_tags<-data.frame(
  PRJ_CD= fn125_tagsprep$PRJ_CD,
  SAM= fn125_tagsprep$SAM,
  EFF="001",
  SPC= fn125_tagsprep$SPC,
  GRP= "00",
  FISH= fn125_tagsprep$FISH,
  FISH_TAG_ID=1,
  TAGID=,
  TAGDOC="", #P=pit tag, 4= abdominal insertion, 99=unknown origin (could add DFO?), 9 = unknown colour
  TAGSTAT="", 
  XCWTSEQ=NA,
  XTAGINCKD= "",
  COMMENT_TAG=""
)




#fn126
FN126 <- data.frame(
  PRJ_CD = fn126$PRJ_CD,
  SAM = fn126$SAM,
  EFF = fn126$EFF,
  SPC = fn126$SPC,
  GRP =  fn126$GRP,
  FISH = fn126$FISH,
  FOOD = fn126$FOOD,
  TAXON = fn126$TAXON,
  FDCNT = fn126$FDCNT,#change to make NA values =0
  FDMES = fn126$FDMES, 
  FDVAL = fn126$FDVAL,
  LIFESTAGE = fn126$LIFESTAGE,
  COMMENT6 = fn126$COMMENT6
)
#FN126$LIFESTAGE[is.na(FN126$LIFESTAGE)] <- ""  
#FN126$FDMES[is.na(FN126$FDMES)] <- ""  
#FN126$FDVAL[is.na(FN126$FDVAL)] <- ""  
#these lines just prevent warnings when appending
#may need to leave FDMES as NA because adam said it prevent upload...


#fn127
FN127<-  data.frame(
  PRJ_CD = fn127$PRJ_CD,
  SAM = fn127$SAM,
  EFF = fn127$EFF,
  SPC = fn127$SPC,
  GRP =  fn127$GRP,
  FISH = fn127$FISH,
  AGEID = fn127$AGEID,
  PREFERRED = fn127$PREFERRED,
  AGEA = fn127$AGEA, 
  AGEMT = fn127$AGEMT,
  EDGE = fn127$EDGE,
  CONF = fn127$CONF,
  NCA = fn127$NCA,
  AGESTRM=fn127$AGESTRM,
  AGELAKE=fn127$AGELAKE,
  SPAWNCHKCNT=fn127$SPAWNCHKCNT,
  AGE_FAIL=fn127$AGE_FAIL,
  COMMENT7 = fn127$COMMENT7)


#fn125 stomach flags
#fn126flag<-FN126
#fn126flag$FDSAM<- "1"
#fn126flag<-fn126flag%>%select(PRJ_CD,SAM,EFF,SPC,FISH,FDSAM)
#fn126flag<-unique(fn126flag) #remove duplicates

#FN125<-left_join(FN125,fn126flag)
#FN125$FDSAM[is.na(FN125$FDSAM)]<-0



#####
##ERROR CHECKS
species<-fn125%>%filter(SPC=="316")
plot(species$FLEN,species$RWT)



#Append the data
TARGET <- odbcConnectAccess2007("./Data/Great_Lakes_Assessment_Template_5NSK.accdb") 

##append will fail if numeric fields are blank instead of NA
##append will also sometimes give warnings if character fields are NA instead of blank
sqlSave(TARGET, FN011, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN012, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN022, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN026, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN026_Subspace, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN028, append = TRUE, rownames = FALSE,verbose=T) #time fields cant have date in front
sqlSave(TARGET, FN121, append = TRUE, rownames = FALSE,verbose=T) 
sqlSave(TARGET, FN122, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN123, append = TRUE, rownames = FALSE,verbose=T) #won't append if fn012 species are missing
sqlSave(TARGET, FN123_NonFish, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN124, append = TRUE, rownames = FALSE,verbose=T) #takes a long time to append (wait for it to finish)
sqlSave(TARGET, FN125, append = TRUE, rownames = FALSE,verbose=T) 
sqlSave(TARGET, FN125_lamprey, append = TRUE, rownames = FALSE,verbose=T) 
sqlSave(TARGET, FN126, append = TRUE, rownames = FALSE,verbose=T) 
sqlSave(TARGET, FN127, append = TRUE, rownames = FALSE,verbose=T) 
sqlSave(TARGET, Gear_Effort_Process_Types, append = TRUE, rownames = FALSE)

odbcClose(TARGET)

