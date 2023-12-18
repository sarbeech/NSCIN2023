library(remotes)
library(gfsR)
library(glfishr)
library(lubridate)
library(dplyr)
library(RODBC)
library(tidyr)
library(readxl)
library(ggplot2)

#Lake St Francis Data Check
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSF.accdb" #pull access database from initial data entry 

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
fn028 <- fetch_data("FN028", params$prj_cd, src_db)
fn121 <- fetch_data("FN121", params$prj_cd, src_db)
fn026subspace <- fetch_data("FN026_Subspace", params$prj_cd, src_db)
fn122 <- fetch_data("FN122", params$prj_cd, src_db)
fn123 <- fetch_data("FN123", params$prj_cd, src_db)
fn123nonfish <- fetch_data("FN123_NonFish", params$prj_cd, src_db)
fn124 <- fetch_data("FN124", params$prj_cd, src_db)
fn125 <- fetch_data("FN125", params$prj_cd, src_db)
fn125_lamprey <- fetch_data("FN125_lamprey", params$prj_cd, src_db)
fn125_tags <- fetch_data("FN125_tags", params$prj_cd, src_db)
fn126 <- fetch_data("FN126", params$prj_cd, src_db)
fn127 <- fetch_data("FN127", params$prj_cd, src_db)
gear_effort_process_types <- fetch_data("Gear_Effort_Process_Types", params$prj_cd, src_db)




#FN011
FN011 = 
  fn011 %>%
  mutate(PRJ_DATE0=as.Date(PRJ_DATE0),
         PRJ_DATE1=as.Date(PRJ_DATE1)) 

FN012<- fn012

FN022 = fn022 %>%
  mutate(SSN_DATE0=as.Date(SSN_DATE0),
         SSN_DATE1=as.Date(SSN_DATE1))

#FN026
FN026<-fn026

#FN026 subspace
FN026_Subspace<-fn026subspace

#FN028
FN028 = fn028 %>%
  mutate(EFFTM0_GE=format(as.POSIXct(EFFTM0_GE), format = "%H:%M:%S"),
         EFFTM0_LT=format(as.POSIXct(EFFTM0_LT), format = "%H:%M:%S")) 

#FN121
FN121 <- fn121 %>%
  mutate(EFFDUR=EFFDURCALC,
         EFFDT1=as.Date(EFFDT1), 
         EFFDT0=as.Date(EFFDT0), 
         EFFTM0=format(as.POSIXct(EFFTM0), format = "%H:%M:%S"), 
         EFFTM1=format(as.POSIXct(EFFTM1), format = "%H:%M:%S"), 
         WIND0 = gsub("(...)(.*)", "\\1-\\2", WIND0), 
         WIND0 <- ifelse(grepl(".{3}-00$", FN121$WIND0), "000", FN121$WIND0)) %>%  #WIND0 needs to be in the format "XXX-XX" and tis adds the dash in between characters
  select(!c(ENTRY_STATUS, LAT0, LON0, LAT1, LON1, GR, EFFDURCALC, EFF0, EFF1)) %>% 
  mutate(WIND0 = ifelse(grepl(".{3}-00$", WIND0), "000", WIND0)) # any wind speed of 0 needs to be in the format "000" 

#FN122
FN122<-fn122

#FN123
FN123 <- fn123 %>% 
  mutate(CATWT = CATWT/1000,
         SUBWT = SUBWT/1000) %>% 
  select(!ENTRY_STATUS) #convert to KG -  can also do this in the database but you have to give the go ahead

#FN123nonfish
FN123_NonFish<-fn123nonfish

#FN124
FN124 <- fn124 %>% 
  select(!ENTRY_STATUS)

# fn125
FN125 <- fn125 %>% 
  select(!c(ENTRY_STATUS,XCONTAM))

# fn125_lamprey 
FN125_lamprey<-fn125_lamprey

#fn125_tag
FN125_tags <- fn125_tags 

#fn126
FN126 <- fn126

#fn127
FN127<- fn127

#Gear_Effort_Process_Types
Gear_Effort_Process_Types <- gear_effort_process_types

######
## Send the tables directly to access - this can be finicky 
##Open connection to the target dataset
TARGET <- odbcConnectAccess2007(file.choose()) #choose template database from downloaded version


#Append the data
##append will also sometimes give warnings if character fields are NA instead of blank
sqlSave(TARGET, FN011, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN012, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN022, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN026, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN026_Subspace, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN028, append = TRUE, rownames = FALSE,verbose=T) 
sqlSave(TARGET, FN121, append = TRUE, rownames = FALSE,verbose=T) 
sqlSave(TARGET, FN122, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN123, append = TRUE, rownames = FALSE,verbose=T) #won't append if fn012 species are missing
sqlSave(TARGET, FN124, append = TRUE, rownames = FALSE,verbose=T) #takes a long time to append (wait for it to finish)
sqlSave(TARGET, FN125, append = TRUE, rownames = FALSE,verbose=T)  
sqlSave(TARGET, FN125_lamprey, append = TRUE, rownames = FALSE,verbose=T)  #if a parent record is missing it won't append
sqlSave(TARGET, FN125_tags, append = TRUE, rownames = FALSE,verbose=T)  
sqlSave(TARGET, FN126, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, FN127, append = TRUE, rownames = FALSE,verbose=T)
sqlSave(TARGET, Gear_Effort_Process_Types, append = TRUE, rownames = FALSE,verbose=T)

odbcClose(TARGET)
