#first get number of fish of each species to be aged for the master list
#then pull out those fish from the fn125 in an excel sheet for agers to enter ages into (along with other fields)

library(RODBC)
#pull in from access database
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSF.accdb"

fetch_data <- function(table, prj_cd, src_db){
  DBConnection <- odbcConnectAccess2007(src_db, uid = "", pwd = "")
  dat <- sqlFetch(DBConnection, table, as.is=TRUE, stringsAsFactors=FALSE)
  odbcClose(DBConnection)
  return(dat)
}

fn125 <- fetch_data("FN125", params$prj_cd, src_db)

library(dplyr)
ages <- fn125 %>% 
  group_by(SPC,AGEST) %>%
  dplyr::summarize(number=n(),
                   sizemin=min(FLEN))


#get entry sheets
fn125 <- fn125 %>% 
  filter(!is.na(AGEST))

fn125 %>% 
  group_by(SPC, AGEST) %>% 
  summarise( n = n()) #154 scales, 55 otoliths, 3 pectoral spines, 25 operculums



library(writexl)

#select species of interest to age 

lsf_aging23 <- fn125 %>% 
  filter(SPC %in% c("131", "314", "316", "317", "331", "334", "483", "172", "168", "171", "319")) %>% 
  mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="") %>% 
  select(PRJ_CD, SAM, EFF, GRP, SPC, FISH, AGEMT, AGEID, NCA, EDGE, CONF, AGEA, PREFERRED, COMMENT7, AGEST)
lsf_aging23$AGEST <- gsub("28d", "28D", lsf_aging23$AGEST)

#pike
pike <- lsf_aging23 %>%
  filter(SPC=="131")

write_xlsx(pike,"./Data/Ageing/pike_NSF_cleithrum_Contractor.xlsx")
write_xlsx(pike,"./Data/Ageing/pike_NSF_Scale_Contractor.xlsx")
write_xlsx(pike,"./Data/Ageing/pike_NSF_AnalFin.xlsx")

#walleye
walleye <- lsf_aging23 %>%
  filter(SPC=="334")
write_xlsx(walleye,"./Data/Ageing/Walleye_NSF_otoliths.xlsx")

#yellow perch
Yperch <- lsf_aging23 %>%
  filter(SPC=="331")
write_xlsx(Yperch,"./Data/Ageing/YellowPerch_NSF.xlsx")

#bluegill
bluegill <- lsf_aging23 %>%
  filter(SPC=="314")
write_xlsx(bluegill,"./Data/Ageing/bluegill_NSF.xlsx")

#smallmouth
SMB <- lsf_aging23 %>%
  filter(SPC=="316")
write_xlsx(SMB,"./Data/Ageing/Smallmouth_NSF.xlsx")

#largemouth
LMB <- lsf_aging23 %>%
  filter(SPC=="317")
write_xlsx(LMB,"./Data/Ageing/Largemouth_NSF.xlsx")

#moxastoma sp. 
moxa <- lsf_aging23 %>%
  filter(SPC %in% c("168", "171", "172"))
write_xlsx(moxa,"./Data/Ageing/Moxastoma_NSF.xlsx")

#tench 
tench <- lsf_aging23 %>%
  filter(SPC=="483")
write_xlsx(tench,"./Data/Ageing/tench_NSF_otolith.xlsx")

#crappie
crappie <- lsf_aging23 %>%
  filter(SPC=="319")
write_xlsx(crappie,"./Data/Ageing/crappie_NSF_otolith.xlsx")

