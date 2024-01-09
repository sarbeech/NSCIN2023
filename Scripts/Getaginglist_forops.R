#first get number of fish of each species to be aged for the master list
#then pull out those fish from the fn125 in an excel sheet for agers to enter ages into (along with other fields)

library(RODBC)
library(glfishr)
#pull in from access database
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSI.accdb"
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSF.accdb"
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSK.accdb"

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
                   sizemin=min(FLEN));ages
#get entry sheets
fn125 <- fn125 %>% 
  filter(!is.na(AGEST))

library(writexl)
#CCreate function to get aging list 
#NOTE THIS FUNCTION DOES NOT SEPARATE AND CREATE MULTIPLE FILES IF MULTIPLE AGING STRUCTURES ARE PRESENT
# MUST CREATE COPIES AND RENAME THEM TO GET DIFFERENT FIELS FOR CONTRACTOR -- for example pike require 3 files since there are clithrum, scales and anal fin
getAgingLists <- function(SPC_Code, data){
  for (i in 1:length(SPC_Code)){
    dat <- data %>% 
      filter(SPC == SPC_Code[i])
    write_xlsx(dat, paste0("./Data/Ageing/", dat$spc_nmco[i], ".", substr(dat$PRJ_CD[i], nchar(dat$PRJ_CD[i]) - 2, nchar(dat$PRJ_CD[i])), dat$AGEST[i], ".xlsx"))
    
  }
}

#select species of interest to age for LSF
#Note -- need to change src_db to connect to the correct project database 

lsf_aging23 <- fn125 %>% 
  filter((SPC %in% c("131", "314", "316", "317", "331", "334", "483", "172", "168", "171", "319") & !(SPC == "319" & AGEST == "2")))  %>% #removed scales from crappie since only aging otoliths
  mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="") %>% 
  select(PRJ_CD, SAM, EFF, GRP, SPC, FISH, AGEMT, AGEID, NCA, EDGE, CONF, AGEA, PREFERRED, COMMENT7, AGEST)
lsf_aging23$AGEST <- gsub("28d", "28D", lsf_aging23$AGEST) #fix typo

SPC_full_name <- get_species(list(spc = lsf_aging23$SPC, detail = TRUE), to_upper = FALSE) #get species name
SPC_full_name <- SPC_full_name %>% 
  select(spc, spc_nmco) %>% 
  rename(SPC =  spc) #rename columsn for join
lsf_aging23 <- left_join(lsf_aging23, SPC_full_name) #add species name to data

SPC_codes <- unique(lsf_aging23$SPC) #create vector of species for aging 
getAgingLists(SPC_Code = SPC_codes, data = lsf_aging23)

###################################################################################################################
#select species of interest to age for NSI 
#make sure to switch src_db to access the correct database for NSI
NSI_aging23 <- fn125 %>% 
  filter(SPC %in% c("131", "314", "316", "317", "331", "334", "319")) %>% 
  mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="") %>% 
  select(PRJ_CD, SAM, EFF, GRP, SPC, FISH, AGEMT, AGEID, NCA, EDGE, CONF, AGEA, PREFERRED, COMMENT7, AGEST)
SPC_full_name <- get_species(list(spc = NSI_aging23$SPC, detail = TRUE), to_upper = FALSE)
SPC_full_name <- SPC_full_name %>% 
  select(spc, spc_nmco) %>% 
  rename(SPC =  spc)
NSI_aging23 <- left_join(NSI_aging23, SPC_full_name)

SPC_codes <- unique(NSI_aging23$SPC)

# Call the function with SPC_codes
#NOTE THIS FUNCTION DOES NOT SEPARATE AND CREATE MULTIPLE FILES IF MULTIPLE AGING STRUCTURES ARE PRESENT
# MUST CREATE COPIES AND RENAME THEM TO GET DIFFERENT FIELS FOR CONTRACTOR
getAgingLists(SPC_codes, NSI_aging23)

###################################################################################################################
#select species of interest to age for NSK 
#make sure to switch src_db to access the correct database for NSK
