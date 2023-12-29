library(RODBC)
library(dplyr)
library(gfsR)
library(writexl)

#pull in from access database
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSK.accdb"

fetch_data <- function(table, prj_cd, src_db){
  DBConnection <- odbcConnectAccess2007(src_db, uid = "", pwd = "")
  dat <- sqlFetch(DBConnection, table, as.is=TRUE, stringsAsFactors=FALSE)
  odbcClose(DBConnection)
  return(dat)
}

fn125 <- fetch_data("FN125", params$prj_cd, src_db)
fn125<-append.spc.names(fn125)
fn125<-fn125%>%filter(!is.na(XCONTAM),!XCONTAM=="")%>%select(PRJ_CD,SAM,SPC,FISH,TLEN,FLEN,RWT,SEX,XCONTAM,SPC_NM)

#refer to taylors file to assign blocks based on contam #
fn121 <- fetch_data("FN121", params$prj_cd, src_db)
fn121<-fn121%>%select(PRJ_CD,SAM, DD_LAT0,DD_LON0,EFFDT1)%>%mutate(Year=2023,EFFDT1=as.Date(EFFDT1),AREA="Kingston North Channel",)

contamslist_NSK<-left_join(fn125,fn121)%>%mutate(Block="11")
write_xlsx(contamslist_NSK,"./Data/Exports/NSK2023_contamlist.xlsx")



#lake st francis
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSF.accdb"
fn125 <- fetch_data("FN125", params$prj_cd, src_db)
fn125<-append.spc.names(fn125)
fn125<-fn125%>%filter(!is.na(XCONTAM),!XCONTAM=="")%>%select(PRJ_CD,SAM,SPC,FISH,TLEN,FLEN,RWT,SEX,XCONTAM,SPC_NM)

#refer to taylors file to assign blocks based on contam #
fn121 <- fetch_data("FN121", params$prj_cd, src_db)
fn121<-fn121%>%select(PRJ_CD,SAM, DD_LAT0,DD_LON0,EFFDT1)%>%mutate(Year=2023,EFFDT1=as.Date(EFFDT1),AREA="Lake St. Francis",)

contamslist_NSF<-left_join(fn125,fn121)%>%mutate(Block="15")
write_xlsx(contamslist_NSF,"./Data/Exports/NSF2023_contamlist.xlsx")



#thousand islands
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2023/LOA_IA23_NSI.accdb"
fn125 <- fetch_data("FN125", params$prj_cd, src_db)
fn125<-append.spc.names(fn125)
fn125<-fn125%>%filter(!is.na(XCONTAM),!XCONTAM=="")%>%select(PRJ_CD,SAM,SPC,FISH,TLEN,FLEN,RWT,SEX,XCONTAM,SPC_NM)

#refer to taylors file to assign blocks based on contam #
fn121 <- fetch_data("FN121", params$prj_cd, src_db)
fn121<-fn121%>%select(PRJ_CD,SAM, DD_LAT0,DD_LON0,EFFDT1)%>%mutate(Year=2023,EFFDT1=as.Date(EFFDT1),AREA="Weller's Bay",)

contamslist_NSI<-left_join(fn125,fn121)%>%mutate(Block="12")
write_xlsx(contamslist_NSI,"./Data/Exports/NSI2023_contamlist.xlsx")