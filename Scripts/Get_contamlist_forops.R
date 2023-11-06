library(RODBC)
library(dplyr)
library(gfsR)
#pull in from access database
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2022/LOA_IA22_NSB.accdb"

fetch_data <- function(table, prj_cd, src_db){
  DBConnection <- odbcConnectAccess2007(src_db, uid = "", pwd = "")
  dat <- sqlFetch(DBConnection, table, as.is=TRUE, stringsAsFactors=FALSE)
  odbcClose(DBConnection)
  return(dat)
}

fn125 <- fetch_data("FN125", params$prj_cd, src_db)
fn125<-append.spc.names(fn125)
fn125<-fn125%>%filter(!is.na(XCONTAM))%>%select(PRJ_CD,SAM,SPC,FISH,TLEN,FLEN,RWT,SEX,XCONTAM,SPC_NM)


fn121 <- fetch_data("FN121", params$prj_cd, src_db)
fn121<-fn121%>%select(PRJ_CD,SAM, DD_LAT0,DD_LON0,EFFDT1)%>%mutate(Year=2022,EFFDT1=as.Date(EFFDT1),AREA="Weller's Bay",Block="")

contamslist_NSB<-left_join(fn125,fn121)
write.csv(contamslist_NSB,"NSB2022_contamlist.csv",row.names = F)

