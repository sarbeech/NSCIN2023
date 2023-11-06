#first get number of fish of each species to be aged for the master list
#then pull out those fish from the fn125 in an excel sheet for agers to enter ages into (along with other fields)

library(RODBC)
#pull in from access database
src_db <- "//cihs.ad.gov.on.ca/MNRF/Groups/LEGACY/LRCPGLENFP00001/FWSB_Glenora/Information Resources/Field Season/2022/LOA_IA22_NSB.accdb"

fetch_data <- function(table, prj_cd, src_db){
  DBConnection <- odbcConnectAccess2007(src_db, uid = "", pwd = "")
  dat <- sqlFetch(DBConnection, table, as.is=TRUE, stringsAsFactors=FALSE)
  odbcClose(DBConnection)
  return(dat)
}

fn125 <- fetch_data("FN125", params$prj_cd, src_db)

library(dplyr)
ages<-fn125%>%group_by(SPC,AGEST)%>%dplyr::summarize(number=n(),sizemin=min(FLEN))


#get entry sheets
fn125<-fn125%>%filter(!is.na(AGEST))


library(writexl)
#pike
pike<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="131",AGEST%in%c("2","2D"))
pike<-pike%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(pike,"./pikeaging.")

#walleye
wall<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="334",AGEST%in%c("2","2A","2B","A"))
wall<-wall%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(wall,"./walleyeaging.xlsx")

#yellow perch
yelperch<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="331",AGEST%in%c("2","2A","2B","A"))
yelperch<-yelperch%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(yelperch,"./yellowperchaging.xlsx")

#rock bass
rockbass<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="311",AGEST%in%c("2","2A","2B","A"))
rockbass<-rockbass%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(rockbass,"./rockbassaging.xlsx")

#pumpkin
pumpkin<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="313",AGEST%in%c("2","2A","2B","A"))
pumpkin<-pumpkin%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(pumpkin,"./pumpkinseedaging.xlsx")

#bluegill
bluegill<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="314",AGEST%in%c("2","2A","2B","A"))
bluegill<-bluegill%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(bluegill,"./bluegillaging.xlsx")

#smallmouth
SMB<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="316",AGEST%in%c("2","2A","2B","A"))
SMB<-SMB%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(SMB,"./smallmouthaging.xlsx")


#crappie
crappie<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="319",AGEST%in%c("2","2A","2B","A"))
crappie<-crappie%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(crappie,"./blackcrappieaging.xlsx")

#largemouth
LMB<-fn125%>%select(c("PRJ_CD","SAM","EFF","GRP","SPC","FISH","AGEST"))%>%filter(SPC=="317",AGEST%in%c("2","2A","2B","A"))
LMB<-LMB%>%mutate(AGEID="",PREFERRED="",AGEA="",AGEMT="",EDGE="",CONF="",NCA="",COMMENT7="")
write_xlsx(LMB,"./largemouthbassaging.xlsx")
