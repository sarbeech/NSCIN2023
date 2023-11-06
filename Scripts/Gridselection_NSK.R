library(dplyr)
library(leaflet)
library(sf)
library(readxl)
grids <- st_read(dsn = "./Data/NSK_grid.kml")%>%mutate(Name=c(1:150))
centriods <- st_read(dsn = "./Data/NSK_centroid.kml")%>%mutate(Name=c(1:150))

my_crs <- st_crs(grids);my_crs

#grids<-grids%>%sf::st_transform('+proj=longlat +datum=WGS84') #need this for leaflet to work

leaflet()%>%addTiles()%>%addPolygons(data=grids, color = "grey",label=~Name, labelOptions = labelOptions(noHide=T,direction = "top"))

leaflet()%>%addTiles()%>%addCircles(data=centriods, color = "grey", label= ~ Name,labelOptions = labelOptions(noHide=T,direction = "top"))

library(ggplot2)
library(ggspatial)
ggplot() + 
  geom_sf(data = grids, size = 1.5, color = "black", fill = "cyan2") + 
  coord_sf()
ggplot()+geom_spatial(data = plot,color = "black")+coord_map()

#SitesCurrentYr<-dplyr::sample_n(grids,36)
gridscurrent<-read.csv("./Data/DDMcentriods_currentyr.csv")
SitescurrentYr<-grids%>%filter(Name%in%gridscurrent$Grid)

leaflet()%>%addTiles()%>%addPolygons(data=grids, color = "grey",label=~Name,labelOptions = labelOptions(noHide=T,direction = "left",textOnly = T,textsize = "15px"))%>%addPolygons(data=SitescurrentYr, color = "red",label=~Name, labelOptions = labelOptions(noHide=T,direction = "left",textOnly = T,textsize = "15px"))

library(lakeontario)
library(rgdal)
library(raster)

plot<-st_as_sf(lakeontario::LOschaner)
plot<-crop(plot,extent(-77,-78,43,44))
plot@data$id<-rownames(plot@data)
plotdata<-fortify(plot,region="id")
plotdf<-merge(plotdata,plot@data,by="id")

ggplot()+geom_polygon(data=plotdf,aes(x=long,y=lat,group=group),fill="white",color="black")+ 
  geom_sf(data = grids, size = 1.5, color = "black", fill = "cyan2")



#get DDM for ops
centriodscoords<-data.frame(st_coordinates(centriods))%>%mutate(Grid=c(1:150))
dd2dm <- function(dd){
  dd<-abs(dd)
  DD <- substr(dd, 1,2)
  mm <- substr(dd, 3, nchar(dd))
  MM <- as.numeric(mm)*60
  MM <- round(MM, 3)
  paste(DD, MM)
}

centriodscoords$newDDM_LAT<-dd2dm(centriodscoords$Y)
centriodscoords$newDDM_LON<-dd2dm(centriodscoords$X)

write.csv(centriodscoords, "Data/DDMandDDcentriods_allgrids.csv",  row.names = F)

SitescurrentYr_centroids<-centriodscoords%>%filter(Grid%in%SitesCurrentYr$Name)
write.csv(SitescurrentYr_centroids, "Data/DDMcentriods_currentyr.csv",  row.names = F)
