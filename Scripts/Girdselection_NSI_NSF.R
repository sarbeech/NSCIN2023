library(sf)
library(leaflet)
library(sf)

#read in shape file (need to have .dbf and .shx also)
grids<-st_read("./THI_NSCIN_GRID.shp")
grids<-grids%>%sf::st_transform('+proj=longlat +datum=WGS84') #need this for leaflet to work

st_crs(grids)#missing

library(ggplot2)
ggplot() + 
  geom_sf(data = grids, size = 1.5, color = "black", fill = "cyan1") + 
  coord_sf()
leaflet()%>%addTiles()%>%addPolygons(data=grids, label = ~Id,color = "grey", labelOptions = labelOptions(noHide=T,textOnly = T,direction = "top"))

##convert centriod to DDM from DD for operations and save as .csv
dd2dm <- function(dd){
  dd<-abs(dd)
  DD <- substr(dd, 1,2)
  mm <- substr(dd, 3, nchar(dd))
  MM <- as.numeric(mm)*60
  MM <- round(MM, 3)
  paste(DD, MM)
}

grids$newDDM_LAT<-dd2dm(grids$Y_coord)
grids$newDDM_LON<-dd2dm(grids$X_coord)





###OR read in .kml
grids2 <- st_read(dsn = "./NSF_grid.kml") # file is missing grid numbers - try to use .shp file from GISWarehouse

st_crs(grids2)
grids2<-grids2%>%sf::st_transform('+proj=longlat +datum=WGS84') #need this for leaflet to work

leaflet()%>%addTiles()%>%addPolygons(data=grids2, label = ~Id,color = "grey", labelOptions = labelOptions(noHide=T,textOnly = T,direction = "top"))



