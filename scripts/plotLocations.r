library(spatstat)
library(sp)
library(rgdal)
library(maptools)
library(ggmap)
library(mapproj)
locations=read.csv("locations.csv")
attach(locations)
locationzoom=locations[(locations$Lat<(44.823) & locations$Lat>(44.799)) & (locations$Long<(-91.476) & locations$Long>(-91.520)),]
mapzoom=get_map(location="Eau Claire,WI",maptype="hybrid",zoom=14)
worship=locationzoom[locationzoom$Place=="Worship ",]
bar=locationzoom[locationzoom$Place=="Bar",]
restbar=locationzoom[locationzoom$Place=="Bar/Restaurant",]
alcohol=locationzoom[locationzoom$Place=="Alcohol Store",]
eschool=locationzoom[locationzoom$Place=="E School",]
hschool=locationzoom[locationzoom$Place=="H School",]
hospital=locationzoom[locationzoom$Place=="Hospital",]
police=locationzoom[locationzoom$Place=="Police",]

ggmap(mapzoom,extent="panel")+
geom_point(data=worship,aes(x=Long,y=Lat),alpha=1,pch=1,color="gold", cex=3)+
geom_point(data=bar,aes(x=Long,y=Lat),alpha=1,pch=0,color="deeppink", cex=3)+
geom_point(data=restbar,aes(x=Long,y=Lat),alpha=1,pch=2,color="deeppink", cex=3)+
geom_point(data=alcohol,aes(x=Long,y=Lat),alpha=1,pch=1,color="deeppink", cex=3)+
geom_point(data=eschool,aes(x=Long,y=Lat),alpha=1,pch=15,color="darkturquoise", cex=3)+
geom_point(data=hschool,aes(x=Long,y=Lat),alpha=1,pch=17,color="darkturquoise", cex=3)+
geom_point(data=hospital,aes(x=Long,y=Lat),alpha=1,pch=16,color="green", cex=3)+
geom_point(data=police,aes(x=Long,y=Lat),alpha=1,pch=15,color="green", cex=3)+
ggtitle("Locations of Interest in Downtown Eau Claire")+labs(x="Longitude",y="Latitude")
