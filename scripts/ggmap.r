rm(list=ls())

library(spatstat)
library(sp)
library(rgdal)
library(maptools)
library(ggmap)
library(mapproj)

crimes=read.csv("crimedata.csv")
attach(crimes)

crimes$Type=sub(crimes$Type,pattern="CRIM DAMAGE",replacement="CRIMINAL DAMAGE")
crimes$Type=sub(crimes$Type,pattern="DRUG INVESTIGATION /INFORMATION",replacement="DRUG INVESTIGATION/INFORMATION")
crimes$Type=sub(crimes$Type,pattern="HEFT",replacement="THEFT")
crimes$Type=sub(crimes$Type,pattern="TTHEFT",replacement="THEFT")
crimes$Type=as.factor(crimes$Type)

crimes$Class=as.factor(crimes$Type)
crimes$Class = sub(crimes$Class, pattern ="VEHICLE", replacement="THEFT")
crimes$Class = sub(crimes$Class, pattern ="ROBBERY", replacement="THEFT")
crimes$Class = sub(crimes$Class, pattern ="BURGLARY", replacement="THEFT")
crimes$Class = sub(crimes$Class, pattern ="DRUG INVESTIGATION/INFORMATION", replacement="DRUG")
crimes$Class = sub(crimes$Class, pattern ="ORD COMPLAINT", replacement="DISTURBANCE")
crimes$Class = sub(crimes$Class, pattern ="ANIMAL", replacement="DISTURBANCE")
crimes$Class = sub(crimes$Class, pattern ="DISORDERLY CONDUCT", replacement="DISTURBANCE")
crimes$Class = sub(crimes$Class, pattern ="DISTURBANCE", replacement="DISTURBANCE")
crimes$Class = sub(crimes$Class, pattern ="FORGED", replacement="FRAUD")
crimes$Class = sub(crimes$Class, pattern ="COUNTERFEIT", replacement="FRAUD")
crimes$Class = sub(crimes$Class, pattern ="OMVWI", replacement="TRAFFIC")
crimes$Class = sub(crimes$Class, pattern ="GRAFFITI", replacement="DAMAGES")
crimes$Class = sub(crimes$Class, pattern ="CRIMINAL DAMAGE", replacement="DAMAGES")
crimes$Class = sub(crimes$Class, pattern ="BATTERY", replacement="BATTERY")
crimes$Class = sub(crimes$Class, pattern ="ASSAULTS", replacement="BATTERY")
crimes$Class = sub(crimes$Class, pattern ="SEX OFFENDER", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="SEXUAL ASSAULTS", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="SEX OFFENSE", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="WEAPONS VIOLATION", replacement="WEAPON")
crimes$Class=as.factor(crimes$Class)
crimes=na.omit(crimes)

map=get_map(location="Eau Claire,WI",maptype="hybrid",zoom=12)
ggmap(map,extent="panel") + geom_point(data=crimes,aes(x=crimes$long,y=crimes$lat),alpha=0.5,color="red")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")

plot(x=crimes$long,y=crimes$lat,xlab="Longitude",ylab="Latitude",main="Locations of Crimes in Eau Claire",cex=0.5)

crimes=crimes[(crimes$lat<44.823 & crimes$lat>44.799) & (crimes$long<(-91.476) & crimes$long>(-91.520)),]
plot(x=crimes$long,y=crimes$lat,xlab="Longitude",ylab="Latitude",main="Locations of Crimes in Eau Claire",cex=0.5)
map2=get_map(location="Eau Claire,WI",maptype="hybrid",zoom=14)
ggmap(map2,extent="panel") + geom_point(data=crimes,aes(x=crimes$long,y=crimes$lat),alpha=0.5,color="red")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")

