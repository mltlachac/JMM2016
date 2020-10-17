rm(list=ls())

#Setup
library(spatstat)
library(sp)
library(rgdal)
library(maptools)
library(ggmap)
library(mapproj)
crimes=read.csv("crimedata.csv")
attach(crimes)

#Cleaning up type
crimes$Type=sub(crimes$Type,pattern="CRIM DAMAGE",replacement="CRIMINAL DAMAGE")
crimes$Type=sub(crimes$Type,pattern="DRUG INVESTIGATION /INFORMATION",replacement="DRUG INVESTIGATION/INFORMATION")
crimes$Type=sub(crimes$Type,pattern="HEFT",replacement="THEFT")
crimes$Type=sub(crimes$Type,pattern="TTHEFT",replacement="THEFT")
crimes$Type=sub(crimes$Type,pattern="SEX OFFENDER",replacement="SEX OFFENSE")
crimes$Type=as.factor(crimes$Type)

#Creating class
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
crimes$Class = sub(crimes$Class, pattern ="SEXUAL ASSAULTS", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="SEX OFFENSE", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="WEAPONS VIOLATION", replacement="WEAPON")
crimes$Class=as.factor(crimes$Class)
crimes=na.omit(crimes)

#Plot on map of Eau Claire
map=get_map(location="Eau Claire,WI",maptype="hybrid",zoom=12)
ggmap(map,extent="panel") + geom_point(data=crimes,aes(x=crimes$long,y=crimes$lat),alpha=0.5,color="red")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")

#Plot without map
plot(x=crimes$long,y=crimes$lat,xlab="Longitude",ylab="Latitude",main="Locations of Crimes in Eau Claire",cex=0.5)

#Plot without map zoomed in
crimezoom=crimes[(crimes$lat<44.823 & crimes$lat>44.799) & (crimes$long<(-91.476) & crimes$long>(-91.520)),]
plot(x=crimezoom$long,y=crimezoom$lat,xlab="Longitude",ylab="Latitude",main="Locations of Crimes in Eau Claire",cex=0.5)

#Plot on map zoomed in
map2=get_map(location="Eau Claire,WI",maptype="hybrid",zoom=14)
ggmap(map2,extent="panel") + geom_point(data=crimezoom,aes(x=crimezoom$long,y=crimezoom$lat),alpha=0.5,color="red")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")

#Setting up spatstat
crimezoom.window=convexhull.xy(x=crimezoom$long,y=crimezoom$lat)
crimezoom.ppp=ppp(x=crimezoom$long,y=crimezoom$lat,window=crimezoom.window)

#Crime type plot
p1=plot.ppp(crimezoom.ppp)
legend(max(coords(crimezoom.ppp))[1]+1000,mean(coords(crimezoom.ppp))[2],pch=p1,legend=class(p1),cex=0.5)
