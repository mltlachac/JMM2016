#Setup
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
crimes$Type=sub(crimes$Type,pattern="SEX OFFENDER",replacement="SEX OFFENSE")
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
crimes$Class = sub(crimes$Class, pattern ="SEXUAL ASSAULTS", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="SEX OFFENSE", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="SEXUAL BATTERY", replacement="SEX CRIME")
crimes$Class = sub(crimes$Class, pattern ="WEAPONS VIOLATION", replacement="WEAPON")
crimes$Class=as.factor(crimes$Class)
crimes=na.omit(crimes)
crimes.window=convexhull.xy(x=crimes$long,y=crimes$lat)
crimes.ppp=ppp(x=crimes$long,y=crimes$lat,window=crimes.window)
map=get_map(location="Eau Claire,WI",maptype="hybrid",zoom=12)
crimesfall = crimes[crimes$DateMonth2015>7 & crimes$DateMonth2015<10,]
crimesspring = crimes[crimes$DateMonth2015>3 & crimes$DateMonth2015<6,]
crimezoom=crimes[(crimes$lat<44.823 & crimes$lat>44.799) & (crimes$long<(-91.476) & crimes$long>(-91.520)),]
mapzoom=get_map(location="Eau Claire,WI",maptype="hybrid",zoom=14)
crimezoomfall = crimezoom[crimezoom$DateMonth2015>7 & crimezoom$DateMonth2015<10,]
crimezoomspring = crimezoom[crimezoom$DateMonth2015>3 & crimezoom$DateMonth2015<6,]
crimezoom.window=convexhull.xy(x=crimezoom$long,y=crimezoom$lat)
crimezoom.ppp=ppp(x=crimezoom$long,y=crimezoom$lat,window=crimezoom.window)
crimezoomfall.window=convexhull.xy(x=crimezoomfall$long,y=crimezoomfall$lat)
crimezoomfall.ppp=ppp(x=crimezoomfall$long,y=crimezoomfall$lat,window=crimezoomfall.window)
crimezoomspring.window=convexhull.xy(x=crimezoomspring$long,y=crimezoomspring$lat)
crimezoomspring.ppp=ppp(x=crimezoomspring$long,y=crimezoomspring$lat,window=crimezoomspring.window)
crimezoomdist=crimezoom[crimezoom$Class=="DISTURBANCE",]
crimezoomdist.window=convexhull.xy(x=crimezoomdist$long,y=crimezoomdist$lat)
crimezoomdist.ppp=ppp(x=crimezoomdist$long,y=crimezoomdist$lat,window=crimezoomdist.window)
crimezoomndist=crimezoom[crimezoom$Class!="DISTURBANCE",]
crimezoomndist.window=convexhull.xy(x=crimezoomndist$long,y=crimezoomndist$lat)
crimezoomndist.ppp=ppp(x=crimezoomndist$long,y=crimezoomndist$lat,window=crimezoomndist.window)
crimezooma=crimezoom[crimezoom$Thour>3 & crimezoom$Thour<10,]
crimezooma.window=convexhull.xy(x=crimezooma$long,y=crimezooma$lat)
crimezooma.ppp=ppp(x=crimezooma$long,y=crimezooma$lat,window=crimezooma.window)
crimezoomb=crimezoom[crimezoom$Thour>9 & crimezoom$Thour<16,]
crimezoomb.window=convexhull.xy(x=crimezoomb$long,y=crimezoomb$lat)
crimezoomb.ppp=ppp(x=crimezoomb$long,y=crimezoomb$lat,window=crimezoomb.window)
crimezoomc=crimezoom[crimezoom$Thour>15 & crimezoom$Thour<22,]
crimezoomc.window=convexhull.xy(x=crimezoomc$long,y=crimezoomc$lat)
crimezoomc.ppp=ppp(x=crimezoomc$long,y=crimezoomc$lat,window=crimezoomc.window)
crimezoomd=rbind(crimezoom[crimezoom$Thour>21,],crimezoom[crimezoom$Thour<4,])
crimezoomd.window=convexhull.xy(x=crimezoomd$long,y=crimezoomd$lat)
crimezoomd.ppp=ppp(x=crimezoomd$long,y=crimezoomd$lat,window=crimezoomd.window)

#Full map
#Full map plot
ggmap(map,extent="panel") + geom_point(data=crimes,aes(x=crimes$long,y=crimes$lat),alpha=0.5,color="darkmagenta")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")
#Full map seasons plot
ggmap(map,extent="panel") + geom_point(data=crimesfall,aes(x=long,y=lat),alpha=0.5,color="red") + geom_point(data=crimesspring,aes(x=long,y=lat),alpha=0.5,color="blue")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")
#Full density
plot(density(crimes.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))

#Downtown
#Downtown map plot
ggmap(mapzoom,extent="panel") + geom_point(data=crimezoom,aes(x=crimezoom$long,y=crimezoom$lat),alpha=0.5,color="darkmagenta")+ggtitle("Locations of Crimes in Downtown Eau Claire")+labs(x="Longitude",y="Latitude")
#Downtown map seasons plot
ggmap(mapzoom,extent="panel") + geom_point(data=crimezoomfall,aes(x=long,y=lat),alpha=0.5,color="red") + geom_point(data=crimezoomspring,aes(x=long,y=lat),alpha=0.5,color="blue")+ggtitle("Locations of Crimes in Downtown Eau Claire")+labs(x="Longitude",y="Latitude")
#Downtown density
plot(density(crimezoom.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown fall density
plot(density(crimezoomfall.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown spring density
plot(density(crimezoomspring.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown disturbance density
plot(density(crimezoomdist.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown non-disturbance density
plot(density(crimezoomndist.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown 4am-9am density
plot(density(crimezooma.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown 10am-3pm density
plot(density(crimezoomb.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown 4pm-9pm density
plot(density(crimezoomc.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))
#Downtown 10pm-3am density
plot(density(crimezoomd.ppp,adjust=1),col=colorRampPalette(c("blue","green","yellow")))

#Scatter
#Full map plot
ggmap(map,extent="panel") + geom_point(data=crimes,aes(x=crimes$long,y=crimes$lat),alpha=0.5,color="darkmagenta")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")
#Full map seasons plot
ggmap(map,extent="panel") + geom_point(data=crimesfall,aes(x=long,y=lat),alpha=0.5,color="red") + geom_point(data=crimesspring,aes(x=long,y=lat),alpha=0.5,color="blue")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")
#Downtown map plot
ggmap(mapzoom,extent="panel") + geom_point(data=crimezoom,aes(x=crimezoom$long,y=crimezoom$lat),alpha=0.5,color="darkmagenta")+ggtitle("Locations of Crimes in Downtown Eau Claire")+labs(x="Longitude",y="Latitude")
#Downtown map seasons plot
ggmap(mapzoom,extent="panel") + geom_point(data=crimezoomfall,aes(x=long,y=lat),alpha=0.5,color="red") + geom_point(data=crimezoomspring,aes(x=long,y=lat),alpha=0.5,color="blue")+ggtitle("Locations of Crimes in Downtown Eau Claire")+labs(x="Longitude",y="Latitude")

#Density
#Full density
plot(density(crimes.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown density
plot(density(crimezoom.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown fall density
plot(density(crimezoomfall.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown spring density
plot(density(crimezoomspring.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown disturbance density
plot(density(crimezoomdist.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown non-disturbance density
plot(density(crimezoomndist.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown 4am-9am density
plot(density(crimezooma.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown 10am-3pm density
plot(density(crimezoomb.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown 4pm-9pm density
plot(density(crimezoomc.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown 10pm-3am density
plot(density(crimezoomd.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))

#PDF Density
pdf("DowntownDensity.pdf")
plot(density(crimezoom.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("DowntownFallDensity.pdf")
plot(density(crimezoomfall.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("DowntownSpringDensity.pdf")
plot(density(crimezoomdist.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("DowntownDisturbanceDensity.pdf")
plot(density(crimezoomdist.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("DowntownNonDisturbanceDensity.pdf")
plot(density(crimezoomndist.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("Downtown49AMDensity.pdf")
plot(density(crimezooma.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("Downtown103AMDensity.pdf")
plot(density(crimezoomb.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("Downtown49PMDensity.pdf")
plot(density(crimezoomc.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()
pdf("Downtown103PMDensity.pdf")
plot(density(crimezoomd.ppp,adjust=0.25),col=colorRampPalette(c("blue","green","yellow","orange","red")))
dev.off()

#Seasons
#Full map seasons plot
ggmap(map,extent="panel") + geom_point(data=crimesfall,aes(x=long,y=lat),alpha=0.5,color="red") + geom_point(data=crimesspring,aes(x=long,y=lat),alpha=0.5,color="blue")+ggtitle("Locations of Crimes in Eau Claire")+labs(x="Longitude",y="Latitude")
#Downtown map seasons plot
ggmap(mapzoom,extent="panel") + geom_point(data=crimezoomfall,aes(x=long,y=lat),alpha=0.5,color="red") + geom_point(data=crimezoomspring,aes(x=long,y=lat),alpha=0.5,color="blue")+ggtitle("Locations of Crimes in Downtown Eau Claire")+labs(x="Longitude",y="Latitude")
#Downtown fall density
plot(density(crimezoomfall.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown spring density
plot(density(crimezoomspring.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))

#Time of day
#Downtown 4am-9am density
plot(density(crimezooma.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown 10am-3pm density
plot(density(crimezoomb.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown 4pm-9pm density
plot(density(crimezoomc.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown 10pm-3am density
plot(density(crimezoomd.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))

#Crime severity
#Downtown disturbance density
plot(density(crimezoomdist.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))
#Downtown non-disturbance density
plot(density(crimezoomndist.ppp,adjust=0.1),col=colorRampPalette(c("blue","green","yellow","orange","red")))