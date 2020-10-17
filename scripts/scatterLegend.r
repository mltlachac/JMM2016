rm(list=ls())

crimes = read.csv("allcrimes.csv")

crimeloc = data.frame(crimes$long, crimes$lat,crimes$DateMonth2015, crimes$Thour)
crimeloc = na.omit(crimeloc)
attach(crimeloc)

library(spatstat)

latt=as.numeric(crimes.lat)
longg=as.numeric(crimes.long)
month=as.numeric(crimes.DateMonth2015)
time=as.numeric(crimes.Thour)
cbind(latt,longg,month,time)

loop=0
c=.00001
while(loop<10){
lattorder = order(latt,longg,month)
latt=latt[lattorder]
longg=longg[lattorder]
month=month[lattorder]
for (i in 2:length(latt)){
if (latt[i]==latt[i-1] & longg[i]==longg[i-1]){
latt[i]= latt[i]+c
longg[i]=longg[i]+c
}}
loop = loop+1
}

plot(x=latt, y=longg, pch=20, main="Locations of Crimes in Eau Claire", ylab="Degrees Longitude", xlab="Degrees Latitude",col="blue")

mycolor=rep("blue",length(latt))
changec=which(time>6 & time<20)
mycolor[changec]="red"

myshape=rep(20,length(latt))
changes=which(month==5)
myshape[changes]=3

plot(x=latt, y=longg, pch=myshape, main="Locations of Crimes in Eau Claire", ylab="Degrees Longitude", xlab="Degrees Latitude",col=mycolor)

legend('topleft', pch=c(15,15,3,20),col=c("red","blue","black","black"),c("Day","Night","Spring","Fall"),bty="o",box.col="black")
