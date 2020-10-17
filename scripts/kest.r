crimes = read.csv("allcrimes.csv")


crimeloc = data.frame(crimes$long, crimes$lat)
crimeloc = na.omit(crimeloc)
attach(crimeloc)

library(spatstat)

latt=as.numeric(crimes.lat)
longg=as.numeric(crimes.long)
cbind(latt, longg)

loop=0
c=.0000005
while(loop<10){
lattorder = order(latt,longg)
latt=latt[lattorder]
longg=longg[lattorder]
for (i in 2:length(latt)){
if (latt[i]==latt[i-1] & longg[i]==longg[i-1]){
latt[i]= latt[i]+c
longg[i]=longg[i]+c
}}
loop = loop+1
}

data = ppp(latt, longg, c(-91.6432,-91.2771), c(44.76255,44.87128))
plot(data,"Locations of Crimes in Eau Claire",pch=20)

summary(data)
plot(Kest(data))

