c=read.csv("crimedata.csv")
attach(c)
l=read.csv("locations.csv")
attach(l)
c$lat=as.numeric(c$lat)
c$long=as.numeric(c$long)
l$lat=as.numeric(l$lat)
l$long=as.numeric(l$long)
l$N=as.numeric(l$N)

r=0.0025
for(j in 2:length(l$lat)){
for(i in 2:length(c$lat)){
if(((c$long[i]-l$long[i])^2+(c$lat[i]-l$lat[i]))<=(r^2)){
l$N[j]=l$N[j]+1}}}