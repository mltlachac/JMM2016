crimes = read.csv("crimedata.csv")
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

MonM=0
MonD=0
MonA=0
MonN=0
TuesM=0
TuesD=0
TuesA=0
TuesN=0
WedM=0
WedD=0
WedA=0
WedN=0
ThursM=0
ThursD=0
ThursA=0
ThursN=0
FriM=0
FriD=0
FriA=0
FriN=0
SatM=0
SatD=0
SatA=0
SatN=0
SunM=0
SunD=0
SunA=0
SunN=0

hour=crimes$Thour
weekday=crimes$DateWeek

for(i in 1:length(Type)){
if(weekday[i]=="M" & hour[i]>=4 & hour[i]<=9){
MonM=MonM+1
}else if(weekday[i]=="M" & hour[i]>=10 & hour[i]<=15){
MonD=MonD+1
}else if(weekday[i]=="M" & hour[i]>=16 & hour[i]<=21){
MonA=MonA+1
}else if(weekday[i]=="M" & (hour[i]>=22 | hour[i]<=3)){
MonN=MonN+1
}else if(weekday[i]=="T" & hour[i]>=4 & hour[i]<=9){
TuesM=TuesM+1
}else if(weekday[i]=="T" & hour[i]>=10 & hour[i]<=15){
TuesD=TuesD+1
}else if(weekday[i]=="T" & hour[i]>=16 & hour[i]<=21){
TuesA=TuesA+1
}else if(weekday[i]=="T" & (hour[i]>=22 | hour[i]<=3)){
TuesN=TuesN+1
}else if(weekday[i]=="W" & hour[i]>=4 & hour[i]<=9){
WedM=WedM+1
}else if(weekday[i]=="W" & hour[i]>=10 & hour[i]<=15){
WedD=WedD+1
}else if(weekday[i]=="W" & hour[i]>=16 & hour[i]<=21){
WedA=WedA+1
}else if(weekday[i]=="W" & (hour[i]>=22 | hour[i]<=3)){
WedN=WedN+1
}else if(weekday[i]=="R" & hour[i]>=4 & hour[i]<=9){
ThursM=ThursM+1
}else if(weekday[i]=="R" & hour[i]>=10 & hour[i]<=15){
ThursD=ThursD+1
}else if(weekday[i]=="R" & hour[i]>=16 & hour[i]<=21){
ThursA=ThursA+1
}else if(weekday[i]=="R" & (hour[i]>=22 | hour[i]<=3)){
ThursN=ThursN+1
}else if(weekday[i]=="F" & hour[i]>=4 & hour[i]<=9){
FriM=FriM+1
}else if(weekday[i]=="F" & hour[i]>=10 & hour[i]<=15){
FriD=FriD+1
}else if(weekday[i]=="F" & hour[i]>=16 & hour[i]<=21){
FriA=FriA+1
}else if(weekday[i]=="F" & (hour[i]>=22 | hour[i]<=3)){
FriN=FriN+1
}else if(weekday[i]=="S" & hour[i]>=4 & hour[i]<=9){
SatM=SatM+1
}else if(weekday[i]=="S" & hour[i]>=10 & hour[i]<=15){
SatD=SatD+1
}else if(weekday[i]=="S" & hour[i]>=16 & hour[i]<=21){
SatA=SatA+1
}else if(weekday[i]=="S" & (hour[i]>=22 | hour[i]<=3)){
SatN=SatN+1
}else if(weekday[i]=="N" & hour[i]>=4 & hour[i]<=9){
SunM=SunM+1
}else if(weekday[i]=="N" & hour[i]>=10 & hour[i]<=15){
SunD=SunD+1
}else if(weekday[i]=="N" & hour[i]>=16 & hour[i]<=21){
SunA=SunA+1
}else if(weekday[i]=="N" & (hour[i]>=22 | hour[i]<=3)){
SunN=SunN+1
}}

MonM=MonM/8
MonD=MonD/8
MonA=MonA/8
MonN=MonN/8
TuesM=TuesM/8
TuesD=TuesD/8
TuesA=TuesA/8
TuesN=TuesN/8
WedM=WedM/8
WedD=WedD/8
WedA=WedA/8
WedN=WedN/8
ThursM=ThursM/8
ThursD=ThursD/8
ThursA=ThursA/8
ThursN=ThursN/8
FriM=FriM/8
FriD=FriD/8
FriA=FriA/8
FriN=FriN/8
SatM=SatM/8
SatD=SatD/8
SatA=SatA/8
SatN=SatN/8
SunM=SunM/8
SunD=SunD/8
SunA=SunA/8
SunN=SunN/8

CMatrix = matrix(c(
FriM,SatM,SunM,MonM,TuesM,WedM,ThursM,
FriD,SatD,SunD,MonD,TuesD,WedD,ThursD,
FriA,SatA,SunA,MonA,TuesA,WedA,ThursA,
FriN,SatN,SunN,MonN,TuesN,WedN,ThursN),
nrow=4, ncol=7,byrow=TRUE)

barplot(CMatrix,main="Weekly Crime Frequency",
names.arg=c("Fri","Sat","Sun","Mon","Tues","Wed","Thurs"), xlab="Days of the Week",
col=c("darkolivegreen1","darkolivegreen2","darkolivegreen3","darkolivegreen4"),
ylab="Frequency", legend=c("4am-9am","10am-3pm","4pm-9pm","10pm-3am"))

CM=CMatrix*8
chisq.test(CM)

WM = matrix(c(
FriM + SatM + SunM, MonM + TuesM + WedM + ThursM,
FriD + SatD + SunD, MonD + TuesD + WedD + ThursD,
FriA + SatA + SunA, MonA + TuesA + WedA + ThursA,
FriN + SatN + SunN, MonN + TuesN + WedN + ThursN),
nrow=4, ncol=2,byrow=TRUE)
WM8 = WM*8
chisq.test(WM8)