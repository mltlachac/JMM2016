barplot(CMatrix,main="Crimes by Type", xlab="Type of Crime", ylab="Average Number of Crimes per Week",
names.arg=c("Battery","Sexual","Weapon","Traffic","Damages","Disturbance","Theft","Fraud","Drug"),
col=c("darkolivegreen1","darkolivegreen2","darkolivegreen3","darkolivegreen4"),
legend=c("4am-9am","10am-3pm","4pm-9pm","10pm-3am"))

barplot(CMatrix,main="Crimes by Day of the Week",
names.arg=c("Fri","Sat","Sun","Mon","Tues","Wed","Thurs"), xlab="Day of the Week",
col=c("darkolivegreen1","darkolivegreen2","darkolivegreen3","darkolivegreen4"),
ylab="Average Number of Crimes per Week", legend=c("4am-9am","10am-3pm","4pm-9pm","10pm-3am"))


b=c(1,2)
a=c(1,2)

plot(x=a,y=b)

legend('center', pch =c(19,19,19), col=c("darkmagenta","blue","red"), legend=c("All Crimes","Spring Crimes","Fall Crimes"))