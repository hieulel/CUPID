#PLOT SYNTEMP
synth.temp=read.csv("Syntemp.csv",header = T)
library(Hmisc)
minor.tick(nx=4)
plot(synth.temp$ï..ts,synth.temp$Original.Temperature,type='l',col='blue',
     main="Original Temperature and Scaled Temperature",ylab="Temperature (C)",
     xlab="Time (hours)",bty="n",xlim=c(0,25),ylim=c(10,45),lwd=(2.25))
lines(synth.temp$ï..ts,synth.temp$Scaled.Temperature,col="indianred",lwd=(2.25))
legend("topleft",legend=c("Original Temperature","Scaled Temperature"),bty="n",
       lty=(1:1),col=c("blue","indianred"),lwd=(2.25))
#################################################################################

#PLOT WIND Prelim
wind.var<-read.csv("Wind.csv",header = T)
library(Hmisc)
minor.tick(nx=4)
plot(wind.var$ï..ts,wind.var$X1.m.s.Wind ,type='l',col='bisque4',
     main="Average Canopy Temperature Under the Effect of Windspeeds",ylab="Temperature (C)",
     xlab="Time (hours)",bty="n",xlim=c(0,50),ylim=c(15,50),lwd=(2))
lines(wind.var$ï..ts,wind.var$X2.m.s.Wind,col="darkblue",lwd=(2))
lines(wind.var$ï..ts,wind.var$X3.m.s.Wind,col="olivedrab4",lwd=(2))
lines(wind.var$ï..ts,wind.var$X4.m.s.Wind,col="orange1",lwd=(2))
lines(wind.var$ï..ts,wind.var$X5.m.s.Wind,col="royalblue",lwd=(2))
lines(wind.var$ï..ts,wind.var$X6.m.s.Wind,col="aquamarine",lwd=(2))
lines(wind.var$ï..ts,wind.var$X7.m.s.Wind,col="gold",lwd=(2))
lines(wind.var$ï..ts,wind.var$X8.m.s.Wind,col="salmon",lwd=(2))
lines(wind.var$ï..ts,wind.var$X9.m.s.Wind,col="darkorchid",lwd=(2))
lines(wind.var$ï..ts,wind.var$X10.m.s.Wind,col="darkseagreen",lwd=(2))
lines(wind.var$ï..ts,wind.var$X15.m.s.Wind,col="orangered",lwd=(2))
lines(wind.var$ï..ts,wind.var$X20.m.s.Wind,col="green2",lwd=(2))
lines(wind.var$ï..ts,wind.var$X30.m.s.Wind,col="hotpink",lwd=(2))
legend("bottom",legend=c("1 m/s","2 m/s","3 m/s","4 m/s","5 m/s","6 m/s","7 m/s","8 m/s","9 m/s",
                         "10 m/s","15 m/s","20 m/s","30 m/s"),lty=(1:1:1:1:1:1:1:1:1:1:1:1:1),
       col=c("bisque4","darkblue","olivedrab4","orange1","royalblue","aquamarine","gold","salmon",
             "darkorchid","darkseagreen","orangered","green2","hotpink"),lwd=(2),ncol = 5,bty="n")
###################################################################################

#IrrigationTempReduct
irri<-read.csv("Irri.csv",header = T)
plot(irri$ï..ts,irri$irri0.25 ,type='l',col='indianred',
     main=paste("Reduction in Average Canopy Temperature","\nwith Irrigation at Various Times","\n(Irrigation = 15mm)"),ylab="Temperature (C)",
     xlab="Time (hours)",bty="n",xlim=c(0,50),ylim=c(0,5),lwd=(2))
lines(irri$ï..ts,irri$irri10.25,col="blue",lwd=(2))
lines(irri$ï..ts,irri$irri14.25,col="olivedrab4",lwd=(2))
lines(irri$ï..ts,irri$irri20.25,col="orange1",lwd=(2))
minor.tick(nx=4)
legend("topleft",legend=c("Irrigation at 12.15AM","Irrigation at 10.15AM","Irrigation at 2.15PM","Irrigation at 8.15PM"),
       lty = (1:1:1:1),col=c("indianred","blue","olivedrab4","orange1"),lwd=(2),bty="n",ncol=(2))
##################################################################################

#MaxTemp
max.temp<-read.csv("maxchange.csv",header = T)
plot(max.temp$ï..ts1,max.temp$X1mm,type='l',col='red',
     main=paste("Maximum Reduction in Average Canopy Temperature","\nwith Various Irrigation Options"),ylab="Temperature (C)",
     xlab="Time (hours)",bty="n",xlim=c(0,35),ylim=c(1,5),lwd=(2))
lines(max.temp$ts2,max.temp$X2.5mm,col="blue",lwd=(2))
lines(max.temp$ts3,max.temp$X5mm,col="olivedrab4",lwd=(2))
lines(max.temp$ts4,max.temp$X7.5mm,col="orange1",lwd=(2))
lines(max.temp$ts5,max.temp$X10mm,col="aquamarine",lwd=(2))
lines(max.temp$ts6,max.temp$X12.5mm,col="hotpink",lwd=(2))
lines(max.temp$ts7,max.temp$X15mm,col="darkorchid",lwd=(2))
minor.tick(nx=3)
legend("bottom",legend=c("Irrigation = 1mm","Irrigation = 2.5mm","Irrigation = 5mm","Irrigation = 7.5mm","Irrigation = 10mm",
                         "Irrigation = 12.5mm", "Irrigation = 15mm"),lty = (1:1:1:1:1:1:1),
       col=c("red","blue","olivedrab4","orange1","aquamarine","hotpink","darkorchid"),
       lwd=(2),bty="n",ncol=(3))
##################################################################################

#PLOT WIND
wind.war<-read.csv("WWind.csv",header = T)
library(Hmisc)

plot(wind.war$ï..ts,wind.war$Wind.Speed...0.m.s,type='l',col='bisque4',
     main="Average Canopy Temperature Under the Effect of Windspeeds",ylab="Temperature (C)",
     xlab="Time (hours)",bty="n",xlim=c(0,50),ylim=c(10,60),lwd=(2))
lines(wind.war$ï..ts,wind.war$Wind.Speed...10.m.s,col="hotpink",lwd=(2))
lines(wind.war$ï..ts,wind.war$Wind.Speed...30.m.s,col="green2",lwd=(2))
lines(wind.war$ï..ts,wind.war$Wind.Speed...60.m.s,col="orange1",lwd=(2))
lines(wind.war$ï..ts,wind.war$Wind.Speed...80.m.s,col="royalblue",lwd=(2))
minor.tick(nx=4)
legend("bottom",legend=c("0 m/s","10 m/s","30 m/s","60 m/s","80 m/s"),lty=(1:1:1:1:1),
       col=c("bisque4","hotpink","green2","orange1","royalblue"),lwd=(2),ncol = 5,bty="n")
###################################################################################

#Effective Cooling
eff.coo<-read.csv("Effective cooling.csv",header = T)
plot(eff.coo$ï..ts,eff.coo$eff,type='b',col="royalblue",
     main=paste("Effective Cooling Duration with Irrigation Timing","\n(Irrigation = 1mm)"),
     lwd=(2),pch=19,bty="n",ylab="Time Duration (hours)",xlab="Time (hours)",xlim=c(0,25),ylim=c(2,12))
minor.tick(nx=4)
###################################################################################

#MaxTemperature TOTAL change
x<-c(1,1.25,2.5,5,7.5,10,12.5,15,50,100)
y<-c(10.91,11.88,15.36,21.01,23.95,22.98,26.13,28.08,30,30.93)

plot(x,y,type="b",col="royalblue",
     main=paste("Temperature Reduction Convergence","\nWith Increasing Water Input"),
     lwd=(2),pch=19,bty="n",ylab="Total Temperature Reduction (C)",xlab="Water Input (mm)",xlim=c(0,120),ylim=c(5,35))
minor.tick(nx=4)
###################################################################################

#Schedulling
sedule<-read.csv("Scheduleing.csv",header = T)

plot(sedule$ï..ts,sedule$Schedule.1,type="l",col="black",lwd=(2),bty="n",
     main=paste("Reduction in Average Canopy Temperature","\nfrom 9.15AM to 5.45PM","\nWith Various Irrigation Schedules"),
     xlab="Time (hours)",ylab="Temperature (C)",ylim=c(0,3),xlim=c(9,18))
lines(sedule$ï..ts,sedule$Schedule.2,col="blue",lwd=(2))
lines(sedule$ï..ts,sedule$Schedule.3,col="orange1",lwd=(2))
lines(sedule$ï..ts,sedule$Schedule.4,col="royalblue",lwd=(2))
lines(sedule$ï..ts,sedule$Schedule.5,col="aquamarine",lwd=(2))
lines(sedule$ï..ts,sedule$Schedule.6,col="gold4",lwd=(2))
lines(sedule$ï..ts,sedule$Schedule.7,col="salmon",lwd=(2))
lines(sedule$ï..ts,sedule$Schedule.8,col="darkorchid",lwd=(2))
lines(sedule$ï..ts,sedule$Schedule.9,col="darkseagreen",lwd=(2))
minor.tick(nx=4)
legend(12.5,1.1,legend=c("Schedule 1","Schedule 2","Schedule 3","Schedule 4","Schedule 5","Schedule 6","Schedule 7","Schedule 8","Schedule 9"),
       lty=(1:1:1:1:1:1:1:1:1),col=c("black","olivedrab4","orange1","royalblue","aquamarine","gold4","salmon","darkorchid","darkseagreen"),
       lwd=(2),bty="n",ncol=(2))
