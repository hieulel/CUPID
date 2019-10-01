#Plotting
#Average Canopy Temperature over time with different water input
#input data
T_cano_ave <- read.csv("10mmAverageCanoTemp.csv",header = T)
timestep <- T_cano_ave$Ts
T_base <- T_cano_ave$Base

plot(T_base,main = "Average Canopy Temperature",sub = "(1 mm water input at each timestep)",xlab = "Timestep",ylab = "Temperature (C)",xlim = c(0,100),ylim = c(20,50),bty="n")
lines(T_cano_ave$T1,col="indianred3")
lines(T_cano_ave$T6,col="royalblue3")
lines(T_cano_ave$T12,col="goldenrod2")
lines(T_cano_ave$T18,col="forestgreen")
lines(T_cano_ave$T24,col="darkorange2")
lines(T_cano_ave$T3NA,col="black")
lines(T_cano_ave$T36,col="darkorchid3")
lines(T_cano_ave$T42,col="slategray4")
#par(xpd=TRUE)
legend("topleft",legend = c("Irrigate at 12AM","Irrigate at 3AM","Irrigate at 6AM","Irrigate at 9AM","Irrigate at 12PM","Irrigate at 3PM","Irrigate at 6PM","Irrigate at 9PM","No Irrigation"), col = c("indianred3","royalblue3","goldenrod2","forestgreen","darkorange2","black","darkorchid3","slategray4","black"), lty = c(1,1,1,1,1,1,1,1,NA), pch =c(NA,NA,NA,NA,NA,NA,NA,NA,1), cex = 0.6,bty="n",box.lwd = 0)

       