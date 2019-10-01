#read Riverland input
Input.RL <- read.csv("inputfix.csv",header = T)

#plot input
plot(Input.RL$air.temp,type = "l",bty="n",xlim = c(0,100),ylab = "Temperature (C)",xlab = "Timestep (dt = 0.5 hr)", main = "Recorded Air Temperature at the River Land \n(01/09/19-02/09/19)",cex.main=1)

plot(Input.RL$rad.flux,type = "l",bty="n",xlim = c(0,100),ylim=c(0,800),ylab = "Radiation flux (W/m2)",xlab = "Timestep (dt = 0.5 hr)", main = "Recorded Solar Radiation at the River Land \n(01/09/19-02/09/19)",cex.main=1)

plot(Input.RL$air.water.vapor,type = "l",bty="n",xlim = c(0,100),ylim=c(5,12),ylab = "Vapour pressure (mBar)",xlab = "Timestep (dt = 0.5 hr)", main = "Calculated Air Vapor Pressure at the River Land \n(01/09/19-02/09/19)",cex.main=1)

plot(Input.RL$Relative.Humidity...RH,type = "l",bty="n",xlim = c(0,100),ylim = c(20,100),ylab = "Percentage (%)",xlab = "Timestep (dt = 0.5 hr)", main = "Recorded Relative Humidity at the River Land \n(01/09/19-02/09/19)",cex.main=1)

plot(Input.RL$soil_temp,bty="n",xlim = c(0,100),ylim = c(12,19),ylab = "Temperature (C)",xlab = "Timestep (dt = 0.5 hr)", main = "Soil Temperature at the River Land \n(at 10 cm depth) \n(01/09/19-02/09/19)",cex.main=1)
lines(Input.RL$layer.6, col = "red")
legend("topleft",legend = c("Recorded Soil Temperature ","Calculated Soil Temperature"), col = c("black","red"), lty = c(NA,1),pch = c(1,NA), cex = 0.7,bty="n",box.lwd = 0)

