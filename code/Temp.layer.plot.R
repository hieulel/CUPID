#PlotT.Layer
T_layer <- read.csv("layer7.csv",header = T)
T_input <- cupid.input$air.temp
T_can.ini <- T_layer[,1]*0

#compute Average canopy temp
for (i in 6:20){
  T_can.layer.sum <- T_can.ini + T_layer[,i]
  T_can.ini <- T_can.layer.sum
}
T_can.mean <- T_can.layer.sum/15
#############################

#compute Average Under Canopy Temp
T_BCan.mean <- (T_layer[,21]+T_layer[,22])/2

#Compute Average Temperature Above 
T_Acan.ini <- T_layer[,1]*0
for (i in 1:5){
  T_Acan.layer.sum <- T_Acan.ini + T_layer[,i]
  T_Acan.ini <- T_Acan.layer.sum
}
T_Acan.mean <- T_Acan.layer.sum/5
#############################

#Compute Average Temperature in soil
T_soil.ini <- T_layer[,1]*0
for (i in 23:43){
  T_soil.layer.sum <- T_soil.ini + T_layer[,i]
  T_soil.ini <- T_soil.layer.sum
}
T_soil.mean <- T_soil.layer.sum/21

plot(T_input[49:145],main = "Average Temperature of Various Layers vs Input Temperature",xlab = "Timestep",ylab = "Temperature (C)",xlim = c(0,100),ylim = c(25,50))
lines(T_can.mean,col="indianred3")
lines(T_BCan.mean,col="royalblue3")
lines(T_Acan.mean,col="goldenrod2")
lines(T_soil.mean,col="forestgreen")
# lines(T_cano_ave$T24,col="darkorange2")
# lines(T_cano_ave$T3NA,col="black")
# lines(T_cano_ave$T36,col="darkorchid3")
# lines(T_cano_ave$T42,col="slategray4")

legend("topleft",legend = c("Average Temperature in Canopy","Average Temperature Below Canopy","Average Temperature Above Canopy","Average Temperature in Soil","Input Temperature"),
       col = c("indianred3","royalblue3","goldenrod2","forestgreen","black"),
       lty = c(1,1,1,1,NA), pch =c(NA,NA,NA,NA,1), 
       cex = 0.6,bty="n",
       box.lwd = 0)

