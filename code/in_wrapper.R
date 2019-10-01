require(base)
require(MASS)
require(stats)
require(forecast)
require(ggplot2)


cupid.input <- read.table(file="input.c7.in",
                                      fill = TRUE,
                                      header = FALSE,
                                      skip = 59)

names(cupid.input) <- c('year','doy','local',
                 'windspeed','NA','rad.flux',
                 'NA','NA','NA',
                 'air.temp','air.water.vapor',
                 'precip',
                 'precip.type','day','idxx')
# ---------------------------------------------

# Get a first look for the input (meterological data)
op = par(mfrow=c(2,2))

plot(cupid.input$windspeed,type = 'l', main = 'Wind Speed',
     ylab = 'Wind Speed (m/s)',xlab = 'Timestep')
plot(cupid.input$rad.flux,type = 'l', main = 'Solar Radiation',
     ylab = 'Radiation flux (W/m2)',xlab = 'Timestep (dt = 0.5 hr)',cex.main=1,bty="n")
plot(cupid.input$air.temp, type = 'l', main = 'Air Temperature',
     ylab = 'Temperature (C)',xlab = 'Timestep (dt = 0.5 hr)',cex.main=1,bty="n",ylim = c(30,41))
plot(cupid.input$air.water.vapor, type ='l', main = 'Air Vapor Pressure',
     ylab = 'Vapor pressure (mBar)',xlab = 'Timestep (dt = 0.5 hr)',cex.main=1,bty="n")

par(op)

# ------------------------------------------

#apply sensitivity analysis through sapply
write.csv(cupid.input,"inputfix.csv")
Input<-read.csv("inputfix.csv")
write.table(Input,"inputfix.txt",row.names = F,col.names = F)
# 10%
wind.10 <- deterministic.SA(cupid.input$windspeed,0.1,'pos')
plot(wind.10,type='l',col='red')
lines(cupid.input$windspeed,col='blue')

In.temp <- cupid.input$air.temp
write.csv(In.temp,"in.csv")
