require(base)
require(MASS)
require(stats)
require(forecast)
require(ggplot2)


cupid.input <- read.table(file="sample.c7.in",
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
     ylab = 'Wind Speed')
plot(cupid.input$rad.flux,type = 'l', main = 'Total Radiation Flux',
     ylab = 'Total Radiation Flux')
plot(cupid.input$air.temp, type = 'l', main = 'Air temperature',
     ylab = 'Air temperature')
plot(cupid.input$air.water.vapor, type ='l', main = 'Air water vapor pressure',
     ylab = 'Air water vapor pressure')

par(op)

# ------------------------------------------

#apply sensitivity analysis through sapply

# 10%
wind.10 <- deterministic.SA(cupid.input$windspeed,0.1,'pos')
plot(wind.10,type='l',col='red')
lines(cupid.input$windspeed,col='blue')



