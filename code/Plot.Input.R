
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