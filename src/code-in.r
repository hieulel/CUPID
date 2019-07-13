# setwd("/home/hieu/Documents/2019.gfortran/CUPID")
setwd("/home/hieu/Documents/2019.R/cupid.R")
# The time step data that follow consist of YEAR, DAY OF YEAR, LOCAL TIME,
# WIND SPEED (m/s), RADIATION FLUX (W/m2), AIR TEMPERATURE (C), AIR WATER
# VAPOR PRESSURE (mbar), PRECIPITATION (mm/time step) and IRRCHK, 
cupid.inputfile.weather <- read.table(file="sample.c7.in.txt",
                              # sep ,
                              fill = FALSE,
                              header = FALSE,
                              skip = 59)
cupid.output.file <- read.table(file = "sample.c7.out.org",
                                skip = 3,
                                fill = TRUE,
                                blank.lines.skip = TRUE,
                                dec =".")

year <- cupid.inputfile.weather$V1
doy <- cupid.inputfile.weather$V2
local.time <- cupid.inputfile.weather$V3
wind.speed <- cupid.inputfile.weather$V4
rad.flux <- cupid.inputfile.weather$V6
air.temp <- cupid.inputfile.weather$V10
air.water.vapor <-cupid.inputfile.weather$V11
precip <- cupid.inputfile.weather$V12
png('DirurnalVariation.Sampleinput.weather.png')
pdf('DirurnalVariation.Sampleinput.weather.pdf')
op = par(mfrow = c(2,2))
plot(x = local.time[1:48],
     y = wind.speed[1:48],
     type = 'l',
     xlab = 'Local Time',
     ylab = 'Wind Speed',
     main = 'Wind speed',
     col = 'red')
abline(v=12,col = 'blue')

plot(x = local.time[1:48],
     y = rad.flux[1:48],
     type = 'l',
     xlab = 'Local Time',
     ylab = 'Radiation Flux',
     main = 'Radiation Flux',
     col = 'red')
abline(v=12,col = 'blue')

plot(x = local.time[1:48],
     y = air.temp[1:48],
     type = 'l',
     xlab = 'Local Time',
     ylab = 'Air temperature',
     main = 'Air temperature',
     col = 'red')
abline(v=12,col = 'blue')

plot(x = local.time[1:48],
     y = air.water.vapor[1:48],
     type = 'l',
     xlab = 'Local Time',
     ylab = 'Air Water Vapor Pressure',
     main = 'Air Water Vapor Pressure',
     col = 'red')
abline(v=12,col = 'blue')
par(op)

dev.off()
