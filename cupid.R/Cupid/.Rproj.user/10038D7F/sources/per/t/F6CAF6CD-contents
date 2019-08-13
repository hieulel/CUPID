dat <- file('sample.c7.in', open = 'r')
no_lines<- length(readLines(dat))



c6list <- read.table(file = 'c6.filist', skip = 1, fill = TRUE,header = FALSE)

dat_input <- c6list$V1

dat_raw <- read.table(file="sample.c7.in",
                      fill = TRUE,
                      header = FALSE, skip = 3, comment.char = "(")
# Infix
ispecl <- dat_raw[5,1]
nn2pnt = dat_raw[7,1];ispn=dat_raw[7,2]

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

system(shQuote("/home/hieu/Documents/2019.gfortran/CUPID/src/cupid7.exe",
               type = c('sh')))

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