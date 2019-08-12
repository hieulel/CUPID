dat <- file('sample.c7.in', open = 'r')
no_lines<- length(readLines(dat))



c6list <- read.table(file = 'c6.filist', skip = 1, fill = TRUE,header = FALSE)

dat_input <- c6list$V1

dat_raw <- read.table(file="sample.c7.in",
                      fill = TRUE,
                      header = FALSE, skip = 3, comment.char = "(")
# Infix 1 -13
ispecl <- dat_raw[5,1]
nn2pnt = dat_raw[7,1]; ispn=dat_raw[7,2]
MMDAYIN <- dat_raw[8,1]; MDAY <- dat_raw[8,2]
LAT <- dat_raw[9,1]; LONG <- dat_raw[9,2]; STD_LONG <- dat_raw[9,3]
Slope <- dat_raw[10,1]; Aspect <- dat_raw[10,2]; HRZANGLE <- dat_raw[10,3]

# Insoil 14 -21
CLODSZ <- dat_raw[11,1]; NDSOIL <- dat_raw[11,2]; REFDPT <- dat_raw[11,3]; resdue <- dat_raw[11,4]; Z0soil <- dat_raw [11,5]
Zsoil <- dat_raw[12,c(1:21)]
NLSOIL <- dat_raw[13,1]
ZLSOIL <- dat_raw[c(14:16),1]; BD <- dat_raw[c(14:16),2]; SAND <- dat_raw[c(14:16),3]; SILT <- dat_raw[c(14:16),4]; CLAY <- dat_raw[c(14:16),5]; QRTZ <- dat_raw[c(14:16),6]; PE <- dat_raw[c(14:16),7]; BX <- dat_raw[c(14:16),8]; DAK <- dat_raw[c(14:16),9]
IDOROK <- dat_raw[17,1]; IROCLY <- dat_raw[17,2]; AKROCK <- dat_raw[17,3]; CPROCK <- dat_raw[17,4]
g(DELD, DRHSFC, RHSLOP)  %=%  dat_raw[18,1:3]

# Inspnkl 22-27
TIRRIG <- dat_raw[19,1:5]
ETSUM <- dat_raw[20,1:6]
QSUM <- dat_raw[21,1:5]
ETFIX <- dat_raw[22,1:4]
QFIX <- dat_raw[23,1:4]
g(ZMAX,HTSPK,ABSPK,BSPK,SPKPRS,BYANG,TDROPI) %=% dat_raw[24,1:7]

# Inplant 28 - 57
g(ibidir,itassl) %=% dat_raw[25,1:2]
ipp <- dat_raw[26,1]
nvzenp <- dat_raw[27,1]
vwzenp <- dat_raw[28,1:6]
NOZENV <- dat_raw[29,1:7]
NOAZMV <- dat_raw[30,1:9]
g(ITOT,JMAX,JMIN,DFMIN,CLUMP,kmax) %=% dat_raw[31,1:6]
g(EMIS,EMISOL,factir) %=% dat_raw[32,1:3]
g(RSOIL,RLEAF,TLEAF) %=% list(dat_raw[33,1:kmax],dat_raw[33,4:(3+kmax)],dat_raw[33,7:(6+kmax)])
g(RLDEAD,TLDEAD) %=% list(dat_raw[34,1:kmax],dat_raw[34,4:(3+kmax)])
g(AVISR,BVISR,CVISR,ANIRR,BNIRR) %=% dat_raw[35,1:5]
g(AVIST,BVIST,CVIST,ANIRT,BNIRT) %=% dat_raw[36,1:5]
g(ISPHER, NALPHA, GR_ALPHA) %=% list(dat_raw[37,1],dat_raw[37,2],dat_raw[37,3:11])
g(IMUU,XMEU,XNEU) %=% dat_raw[38,1:3] #Canopy type ???
g(imunua,xmeuaz,xneuaz,beta0,nbeta) %=% dat_raw[39,1:5]
g(ROWSP,PLSPC,DMAX) %=% dat_raw[40,1:3]
g(ILAIUP,PLTPM2) %=% dat_raw[41,1:2]
g(REFHTW, ZDHCR, amfull, z0dh, dispdh) %=% dat_raw[42,1:5] #REFHTW: wind height
NLACPY <- dat_raw[43,1]; Z_Labove <- dat_raw[43,2:(1+NLABCY)]
NLBCPY <- dat_raw[44,1]; Z_lbelow = dat_raw[44,2:(1+NLBCPY)]
g(RCUT20,RSMIN,ANSTOM,RADN) %=% dat_raw[45,1:4] #RS vs light
g(TRSOPT, TRSMAX, TRSMIN, RSEXP, RSM) %=% dat_raw[46,1:5]
g(PSI1, PSI2, cidcai) %=% dat_raw[47,1:3]
g(IC3IC4, CAIR, FS, CUCOND, D1, bkh, conmin, RXCHAM) %=% dat_raw[48,1:8]
g(NOTEMP,TFIX) %=% dat_raw[49,1:2]
RROOT <- dat_raw[50,1]
g(FRSTEM,FRWTMX,PINTMX) <- dat_raw[51,1:3]
NODYIN <- dat_raw[52,1]
g(LDAYIN,XTLAI,frlive,XHT,XAROT,XZLDH,XZMDH,TSOLBC,WSOLBC) %=% dat_raw[53:54,1:9]

# Initc 58
WT_i <- dat_raw[55,1:NDSOIL]
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

system(shQuote("/Users/Giin/Documents/GitHub/CUPID/src/a.out",
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