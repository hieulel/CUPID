#make Cupid to run a simulaiton
system("make") #makefile of the cupid code
system("./cupid7") #run Cupid
stripT <- "./cup2rdb cupfile.rd output2.c7.out | grep -v N > cupst.strip2.txt"
system(stripT)
##############################

#Modify the c6.filist-Initiating CUPID

#1. FUNCTION TO FIND A STRING INSIDE A FILE & REPLACE IT

#set your targeted in-output file name
current.input <- "input.c7.in"
new.input <- "input3.c7.in"
current.output <- "sample.c7.out"
new.output <- "output.c7.out"
#Modify the c6.filist
replaceStrFile("c6.filist",current.input,new.input) #change input
replaceStrFile("c6.filist",current.output,new.output) #change output

##############################

#2.CHANGES THE SOIL PROFILE IN THE CUPID INPUT
input.file <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")

current.soilPrf <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/Soil_Profile/soilOrg.in") #read current soil profile

new.soilPrf <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/Soil_Profile/soilP1.in") #read new soil profile

for (i in 1:length(current.soilPrf)){
  pat[i] <- paste(current.soilPrf[i],collapse = " ")
  rep[i] <- paste(new.soilPrf[i],collapse = " ")
  input.file_i<- gsub(pattern =  pat[i], replace = rep[i], x = input.file, fixed = TRUE)
  input.file <- input.file_i
}

writeLines(input.file, con="/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input3.c7.in")

##############################

#CHANGE THE WEATHER DATA
input.file <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")

current.weather <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/Weather_Data/weather.data.org.in") #read current weather profile

new.weather <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/Weather_Data/weather.data.01.in") #read new weather profile

for (i in 1:length(current.weather)){
  pat[i] <- paste(current.weather[i],collapse = " ")
  rep[i] <- paste(new.weather[i],collapse = " ")
  input.file_i<- gsub(pattern =  pat[i], replace = rep[i], x = input.file, fixed = TRUE)
  input.file <- input.file_i
}

writeLines(input.file, con="/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")

