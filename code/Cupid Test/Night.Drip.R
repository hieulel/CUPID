#Testing model CUPID
#Variables: Irri
#Tester: Thien Nguyen
#Date: 22 August 2019
##########################################

system("make") #makefile of the cupid code

NSim = 5 #Number of Simulations = Number of soil profiles

#Declaring the data matrix
T_Air = matrix(data=NA,nrow = 384, ncol = NSim)

#Automation loop
for (i in 1:NSim){
  system("./cupid7") #run Cupid
  
  #Stripping the lengthy output file
  stripT <- "./cup2rdb cupfile.rd output.c7.out | grep -v N > cupout.strip.txt"
  system(stripT)
  
  #Storing the stripped file into R
  cupstrip <- read.table("cupout.strip.txt",
                         header = T, fill = T)
  
  #Extract the desired data from cupstrip 
  T_AERO[,i] <- cupstrip[,5]
  
  #updating new input file with the targeted variable
  input.file <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")
  soil_ref <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/Soil_Profile/Soil.Ref")
  
  current.soilPrf <- readLines(soil_ref[i]) #read current soil profile
  
  new.soilPrf <- readLines(soil_ref[i+1]) #read new soil profile
  
  for (j in 1:length(current.soilPrf)){
    input.file_j<- gsub(pattern =  current.soilPrf[j], replace = new.soilPrf[j], x = input.file, fixed = TRUE)
    input.file <- input.file_j
  }
  writeLines(input.file, con="/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")
}

#END simulation
input.new <- read.table('/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/Weather_Data/weather.data.01.in',header = FALSE,fill = TRUE)
