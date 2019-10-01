#Testing model CUPID
#Variables: Soil Profile
#Tester: Thien Nguyen
#Date: 22 August 2019
##########################################
#Wind: 0; 10; 30; 60; 80, for 1 ts: 12.25
system("make") #makefile of the cupid code

NSim = 5 #Number of Simulations = Number of soil profiles

#Declaring the data matrix
T_Average = matrix(data=NA,nrow = 144, ncol = NSim)

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
  T_Average[,1] <- cupstrip[,5]
  
  #updating new input file with the targeted variable
  input.file <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")
  soil_ref <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/Soil_Profile/Soil.Ref")
  
  current.soilPrf <- readLines(soil_ref[i]) #read current soil profile
  
  new.soilPrf <- readLines(soil_ref[i+1]) #read new soil profile
  
  for (j in 1:3){
    input.file_j<- gsub(pattern =  current.soilPrf[j], replace = new.soilPrf[j], x = input.file, fixed = TRUE)
    input.file <- input.file_j
  }
  writeLines(input.file_j, con="/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")
}

#END simulation
write.csv(T_Average,"wind.csv")

#Plot the Air temperature
plot(T_SSOIL[,1],type = 'l',xlab = "Time", ylab = "Temperature(C)",ylim = range(18:40),main = "Soil Surface Temperature vs Soil Profile")
lines(T_SSOIL[,2],col="blue")
lines(T_SSOIL[,3],col="orange")
lines(T_SSOIL[,4],col="green")
lines(T_SSOIL[,5],col="purple")
legend("topleft",legend = c("50% Sand-50% Clay","100% Clay","100% Silt","70% Sand-30% Clay","30% Sand-70% Clay"),lty = c(1,1,1,1,1),col = c("black","blue","orange","green","purple"))
