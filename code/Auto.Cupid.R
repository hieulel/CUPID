#Testing model CUPID
#Variables: 
#Tester: Thien Nguyen
#Date: 22 August 2019
##########################################

system("make") #makefile of the cupid code

NSim = 3 #Number of Simulations = Number of soil profiles

#Declaring the data matrix
T_Air = matrix(data=NA,nrow = 328, ncol = NSim)

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
  T_Air[,3] <- cupstrip[,5]
  
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

#Plotting
plot(T_Air[,3],type = 'l',xlab = "Layer", ylab = "Temperature(C)",main = "Canopy Air Temperature")
lines(T_Air[,1],col="blue")
lines(T_Air[,2],col="orange")
legend("topright",legend = c("No Irrigation","Night Irrigation","Night+Afternoon"),lty = c(1,1,1),col = c("black","blue","orange"))
