#Testing model CUPID
#Variables: Air-Temp at 48 ts per day LUMPING
#Tester: Thien Nguyen
#Date: 06 September 2019
##########################################

system("make") #makefile of the cupid code

NSim = 48 #Number of Simulations = Number of ts

#Declaring the data matrix
T_Air = matrix(data=NA,nrow = 129, ncol = NSim)

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
  T_Air[,i] <- cupstrip[,5]
  
  #updating new input file with the targeted variable
  input.file <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")
  ts_ref <- readLines("/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/code/Cupid Test/ts.txt")
  
  current.ts <- ts_ref[i] #read current soil profile
  
  new.ts <- ts_ref[i+1] #read new soil profile
  
  #for (j in 1:3){
  input.file<- gsub(pattern =  current.ts, replace = new.ts, x = input.file, fixed = T)
  # input.file <- input.file_j
  #}
  writeLines(input.file, con="/Users/Giin/Desktop/Research/CUPID/cupid.R/Run.Cupid/input.c7.in")
}

#END simulation

#Plotting
plot(T_Air[,3],type = 'l',xlab = "Layer", ylab = "Temperature(C)",main = "Canopy Air Temperature")
lines(T_Air[,1],col="blue")
lines(T_Air[,2],col="orange")
legend("topright",legend = c("No Irrigation","Night Irrigation","Night+Afternoon"),lty = c(1,1,1),col = c("black","blue","orange"))

write.csv(T_Air,"Tair3.csv")

