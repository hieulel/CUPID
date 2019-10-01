#Testing model CUPID
#Variables: Input timing
#Tester: Thien Nguyen
#Date: 27 August 2019
##########################################

system("make") #makefile of the cupid code

NSim = 48 #Number of Simulations = Number of ts

#Declaring the data matrix
T_Average = matrix(data=NA,nrow = 144, ncol = NSim)

# Base_input <- readLines("input.c7.in")
# Base_weat <- Base_input[108:206]
# write(Base_weat,"base_weat.txt")

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
  T_Average[,i] <- cupstrip[,5]

  #updating for day 1 Irri
  weat.irri <- readLines("irri_weat.txt")
  weat.base <- readLines("base_weat.txt")

  ######################################
  new.irri <- weat.irri[i+1]
  current.irri <- weat.base[i+1]
  #new.irri1 <- weat.irri[i+49]
  #current.irri1 <- weat.base[i+49]
  input.file <- readLines("input.c7.in")
  input.file<- gsub(pattern =  current.irri, replace = new.irri, x = input.file, fixed = T)
  #input.file<- gsub(pattern =  current.irri1, replace = new.irri1, x = input.file, fixed = T)
  writeLines(input.file, con="input.c7.in")
  ######################################
  input.file <- readLines("input.c7.in")
  new1.irri <- weat.base[i]
  current2.irri <- weat.irri[i]
  #new1.irri1 <- weat.base[i+48]
  #current2.irri1 <- weat.irri[i+48]
  input.file<- gsub(pattern =  current2.irri, replace = new1.irri, x = input.file, fixed = T)
  #input.file<- gsub(pattern =  current2.irri1, replace = new1.irri1, x = input.file, fixed = T)
  writeLines(input.file, con="input.c7.in")
  ######################################
}

#END simulation
write.csv(T_Average,"Tave.csv")
t<-T_Average[,1]
#Plotting
# plot(T_Air[,3],type = 'l',xlab = "Layer", ylab = "Temperature(C)",main = "Canopy Air Temperature")
# lines(T_Air[,1],col="blue")
# lines(T_Air[,2],col="orange")
# legend("topright",legend = c("No Irrigation","Night Irrigation","Night+Afternoon"),lty = c(1,1,1),col = c("black","blue","orange"))


#1.25
#0.15
#5
#10
#15

#Wind: 0; 10; 30; 60; 80, for 1 ts: 12.25