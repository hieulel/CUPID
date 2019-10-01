#Testing model CUPID
#Variables: Air-Temp at 48 ts per day
#Tester: Thien Nguyen
#Date: 22 August 2019
##########################################

system("make") #makefile of the cupid code

NSim = 48 #Number of Simulations = Number of ts

#Declaring the data matrix
T_Air = matrix(data=NA,nrow = 144, ncol = NSim)

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
  T_Air[,1] <- cupstrip[,5]
  
  #updating new input file with the targeted variable
  input.file <- readLines("input.c7.in")
  ts_ref <- readLines("ts.txt")
  
  current.ts <- ts_ref[i] #
  
  new.ts <- ts_ref[i+1] #
  
  #for (j in 1:3){
    input.file<- gsub(pattern =  current.ts, replace = new.ts, x = input.file, fixed = T)
   # input.file <- input.file_j
  #}
  writeLines(input.file, con="input.c7.in")
}

#END simulation

#Plotting


write.csv(T_Air,"Tair4.csv")

