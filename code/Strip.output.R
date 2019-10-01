
NSim <- 2 # number of simulation
c6.strip <- readLines("c6.strip")

for (i in 1:NSim){
  system(c6.strip[i])
}

