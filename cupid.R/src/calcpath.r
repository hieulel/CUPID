# Replicate of calcpath subroutine

calcpath <- function(nohrs,slope,aspect,path){
  for (i in 1:nohrs){
    if (zenang[i] < pid2*0.998){
      print(paste('zenang of i = '),zenang[i])
      return()
    }
    path[i] = cos(zenang[i])*cos(slope)+sin(zenang[i])*sin(slope)*cos(aspect-sunazm[i])
    if (path[i] == 0){
      path[i] = 1.0e-11
      path[i] = 1.0/path[i]
    }
    if (path[i] <= 0.0 | path[i] > 1.0e10){
      if (path[i] <= 0.0){
        path[i] = -1.0
      }
    } else {
      # Sun below horizon
      path[i] = -1.0
    }
  }
}

