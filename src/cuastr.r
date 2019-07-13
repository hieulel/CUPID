# This is a replication of sub program cuastr.f
# Include subroutine date

date.func <- function(mh,
                      noday){
                          if (ifix(2.5*(iyear-1990))-10*ifix((iyear-1900)/4.0)){
                              for (i in 12){
                                  noday[i] = noday[i] + 1
                                  
                              }
                          }
                      }