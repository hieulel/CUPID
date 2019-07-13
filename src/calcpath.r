# Calcpath
# | calculate the path length a beam of
# | light given the slope and aspect of the
# | ground and the angle of the sun.  This
# | is returns a unitless ratio that is
# | multiplied with the height of the canopy
# | to give you the path length through
# | the canopy.
calc.path.length <- function(mh = 98, # 
                    slope,
                    aspect,
                    path = matrix(data=NA,nrow=mh,ncol=1),
                    pid180,
                    pid2,
                    sigma,
                    radtop = matrix(data=NA,nrow=3,ncol=mh),
                    fbeam1 = matrix(data=NA,nrow=3,ncol=mh),
                    coszen = matrix(data=NA,nrow=mh,ncol=1),
                    zenang = matrix(data=NA,nrow=mh,ncol=1),
                    # hfday,
                    # ratiod,
                    # ration,
                    # ratio = matrix(data=NA,nrow=mh,ncol=1),
                    sunazm = matrix(data=NA,nrow=mh,ncol=1),
                    nozenv,
                    viewenv = matrix(data=NA,nrow=10,ncol=1),
                    viewzn,
                    noazmv,
                    viewaz = matrix(data=NA,nrow=50,ncol=1),
                    xintv = matrix(data=NA,nrow=10,ncol=50)){
                        for (i in nohrs){
                            if (zenang[i] < pid2*0.998){
                                if (sunazm[i]==0.0){
                                    print ('sunazm of i = ',i,'equals 0.0')
                                    print ('zenang of i =', zenang[i])
                                }
                                path[i] = cos(zenang[i])*cos(slope)+sin(zenang[i])*sin(slope)*cos(aspect-sunazm[i])
                                if (path[i] == 0){
                                    path[i]=1e-11
                                    path[i]=1/path[i]
                                
                                    if (path[i] <= 0 | path[i] > 1e10){
                                        path[i] = -1.0
                                    }
                                } else {
                                    # Sun below horizon
                                    path[i] = -1.0
                                }
                            }
                        }
                        return(path)
                    }