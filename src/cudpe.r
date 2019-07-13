dpe <- function(precip,
                ihr,
                tirrig,
                nlabcy,
                dt,
                ettot,
                qtot){
                    # Calculate the average layer condition and tirrig
                    htdrop = htspk + zmax
                    aprate = precip/dt
                    ettot = 0.0
                    qtot = 0.0
                    rhsum = 0.0
                    tsum = 0.0
                    zsum = 0.0

                    for (jz in nlabcy){
                        rh = en[ihr,jz]/etsat[jz]*100

                        if (-z[jz] > htdrop) {
                            z2 = -z[jz]-z[jz+1]-htdrop
                        }
                        if (-z[jz+1] > htdrop){
                            z2=-z[jz]
                        }
                        if (-z[jz] <= htdrop){
                            z2 = -z[jz]
                        }
                        rhsum = rhsum +(-z[jz]-z2)*rh
                        tsum = tsum+(-z[jz]-z2)*tn[ihr,jz]
                        zsum=zsum+(-z[jz]-z2)
                    }
                    rhsum=rhsum/zsum
                    tsum=tsum/zsum
                    tirrig=regf(tsum,rhsum,ctwa,dtwa,etwa,ftwa,gtwa,1.) #function regf
                }