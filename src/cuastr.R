# Replicate of cuastr.f

date <- function(){
  noday = c(1,32,60,91,121,152,182,213,244,274,305,335)
  
}


# 5    if(ifix(2.5*(iyear-1900))-10*ifix((iyear-1900)/4.))10,10,18
# 10   do15i=3,12
# 15   noday(i)=noday(i)+1
# 18   if(icumdy)100,20,60
# 20   if(month)100, 60,25
# 25   if(jday)100, 60,30
# 30   if(month-12)35,35,100
# 35   if(jday-31)40,40,100
# 40   icumdy=noday(month)+jday-1
# 60   if(icumdy-366)65,65,100
# 100  write(6,110) month,jday,iyear,icumdy
# 110  format(1x,4i6,'  trouble with date')
# 65   m0=13
# 70   m0=m0-1
# 80   month=m0
# 200  continue