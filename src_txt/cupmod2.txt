c $VERSION "@(#)cupmod2.f	7.1 08/16/95"
      subroutine vmatr(nobs,free,ifree,vv,vij,drag,wg)                  
      real*8 vv(30,30),vij(75,30),xa,drag(30),wg(75)                    
      integer nobs,free,k,i,j,ifree(30)                                 
c     write(2,700)                                                      
 700  format('v-matrix')                                                
      do 1 i=1,free                                                     
       do 2 j=i,free                                                    
        xa=0.                                                           
        ii=ifree(i)                                                     
        if (i.eq.j)xa=drag(ii)                                          
        do 3 k=1,nobs                                                   
         xa=xa+vij(k,i)*vij(k,j)*wg(k)                                  
    3   continue                                                        
        vv(i,j)=xa                                                      
        if (i .lt. j) vv(j,i)=xa                                        
    2  continue                                                         
c      write(2,701)(vv(j,i),j=1,free)                                   
  701  format(f30.10)                                                   
    1 continue                                                          
      return                                                            
      end                                                               
      subroutine rmsr(rms,refl,refs,nobs,j)                             
      real*8 refl(75),refs(75,2),rms,xb                                 
      integer nobs,j,i                                                  
      rms=0.                                                            
      xb=nobs                                                           
      do 1 i=1,nobs                                                     
       rms=rms+(refl(i)-refs(i,j))**2                                   
    1 continue                                                          
      rms=dsqrt(rms/xb)                                                 
      return                                                            
      end                                                               
      subroutine gauss(a,y,coef,ncol,error)                             
      logical error                                                     
      real*8 a(30,30),y(30),coef(30),b(30,31),deter                     
      integer ncol                                                      
      error=.false.                                                     
      n=ncol                                                            
       np1=n+1                                                          
      do 20 i=1,n                                                       
       do 10 j=1,n                                                      
        b(i,j)=a(i,j)                                                   
   10  continue                                                         
       b(i,np1)=y(i)                                                    
   20 continue                                                          
       call simq(n,b,coef,1.0e-20,0,np1,deter)                          
c      write(2,666)deter                                                
 666   format(' deter =', e40.10)                                       
      if (deter.lt.1.0d-76) go to 99                                    
      return                                                            
   99 write(2,1000)                                                     
 1000 format(' matrix is singular')                                     
      error=.true.                                                      
      return                                                            
      end                                                               
      subroutine simq(n,cq,cr,eps,indic,nrc,simul)                      
c this subroutine solves simultaneous equations, you must be careful    
c to set the dimensions of cq,cr, a and x to the proper size before use.
c a is the matrix to be inverted, and cq is dummy equivalent            
c likewise cr is the dummy  equivalent of x                             
c                                                                       
c     when indic is negative,simul compute the inverse of the n by      
c     n matrix a in place. when indic is zero,simul computes the        
c     n solutions x(u)....x(n) corresponding to the set of linear       
c     equations with augumented matrix of coefficents in the n by       
c     n+1 arrary and in addition computes the inverse of the coefficent 
c     in place.                                                         
      parameter(mh=30)                                                  
      implicit real*8(a-h,o-z)                                          
      real*8 a,x,eps,simul                                              
      dimension irow(mh),jcol(mh),jord(mh),y(mh),a(mh,mh+1),x(mh)       
      real*8 cq(mh,mh+1),cr(mh)                                         
      integer indic,nrc,n                                               
      do 4962 i=1,n                                                     
 4962 x(i) = cr(i)                                                      
      do 4963 i= 1,n                                                    
      do 4963 j=1,nrc                                                   
 4963 a(i,j) = cq(i,j)                                                  
      max=n                                                             
      if(indic.ge.0) max=n+1                                            
c     is n larger than 50                                               
      if(n.le.50) go to 5                                               
      write(2,200)                                                      
      simul=0.                                                          
      return                                                            
c     begin elimination procedure                                       
    5 deter=1.                                                          
      do 18 k=1,n                                                       
      km1=k-1                                                           
c     search for the pivot element                                      
      pivot=0.                                                          
      do 11 i=1,n                                                       
      do 11 j=1,n                                                       
c     scan irow and jcol arrays for invalid pivot subscripts            
      if(k.eq.1) go to 9                                                
      do 8 iscan=1,km1                                                  
      do 8 jscan=1,km1                                                  
      if(i.eq.irow(iscan)) go to 11                                     
      if(j.eq.jcol(jscan)) go to 11                                     
  8   continue                                                          
    9 if(dabs(a(i,j)).le.dabs(pivot)) go to 11                          
      pivot=a(i,j)                                                      
      irow(k)=i                                                         
      jcol(k)=j                                                         
   11 continue                                                          
c     insure that selected pivot is larger than eps                     
      if(dabs(pivot).gt.eps) go to 13                                   
      simul=0.                                                          
      return                                                            
c     update the determinant value                                      
   13 irowk=irow(k)                                                     
      jcolk=jcol(k)                                                     
      deter=deter*pivot                                                 
c     normalize pivot row elements                                      
      do 14 j=1,max                                                     
   14 a(irowk,j)=a(irowk,j)/pivot                                       
c     carry out elimination and develop inverse                         
      a(irowk,jcolk)=1./pivot                                           
      do 18 i=1,n                                                       
      aijck=a(i,jcolk)                                                  
      if(i.eq.irowk) go to 18                                           
      a(i,jcolk)=-aijck/pivot                                           
      do 17 j=1,max                                                     
   17 if(j.ne.jcolk) a(i,j)=a(i,j)-aijck*a(irowk,j)                     
   18 continue                                                          
c     order solution values (if any ) and create jord arry              
      do 20 i=1,n                                                       
      irowi=irow(i)                                                     
      jcoli=jcol(i)                                                     
      jord(irowi)=jcoli                                                 
   20 if(indic.ge.0) cr(jcoli)=a(irowi,max)                             
c     adjust sign of determinant                                        
      intch=0                                                           
      nm1=n-1                                                           
      do 22 i=1,nm1                                                     
      ip1=i+1                                                           
      do 22 j=ip1,n                                                     
      if(jord(j).ge.jord(i)) go to 22                                   
      jtemp=jord(j)                                                     
      jord(i)=jtemp                                                     
      intch=intch+1                                                     
   22 continue                                                          
      if(intch/2*2.ne.intch) deter=-deter                               
c     if indic is positive return with results                          
   24 if(indic.le.0) go to 26                                           
      simul=deter                                                       
      return                                                            
c     if indic is negative or zero,unsramble the inverse                
c     first by rows                                                     
   26 do 28 j=1,n                                                       
      do 27 i=1,n                                                       
      irowi=irow(i)                                                     
      jcoli=jcol(i)                                                     
   27 y(jcoli)=a(irowi,j)                                               
      do 28 i=1,n                                                       
   28 a(i,j)=y(i)                                                       
c      then by columns                                                  
      do 30 i=1,n                                                       
      do 29 j=1,n                                                       
      irowj=irow(j)                                                     
      jcolj=jcol(j)                                                     
   29 y(irowj)=a(i,jcolj)                                               
      do 30 k=1,n                                                       
   30 a(i,k)=y(k)                                                       
      if(indic.lt.0)then                                                
      do 777 j = 1,n                                                    
      do 777 k = 1,n                                                    
  777 cq(j,k) = a(j,k)                                                  
      endif                                                             
c     return for indic negative or zero                                 
      simul=deter                                                       
      return                                                            
c     format for output statement                                       
  200 format( 10h0n too big )                                           
      end                                                               
      subroutine prmsr(prms,refl,refs,nobs,j)                           
      real*8 refl(75),refs(75,2),prms,xb                                
      integer nobs,j,i                                                  
      prms=0.                                                           
      xb=nobs                                                           
      do 1 i=1,nobs                                                     
       if (refs(i,j).eq.0.)refs(i,j)=0.0001                             
       prms=prms+(1.-refl(i)/refs(i,j))**2                              
    1 continue                                                          
      prms=dsqrt(prms/xb)                                               
      prms=prms*100.                                                    
      return                                                            
      end                                                               
      subroutine obsct(nobs,jtime,jcode,date,psn,pvw,tsun,tview,ref)    
      real tir,ftime,frze,fraz,vize,viaz                                
      character*80 junk
      integer jtime(75),jcode(75),icode                       
      real*8 psn(75),tsun(75),tview(75),ref(75),pvw(75),date(75)        
      read(5,3333)junk
      read(5,3333)junk
3333  format(80a)
      ndata=1                                                           
      j=1                                                               
      do 160 ic=1,ndata                                                 
 120  read(5,*  )icode,ftime,frze,fraz,vize,viaz,tir  
c     write(6,*)icode,ftime,frze,fraz,vize,viaz,tir  
c1070 format(2i8,4f8.2,f5.2)                                         
      if (icode.eq.0)go to 140                                          
      psn(j)=fraz                                                       
      pvw(j)=viaz                                                       
      tsun(j)=frze                                                      
      tview(j)=vize                                                     
      jtime(j)=int(ftime)+1                                                    
      jcode(j)=icode                                                    
      ref(j)=tir                                                
      j=j+1                                                             
      go to 120                                                         
 140  continue                                                          
 160  continue                                                          
      nobs=j-1                                                          
      return                                                            
      end
      subroutine deriv(nobs,ifree,free,psn,pvw,ts,tv,xx,refl,vij,       
     &nnp,del,icycno,isw1,jcode,jtime,iquit)                             
      real*8 psn(75),ts(75),tv(75),xx(30),refl(75),vij(75,30),          
     1del(30),yy(30),refly(75),pvw(75),pabd(3)                          
      real  r,t,sunzn                                                   
      integer iquit,jtime(75)
      integer free,ifree(30),jcode(75),nobs,nnp,icycno,isw1             
      call reflt(nobs,psn,pvw,ts,tv,xx,refl,                            
     &nnp,icycno,isw1,jcode,jtime,iquit)                                      
       do 2 i=1,free                                                    
        ii=ifree(i)                                                     
*        write(2,298)ii                                                 
 298     format('# parameter free = ',i2)                               
*                                                                       
        do 3 k=1,nnp                                                    
         yy(k)=xx(k)                                                    
         if (k .eq. ii) yy(k)=yy(k)+del(ii)                             
    3   continue                                                        
      if ((ii.ne.1).and.(ii.ne.2)) go to 20                             
 18   continue                                                          
 20   continue                                                          
      call reflt(nobs,psn,pvw,ts,tv,yy,refly,                           
     &nnp,icycno,isw1,jcode,jtime,iquit)                                      
      do 5 j =1,nobs                                                    
           vij(j,i) =(refly(j)-refl(j))/del(ii)                         
*          write(2,398)refly(j),refl(j),del(ii),vij(j,i)                
 398  format('refly=',f10.5,'  refl=',f10.5,'  del=',f8.4,              
     &'  vij=',f10.5)                                                   
 5    continue                                                          
 2    continue                                                          
      return                                                            
      end                                                               
      subroutine error(xx,vv,nobs,free,ifree,name,vij,refl,err,wg,nnp)  
      real*8 xx(30),vv(30,30),vij(75,30),refl(75),wk(30),sig(30),       
     &wij(30,75),err,yy(30),zz(30),ww(30,30),vw,sum,sa(30),wg(75)       
      integer nobs,free,ifree(30),kk,i,nnp                              
      character*6 name(30)                                              
      logical ier                                                       
      do 101 j=1,free                                                   
       do 102 k=1,free                                                  
        yy(k)=0.                                                        
        if (k.eq.j) yy(k)=1.                                            
  102  continue                                                         
       call gauss(vv,yy,zz,free,ier)                                    
       if (ier) stop                                                    
       do 103 k=1,free                                                  
        ww(k,j)=zz(k)                                                   
  103  continue                                                         
  101 continue                                                          
      sum=0.                                                            
      do 107 k=1,free                                                   
       do 108 k1=1,free                                                 
        vw=0.                                                           
        if (k.eq.k1) vw=-1.                                             
        do 109 k2=1,free                                                
         vw=vw+vv(k,k2)*ww(k2,k1)                                       
  109   continue                                                        
        sum=sum+vw**2                                                   
  108  continue                                                         
  107 continue                                                          
*     write(2,1999) sum                                                 
 1999 format(' sum= ',d15.8)                                            
      do 104 i=1,nobs                                                   
       do 105 k=1,free                                                  
        wij(k,i)=0.                                                     
        do 106 k1=1,free                                                
         wij(k,i)=wij(k,i)+ww(k,k1)*vij(i,k1)                           
  106   continue                                                        
  105  continue                                                         
  104 continue                                                          
*     write(2,1003)                                                     
 1003 format('  error contribution per angle:')                         
      do 30 i=1,nnp                                                     
      sig(i)=0.                                                         
      sa(i) =0.                                                         
  30   continue                                                         
*                                                                       
*********************************************************************   
*                                                                   *   
*     unscale parameters                                            *   
*                                                                   *   
*********************************************************************   
*                                                                   *   
      xx(1)=10.*xx(1)                                                   
      xx(2)=10.*xx(2)                                                   
*                                                                       
 724  continue                                                          
      do 20 i=1,nobs                                                    
         do 10 k=1,free                                                 
         kk=ifree(k)                                                    
         sa(kk)=dabs(refl(i)*wij(k,i))*err*wg(i)/xx(kk)                 
         sig(kk)=sig(kk)+sa(kk)**2                                      
 10      continue                                                       
*     write(2,1002)i,(sa(j),j=1,nnp)                                    
 1002 format(5x,i4,2x,5f12.4)                                           
 20   continue                                                          
      do 40 k=1,free                                                    
      kk=ifree(k)                                                       
      sig(kk)=dsqrt(sig(kk))                                            
 40   continue                                                          
      write(2,1000) err                                                 
 1000 format(5x,'vble.  value',9x,' expected error when err= ',f6.2,'%')
      do 3 i=1,nnp                                                      
       write(2,1001) name(i),xx(i),sig(i)                               
 1001 format(5x,a6,2x,f15.4,2x,f15.4,'%')                               
    3 continue                                                          
*                                                                       
*********************************************************************   
*                                                                   *   
*     rescale parameters                                            *   
*                                                                   *   
*********************************************************************   
*                                                                       
      xx(1)=xx(1)/10.                                                   
      xx(2)=xx(2)/10.                                                   
*                                                                       
 725  continue                                                          
      return                                                            
      end                                                               
      subroutine genref(nnp,xr,name,iquit)                              
*                                                                       
      character*6 name(30)                                              
      real ra,spp,rc(7)                                                 
      real*8 sp(75),vp(75),sa(75),va(75),xr(30),refl(75)                
      integer jcode(75),nnp,iquit,jtime(75)                             
      real psi,ref                                                      
      real rho,tau,llai,rosoil,skyl,mu,nu,pd,amp,sunz,sunaz             
      real stepvp,stepva,fd,fs                                          
      real vzmin,vzmax,vamin,vamax                                      
c     data nvp,stepvp,nva,stepva/31,2.,121,3./                          
c     data nvp,stepvp,nva,stepva/36,2.,61,3./                           
c     data stepvp,stepva/2.,3./                                         
*     data stepvp,stepva/15.,45./                                       
*     data stepvp,stepva/2.,5./                                         
*                                                                       
      read(5,100)stepvp,stepva                                          
      icycno=0                                                          
      ii=0                                                              
*                                                                       
      write(2,1015)                                                     
 1015 format('  parameter values:')                                     
      do 16 i=1,nnp                                                     
       write(2,1016) name(i),xr(i)                                      
 1016 format(5x,a6,1x,f12.4)                                            
   16 continue                                                          
      write(2,110)                                                      
 110  format(/,' sunzn       sun az')                                   
      read(5,100)sp(1),sa(1)                                            
 100  format(2f10.2)                                                    
      write(2,100)sp(1),sa(1)                                           
      spp=sp(1)                                                         
  85  format(7f10.4)                                                    
 239  continue                                                          
      read(5,18)vzmin,vzmax                                             
 18   format(2f10.4)                                                    
      write(2,17)vzmin,vzmax                                            
 17   format(/,'view zenith range:   ',2f10.2)                          
      read(5,18)vamin,vamax                                             
      write(2,19)vamin,vamax                                            
 19   format(/,'view azimuth range:  ',2f10.2)                          
      write(2,50)                                                       
 50   format(/,' view zen  view az    refl',/)                          
      nvp=int((vzmax-vzmin)/stepvp)+1                                   
      nva=int((vamax-vamin)/stepva)+1                                   
      va(1)=vamin                                                       
      do 63 i=2,nva                                                     
         va(i)=va(i-1)+stepva                                           
         sp(i)=sp(1)                                                    
         sa(i)=sa(1)                                                    
 63   continue                                                          
      vp(1)=vzmin-stepvp                                                
      do 20 j=1,nvp                                                     
         vp(1)=vp(1)+stepvp                                             
         do 21 i=2,nva                                                  
            vp(i)=vp(1)                                                 
 21      continue                                                       
         nobs=nva                                                       
         ipbm=1                                                         
         call reflt(nobs,sa,va,sp,vp,xr,refl,                           
     &nnp,icycno,isw1,jcode,jtime,iquit)                                      
          do 31 k=1,nva                                                 
             write(2,40)vp(k),va(k),refl(k)                             
  40         format(2f10.0,f10.4)                                       
 31       continue                                                      
 20   continue                                                          
      return                                                            
      end                                                               
