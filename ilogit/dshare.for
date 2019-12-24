C-------------------------------------------------------------------
C        DISTRICT LEVEL MODE SHARE COMPUTATION
C-------------------------------------------------------------------
       SUBROUTINE DSHARE
       INCLUDE 'stadat.inc'
       INCLUDE 'param.inc'
	     include 'ilogitpar.inc'
	     include 'dvalues.inc'
	     integer*4    iz,t,jz,k,kz
	     real*4       gobus(60,60,3),gotrain(60,60,3)
	     real*4       ttc(60,60,3),total(60,60,3),numer
	     real*4       inpdist(60),tottrp(4,4),tlf(101,3)
	     real*4       trntot(3),tlfper(101,3),tlfcum(101,3)
	     real*4       avgshr(3)
	     logical      exists
c
c     read district share matrix, if provided
c
      if(shares) then
      inquire (file=fdshare,exist=exists)
      if(exists) then
      open(85,file=fdshare,status='old',
     *        form='formatted')
      else
      write(26,7005) fdshare
 7005 format(/' DSHARE 7005 (F) DISTRICT SHARE FILE ',A40,
     *       ' NOT FOUND -- SHARES=T'/)
      write(*,7005) fdshare
      stop 7005
      end if      
      do iz=1,60
      do t=1,3
      read(85,*,end=70,err=71) kz,k,(dshares(iz,jz,t),jz=1,60)
      end do
      end do
   70 continue
      return
   71 write(26,7006) iz,t 
      write(*,7006) iz,t
 7006 format(/' DHARE 71 (F) ERROR READING DISTRICT SHARE FILE',
     *       ' FOR ORIGIN DISTRICT ',I2,' MARKET SEGMENT ',I1/)
      STOP 71
      RETURN
      end if
C
      inquire (file=dgobus,exist=exists)
      if(exists) then
      open(81,file=dgobus,status='old',
     *        form='formatted')
      else
      write(26,7001) dgobus
 7001 format(/' DSHARE 7001 (F) GO BUS DISTRICT FILE ',A40,
     *       ' NOT FOUND'/)
      write(*,7001) dgobus
      stop 7001
      end if
C
      inquire (file=dgotrain,exist=exists)
      if(exists) then
      open(82,file=dgotrain,status='old',
     *        form='formatted')
      else
      write(26,7002) dgotrain
 7002 format(/' DSHARE 7002 (F) GO TRAIN DISTRICT FILE ',A40,
     *       ' NOT FOUND'/)
      write(*,7002) dgotrain
      stop 7002
      end if
C
      inquire (file=dttc,exist=exists)
      if(exists) then
      open(83,file=dttc,status='old',
     *        form='formatted')
      else
      write(26,7003) dttc
 7003 format(/' DSHARE 7003 (F) TTC SUBWAY DISTRICT FILE ',A40,
     *       ' NOT FOUND'/)
      write(*,7003) dttc
      stop 7003
      end if
C
      inquire (file=dtotal,exist=exists)
      if(exists) then
      open(84,file=dtotal,status='old',
     *        form='formatted')
      else
      write(26,7004) dtotal
 7004 format(/' DSHARE 7004 (F) TOTAL PERSON DISTRICT FILE ',A40,
     *       ' NOT FOUND'/)
      write(*,7004) dtotal
      stop 7004
      end if
C
      open(85,file=fdshare,status='unknown',
     *        form='formatted')
      open(86,file=ftlf,status='unknown',
     *        form='formatted')
c
c     GO Bus, GO Train, TTC Subway, Total Person Inputs
c
    1 read(81,*,end=10,err=9) iz,t,(gobus(iz,jz,t),jz=1,60)
      go to 1
    9 write(26,8) iz,(t-1)
    8 format(' error reading GO Bus district values for origin',
     *       ' district ',i2,' auto ownership level ',i1)
   10 read(82,*,end=20,err=19) iz,t,(gotrain(iz,jz,t),jz=1,60)
      go to 10
   19 write(26,18) iz,(t-1)
   18 format(' error reading GO Train district values for origin',
     *       ' district ',i2,' auto ownership level ',i1)
   20 read(83,*,end=30,err=29) iz,t,(ttc(iz,jz,t),jz=1,60)
      go to 20
   29 write(26,28) iz,(t-1)
   28 format(' error reading TTC Subway district values for origin',
     *       ' district ',i2,' auto ownership level ',i1)
   30 read(84,*,end=40,err=39) iz,t,(total(iz,jz,t),jz=1,60)
      go to 30
   39 write(26,38) iz,(t-1)
   38 format(' error reading Total Person district values for origin',
     *       ' district ',i2,' auto ownership level ',i1)
   40 continue
c
c     compute total trips by mode and auto ownership level
c
      do iz=1,60
      do jz=1,60
      do t=1,3
      tottrp(1,t)=tottrp(1,t)+gobus(iz,jz,t)
      tottrp(2,t)=tottrp(2,t)+gotrain(iz,jz,t)
      tottrp(3,t)=tottrp(3,t)+ttc(iz,jz,t)
      tottrp(4,t)=tottrp(4,t)+total(iz,jz,t)
       if(jz.eq.1) then
       cbdtrp(1)=cbdtrp(1)+gotrain(iz,jz,t)
       cbdtrp(2)=cbdtrp(2)+ttc(iz,jz,t)
       cbdtrp(3)=cbdtrp(3)+gobus(iz,jz,t)
       end if
      end do
      end do
      end do
      do iz=1,4
      do jz=1,3
      tottrp(iz,4)=tottrp(iz,4)+tottrp(iz,jz)
      end do 
      end do
c
c     summarize inputs
c
      write(26,50)
   50 format(///'      DISTRICT LEVEL MODE SHARE COMPUTATIONS'/
     *       '      --------------------------------------'//
     *       '         SUMMARY OF INPUT SURVEY TRIP VALUES'/
     *       '  ---------------------------------------------------'/
     *       '  MARKET      GO          GO          TTC       TOTAL'/
     *       ' SEGMENT      BUS        TRAIN       SUBWAY    PERSON'/
     *       ' -------  ----------  ----------  ----------  ----------')
      do t=1,3
      write(26,51) (t-1),(tottrp(iz,t),iz=1,4)
   51 format(4x,i1,3x,4(2x,f10.1))
      end do
      write(26,52) (tottrp(iz,4),iz=1,4)
   52 format(/2x,'Total',1x,4(2x,f10.1))
c
c     compute average share by auto ownership level
c
      do t=1,3
      avgshr(t)=(tottrp(1,t)+tottrp(2,t)+tottrp(3,t))/tottrp(4,t)
      end do
      write(26,53) avgshr
   53 format(/' AVERAGE TRANSIT SHARE BY AUTO OWNERSHIP LEVEL'/
     *       ' ---------------------------------------------'/
     *       ' 0-CAR =',f6.4/
     *       ' 1-CAR =',f6.4/
     *       ' 2+CAR =',f6.4/)
c
c     compute transit mode shares
c
      do iz=1,60
      do jz=1,60
      do t=1,3
      numer=gobus(iz,jz,t)+gotrain(iz,jz,t)+ttc(iz,jz,t)
      trntot(t)=trntot(t)+numer
      if(total(iz,jz,t).gt.0.0) then
      dshares(iz,jz,t)=numer/total(iz,jz,t)
      else
      dshares(iz,jz,t)=0.0
      end if
      dshares(iz,jz,t)=amin1(dshares(iz,jz,t),0.99)
c
c     save in frequency plot
c
      index=ifix(dshares(iz,jz,t)*100.0)+1
      tlf(index,t)=tlf(index,t)+numer
      end do
      end do
      end do
c
c     set zero share to average share
c
      do iz=1,60
      do jz=1,60
      do t=1,3
      if(dshares(iz,jz,t).le.0) dshares(iz,jz,t)=avgshr(t)
      end do
      end do
      end do
c
c     write out frequency distribution
c
      do iz=1,101
      numer=tlf(iz,1)+tlf(iz,2)+tlf(iz,3)
       do jz=1,3
       tlfper(iz,jz)=tlf(iz,jz)/trntot(jz)
       if(iz.gt.1) then
       tlfcum(iz,jz)=tlfcum((iz-1),jz)+tlfper(iz,jz)
       else
       tlfcum(iz,jz)=tlfper(iz,jz)
       end if
      end do
      end do
      do iz=1,101
      write(86,61) (iz-1),(tlfper(iz,jz),jz=1,3),(tlfcum(iz,t),t=1,3)
   61 format(i3,6(',',f10.4))
      end do
c
c     write out district shares
c
      do iz=1,60
      do t=1,3
      write(85,60) iz,t,(dshares(iz,jz,t),jz=1,60)
   60 format(i2,',',i1,60(',',f10.4))
      end do
      end do
      return
      end
