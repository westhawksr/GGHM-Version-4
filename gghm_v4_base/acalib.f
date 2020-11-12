c ----------------------------------------------------------
c     air passenger self calibration subroutine
c ----------------------------------------------------------
      subroutine acalib
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
      integer*2     index
      character*16  name,names(26)
      real*4        value,oesum(26),tesum(26),desum(26)
      real*4        ncon,rcon,econ
      logical       exists
      index=0
      names(23)='GO Rail Stations'
c
c     open calibration target values
c
      inquire(file=fctvinp,exist=exists)
      if(.not.exists) then
      write(*,9001)  fctvinp
      write(26,9001) fctvinp
 9001 format(//' ACALIB 9001 (F) CALIBRATION TARGET VALUE FILE=',
     *         A40,' NOT FOUND'//)
      stop 9001
      else
      open(98,file=fctvinp,status='old',form='formatted')
      end if
   10 read(98,*,end=100,err=50) name,value
      index=index+1
      if(index.eq.23) index=24
      names(index)=name
      oesum(index)=value
      go to 10
   50 write(26,51) name
   51 format(' acalib 51 (f) error reading calibration target',
     *       ' value file for mode name=',a16)
      stop 51
  100 close(98,status='keep')
      oesum(23)=oesum(11)-oesum(20)-oesum(21)-oesum(22)
c
c     open model estimated values
c
      inquire(file=faircalib,exist=exists)
      if(.not.exists) then
      write(*,9002)  faircalib
      write(26,9002) faircalib
 9002 format(//' ACALIB 9002 (F) MODEL ESTIMATED VALUE FILE=',
     *         A40,' NOT FOUND'//)
      stop 9002
      else
      open(98,file=faircalib,status='old',form='formatted')
      end if
      index=0
   20 read(98,*,end=200,err=150) name,value
      index=index+1
      if(index.eq.23) index=24
      tesum(index)=value
      go to 20
  150 write(26,151) name
  151 format(' acalib 151 (f) ERROR READING MODEL ESTIMATION',
     *       ' FILE FOR MODE NAME=',a16)
      stop 151
  200 close(98,status='delete')
      tesum(23)=tesum(11)-tesum(20)-tesum(21)-tesum(22)
c
c     compute difference = estimated-observed
c
      do k=1,26
      desum(k)=tesum(k)-oesum(k)
      end do
c
c     report summary
c
      write(26,301)
  301 format('         AIR PASSENGER MDOE COMPARISON'/
     *       '       MODE          TARGET     EST       DIFF'/
     *       '  ----------------  --------  --------  --------')
      do k=1,26
      write(26,300) names(k),oesum(k),tesum(k),desum(k)
  300 format(2x,a16,3(2x,f8.0))
      end do
      write(26,7001)
 7001 FORMAT(/15X,'MODE CHOICE MODEL CONSTANT COMPUTATIONS',/,
     *        15X,'---------------------------------------')
      write(26,7002)
 7002 format(
     *  1X,'Constant  Obs     Est     Existing               New'/
     *  1X,'  Name    Value   Value   Constant Adjustment  Constant'/
     *  1X,'-------  ------- ------- --------- ---------- ---------')
C
C----------------------------------------------------------------------
C     Calculate Drive Alone Constants
C
      if(ccode(1)) then
      rcon=0.0
      if(tesum(1).gt.0) then
      rcon=log(oesum(1)/tesum(1))
      end if
      econ=kda(1)
      ncon=econ+adjfct*rcon
      kda(1)=ncon
      kda(2)=ncon
      if(ncats.eq.3) kda(3)=ncon
      write(26,8001) oesum(1),tesum(1),econ,rcon,ncon,names(1)
 8001 format(2x,'KDA     ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1X,A16)
      write(100,6001) (kda(k),k=1,ncats)
 6001 format(3X,'KDA=',F9.5,2(',',F9.5))  
      end if
C
C----------------------------------------------------------------------
C     Calculate 2-Person Constants
C
      if(ccode(2)) then
      rcon=0.0
      if(tesum(2).gt.0) then
      rcon=log(oesum(2)/tesum(2))
      end if
      econ=k2p(1)
      ncon=econ+adjfct*rcon
      k2p(1)=ncon
      k2p(2)=ncon
      if(ncats.eq.3) k2p(3)=ncon
      write(26,8002) oesum(2),tesum(2),econ,rcon,ncon,names(2)
 8002 format(2x,'K2P     ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      write(100,6002) (k2p(k),k=1,ncats)
 6002 format(3X,'K2P=',F9.5,2(',',F9.5))  
      end if      
C
C----------------------------------------------------------------------
C     Calculate 3+-Person Constants
C
      if(ccode(3)) then
      rcon=0.0
      if(tesum(3).gt.0) then
      rcon=log(oesum(3)/tesum(3))
      end if
      econ=k3p(1)
      ncon=econ+adjfct*rcon
      k3p(1)=ncon
      k3p(2)=ncon
      if(ncats.eq.3) k3p(3)=ncon
      write(26,8003) oesum(3),tesum(3),econ,rcon,ncon,names(3)
 8003 format(2x,'K3P     ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      write(100,6003) (k3p(k),k=1,ncats)
 6003 format(3X,'K3P=',F9.5,2(',',F9.5))  
      end if       
C
C----------------------------------------------------------------------
C     Calculate Drop-Off Constants
C
      if(ccode(4)) then
      rcon=0.0
      if(tesum(4).gt.0) then
      rcon=log(oesum(4)/tesum(4))
      end if
      econ=acnst(4)
      ncon=econ+adjfct*rcon
      acnst(4)=ncon
      write(26,8004) oesum(4),tesum(4),econ,rcon,ncon,names(4)
 8004 format(2X,'ACNST(4)',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if    
C
C----------------------------------------------------------------------
C     Calculate Rental Car Constants
C
      if(ccode(5)) then
      rcon=0.0
      if(tesum(5).gt.0) then
      rcon=log(oesum(5)/tesum(5))
      end if
      econ=acnst(2)
      ncon=econ+adjfct*rcon
      acnst(2)=ncon
      write(26,8005) oesum(5),tesum(5),econ,rcon,ncon,names(5)
 8005 format(2X,'ACNST(2)',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if      
C
C----------------------------------------------------------------------
C     Calculate Limo/Town Car Constants
C
      if(ccode(6)) then
      rcon=0.0
      if(tesum(6).gt.0) then
      rcon=log(oesum(6)/tesum(6))
      end if
      econ=acnst(3)
      ncon=econ+adjfct*rcon
      acnst(3)=ncon
      write(26,8006) oesum(6),tesum(6),econ,rcon,ncon,names(6)
 8006 format(2X,'ACNST(3)',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if
C
C----------------------------------------------------------------------
C     Calculate Taxi Constants
C
      if(ccode(7)) then
      rcon=0.0
      if(tesum(7).gt.0) then
      rcon=log(oesum(7)/tesum(7))
      end if
      econ=acnst(1)
      ncon=econ+adjfct*rcon
      acnst(1)=ncon
      write(26,8007) oesum(7),tesum(7),econ,rcon,ncon,names(7)
 8007 format(2X,'ACNST(1)',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if
C
C----------------------------------------------------------------------
C     Calculate On-Call Constants
C
      if(ccode(8)) then
      rcon=0.0
      if(tesum(8).gt.0) then
      rcon=log(oesum(8)/tesum(8))
      end if
      econ=acnst(6)
      ncon=econ+adjfct*rcon
      acnst(6)=ncon
      write(26,8008) oesum(8),tesum(8),econ,rcon,ncon,names(8)
 8008 format(2X,'ACNST(6)',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if
C
C----------------------------------------------------------------------
C     Calculate Transit Constants
C
      if(ccode(9)) then
      rcon=0.0
      if(tesum(9).gt.0) then
      rcon=log(oesum(9)/tesum(9))
      end if
      econ=acnst(7)
      ncon=econ+adjfct*rcon
      acnst(7)=ncon
      write(26,8009) oesum(9),tesum(9),econ,rcon,ncon,names(9)
 8009 format(2X,'ACNST(7)',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if
C
C----------------------------------------------------------------------
C     Calculate Uber Constants
C
      if(ccode(10)) then
      rcon=0.0
      if(tesum(10).gt.0) then
      rcon=log(oesum(10)/tesum(10))
      end if
      econ=acnst(10)
      ncon=econ+adjfct*rcon
      acnst(10)=ncon
      write(26,8010) oesum(10),tesum(10),econ,rcon,ncon,names(10)
 8010 format(2X,'ACNST(10)',F7.0,1X,F7.0,1x,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if
C
C----------------------------------------------------------------------
C     Calculate UPX Constants
C
      if(ccode(11)) then
      rcon=0.0
      if(tesum(11).gt.0) then
      rcon=log(oesum(11)/tesum(11))
      end if
      econ=upxcnst
      ncon=econ+adjfct*rcon
      upxcnst=ncon
      write(26,8011) oesum(11),tesum(11),econ,rcon,ncon,names(11)
 8011 format(2X,'UPXCNST  ',F7.0,1X,F7.0,1x,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      write(100,6005) upxcnst
 6005 format(3X,'UPXCNST=',F9.5)  
      end if
C
C----------------------------------------------------------------------
C     Calculate Total Drive Constants
C
      if(ccode(12)) then
      rcon=0.0
      if(tesum(13).gt.0) then
      rcon=log(oesum(13)/tesum(13))
      end if
      econ=acnst(5)
      ncon=econ+adjfct*rcon
      acnst(5)=ncon
      write(26,8012) oesum(13),tesum(13),econ,rcon,ncon,names(13)
 8012 format(2X,'ACNST(5)',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if
C
C----------------------------------------------------------------------
C     Calculate Shared Ride to UPX Constants
C
      if(ccode(13)) then
      rcon=0.0
      if(tesum(18).gt.0) then
      rcon=log(oesum(18)/tesum(18))
      end if
      econ=kkcr(1)
      ncon=econ+adjfct*rcon
      kkcr(1)=ncon
      kkcr(2)=ncon
      if(ncats.eq.3) kkcr(3)=ncon
      write(26,8013) oesum(18),tesum(18),econ,rcon,ncon,names(18)
 8013 format(2x,'KKCR    ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1X,A16)
      write(100,6006) (kkcr(k),k=1,ncats)
 6006 format(3X,'KKCR=',F9.5,2(',',F9.5))  
      end if
C
C----------------------------------------------------------------------
C     Calculate TTC Subway Constants
C
      if(ccode(14)) then
      rcon=0.0
      if(tesum(24).gt.0) then
      rcon=log(oesum(24)/tesum(24))
      end if
      econ=kur(1)
      ncon=econ+adjfct*rcon
      kur(1)=ncon
      kur(2)=ncon
      if(ncats.eq.3) kur(3)=ncon
      write(26,8014) oesum(24),tesum(24),econ,rcon,ncon,names(24)
 8014 format(2x,'KUR     ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1X,A16)
      write(100,6007) (kur(k),k=1,ncats)
 6007 format(3X,'KUR=',F9.5,2(',',F9.5))  
      end if
C
C----------------------------------------------------------------------
C     Calculate GO Bus Constants
C
      if(ccode(15)) then
      rcon=0.0
      if(tesum(25).gt.0) then
      rcon=log(oesum(25)/tesum(25))
      end if
      econ=kgbus(1)
      ncon=econ+adjfct*rcon
      kgbus(1)=ncon
      kgbus(2)=ncon
      if(ncats.eq.3) kgbus(3)=ncon
      write(26,8015) oesum(25),tesum(25),econ,rcon,ncon,names(25)
 8015 format(2x,'KGBUS   ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1X,A16)
      write(100,6008) (kda(k),k=1,ncats)
 6008 format(3X,'KGBUS=',F9.5,2(',',F9.5))  
      end if
C
C----------------------------------------------------------------------
C     Calculate Local Bus Constants
C
      if(ccode(16)) then
      rcon=0.0
      if(tesum(26).gt.0) then
      rcon=log(oesum(26)/tesum(26))
      end if
      econ=klbus(1)
      ncon=econ+adjfct*rcon
      klbus(1)=ncon
      klbus(2)=ncon
      if(ncats.eq.3) klbus(3)=ncon
      write(26,8016) oesum(26),tesum(26),econ,rcon,ncon,names(26)
 8016 format(2x,'KLBUS   ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1X,A16)
      write(100,6009) (klbus(k),k=1,ncats)
 6009 format(3X,'KLBUS=',F9.5,2(',',F9.5))  
      end if
C
C----------------------------------------------------------------------
C     Calculate TTC Subway to GO Rail Constant
C
      if(ccode(17)) then
      rcon=0.0
      if(tesum(19).gt.0) then
      rcon=log(oesum(19)/tesum(19))
      end if
      econ=ttcnst
      ncon=econ+adjfct*rcon
      ttcnst=ncon
      write(26,8017) oesum(19),tesum(19),econ,rcon,ncon,names(19)
 8017 format(2x,'TTCNST  ',2(1X,F7.0),1X,
     *                F9.5,1X,F10.5,1X,F9.5,1X,A16)
      write(100,6010) ttcnst
 6010 format(3X,'TTCNST=',F9.5)  
      end if
C
C----------------------------------------------------------------------
C     Calculate UPX Uber Access Constants
C
      if(ccode(18)) then
      rcon=0.0
      if(tesum(16).gt.0) then
      rcon=log(oesum(16)/tesum(16))
      end if
      econ=acnst(11)
      ncon=econ+adjfct*rcon
      acnst(11)=ncon
      write(26,8018) oesum(16),tesum(16),econ,rcon,ncon,names(16)
 8018 format(2X,'ACNST(11)',F7.0,1X,F7.0,1x,
     *                F9.5,1X,F10.5,1X,F9.5,1x,a16)
      end if
C ------------------------------------------------------------------------
C     Write Air Passenger Constant Values
C
      write(100,6004) (acnst(k),k=1,11)
 6004 format(3X,'ACNST=',F9.5,10(',',F9.5))   
      return
      end
