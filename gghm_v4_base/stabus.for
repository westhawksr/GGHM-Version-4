C-------------------------------------------------------------------
C       WALK AND BUS ACCESS TO STATION SELECTION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STABUS(IZ,BSTA,BUTL,TWALK,BTXFER,imode,SKIM,BUSMODE,
     *                   WDIST,WSTA,EWALK)
       include 'stadat.inc'
       include 'param.inc'
       include 'dvalues.inc'
       include 'mlogitpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2    IMODE,IZ,IC,SC,SC2,IC1,IC2
      INTEGER*2    BSTA(2,2),BTXFER(2,2),BUSMODE(2,5,2)
      INTEGER*2    WSTA(2,5),SC3,SC4,SC5
      integer*2    unit,orgzn,destzn,type,bunit
	    real*4       frow(4000)
	    real*4       arow(1000,4000)
	    real*4       srow(1000,1000)
      REAL*4       ACCUTL,BUSUTL
      REAL*4       BDIST(2,2),BUTL(2,2),TWALK(2,2),EWALK(2,2)
      REAL*4       INVEH(1000),WALKACC(1000),
     *             WALKEGR(1000),
     *             FARE(1000),TRANSF(1000),
     *             WAIT1(1000),WAIT2(1000)
      REAL*4       INVEHL(1000),INVEHG(1000),
     *             INVEHS(1000),DIRWALK(1000),
     *             INVEHN(1000),
     *             INVEHP(1000),
     *             TTCFAC(1000),
     *             GORFAC(1000)
      REAL*4       NCAPAC(1000),
     *             EAWT(1000),
     *             CROWD(1000),
     *             LUNREL(1000)
      REAL*4       WALKTFR(1000),SKIM(2,18)
      REAL*4       WDIST(2,5)
      CHARACTER*13 NAME(2)
      DATA         NAME/'GO Rail      ',
     *                  'TTC Subway   '/
C
C INITIALIZE CHOICE VARIABLES
C
      BDIST(imode,1)=-99999.9
      BDIST(imode,2)=-99999.9
      BUTL(imode,1)=0.0
      BUTL(imode,2)=0.0
      BSTA(imode,1)=0
      BSTA(imode,2)=0
      BTXFER(IMODE,1)=0
      BTXFER(IMODE,2)=0
      WDIST(imode,1)=99999.9
      WSTA(imode,1)=0
      WDIST(imode,2)=99999.9
      WSTA(imode,2)=0
      WDIST(imode,3)=99999.9
      WSTA(imode,3)=0
      WDIST(imode,4)=999999.9
      WSTA(imode,4)=0
      WDIST(imode,5)=999999.9
      WSTA(imode,5)=0
      SKIM=0.0
      INVEHT=0.0
      DO 10 K1=1,3
      DO 10 K2=1,2
      BUSMODE(IMODE,K1,K2)=0
   10 CONTINUE
      ncapac=0.0
      eawt=0.0
      crowd=0.0
      lunrel=0.0
C
C OBTAIN ACCESS PORTION OF PATH DATA
C
      type=3
      orgzn=iz
      destzn=max_zones
      bunit=10
C.....1ST WAIT TIME
      unit=bunit+5
      call mfread(unit,type,orgzn,destzn,frow,srow,wait1,arow)
C.....TOTAL WAIT TIME
      unit=bunit+6
      call mfread(unit,type,orgzn,destzn,frow,srow,wait2,arow)
C.....NUMBER OF BOARDINGS
      unit=bunit+7
      call mfread(unit,type,orgzn,destzn,frow,srow,transf,arow)
C.....FARE
      unit=62
      call mfread(unit,type,orgzn,destzn,frow,srow,fare,arow)      
      unit=63
      call mfread(unit,type,orgzn,destzn,frow,srow,ttcfac,arow)   
      unit=64
      call mfread(unit,type,orgzn,destzn,frow,srow,gorfac,arow)  
C...LOCAL BUS IN-VEHICLE TIME
      unit=bunit+1
      call mfread(unit,type,orgzn,destzn,frow,srow,invehl,arow)
C...EXPRESS BUS IN-VEHICLE TIME
      unit=bunit+2
      call mfread(unit,type,orgzn,destzn,frow,srow,invehg,arow)
C...STREETCAR IN-VEHICLE TIME
      unit=bunit+3
      call mfread(unit,type,orgzn,destzn,frow,srow,invehs,arow)
C....CENTROID WALK EGRESS
      unit=bunit+8
      call mfread(unit,type,orgzn,destzn,frow,srow,walkegr,arow)
C....CENTROID WALK ACCESS
      unit=32
      call mfread(unit,type,orgzn,destzn,frow,srow,walkacc,arow)
C...WALK TIME TRANSFER
      unit=bunit+4
      call mfread(unit,type,orgzn,destzn,frow,srow,walktfr,arow)
C...RAPID BUS IN-VEHICLE TIME
      unit=65
      call mfread(unit,type,orgzn,destzn,frow,srow,invehn,arow)
C...PREMIUM BUS IN-VEHICLE TIME
      unit=66
      call mfread(unit,type,orgzn,destzn,frow,srow,invehp,arow)
C...CAPACITY, CROWDING & RELIABILITY VARAIBLES
      IF(CCR) THEN
C.....NODE CAPACITY PENALTY
      unit=108
      call mfread(unit,type,orgzn,destzn,frow,srow,ncapac,arow)
C.....EXTRA ADDED WAIT TIME
      unit=109
      call mfread(unit,type,orgzn,destzn,frow,srow,eawt,arow)      
C.....LINK CROWDING TIME
      unit=110
      call mfread(unit,type,orgzn,destzn,frow,srow,crowd,arow)
C.....LINK UNRELIABILITY
      unit=111
      call mfread(unit,type,orgzn,destzn,frow,srow,lunrel,arow)      
      END IF
C
C DIRECT WALK TO STATION
C
      unit=19
      call mfread(unit,type,orgzn,destzn,frow,srow,dirwalk,arow)
C ------------------------------------------------------------------------
C WALK ACCESS TO STATIONS
C -----------------------------------------------------------------------
      DO 210 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GOTO 210
      IF(STADATA(SC,6).Lt.1.0) GOTO 210
      IF(DIRWALK(SC).LE.0) GO TO 210
      XDIST=(3.0*1.60934*DIRWALK(SC))/60.0
C
C..EVALUATE FOR FIVE CLOSEST STATIONS
C
      IF(XDIST.LT.WDIST(IMODE,1)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=WDIST(IMODE,2)
         WSTA(IMODE,3)=WSTA(IMODE,2)
         WDIST(imode,2)=WDIST(imode,1)
         WSTA(imode,2)=WSTA(imode,1)
         WDIST(imode,1)=XDIST
         WSTA(imode,1)=IC
         GO TO 210
      END IF
      IF(XDIST.LT.WDIST(IMODE,2)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=WDIST(IMODE,2)
         WSTA(IMODE,3)=WSTA(IMODE,2)
         WDIST(imode,2)=XDIST
         WSTA(imode,2)=IC
         GO TO 210
      END IF
      IF(XDIST.LT.WDIST(IMODE,3)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=WDIST(IMODE,3)
         WSTA(IMODE,4)=WSTA(IMODE,3)
         WDIST(IMODE,3)=XDIST
         WSTA(IMODE,3)=IC
         GO TO 210
      END IF
      IF(XDIST.LT.WDIST(IMODE,4)) THEN
         WDIST(IMODE,5)=WDIST(IMODE,4)
         WSTA(IMODE,5)=WSTA(IMODE,4)
         WDIST(IMODE,4)=XDIST
         WSTA(IMODE,4)=IC
         GO TO 210
      END IF
      IF(XDIST.LT.WDIST(IMODE,5)) THEN
         WDIST(IMODE,5)=XDIST
         WSTA(IMODE,5)=IC
      END IF
  210 CONTINUE
      IF(WSTA(IMODE,1).LE.0) WDIST(IMODE,1)=0.0
      IF(WSTA(IMODE,2).LE.0) WDIST(IMODE,2)=0.0
      IF(WSTA(IMODE,3).LE.0) WDIST(IMODE,3)=0.0
      IF(WSTA(IMODE,4).LE.0) WDIST(IMODE,4)=0.0
      IF(WSTA(IMODE,5).LE.0) WDIST(IMODE,5)=0.0  
      IF(WSTA(IMODE,1).LE.0) WSTA(IMODE,1)=MAX_ZONES
      IF(WSTA(IMODE,2).LE.0) WSTA(IMODE,2)=MAX_ZONES
      IF(WSTA(IMODE,3).LE.0) WSTA(IMODE,3)=MAX_ZONES
      IF(WSTA(IMODE,4).LE.0) WSTA(IMODE,4)=MAX_ZONES
      IF(WSTA(IMODE,5).LE.0) WSTA(IMODE,5)=MAX_ZONES
      SC=WSTA(imode,1)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS 
      SC2=WSTA(imode,2)-MAX_IZONES
      IF(SC2.LT.0) SC2=MAX_STATIONS 
      SC3=WSTA(IMODE,3)-MAX_IZONES
      IF(SC3.LT.0) SC3=MAX_STATIONS
      SC4=WSTA(IMODE,4)-MAX_IZONES
      IF(SC4.LT.0) SC4=MAX_STATIONS
      SC5=WSTA(IMODE,5)-MAX_IZONES
      IF(SC5.LT.0) SC5=MAX_STATIONS
C................................................................... 
      IF(DEBUG) THEN
      WRITE(26,9007) NAME(IMODE),IEQUIV(IZ),IEQUIV(WSTA(imode,1)),
     *               STANAME(SC),WDIST(imode,1),
     *               IEQUIV(WSTA(imode,2)),STANAME(SC2),WDIST(imode,2),
     *               IEQUIV(WSTA(imode,3)),STANAME(SC3),WDIST(imode,3),
     *               IEQUIV(WSTA(imode,4)),STANAME(SC4),WDIST(imode,4),
     *               IEQUIV(WSTA(imode,5)),STANAME(SC5),WDIST(imode,5)
 9007 FORMAT(//1X,'Candidate Station Selection (Walk Access) -- ',a13/
     *       1X,'-------------------------------------------'/
     *       1X,'ORIGIN   ZONE   NUMBER  =',I9/  
     *       1X,'CLOSEST STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'SECOND  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'THIRD   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FOURTH  STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3/
     *       1X,'FIFTH   STATION NUMBER  =',I9,2X,A29/
     *       1X,'                DISTANCE=',F9.3)
      END IF   
C----------------------------------------------------------------------------
C FEEDER BUS ACCESS TO STATIONS
C LOOP THROUGH STATIONS
C----------------------------------------------------------------------------
      DO 110 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
C
C COMPUTE TOTAL IN-VEHICLE TIME
C
      INVEH(SC)=INVEHL(SC)+INVEHG(SC)+INVEHS(SC)+
     *          INVEHN(SC)+INVEHP(SC)
C
C CONVERT BOARDINGS TO TRANSFERS
C
      TRANSF(SC)=TRANSF(SC)-1.0
C...........................................................................
       IF(STANUM(SC).NE.IMODE) GOTO 110
       IF(STADATA(SC,6).Lt.1.0) GOTO 110
C
C CHECK FOR NO BUS IN-VEHICLE TIME
C
      IF(INVEH(SC).LE.0.0) THEN
      IF(SDETAIL) WRITE(26,8003) IEQUIV(IZ),IEQUIV(IC),
     *                           INVEH(SC),WALKACC(SC),
     *                           WALKEGR(SC)
 8003 FORMAT(' IZ=',I4,
     *       ' STATION=',I4,' INVEH=',F6.2,' WALKACC=',F6.2,
     *       ' WALKEGR=',F6.2)
      GO TO 110
      END IF
C
C COMPUTE ACCESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      BUSUTL=COEFF(1)*INVEH(SC) + COEFF(3)*WAIT1(SC) + 
     *       COEFF(4)*(WAIT2(SC)-WAIT1(SC)) + 
     *       COEFF(5)*(TRANSF(SC)) +
     *       COEFF(7)*(WALKTFR(SC)*WALKACC(SC)+WALKEGR(SC)) +
     *       COEFF(20)*NCAPAC(sc)/(LSUM2CR*LSUM1TRN*LSUM3CW) + 
     *       COEFF(21)*EAWT(sc)/(LSUM2CR*LSUM1TRN*LSUM3CW) +
     *       COEFF(22)*CROWD(sc)/(LSUM2CR*LSUM1TRN*LSUM3CW) + 
     *       COEFF(23)*LUNREL(sc)/(LSUM2CR*LSUM1TRN*LSUM3CW)
C.........................................................................
      IF(SDETAIL) THEN
      WRITE(26,8002) IEQUIV(IC),BUSUTL
 8002 FORMAT( ' STATION=',I4,' MODEL UTIL=',F10.2)
      END IF
C........................................................................
C
C..EVALUATE FOR TWO "BEST" STATIONS
C
      IF(BUSUTL.GT.BDIST(imode,2)) THEN
       IF(BUSUTL.GT.BDIST(imode,1)) THEN
       BDIST(imode,2)=BDIST(imode,1)
       BUTL(imode,2)=BUTL(imode,1)
       TWALK(imode,2)=TWALK(imode,1)
       BSTA(imode,2)=BSTA(imode,1)
       BDIST(imode,1)=BUSUTL
       BUTL(imode,1)=BUSUTL
       TWALK(imode,1)=WALKACC(SC)
       EWALK(imode,1)=WALKEGR(SC)
       BSTA(imode,1)=IC
       ELSE
       BDIST(imode,2)=BUSUTL
       BUTL(imode,2)=BUSUTL
       TWALK(imode,2)=WALKACC(SC)
       EWALK(imode,2)=WALKEGR(SC)
       BSTA(imode,2)=IC
       END IF
      END IF
C....................................................................
  110 CONTINUE
      IF(BSTA(IMODE,1).GT.0) 
     *      BTXFER(IMODE,1)=IFIX(TRANSF(BSTA(IMODE,1)-MAX_IZONES))
      IF(BSTA(IMODE,2).GT.0) 
     *      BTXFER(IMODE,2)=IFIX(TRANSF(BSTA(IMODE,2)-MAX_IZONES))
C
C STORE SKIM INFORMATION
C
      IF(BSTA(IMODE,1).GT.0) THEN
      IC=BSTA(IMODE,1)
      SKIM(1,1)=INVEH(IC-MAX_IZONES)
      SKIM(1,2)=INVEHL(IC-MAX_IZONES)
      SKIM(1,3)=WAIT1(IC-MAX_IZONES)
      SKIM(1,4)=WAIT2(IC-MAX_IZONES)
      SKIM(1,5)=WALKTFR(IC-MAX_IZONES)
      SKIM(1,6)=FARE(IC-MAX_IZONES)*100.0
      SKIM(1,7)=WALKACC(IC-MAX_IZONES)
      SKIM(1,8)=INVEHG(IC-MAX_IZONES)
      SKIM(1,9)=INVEHS(IC-MAX_IZONES)
      SKIM(1,10)=WALKEGR(IC-MAX_IZONES)
      SKIM(1,11)=INVEHN(IC-MAX_IZONES)
      SKIM(1,12)=TTCFAC(IC-MAX_IZONES)
      SKIM(1,13)=GORFAC(IC-MAX_IZONES)
      SKIM(1,14)=INVEHP(IC-MAX_IZONES)
      SKIM(1,15)=NCAPAC(IC-MAX_IZONES)
      SKIM(1,16)=EAWT(IC-MAX_IZONES)
      SKIM(1,17)=CROWD(IC-MAX_IZONES)
      SKIM(1,18)=LUNREL(IC-MAX_IZONES)
      END IF
C....
      IF(BSTA(IMODE,2).GT.0) THEN
      IC=BSTA(IMODE,2)
      SKIM(2,1)=INVEH(IC-MAX_IZONES)
      SKIM(2,2)=INVEHL(IC-MAX_IZONES)
      SKIM(2,3)=WAIT1(IC-MAX_IZONES)
      SKIM(2,4)=WAIT2(IC-MAX_IZONES)
      SKIM(2,5)=WALKTFR(IC-MAX_IZONES)
      SKIM(2,6)=FARE(IC-MAX_IZONES)*100.0
      SKIM(2,7)=WALKACC(IC-MAX_IZONES)
      SKIM(2,8)=INVEHG(IC-MAX_IZONES)
      SKIM(2,9)=INVEHS(IC-MAX_IZONES)  
      SKIM(2,10)=WALKEGR(IC-MAX_IZONES) 
      SKIM(2,11)=INVEHN(IC-MAX_IZONES)
      SKIM(2,12)=TTCFAC(IC-MAX_IZONES)
      SKIM(2,13)=GORFAC(IC-MAX_IZONES)
      SKIM(2,14)=INVEHP(IC-MAX_IZONES)
      SKIM(2,15)=NCAPAC(IC-MAX_IZONES)
      SKIM(2,16)=EAWT(IC-MAX_IZONES)
      SKIM(2,17)=CROWD(IC-MAX_IZONES)
      SKIM(2,18)=LUNREL(IC-MAX_IZONES)
      END IF
C
C DETERMINE IF BUS MODES ARE USED
C
      IF(BSTA(IMODE,1).GT.0) THEN
      IF(INVEHL(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,1,1)=1
      IF(INVEHG(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,2,1)=1
      IF(INVEHS(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,3,1)=1
      IF(INVEHN(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,4,1)=1
      IF(INVEHP(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,5,1)=1
      END IF
      IF(BSTA(IMODE,2).GT.0) THEN
      IF(INVEHL(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,1,2)=1
      IF(INVEHG(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,2,2)=1
      IF(INVEHS(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,3,2)=1
      IF(INVEHN(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,4,2)=1
      IF(INVEHP(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,5,2)=1
      END IF
C...................................................................
      IF(DEBUG) THEN
      SC=BSTA(imode,1)-MAX_IZONES
      IF(SC.LT.0) THEN
      IC1=MAX_STATIONS
      ELSE
      IC1=BSTA(IMODE,1)-MAX_IZONES
      END IF
      IF(SC.LT.0) SC=MAX_STATIONS
      SC2=BSTA(imode,2)-MAX_IZONES
      IF(SC2.LT.0) THEN
      IC2=MAX_STATIONS
      ELSE
      IC2=BSTA(IMODE,2)-MAX_IZONES
      END IF
      IF(SC2.LT.0) SC2=MAX_STATIONS
      WRITE(26,9017) NAME(IMODE), 
     * IEQUIV(BSTA(imode,1)),STANAME(SC),(SKIM(1,K1),K1=1,18),
     * BTXFER(IMODE,1),
     * BUTL(imode,1),
     * IEQUIV(BSTA(imode,2)),STANAME(SC2),
     * (SKIM(2,K2),K2=1,18),
     *  BTXFER(IMODE,2),
     * BUTL(imode,2)
 9017 FORMAT(//1X,'Candidate Station Selection (Bus Access) -- ',a13/
     *       1X,'------------------------------------------'/
     *       1X,'CLOSEST     STATION NUMBER  =',I10,2X,A29/
     *       1X,'       TOTAL IN-VEHICLE TIME=',F10.3/
     *       1X,'   LOCAL BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'             FIRST WAIT TIME=',F10.3/
     *       1X,'             TOTAL WAIT TIME=',F10.3/
     *       1X,'          TRANSFER WALK TIME=',F10.3/
     *       1X,'                        FARE=',F10.3/
     *       1X,'            ACCESS WALK TIME=',F10.3/
     *       1X,'      GO BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'   STREETCAR IN-VEHICLE TIME=',F10.3/
     *       1X,'            EGRESS WALK TIME=',F10.3/
     *       1X,'   RAPID BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'          TTC FRACTION VALUE=',F10.5/
     *       1X,'      GO RAIL FRACTION VALUE=',F10.5/
     *       1X,'     PREMIUM IN-VEHICLE TINE=',F10.5//
     *       1X,'   NODE CAPACITY PENALTY    =',F10.3/
     *       1X,'   EXTRA ADDED WAIT TIME    =',F10.3/
     *       1X,'   CROWDING TIME            =',F10.3/
     *       1X,'   LINK UNRELIABILITY TIME  =',F10.3//
     *       1X,'         NUMBER OF TRANSFERS=',I10/
     *       1X,'            ACCESS  UTILITY =',F10.3//
     *       1X,'SECOND      STATION NUMBER  =',I10,2X,A29/
     *       1X,'       TOTAL IN-VEHICLE TIME=',F10.3/
     *       1X,'   LOCAL BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'             FIRST WAIT TIME=',F10.3/
     *       1X,'             TOTAL WAIT TIME=',F10.3/
     *       1X,'          TRANSFER WALK TIME=',F10.3/
     *       1X,'                        FARE=',F10.3/
     *       1X,'            ACCESS WALK TIME=',F10.3/
     *       1X,'      GO BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'   STREETCAR IN-VEHICLE TIME=',F10.3/
     *       1X,'            EGRESS WALK TIME=',F10.3/
     *       1X,'   RAPID BUS IN-VEHICLE TIME=',F10.3/
     *       1X,'          TTC FRACTION VALUE=',F10.5/
     *       1X,'      GO RAIL FRACTION VALUE=',F10.5/
     *       1X,'     PREMIUM IN-VEHICLE TINE=',F10.5//
     *       1X,'   NODE CAPACITY PENALTY    =',F10.3/
     *       1X,'   EXTRA ADDED WAIT TIME    =',F10.3/
     *       1X,'   CROWDING TIME            =',F10.3/
     *       1X,'   LINK UNRELIABILITY TIME  =',F10.3//
     *       1X,'         NUMBER OF TRANSFERS=',I10/
     *       1X,'            ACCESS  UTILITY =',F10.3/)
      END IF
C....................................................................
C
C     OPEN ZONE-TO-STATION & STATION-TO-ZONE FILE NAMES
C
      close(11,status='keep')
      close(12,status='keep')
      close(13,status='keep')
      close(14,status='keep')
      close(15,status='keep')
      close(16,status='keep')
      close(17,status='keep')
      close(18,status='keep')
      close(19,status='keep')
      close(32,status='keep')
      close(62,status='keep')
      close(63,status='keep')
      close(64,status='keep')
      close(65,status='keep')
      close(66,status='keep')
      if(ccr) then
      close(108,status='keep')
      close(109,status='keep')
      close(110,status='keep')
      close(111,status='keep')
      end if      
      open(11,file=fzsta(1),
     *       status='old',form='binary')
      open(12,file=fzsta(2),
     *       status='old',form='binary')
      open(13,file=fzsta(3),
     *       status='old',form='binary')
      open(14,file=fzsta(4),
     *       status='old',form='binary')
      open(15,file=fzsta(5),
     *       status='old',form='binary')
      open(16,file=fzsta(6),
     *       status='old',form='binary')
      open(17,file=fzsta(7),
     *       status='old',form='binary')
      open(18,file=fzsta(8),
     *       status='old',form='binary')
      open(19,file=fwlksta,
     *       status='old',form='binary')
      open(32,file=fzsta(9),
     *       status='old',form='binary')
      open(65,file=fzsta(10),
     *       status='old',form='binary')
      open(66,file=fzsta(11),
     *       status='old',form='binary')
C.....FARE MATRICES
      open(62,file=fzstafr(1),
     *       status='old',form='binary')
      open(63,file=fzstafr(2),
     *       status='old',form='binary')
      open(64,file=fzstafr(3),
     *       status='old',form='binary')
      if(ccr) then
      open(108,file=fzsta(12),
     *       status='old',form='binary')
      open(109,file=fzsta(13),
     *       status='old',form='binary')
      open(110,file=fzsta(14),
     *       status='old',form='binary')
      open(111,file=fzsta(15),
     *       status='old',form='binary')
      end if
      RETURN
      END
