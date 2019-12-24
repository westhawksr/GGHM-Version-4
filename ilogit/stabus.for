C-------------------------------------------------------------------
C       WALK AND BUS ACCESS TO STATION SELECTION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STABUS(IZ,BSTA,BUTL,TWALK,BTXFER,imode,SKIM,BUSMODE,
     *                   WDIST,WSTA,EWALK)
       include 'stadat.inc'
       include 'param.inc'
       include 'dvalues.inc'
       include 'ilogitpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2    IMODE,IZ,IC,SC,SC2,IC1,IC2
      INTEGER*2    BSTA(2,2),BTXFER(2,2),BUSMODE(2,3,2)
      INTEGER*2    WSTA(2,5),SC3,SC4,SC5
      integer*2    unit,orgzn,destzn,type,bunit
	    real*4       frow(max_izones)
	    real*4       arow(max_stations,max_izones)
	    real*4       srow(max_stations,max_stations)
      REAL*4       ACCUTL,BUSUTL
      REAL*4       BDIST(2,2),BUTL(2,2),TWALK(2,2),EWALK(2,2)
      REAL*4       INVEH(max_stations),WALKACC(max_stations),
     *             WALKEGR(max_stations),
     *             FARE(max_stations),TRANSF(max_stations),
     *             WAIT1(max_stations),WAIT2(max_stations)
      REAL*4       INVEHL(max_stations),INVEHG(max_stations),
     *             INVEHS(max_stations),DIRWALK(max_stations),
     *             INVEHT(max_stations),TTCFAC(max_stations),
     *             GORFAC(max_stations)
      REAL*4       WALKTFR(max_stations),SKIM(2,13)
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
C
C OBTAIN ACCESS PORTION OF PATH DATA
C
      type=3
      orgzn=iz
      destzn=max_zones
      bunit=70
      if(imode.ge.2) bunit=10
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
      if(imode.eq.1) then
      unit=65
      call mfread(unit,type,orgzn,destzn,frow,srow,fare,arow)      
      unit=66
      call mfread(unit,type,orgzn,destzn,frow,srow,ttcfac,arow)   
      unit=67
      call mfread(unit,type,orgzn,destzn,frow,srow,gorfac,arow)
      else
      unit=62
      call mfread(unit,type,orgzn,destzn,frow,srow,fare,arow)      
      unit=63
      call mfread(unit,type,orgzn,destzn,frow,srow,ttcfac,arow)   
      unit=64
      call mfread(unit,type,orgzn,destzn,frow,srow,gorfac,arow)
      end if      
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
      unit=bunit+9
      if(imode.ge.2) unit=32
      call mfread(unit,type,orgzn,destzn,frow,srow,walkacc,arow)
C...WALK TIME TRANSFER
      unit=bunit+4
      call mfread(unit,type,orgzn,destzn,frow,srow,walktfr,arow)
C...TTC SUBWAY IN-VEHICLE TIME
      if(imode.eq.1) then
      unit=80
      call mfread(unit,type,orgzn,destzn,frow,srow,inveht,arow)
      end if
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
      INVEH(SC)=INVEHL(SC)+INVEHG(SC)+INVEHS(SC)
      IF(IMODE.EQ.1) INVEH(SC)=INVEH(SC)+INVEHT(SC)
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
     *       COEFF(7)*(WALKTFR(SC)*WALKACC(SC)+WALKEGR(SC))
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
      SKIM(1,11)=INVEHT(IC-MAX_IZONES)
      SKIM(1,12)=TTCFAC(IC-MAX_IZONES)
      SKIM(1,13)=GORFAC(IC-MAX_IZONES)
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
      SKIM(2,11)=INVEHT(IC-MAX_IZONES)
      SKIM(2,12)=TTCFAC(IC-MAX_IZONES)
      SKIM(2,13)=GORFAC(IC-MAX_IZONES)
      END IF
C
C DETERMINE IF BUS MODES ARE USED
C
      IF(BSTA(IMODE,1).GT.0) THEN
      IF(INVEHL(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,1,1)=1
      IF(INVEHG(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,2,1)=1
      IF(INVEHS(BSTA(IMODE,1)-MAX_IZONES).GT.0) BUSMODE(IMODE,3,1)=1
      END IF
      IF(BSTA(IMODE,2).GT.0) THEN
      IF(INVEHL(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,1,2)=1
      IF(INVEHG(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,2,2)=1
      IF(INVEHS(BSTA(IMODE,2)-MAX_IZONES).GT.0) BUSMODE(IMODE,3,2)=1
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
     * IEQUIV(BSTA(imode,1)),STANAME(SC),(SKIM(1,K1),K1=1,13),
     * BTXFER(IMODE,1),
     * BUTL(imode,1),
     * IEQUIV(BSTA(imode,2)),STANAME(SC2),
     * (SKIM(2,K2),K2=1,13),
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
     *       1X,'  TTC SUBWAY IN-VEHICLE TIME=',F10.3/
     *       1X,'          TTC FRACTION VALUE=',F10.5/
     *       1X,'      GO RAIL FRACTION VALUE=',F10.5/
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
     *       1X,'  TTC SUBWAY IN-VEHICLE TIME=',F10.3/
     *       1X,'          TTC FRACTION VALUE=',F10.5/
     *       1X,'      GO RAIL FRACTION VALUE=',F10.5/
     *       1X,'         NUMBER OF TRANSFERS=',I10/
     *       1X,'            ACCESS  UTILITY =',F10.3/)
      END IF
C....................................................................
      RETURN
      END
