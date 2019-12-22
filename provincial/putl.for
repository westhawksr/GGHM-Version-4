C-------------------------------------------------------------------
C        PARK&RIDE ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE PUTL(IX,IZ,JZ,IC,PDSTA,PUTIL,
     *                 STASTA,STAZNE,HTIME,HDIST,imode,PNRRAT,PNRRAT2,
     *                 hwydst,drvutil,staegr)
      include 'stadat.inc'
      include 'param.inc'
      include 'mlogitpar.inc'
      include 'dvalues.inc'
      INTEGER*2     IZ,JZ,IC,DC,IMODE
      INTEGER*2     IX,PSTA,PDSTA,index
      REAL*4        PUTIL,HTIME(1000),HDIST(1000)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(7,MAX_STATIONS,MAX_IZONES),
     *              STAEGR(3,MAX_STATIONS,MAX_IZONES)
      REAL*4        PNRRAT,PNRRAT2,hwydst(4000),drvutil
      REAL*4        stawalk,pcost,GORWLK
      character*3   type(3)
      CHARACTER*13  NAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   '/
      data          type/'PNR','KNR','   '/
C
C CALCULATE TOTAL UTILITY VALUE
C
      PSTA=IC+MAX_IZONES
      DC=PDSTA-MAX_IZONES
C
C DETERMINE IF PARKING AVAILABLE AT STATION
C
      PUTIL=0.0
      PNRRAT=0.0
      PRNRAT2=0.0
      IF(IC.GT.0) THEN
      IF((STADATA(IC,3).LE.0.0).OR.(STADATA(IC,7).NE.1)) THEN
C....................................................................
      IF(DEBUG) THEN
      index=iidint(stadata(ic,7))
      WRITE(26,9026) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               PSTA,PDSTA,STADATA(IC,4),
     *               type(index),PUTIL
 9026 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE          =',I10/
     *       1X,'DESTINATION ZONE     =',I10/
     *       1X,'ACCESS STATION       =',I10/
     *       1X,'EGRESS STATION       =',I10//
     *       1X,'PARKING CAPACITY     =',F10.2,' <---'/
     *       1X,'PARK-N-RIDE INDICATOR=',7x,a3,' <---'/
     *       1X,'TOTAL     UTILITY    =',F10.5)
      END IF
C.....................................................................
      RETURN
      END IF
      END IF
C
C CHECK FOR VALID ACCESS/EGRESS STATION VALUES
C
      IF(IC.LT.0.OR.DC.LT.0.OR.
     *   IC.EQ.MAX_STATIONS.OR.DC.EQ.MAX_STATIONS) THEN
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9024) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               PSTA,PDSTA,PUTIL
 9024 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'*********** PATH UNAVAILABLE *******************'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
C.....................................................................
      RETURN
      END IF
C
C COMPUTED UTILITY VALUES
C
C...MODEL UTILITY VALUE
      IF(PEAK) THEN
      PCOST=(STADATA(IC,1)*100.0)/2.0
      ELSE
      PCOST=(STADATA(IC,10)*100.0)/2.0
      END IF
      STAWALK=STADATA(DC,9)
      GORWLK=0.0
      IF((IEQUIV(PDSTA).EQ.gounion).AND.(STAIND(DC,JZ).EQ.1)) THEN
      GORWLK=KCRWLK/(LSUM1TRN*LSUM2CR*LSUM3CW)
      END IF
      DRVUTIL=COEFF(2)*HTIME(IC)
     *     + COEFF(6)*HDIST(IC)*opcost
     *     + COEFF(8)* STADATA(IC,4)
     *     + COEFF(6)* PCOST
     *     + COEFF(7) * STADATA(IC,11)
     *     + COEFF(1) * STADATA(IC,12) 
      PUTIL=STASTA(2,IC,DC) + STAZNE(2,DC,JZ) + DRVUTIL + GORWLK +
     *      COEFF(7) * (STADATA(IC,9) + STAWALK)
      IF(TRNEGR) THEN
      PUTIL=STASTA(2,IC,DC) + STAEGR(3,DC,JZ) + DRVUTIL + GORWLK +
     *      COEFF(7) * (STADATA(IC,9) + STAWALK) 
      END IF
      IF(STASTA(3,IC,DC).GT.0) THEN
      PNRRAT=HTIME(IC)/STASTA(3,IC,DC)
      PUTIL=PUTIL+CCRPNRT*PNRRAT
      END IF      
      IF(HDIST(IC).GT.0.AND.HWYDST(JZ).GT.0) THEN
      PNRRAT2=HDIST(IC)/HWYDST(JZ)
      PUTIL=PUTIL+CCRPNRD*PNRRAT2
      END IF
C....................................................................
      IF(DEBUG) THEN
      IF(TRNEGR) THEN
      WRITE(26,9027) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(PSTA),IEQUIV(PDSTA),
     *               HTIME(IC),HDIST(IC),HWYDST(JZ),STADATA(IC,4),
     *               STADATA(IC,1),STADATA(IC,10),
     *         STASTA(2,IC,DC),STAEGR(3,DC,JZ),
     *         STASTA(3,IC,DC),PNRRAT,PNRRAT2,STADATA(IC,11),
     *         STADATA(IC,12),STADATA(IC,9),STADATA(DC,9),
     *         GORWLK,PUTIL
 9027 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'HIGHWAY ACCESS TIME =',F10.2/
     *       1X,'HIGHWAY ACCESS DIST =',F10.2/
     *       1X,'HIGHWAY DEST DIST   =',F10.2/
     *       1X,'PARKING CAPACITY    =',F10.2/
     *       1X,'DAILY   PARKING COST=',F10.2/
     *       1X,'OFFPEAK PARKING COST=',F10.2/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'RAIL IN VEHICLE TIME=',F10.5/
     *       1X,'DRIVE/IVT RATIO     =',F10.5/
     *       1X,'DRIVE DIST/TOT DIST =',F10.5/
     *       1X,'ACCESS STA WALK TIME=',F10.5/
     *       1X,'ACCESS STA IVT  TIME=',F10.5/
     *       1X,'ACCESS STA PLAT TIME=',F10.5/
     *       1X,'EGRESS STA PLAT TIME=',F10.5/
     *       1X,'WALK EGRESS CONSTANT=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      ELSE
      WRITE(26,9025) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(PSTA),IEQUIV(PDSTA),
     *               HTIME(IC),HDIST(IC),HWYDST(JZ),STADATA(IC,4),
     *               STADATA(IC,1),STADATA(IC,10),
     *         STASTA(2,IC,DC),STAZNE(2,DC,JZ),
     *         STASTA(3,IC,DC),PNRRAT,PNRRAT2,STADATA(IC,11),
     *         STADATA(IC,12),STADATA(IC,9),STADATA(DC,9),
     *         STAIND(DC,JZ),GORWLK,PUTIL
 9025 FORMAT(/1X,'PARK&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'HIGHWAY ACCESS TIME =',F10.2/
     *       1X,'HIGHWAY ACCESS DIST =',F10.2/
     *       1X,'HIGHWAY DEST DIST   =',F10.2/
     *       1X,'PARKING CAPACITY    =',F10.2/
     *       1X,'DAILY   PARKING COST=',F10.2/
     *       1X,'OFFPEAK PARKING COST=',F10.2/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'RAIL IN VEHICLE TIME=',F10.5/
     *       1X,'DRIVE/IVT RATIO     =',F10.5/
     *       1X,'DRIVE DIST/TOT DIST =',F10.5/
     *       1X,'ACCESS STA WALK TIME=',F10.5/
     *       1X,'ACCESS STA IVT  TIME=',F10.5/
     *       1X,'ACCESS STA PLAT TIME=',F10.5/
     *       1X,'EGRESS STA PLAT TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2,DRIVE EGRESS=3'/
     *       1X,'WALK EGRESS CONSTANT=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
      END IF
C.....................................................................
      WRITE(190,9021) PUTIL
 9021 FORMAT(' PUTIL=',F10.5)
      RETURN
      END
