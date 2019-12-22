C-------------------------------------------------------------------
C        KISS&RIDE ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE KUTL(IX,IZ,JZ,IC,PDSTA,PUTIL,
     *                    STASTA,STAZNE,HTIME,HDIST,IMODE,DRVUTIL,
     *                    UBERUTIL,UBERCOST,STAEGR)
      include 'stadat.inc'
      include 'param.inc'
      include 'mlogitpar.inc'
      include 'dvalues.inc'
      INTEGER*2     IZ,JZ,IC,DC,IMODE
      INTEGER*2     IX,PSTA,PDSTA,INDEX,YINDEX
      REAL*4        PUTIL,HTIME(1000),HDIST(1000)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(7,MAX_STATIONS,MAX_IZONES),
     *              STAEGR(3,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAWALK,DRVUTIL,GORWLK
      REAL*4        STAUTIL,RANVAL
      REAL*4        KWAIT,UBERCOST,UBERACC,UBERUTIL
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
      PUTIL=0.0
C
C DETERMINE IF KISS-N-RIDE INDICATOR = 3
C
      IF(IC.GT.0) THEN
      IF(STADATA(IC,7).EQ.3) THEN
C....................................................................
      IF(DEBUG) THEN
      index=iidint(stadata(ic,7))
      WRITE(26,9026) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               PSTA,PDSTA,STADATA(IC,4),
     *               TYPE(INDEX),PUTIL
 9026 FORMAT(/1X,'KISS&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE          =',I10/
     *       1X,'DESTINATION ZONE     =',I10/
     *       1X,'ACCESS STATION       =',I10/
     *       1X,'EGRESS STATION       =',I10//
     *       1X,'PARKING CAPACITY     =',F10.2,' <---'/
     *       1X,'PARK-N-RIDE INDICATOR=',7X,A3,' <---'/
     *       1X,'TOTAL     UTILITY    =',F10.5)
      END IF
C.....................................................................
      RETURN
      END IF
      END IF
C
C
C CHECK FOR VALID ACCESS/EGRESS STATION VALUES
C
      IF(IC.LT.0.OR.DC.LT.0.OR.
     *   IC.EQ.MAX_STATIONS.OR.DC.EQ.MAX_STATIONS) THEN
C....................................................................
      IF(DEBUG) THEN
	WRITE(26,9024) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),PSTA,PDSTA,PUTIL
 9024 FORMAT(/1X,'KISS&RIDE ACCESS #',I2,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'*************** PATH UNAVAILABLE ****************'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'TOTAL     UTILITY   =',F10.2)
      END IF
C.....................................................................
      RETURN
      END IF
C
C COMPUTE UTILITY VALUES -- KISS&RIDE
C
      STAWALK=STADATA(DC,9)
      GORWLK=0.0
      IF((IEQUIV(PDSTA).EQ.gounion).AND.(STAIND(DC,JZ).EQ.1)) THEN
      GORWLK=KCRWLK/(LSUM1TRN*LSUM2CR*LSUM3CW)
      END IF
      STAUTIL=COEFF(7) * STADATA(IC,13) + COEFF(1) * STADATA(IC,14)
      DRVUTIL=COEFF(2)*HTIME(IC) + COEFF(6)*HDIST(IC)*opcost + STAUTIL
      PUTIL=STASTA(2,IC,DC) + STAZNE(2,DC,JZ) + DRVUTIL + GORWLK +
     *      COEFF(7) * (STADATA(IC,9) + STAWALK)
      IF(TRNEGR) THEN
      PUTIL=STASTA(2,IC,DC) + STAEGR(3,DC,JZ) + DRVUTIL + GORWLK +
     *      COEFF(7) * (STADATA(IC,9) + STAWALK)
      END IF
      WRITE(190,9027) IX
 9027 FORMAT(1X,'KISS&RIDE ACCESS #',I2)
C.....................................................................
      IF(DEBUG) THEN
      IF(TRNEGR) THEN
      WRITE(26,9025) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(PSTA),IEQUIV(PDSTA),
     *               HTIME(IC),HDIST(IC),
     *               STASTA(2,IC,DC),STAEGR(3,DC,JZ),
     *               STADATA(IC,9),STADATA(IC,13),
     *               STADATA(IC,14),STAWALK,
     *               GORWLK,PUTIL
 9025 FORMAT(/1X,'KISS&RIDE ACCESS #',I2,
     *           ' --> RAIL COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'HIGHWAY ACCESS TIME =',F10.2/
     *       1X,'HIGHWAY ACCESS DIST =',F10.2//
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'ACCESS PLATFORM TIME=',F10.5/
     *       1X,'ACCESS STA WALK TIME=',F10.5/
     *       1X,'ACCESS STA IVT  TIME=',F10.5/
     *       1X,'EGRESS PLATFORM TIME=',F10.5/
     *       1X,'WALK EGRESS CONSTANT=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)      
      ELSE
      WRITE(26,9031) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(PSTA),IEQUIV(PDSTA),
     *               HTIME(IC),HDIST(IC),
     *               STASTA(2,IC,DC),STAZNE(2,DC,JZ),
     *               STADATA(IC,9),STADATA(IC,13),
     *               STADATA(IC,14),STAIND(DC,JZ),STAWALK,
     *               GORWLK,PUTIL
 9031 FORMAT(/1X,'KISS&RIDE ACCESS #',I2,
     *           ' --> RAIL COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'HIGHWAY ACCESS TIME =',F10.2/
     *       1X,'HIGHWAY ACCESS DIST =',F10.2//
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'ACCESS PLATFORM TIME=',F10.5/
     *       1X,'ACCESS STA WALK TIME=',F10.5/
     *       1X,'ACCESS STA IVT  TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2'/
     *       1X,'EGRESS PLATFORM TIME=',F10.5/
     *       1X,'WALK EGRESS CONSTANT=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
      END IF
C.....................................................................
C
C COMPUTE UTILITY VALUES -- UBER
C
C..OBTAIN WAIT TIME FROM RANDOM DRAW OF FREQUENCY DISTRIBUTION
C
      IF(UBERTRN) THEN
      CALL UBERCOMP(IZ,HTIME(IC),HDIST(IC),COEFF(3),COEFF(2),
     *              UBERCOST,UBERACC,KWAIT)
      UBERUTIL=STASTA(2,IC,DC) + STAZNE(2,DC,JZ) + GORWLK 
     *        +STAUTIL + UBERACC
      IF(TRNEGR) THEN
      UBERUTIL=STASTA(2,IC,DC) + STAEGR(3,DC,JZ) + GORWLK 
     *        +STAUTIL + UBERACC      
      END IF
C.....................................................................
      IF(DEBUG) THEN
      IF(TRNEGR) THEN
      WRITE(26,9039) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(PSTA),IEQUIV(PDSTA),
     *               HDIST(IC),KWAIT,UBERCOST,
     *               STASTA(2,IC,DC),STAEGR(3,DC,JZ),
     *               UBERACC,UBERUTIL
 9039 FORMAT(/1X,'UBER ACCESS #',I2,
     *           ' --> RAIL COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'ZONE-TO-STATION DIST=',F10.2/
     *       1X,'UBER WAIT TIME      =',F10.2/
     *       1X,'UBER COST (DOLLARS) =',F10.2//
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'UBER ACCESS UTILITY =',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      ELSE
      WRITE(26,9029) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(PSTA),IEQUIV(PDSTA),
     *               HDIST(IC),KWAIT,UBERCOST,
     *               STASTA(2,IC,DC),STAZNE(2,DC,JZ),
     *               UBERACC,
     *               STAIND(DC,JZ),UBERUTIL
 9029 FORMAT(/1X,'UBER ACCESS #',I2,
     *           ' --> RAIL COMPUTATION -- ',A13/
     *       1X,'--------------------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10/
     *       1X,'EGRESS STATION      =',I10//
     *       1X,'ZONE-TO-STATION DIST=',F10.2/
     *       1X,'UBER WAIT TIME      =',F10.2/
     *       1X,'UBER COST (DOLLARS) =',F10.2//
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'UBER ACCESS UTILITY =',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2'/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
      END IF
C.....................................................................
      ELSE
      UBERUTIL=0.0
      UBERCOST=0.0
      END IF
      RETURN
      END
