C-------------------------------------------------------------------
C        KISS&RIDE ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE KUTL(IX,IZ,JZ,IC,PDSTA,PUTIL,
     *                    STASTA,STAZNE,HTIME,HDIST,IMODE)
      include 'stadat.inc'
      include 'param.inc'
      include 'ilogitpar.inc'
      include 'dvalues.inc'
      INTEGER*2     IZ,JZ,IC,DC,IMODE
      INTEGER*2     IX,PSTA,PDSTA,INDEX
      REAL*4        PUTIL,HTIME(max_stations),HDIST(max_stations)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(6,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAWALK
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
C COMPUTE UTILITY VALUES
C
C...MODEL UTILITY VALUE
      PUTIL=STASTA(2,IC,DC) + STAZNE(2,DC,JZ) +
     *  COEFF(2)*HTIME(IC) + COEFF(6)*HDIST(IC)*opcost
      STAWALK=STADATA(DC,9)
      PUTIL=PUTIL 
     *      + COEFF(7) * (STADATA(IC,9) + STADATA(IC,13) + STAWALK)
     *      + COEFF(1) * STADATA(IC,14)
C.....................................................................
      IF(DEBUG) THEN
      WRITE(26,9025) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(PSTA),IEQUIV(PDSTA),
     *               HTIME(IC),HDIST(IC),
     *               STASTA(2,IC,DC),STAZNE(2,DC,JZ),
     *               STADATA(IC,9),STADATA(IC,13),
     *               STADATA(IC,14),STAIND(DC,JZ),STAWALK,PUTIL
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
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2'/
     *       1X,'EGRESS PLATFORM TIME=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5)
      END IF
C.....................................................................
      RETURN
      END
