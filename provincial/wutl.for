C-------------------------------------------------------------------
C        WALK ACCESS --> LINE-HAUL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE WUTL(IX,IZ,JZ,WSTA,WDSTA,WUTIL,
     *                    STASTA,STAZNE,imode,staegr)
      include 'stadat.inc'
      include 'param.inc'
      include 'mlogitpar.inc'
      include 'dvalues.inc'
      INTEGER*2     IX,IZ,JZ,IC,DC,imode
      INTEGER*2     WSTA,WDSTA
      REAL*4        WUTIL,STAWALK,GORWLK
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(7,MAX_STATIONS,MAX_IZONES)
      real*4        staegr(3,max_stations,max_izones)
      CHARACTER*13  NAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   '/
C
C CALCULATE TOTAL UTILITY VALUE
C
      IF(WSTA.LE.0.OR.WDSTA.LE.0.OR.WDSTA.EQ.MAX_ZONES) THEN
      WUTIL=0.0
C...................................................................
      IF(DEBUG) THEN
      WRITE(26,9024) IX,NAME(IMODE)
 9024 FORMAT(/1X,'WALK ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------')
      WRITE(26,9023) IEQUIV(IZ),IEQUIV(JZ),IEQUIV(WSTA),
     *               IEQUIV(WDSTA),WUTIL
 9023 FORMAT(1X,'********* PATH UNAVAILABLE *************'/
     *       1X,'ORIGIN ZONE       =',I10/
     *       1X,'DESTINATION ZONE  =',I10/
     *       1X,'ACCESS STATION    =',I10/
     *       1X,'EGRESS STATION    =',I10/
     *       1X,'TOTAL UTILITY     =',F10.2/)
	    END IF
C...................................................................
      RETURN
	    ELSE
      IC=WSTA-MAX_IZONES
      DC=WDSTA-MAX_IZONES
      STAWALK=STADATA(DC,9)
      GORWLK=0.0
      IF((IEQUIV(WDSTA).EQ.gounion).AND.(STAIND(DC,JZ).EQ.1)) THEN
      GORWLK=KCRWLK/(LSUM1TRN*LSUM2CR*LSUM3CW)
      END IF
      WUTIL=STASTA(2,IC,DC) +  STAZNE(2,DC,JZ) + 
     *      COEFF(7)* (STADATA(IC,9) + STAWALK) + GORWLK
      IF(TRNEGR) THEN
      WUTIL=STASTA(2,IC,DC) +  STAEGR(3,DC,JZ) + 
     *      COEFF(7)* (STADATA(IC,9) + STAWALK) + GORWLK      
      END IF
      WRITE(190,9027) IX
 9027 FORMAT(1X,'WALK ACCESS #',I1)
C     WRITE(190,9026) IEQUIV(IZ),IEQUIV(JZ),IEQUIV(WSTA),
C    *   STANAME(IC),
C    *   IEQUIV(WDSTA),STANAME(DC),STASTA(2,IC,DC),
C    *   STAZNE(2,DC,JZ),STADATA(IC,9),STAIND(DC,JZ),
C    *   STAWALK,GORWLK,WUTIL
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9025) IX,NAME(imode)
 9025 FORMAT(/1X,'WALK ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'-----------------------------------------')
      IF(TRNEGR) THEN
      WRITE(26,9028) IEQUIV(IZ),IEQUIV(JZ),IEQUIV(WSTA),STANAME(IC),
     *   IEQUIV(WDSTA),STANAME(DC),STASTA(2,IC,DC),
     *   STAEGR(3,DC,JZ),STADATA(IC,9),
     *   STAWALK,GORWLK,WUTIL
 9028 FORMAT(1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,5X,A37/
     *       1X,'EGRESS STATION      =',I10,5X,A37/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'PLATFORM WALK TIME  =',F10.5/
     *       1X,'EGRESS PLATFORM WALK=',F10.5/
     *       1X,'WALK EGRESS CONSTANT=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5/
     *       1X,60('='))      
      ELSE
      WRITE(26,9026) IEQUIV(IZ),IEQUIV(JZ),IEQUIV(WSTA),STANAME(IC),
     *   IEQUIV(WDSTA),STANAME(DC),STASTA(2,IC,DC),
     *   STAZNE(2,DC,JZ),STADATA(IC,9),STAIND(DC,JZ),
     *   STAWALK,GORWLK,WUTIL
 9026 FORMAT(1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10/
     *       1X,'ACCESS STATION      =',I10,5X,A37/
     *       1X,'EGRESS STATION      =',I10,5X,A37/
     *       1X,'STA-->STA UTILITY   =',F10.5/
     *       1X,'STA-->ZNE UTILITY   =',F10.5/
     *       1X,'PLATFORM WALK TIME  =',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2'/
     *       1X,'EGRESS PLATFORM WALK=',F10.5/
     *       1X,'WALK EGRESS CONSTANT=',F10.5/
     *       1X,'TOTAL     UTILITY   =',F10.5/
     *       1X,60('='))
      END IF
      END IF
C.....................................................................
      END IF
      RETURN
      END
