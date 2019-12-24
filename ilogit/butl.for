C-------------------------------------------------------------------
C        BUS ACCESS --> RAIL UTILITY CALCULATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE BUTL(IX,IZ,JZ,BDIST,BSTA,BDSTA,BUTIL,
     *                    STASTA,STAZNE,imode)
      include 'stadat.inc'
      include 'param.inc'
      include 'ilogitpar.inc'
      include 'dvalues.inc'
      INTEGER*2     IX,IZ,JZ,IC,DC,IMODE
      INTEGER*2     BSTA,BDSTA
      REAL*4        BDIST,BUTIL
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(6,MAX_STATIONS,MAX_IZONES)
      REAL*4        STAWALK
      CHARACTER*13  NAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   '/
C
C CALCULATE TOTAL UTILITY VALUE
C
      IC=BSTA-MAX_IZONES
      DC=BDSTA-MAX_IZONES
      IF(IC.LT.0.OR.DC.LT.0.OR.DC.EQ.MAX_STATIONS) THEN
      BUTIL=0.0
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9024) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),IEQUIV(BSTA),
     *               IEQUIV(BDSTA),BUTIL
 9024 FORMAT(/1X,'BUS ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'----------------------------------------'/
     *       1X,'************ PATH UNAVAILABLE *********'/
     *       1X,'ORIGIN ZONE       =',I10/
     *       1X,'DESTINATION ZONE  =',I10/
     *       1X,'ACCESS STATION    =',I10/
     *       1X,'EGRESS STATION    =',I10/
     *       1X,'TOTAL     UTILITY =',F10.2)
      END IF
C.....................................................................
      RETURN
      END IF
      STAWALK=STADATA(DC,9)
      BUTIL=BDIST + STASTA(2,IC,DC) +
     *      STAZNE(2,DC,JZ) +
     *      COEFF(7)* (STADATA(IC,9) + STAWALK)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9025) IX,NAME(IMODE),IEQUIV(IZ),IEQUIV(JZ),
     *               IEQUIV(BSTA),STANAME(IC),IEQUIV(BDSTA),
     *               STANAME(DC),BDIST,STASTA(2,IC,DC),
     *               STAZNE(2,DC,JZ),STADATA(IC,9),STAIND(DC,JZ),
     *               STAWALK,BUTIL
 9025 FORMAT(/1X,'BUS ACCESS #',I1,
     *           ' --> RAIL UTILITY COMPUTATION -- ',A13/
     *       1X,'----------------------------------------'/
     *       1X,'ORIGIN ZONE       =',I10/
     *       1X,'DESTINATION ZONE  =',I10/
     *       1X,'ACCESS STATION    =',I10,5X,A37/
     *       1X,'EGRESS STATION    =',I10,5X,A37/
     *       1X,'ACCESS    UTILITY =',F10.5/
     *       1X,'STA-->STA UTILITY =',F10.5/
     *       1X,'STA-->ZNE UTILITY =',F10.5/
     *       1X,'ACCESS PLATFORM TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2'/
     *       1X,'EGRESS PLATFORM TIME=',F10.5/
     *       1X,'TOTAL     UTILITY =',F10.5)
      END IF
C.....................................................................
      RETURN
      END

