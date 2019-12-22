C-------------------------------------------------------------------
C        SORT UTILITY VALUES SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE USORT(TYPE,NNDEX,STA,ISTA,DSTA,DUTIL,imode,
     *                 dsta1,dsta2,dind,uutil,ucost)
       include 'stadat.inc'
       include 'param.inc'
       include 'mlogitpar.inc'
       include 'dvalues.inc'
C
C DECLARATIONS REQUIRED FOR DRIVE ACCESS STATION CHOICE
C
      INTEGER*2     NNDEX(2,10),JZ,TJZ,STA(MAX_STATIONS,2),DSTA(2,10),
     *              ISTA(2,10,2),IX,IMODE,QJZ,SC,TYPE
      INTEGER*2     DSTA1(2,10),DSTA2(2,10),DIND(2,10)
      INTEGER*2     QSTA1,QSTA2,QIND,PSTA(10),ASTA(10)
      REAL*4        DUTIL(2,10),QUTIL,UUTIL(2,10),QUTIL2
      REAL*4        UCOST(2,10),QCOST
      LOGICAL       CSORT
      CHARACTER*3   NTYPE(2)
      CHARACTER*13  NAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   '/
      DATA          NTYPE/'PNR','KNR'/
C
      DO 100 IX=1,10
         NNDEX(IMODE,IX)=STA(IX,TYPE)+MAX_IZONES
         DSTA(imode,IX)=ISTA(imode,IX,TYPE)
         IF(DUTIL(IMODE,IX).EQ.0) DUTIL(IMODE,IX)=-999.9
  100 CONTINUE
C
C...SORT UTILITY VALUES IN DESCENDING ORDER
C
  160 CSORT=.FALSE.
      DO 161 JZ=2,10
      IF(DUTIL(IMODE,JZ).GT.DUTIL(IMODE,JZ-1)) THEN
      TJZ=NNDEX(IMODE,JZ)
      QJZ=DSTA(imode,JZ)
      QUTIL=DUTIL(IMODE,JZ)
      IF(TYPE.EQ.2) QUTIL2=UUTIL(IMODE,JZ)
      IF(TYPE.EQ.2) QCOST=UCOST(IMODE,JZ)
      QSTA1=DSTA1(IMODE,JZ)
      QSTA2=DSTA2(IMODE,JZ)
      QIND=DIND(IMODE,JZ)
      NNDEX(IMODE,JZ)=NNDEX(IMODE,(JZ-1))
      DSTA(imode,JZ)=DSTA(imode,(JZ-1))
      NNDEX(IMODE,(JZ-1))=TJZ
      DSTA(imode,JZ-1)=QJZ
      DUTIL(IMODE,JZ)=DUTIL(IMODE,JZ-1)
      IF(TYPE.EQ.2) UUTIL(IMODE,JZ)=UUTIL(IMODE,JZ-1)
      IF(TYPE.EQ.2) UCOST(IMODE,JZ)=UCOST(IMODE,JZ-1)      
      DUTIL(IMODE,JZ-1)=QUTIL
      IF(TYPE.EQ.2) UUTIL(IMODE,JZ-1)=QUTIL2
      IF(TYPE.EQ.2) UCOST(IMODE,JZ-1)=QCOST
      DSTA1(IMODE,JZ)=DSTA1(IMODE,(JZ-1))
      DSTA2(IMODE,JZ)=DSTA2(IMODE,(JZ-1))
      DIND(IMODE,JZ)=DIND(IMODE,(JZ-1))
      DSTA1(IMODE,(JZ-1))=QSTA1
      DSTA2(IMODE,(JZ-1))=QSTA2
      DIND(IMODE,(JZ-1))=QIND
      CSORT=.TRUE.
      END IF
  161 CONTINUE
      IF(CSORT) GO TO 160
C
C...................................................................
      IF(DEBUG) THEN
      PSTA=0
      ASTA=0
      DO IX=1,10
      IF(NNDEX(IMODE,IX).GT.0) PSTA(IX)=IEQUIV(NNDEX(IMODE,IX))
      IF(DSTA(IMODE,IX).GT.0)  ASTA(IX)=IEQUIV(DSTA(IMODE,IX))
      END DO
      WRITE(26,9031) NAME(IMODE),NTYPE(TYPE)
 9031 FORMAT(//1X,'UTILITY SORT BY STATION FOR  ',A13,'-- ',A3/
     *       1X,'-------------------------------')
      DO 9032 IX=1,10
      SC=NNDEX(IMODE,IX)-MAX_IZONES
      IF(SC.LE.0) SC=MAX_IZONES
      WRITE(26,9033) PSTA(IX),ASTA(IX),
     *               DUTIL(IMODE,IX),STANAME(SC)
 9033 FORMAT(1X,'UTIL(',I4,'-->',I4')=',F8.2,5X,A37)
 9032 CONTINUE
      IF(TYPE.EQ.2.AND.UBERTRN) THEN
      WRITE(26,9034) NAME(IMODE)
 9034 FORMAT(//1X,'UTILITY SORT BY STATION FOR  ',A13,'-- UBER'/
     *       1X,'-------------------------------')
      DO IX=1,10
      SC=NNDEX(IMODE,IX)-MAX_IZONES
      IF(SC.LE.0) SC=MAX_IZONES
      WRITE(26,9033) PSTA(IX),ASTA(IX),
     *               UUTIL(IMODE,IX),STANAME(SC)
      END DO
      END IF
      END IF
C....................................................................
      RETURN
      END
