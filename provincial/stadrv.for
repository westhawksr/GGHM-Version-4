C-------------------------------------------------------------------
C        DRIVE ACCESS TO STATION SELECTION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STADRV(IZ,ZINDEX,IMODE,HDIST)
       INCLUDE 'stadat.inc'
       INCLUDE 'param.inc'
       include 'mlogitpar.inc'
       include 'dvalues.inc'
C
C DECLARATIONS REQUIRED FOR DRIVE ACCESS STATION CHOICE
C
      INTEGER*2     IZ,JS,SC,IC,IMODE
      integer*2     unit,orgzn,destzn,type
      INTEGER*2     ZINDEX(MAX_STATIONS,2),TJZ,IX,IY
      REAL*4        SDIST(MAX_STATIONS),
     *              TDIST,HDIST(1000)
      LOGICAL       CSORT,AVAIL
      CHARACTER*13  NAME(2)
      CHARACTER*11  PNAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   '/
      DATA          PNAME/'Park-n-Ride',
     *                    'Kiss-n-Ride'/
C
C     INITIALIZE ZINDEX
c
      ZINDEX=0
C
C PARK-N-RIDE & KISS-N-RIDE LOOP
C
      DO T=1,2
      SDIST=0.0
C
C LOOP THROUGH STATIONS
C
      DO 150 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      ZINDEX(SC,T)=SC
      AVAIL=.FALSE.
      IF(T.EQ.1.AND.STADATA(SC,7).EQ.1) AVAIL=.TRUE.
      IF((T.EQ.2).AND.(STADATA(SC,7).EQ.1.OR.STADATA(SC,7).EQ.2)) 
     *           AVAIL=.TRUE.
      IF(STADATA(SC,6).lt.1.0.OR.HDIST(SC).LE.0.0) THEN
      SDIST(SC)=99999.9
      GO TO 150
      END IF
      IF(STANUM(SC).NE.IMODE.OR.(.NOT.AVAIL)) THEN
      SDIST(SC)=99999.9
      IF(SDETAIL.AND.(STANUM(SC).EQ.IMODE)) THEN
      WRITE(26,9040) IEQUIV(IC),STANAME(SC),STADATA(SC,7),
     *               STADATA(SC,3),SDIST(SC)
 9040 FORMAT(' STATION=',I4,1X,A37,' TYPE=',F3.0,' SPACES=',F6.0,
     *       ' SDIST=',F8.2)
      END IF
      ELSE
      SDIST(SC)=HDIST(SC)
      IF(SDETAIL) WRITE(26,9040) IC,STANAME(SC),STADATA(SC,7),
     *            STADATA(SC,3),SDIST(SC)
      END IF
  150 CONTINUE
C
C...SORT HIGHWAY TRAVEL DISTANCE VALUES IN DESCENDING ORDER
C
  160 CSORT=.FALSE.
	    DO 161 JS=2,MAX_STATIONS
	    k=js-1
	    IF(SDIST(JS).lt.SDIST(JS-1)) THEN
	    TJZ=ZINDEX(JS,T)
      TDIST=SDIST(JS)
      ZINDEX(JS,T)=ZINDEX((JS-1),T)
      ZINDEX((JS-1),T)=TJZ
      SDIST(JS)=SDIST(JS-1)
      SDIST(JS-1)=TDIST
      CSORT=.TRUE.
      END IF
  161 CONTINUE
      IF(CSORT) GO TO 160
C...................................................................
      IF(DEBUG) THEN
      WRITE(26,9031) IEQUIV(IZ),NAME(IMODE),PNAME(T)
 9031 FORMAT(//1X,'STATION CHOICE SORT BY DISTANCE FOR PRODUCTION ZONE',
     *       '=',I4,' -- ',a13,1x,a11/
     *       1X,'---------------------------------------------------',
     *       '-----'/)
      DO 9032 IX=1,10
	    IY=ZINDEX(IX,T)+MAX_IZONES
        SC=ZINDEX(IX,T)
        IF(SC.LE.0) SC=MAX_STATIONS
      WRITE(26,9033) IEQUIV(IY),SDIST(IX),STANAME(SC)
 9033 FORMAT(1X,'SDIST(',I4,')=',F8.2,5X,A37)
 9032 CONTINUE
      END IF
C....................................................................
      END DO
      RETURN
      END
