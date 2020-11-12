C-------------------------------------------------------------------
C        ORIGIN ZONE -->  STATION UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE ACCESS(IZ,IMODE,STASTA,WSTA,WDIST,BSTA,BDIST,
     *                  ZNESTAU,ZNESTA)
       include 'stadat.inc'
       include 'param.inc'
	     include 'mlogitpar.inc'
	     include 'dvalues.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,IZ,SC2,SC3,SC4
      INTEGER*2     WSTA(2,5),BSTA(2,2),DSTA,TSTA
      INTEGER*2     ZNESTA(50,3)
      REAL*4        ZNESTAU(50)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS)
      REAL*4        WDIST(2,5),BDIST(2,2),BUTIL,WTIME
      CHARACTER*15  NAME(2)
      DATA          NAME/'GO Rail        ',
     *                   'TTC Subway     '/
C
      ZNESTA=0
      ZNESTAU=0.0
C ------------------------------------------------------------
      IF(DEBUG) THEN
      SC=WSTA(2,1)-MAX_IZONES
      SC2=WSTA(2,2)-MAX_IZONES
      SC3=BSTA(2,1)-MAX_IZONES
      SC4=BSTA(2,2)-MAX_IZONES
      WRITE(26,8004) 
     *     IEQUIV(WSTA(2,1)),STANAME(SC),WDIST(2,1),
     *     IEQUIV(WSTA(2,2)),STANAME(SC2),WDIST(2,2),
     *     IEQUIV(BSTA(2,1)),STANAME(SC3),BDIST(2,1),
     *     IEQUIV(BSTA(2,2)),STANAME(SC4),BDIST(2,2)
 8004 FORMAT(//10X,'      TTC SUBWAY ACCESS'/
     *         10X,' -----------------------------'//
     *         ' 1ST WALK ACCESS STATION =',I4,1X,A37/
     *         ' 1ST WALK ACCESS DISTANCE=',F10.5//
     *         ' 2ND WALK ACCESS STATION =',I4,1X,A37/
     *         ' 2ND WALK ACCESS DISTANCE=',F10.5//
     *         ' 1ST  BUS ACCESS STATION =',I4,1X,A37/
     *         ' 1ST  BUS ACCESS UTILITY =',F10.5//
     *         ' 2ND  BUS ACCESS STATION =',I4,1X,A37/
     *         ' 2ND  BUS ACCESS UTILITY =',F10.5//)
      END IF
C ---------------------------------------------------------------
C
C COMMUTER STATION POSSITIBLIES
C
      DO K=1,50
      BUTIL=-999.0
      IF(ZNEREF(K,1).LE.0) CYCLE
      TSTA=IEQUIV(ZNEREF(K,2))
      IF((TSTA.NE.TTCUNION).AND.AIR.AND.UNION_ONLY) CYCLE
C -------------------------------------------------------------------
      IF(DEBUG) THEN
      WRITE(26,8003) IEQUIV(ZNEREF(K,1)),
     *               STANAME(ZNEREF(K,1)-MAX_IZONES),
     *               IEQUIV(ZNEREF(K,2)),
     *               STANAME(ZNEREF(K,2)-MAX_IZONES),K
 8003 FORMAT(/' ACCESS COMPUTATIONS TO GO RAIL STATION=',I5,1X,A20,
     *       ' AND TTC SUBWAY STATION=',I5,1X,A29,' --- OPTION #',I2)
      END IF
C ----------------------------------------------------------------------
C
C WALK ACCESS STATIONS
C
      DO IC=1,2
      IF(WSTA(2,IC).LE.0) CYCLE
      IC2=WSTA(2,IC)-MAX_IZONES
C
C ACCESS STATION LOOP
C
      UTIL=-9999.9
      DO 200 SC=1,MAX_STATIONS
      SC2=SC+MAX_IZONES
      IF(STADATA(SC,6).LE.0.0) GOTO 200
      IF(STANUM(SC).NE.2) GOTO 200
      IF(STASTA(2,IC2,SC).EQ.0.0) GO TO 200
      IF(ZNEREF(K,2).EQ.SC2.AND.WDIST(2,IC).NE.0.0) THEN
      WTIME=(60.0*WDIST(2,IC))/(3.0*1.60934)
      UTIL=COEFF(7)*WTIME+STASTA(2,IC2,SC)
C ------------------------------------------------------------
      IF(SDETAIL) THEN
      WRITE(26,8001) K,IC,IEQUIV(WSTA(2,IC)),
     *               IEQUIV(SC2),WDIST(2,IC),STASTA(2,IC2,SC),UTIL
 8001 FORMAT(' K=',I2,' IC=',I1,' WSTA=',I5,
     *       ' EGRESS STATION=',I5,' WDIST=',F8.5,' STASTA=',F9.5,
     *       ' UTIL=',F10.5)
      END IF
C -------------------------------------------------------------------
      DSTA=SC2
      END IF
  200 CONTINUE
      IF(UTIL.GT.BUTIL) THEN
      BUTIL=UTIL
      ZNESTAU(K)=UTIL
      ZNESTA(K,1)=WSTA(2,IC)
      ZNESTA(K,2)=DSTA
      ZNESTA(K,3)=IC
      END IF
      END DO
C
C BUS ACCESS STATIONS
C
      DO IC=1,2
      IF(BSTA(2,IC).LE.0) CYCLE
      IC2=BSTA(2,IC)-MAX_IZONES
C
C ACCESS STATION LOOP
C
      DO 300 SC=1,MAX_STATIONS
      SC2=SC+MAX_IZONES
      IF(STADATA(SC,6).LE.0.0) GOTO 300
      IF(STANUM(SC).NE.2) GOTO 300
      IF(STASTA(2,IC2,SC).EQ.0.0) GO TO 300
      IF(ZNEREF(K,2).EQ.SC2.AND.BDIST(2,IC).NE.0.0) THEN
      UTIL=STASTA(2,IC2,SC)+BDIST(2,IC)
C -----------------------------------------------------------------
      IF(SDETAIL) THEN
      WRITE(26,8002) K,IC,IEQUIV(BSTA(2,IC)),
     *               IEQUIV(SC2),BDIST(2,IC),STASTA(2,IC2,SC),UTIL
 8002 FORMAT(' K=',I2,' IC=',I1,' BSTA=',I5,
     *       ' EGRESS STATION=',I5,' BDIST=',F8.2,' STASTA=',F9.5,
     *       ' UTIL=',F10.5)
      END IF
C ------------------------------------------------------------------
      DSTA=SC2
      END IF
  300 CONTINUE
      IF(UTIL.GT.BUTIL) THEN
      BUTIL=UTIL
      ZNESTAU(K)=UTIL
      ZNESTA(K,1)=BSTA(2,IC)
      ZNESTA(K,2)=DSTA
      ZNESTA(K,3)=IC+2
      END IF
      END DO
C -----------------------------------------------------------
      IF(DEBUG) THEN
      IF(ZNESTA(K,1).GT.0) THEN
      WRITE(26,8005) ZNESTAU(K),IEQUIV(ZNESTA(K,1)),
     *          STANAME(ZNESTA(K,1)-MAX_IZONES),
     *          IEQUIV(ZNESTA(K,2)),
     *          STANAME(ZNESTA(K,2)-MAX_IZONES),
     *          ZNESTA(K,3)
 8005 FORMAT(' BEST UTILITY=',F10.5,' ORIGIN STA=',I5,1X,A20,
     *       ' DEST STA=',I5,1X,A20,' ACCESS MODE=',I1)
      ELSE
      WRITE(26,8006)
 8006 FORMAT(' *****  NO PATH *****')
      END IF
      END IF
C -----------------------------------------------------------
      END DO
      RETURN
      END
