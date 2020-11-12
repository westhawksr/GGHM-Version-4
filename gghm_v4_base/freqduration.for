C***********************************************************
C     CREATE TRIP DURATION FREQUENCY DISTRIBUTION          *
C***********************************************************
      SUBROUTINE FREQDURATION
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
      INTEGER*4 YINDEX,XINDEX,CATS
      REAL*4    RANVAL
      REAL*4    A,B,Y
      REAL*4    KVAL,KBACK
C
      IF(LDEBUG) open(281,file='duration_freqdist.csv',
     *           status='unknown',form='formatted')
C
      IF(DURCOEF(1).EQ.0.OR.DURCOEF(2).EQ.0) THEN
      WRITE(*,9001) DURCOEF
      WRITE(26,9001) DURCOEF
 9001 FORMAT(/' COEFFICIENTS FOR THE TRIP DURATION FREQUENCY',
     *        ' DISTRIBUTION ARE ZERO'/
     *        '     DURCOEF(1)=',F10.5/
     *        '     DURCOEF(2)=',F10.5/
     *        ' *** PROGRAM TERMINATED ***'/)
      STOP 8
      END IF
C
C     COMPUTE PERCENT DISTRIBUTION BASED UPON INPUT TRIP DURATION VALUE
C
      DO K=1,200
      KVAL=FLOAT(K)/10.0
      Y=1.0/(DURCOEF(1)+(DURCOEF(2)/(KVAL**2)))
      YINDEX=IFIX(Y*1000.0)
C     IF(LDEBUG) WRITE(281,100) K,KVAL,Y,YINDEX
C 100 FORMAT(' K=',I5,' KVAL=',F8.2,' Y=',F12.5,
C    *       ' YINDEX=',I5)
      IF(YINDEX.LE.0.OR.YINDEX.GT.1000) CYCLE
      DURDIST(YINDEX)=KVAL
      END DO
C
C     FILL IN MISSING VALUES
C
      KBACK=0.1
      DO K=1,1000
      IF(DURDIST(K).LE.0) DURDIST(K)=KBACK
      KVAL=FLOAT(K)/10.0
      KBACK=DURDIST(K)
      END DO
      IF(LDEBUG) THEN
      DO K=1,1000
      KVAL=FLOAT(K)/1000.0
      WRITE(281,200) KVAL,DURDIST(K)
  200 FORMAT(F6.4,',',F8.4)      
      END DO
      END IF
      RETURN
      END
