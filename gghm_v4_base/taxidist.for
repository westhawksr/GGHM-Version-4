C***********************************************************
C     CREATE TAXI WAIT FREQUENCY DISTRIBUTIONS             *
C***********************************************************
      SUBROUTINE TAXIDIST
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
      INTEGER*4 YINDEX,XINDEX,CATS
      REAL*4    RANVAL
      REAL*4    A,B,Y
      REAL*4    KVAL,KBACK
C
      IF(LDEBUG) open(213,file='txwait_tlf.csv',
     *          status='unknown',form='formatted')
C
      DO CATS=1,6
C
C     COMPUTE PERCENT DISTRIBUTION BASED UPON INPUT WAIT VALUE
C
      DO K=1,1000
      KVAL=FLOAT(K)/10.0
      Y=EXP(TXCOEF(CATS,1)+(TXCOEF(CATS,2)/(KVAL**2)))
      YINDEX=IFIX(Y*1000.0)
C     WRITE(26,100) CATS,K,KVAL,Y,YINDEX
C 100 FORMAT(' CATS=',I2,' K=',I5,' KVAL=',F8.2,' Y=',F12.5,
C    *       ' YINDEX=',I5)
      IF(YINDEX.LE.0.OR.YINDEX.GT.1000) CYCLE
      TXDIST(YINDEX,CATS)=KVAL
      END DO
C
C     SUMMARIZE ARRAY VALUES
C
      KBACK=0.5
      DO K=1,1000
      IF(TXDIST(K,CATS).LE.0) TXDIST(K,CATS)=KBACK
      KVAL=FLOAT(K)/10.0
      KBACK=TXDIST(K,CATS)
      END DO
      END DO
      IF(LDEBUG) THEN
      DO K=1,1000
      KVAL=FLOAT(K)/10.0
      WRITE(213,200) KVAL,(TXDIST(K,CATS),CATS=1,6)
  200 FORMAT(F6.1,6(',',F6.2))      
      END DO
      END IF
      RETURN
      END
