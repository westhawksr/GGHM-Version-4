C--------------------------------------------------------------------
C     STATION CAPACITY RESTRAINT ALGORITHM
C--------------------------------------------------------------------
      SUBROUTINE STACAP(ITER,stasum)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
C
      INTEGER*2     ITER
      INTEGER*4     K,KS
      REAL*4        VC,VC2,OBS,RCON
      REAL*8        STASUM(1000,12)
      CHARACTER*15  NAME(5)
      LOGICAL       CONVERG,EXISTS
      DATA          NAME/'  GO RAIL      ',
     *                   '  TTC SUBWAY   ',
     *                   '  GO BUS       ',
     *                   '               ',
     *                   '               '/
C
C FIRST ADD TOGETHER ALL DRIVE TRIPS USING STATION
C
C  BUSPNROCC = AUTO OCCUPANCY FOR DRIVE TO GO BUS
C  BPFACT = FACTOR FOR SEPERATING PNR FROM KNR TRIPS (TRANSITWAY)
C  BPFACE = FACTOR FOR SEPARATING PNR FROM KNR TRIPS (EXPRESS BUS)
C  BPFACR = FACTOR FOR SEPARATING PNR FROM KNR TRIPS (BUS RAPID TRANSIT)
C
       CONVERG=.TRUE.
       DO 500,IK=1,MAX_STATIONS
       if(stadata(ik,6).gt.0.0) then
        if(stadata(ik,4).le.0.0) go to 500
       K=SEQUIV(IK)
       IF(K.EQ.0) GO TO 500
       STASUM(K,12)=STASUM(K,12)+STASUM(IK,3) + STASUM(IK,8)
     *            + STASUM(IK,11)*(BPFACE/BUSPNROCC)
C    *            + STASUM(IK,XX)*(BPFACT/BUSPNROCC)
C    *            + STASUM(IK,XX)*(BPFACR/BUSPNROCC)
	     endif
 500   CONTINUE
C
C SET EQUIV STATIONS EQUAL
C
      DO 510,IK=1,MAX_STATIONS
      K=SEQUIV(IK)
      IF(K.EQ.0) GO TO 510
      STASUM(IK,12)=STASUM(K,12)
  510 CONTINUE
      IF(SPACESUM) GO TO 600
C
C STATION TYPE LOOP
C
      DO 200 IK=1,3
      WRITE(26,9000) ITER,NAME(IK)
 9000 FORMAT(//1X,'STATION PARKING CAPACITY ANALYSIS - ITERATION ',I2,
     *            '-- ',A15/
     *       1X,'------------------------------------------------'//
     *       1X,'         ','                             ',
     *          ' SPACE  A','DJUSTED ','  PREV ',' OVERALL',' REMAIN ',
     *          '  REVISED ',' SHADOW'/
     *       1X,'STATION  ','      STATION  NAME          ',
     *          ' DEMAND ',' SPACES ','   USED ','   V/C  ','   V/C  ',
     *          ' CONSTANT','   PRICE'/
     *       1X,'---------','-----------------------------',
     *          '---------','-------','-------','--------','--------',
     *          '---------','---------')
C
C STATION LOOP
C
      DO 100 K=1,MAX_STATIONS
      IF(SEQUIV(K).EQ.0) GO TO 100
      IF(STANUM(K).NE.IK) GO TO 100
      ks=MAX_IZONES+k
      VC=0.0
      VC2=0.0
      OBS=0.0
C
C  COMPUTE OVERALL STATION CAPACITY V/C RATIO
C
        IF(STADATA(K,4).GT.0) THEN
        VC=(STASUM(K,12)+(STADATA(K,17)*2.0))/(STADATA(K,4)*2.0)
        ENDIF
C
C  COMPUTE REMAINING CAPACITY V/C RATIO
C
       IF(stasum(k,12).gt.0.0) then
         OBS=(STADATA(K,3)*2.0)/STASUM(K,12)
         IF(OBS.GT.0.0) THEN
         VC2=1.0/OBS
         ELSE
         VC2=1.0
         END IF
       ENDIF
C
C  COMPUTE REVISED CONSTANT TERM
C
        RCON=0.0
	      if((OBS.LE.1.0).AND.(OBS.ne.0.0)) then
          RCON=ALOG(1.0/OBS)
        END IF
        IF(VC2.GT.1.05) CONVERG=.FALSE.
C
C  COMPUTE SHADOW PRICE AND SUMMARIZE
C
	if(stadata(k,6).eq.1.0) then
       stadata(k,5)=-0.5*(RCON/COEFF(6)) + STADATA(K,5)
	WRITE(26,101) IEQUIV(KS),STANAME(K),STASUM(K,12),STADATA(K,4),
     *              STADATA(K,17),VC,
     *              VC2,RCON,STADATA(K,5)
  101  FORMAT(2X,I4,3X,A29,3F8.0,2F8.3,1X,F9.3,F8.0)
      if(calib) write(155,102) IEQUIV(KS),STANAME(K),IK,STADATA(K,8),
     *              STASUM(K,12),STADATA(K,4),
     *              STADATA(K,17),VC,
     *              VC2,RCON,STADATA(K,5)
  102  FORMAT(I4,',',A29,',',I1,',',F4.0,3(',',F8.0),
     *  2(',',F8.3),',',F9.3,',',F8.0)
      endif 
      STAVCR(K,ITER)=VC2
 100  CONTINUE
 200  CONTINUE
C
C  WRITE OUT SPACES AND SHADOW PRICE
C
 600  CONTINUE
      IF(CONVERG.OR.(ITER.EQ.CITER)) THEN
      OPEN(10,FILE=FSTAOUT,FORM='FORMATTED',STATUS='UNKNOWN')
      WRITE(10,8337) 
 8337 FORMAT('STATION,SPACES_USED,CAPACITY,PREV_USED')
C     OPEN(8,FILE='STAVCR.CSV',FORM='FORMATTED',STATUS='UNKNOWN')
       DO NS=1,MAX_STATIONS
       NSTA=NS+MAX_IZONES
       IF(STADATA(NS,6).gt.0.0) then
C..SPACES USED & SHADOW PRICE
       USED=(STASUM(NS,12)/2.0)+(STADATA(NS,4)-STADATA(NS,3))
       IUSED=IFIX(USED)
       IF(IUSED.GT.0) THEN
       WRITE(10,8335) IEQUIV(NSTA),IUSED,STADATA(NS,4),STADATA(NS,17)
 8335  FORMAT(I4,',',I8,',',F5.0,',',F5.0)
C      WRITE(8,8336) IEQUIV(NSTA),(STAVCR(NS,K),K=1,ITER)
 8336  FORMAT(I4,50(',',F6.2))
       END IF
       ENDIF
       END DO
       CLOSE(10,STATUS='KEEP')
C
C PROGRAM COMPLETION
C
       CALL GETTIM(IHR,IMIN,ISEC,I100)
       WRITE(26,8004) IHR,IMIN,ISEC
       WRITE(*,8004) IHR,IMIN,ISEC
 8004  FORMAT(/' Station Capacity Restraint Closure Acheived'/
     *         ' Program Completed: ',I2,':',I2,':',I2/)
      CLOSE(140,STATUS='DELETE')
      STOP
      END IF
      RETURN
      END
