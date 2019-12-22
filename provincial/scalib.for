      subroutine scalib(iter,tesum,ttrip,ptrip,tottrn,gorail,ttcsubway,
     *                  stasum,nonmot,stasum2)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
c     self calibration subroutine
c
      character*80   header
      integer*2      iter,mkt
      integer*4      nseg,ci,c,t,t1,t2,origsta,deststa
      real*8         values(7),pobs,pest,econ,ncon,rcon
      real*8         tesum(58,7),oesum(41,7),dfsum(41,7)
      real*8         maxval,minval,incdat(6),oshare
      real*8         otrip(25,6),ttrip(25,6),ptrip(25,6)
      real*8         tottrn(31,31,6),gorail(21,21)
      real*8         ttcsubway(21,21),stasum(1000,13)
      real*8         nonmot(31,31,3),stasum2(max_stations,8)
      logical        exists
      data           minval/-15.0/
c
      oesum=0.0
      dfsum=0.0
      otrip=0.0
c     open(141,file='..\outputs\ptrip.csv',status='unknown',
c    *         form='formatted')
c     do k=1,6
c     do k1=1,25
c     k2=k1*5
c     write(141,200) k,k2,ptrip(k1,k)
c 200 format(i2,',',i3,',',f12.2)
c     end do
c     end do
c
c     open calibration target values
c
      inquire(file=fctvinp,exist=exists)
      if(.not.exists) then
      write(*,9001)  fctvinp
      write(26,9001) fctvinp
 9001 FORMAT(//' SCALIB 9001 (F) CALIBRATION TARGET VALUE FILE=',
     *         A40,' NOT FOUND'//)
      STOP 9001
      ELSE
      open(98,file=fctvinp,status='old',form='formatted')
      end if
      read(98,1,end=100,err=50) header
    1 format(a80)
   10 read(98,*,end=100,err=50) mkt,values
      if(mkt.eq.99) go to 100
      do k=1,ncats
      oesum(mkt,k)=values(k)
      end do
      go to 10
   50 write(26,51) mkt
   51 format(' scalib 51 (f) error reading calibration target',
     *       ' value file for array index=',i2)
      stop 51
  100 continue
  150 read(98,*,end=300) pindex,incdat
      if(pindex.lt.1.or.pindex.gt.25) then
      write(26,151) pindex
  151 FORMAT(' SCALIB 151 (F) ILLOGICAL VALUE (',I3,') FOR TRIP',
     *       ' LENGTH INDICATOR IN OBSERVED TRANSIT SHARE FILE')
      STOP 151
      END IF
      otrip(pindex,1)=incdat(1)
      otrip(pindex,2)=incdat(2)
      otrip(pindex,3)=incdat(3)
      otrip(pindex,4)=incdat(4)
      otrip(pindex,5)=incdat(5)
      otrip(pindex,6)=incdat(6)
      maxt=pindex
      GO TO 150
  300 CLOSE(98,STATUS='KEEP')
C
C     FILL IN REMAINDER OF OESUM
C
      CI=NCATS+1
      DO K1=1,41
      DO K=1,NCATS
      OESUM(K1,CI)=OESUM(K1,CI)+OESUM(K1,K)
      END DO
      END DO
      DO K=1,CI
      OESUM(3,K)=OESUM(9,K)+OESUM(10,K)+OESUM(11,K)+OESUM(12,K)
      OESUM(4,K)=OESUM(13,K)+OESUM(14,K)+OESUM(15,K)+OESUM(16,K)
      OESUM(5,K)=OESUM(7,K)+OESUM(8,K)
      OESUM(36,K)=OESUM(34,K)+OESUM(35,K)
      OESUM(37,K)=OESUM(17,K)+OESUM(19,K)
      OESUM(38,K)=OESUM(18,K)+OESUM(20,K)
      TESUM(3,K)=TESUM(9,K)+TESUM(10,K)+TESUM(11,K)+TESUM(12,K)
      TESUM(4,K)=TESUM(13,K)+TESUM(14,K)+TESUM(15,K)+TESUM(16,K)
      TESUM(5,K)=TESUM(7,K)+TESUM(8,K)
      TESUM(37,K)=TESUM(17,K)+TESUM(19,K)
      TESUM(38,K)=TESUM(18,K)+TESUM(20,K)
      TESUM(31,K)=TESUM(21,K)+TESUM(22,K)
      TESUM(32,K)=TESUM(23,K)+TESUM(24,K)+TESUM(25,K)+TESUM(26,K)
      TESUM(33,K)=TESUM(27,K)+TESUM(28,K)+TESUM(29,K)+TESUM(30,K)
      END DO

      DO K=1,41
      DO C=1,CI
      DFSUM(K,C)=TESUM(K,C)-OESUM(K,C)
      END DO
      END DO
C
      WRITE(26,8001)
 8001 FORMAT(//,10X,'R E P O R T   1A  (O B S E R V E D  D A T A)')
      WRITE(26,8002)
 8002 FORMAT(   10X,
     *          'SUMMARIZE TRIPS BY MODE AND MARKET GROUP',//,
     *       1X,'Market   ','         ','    2    ','      3   ',
     *          '           ','          ','   TOTAL  '/
     *       1X,'Segment  ','  DRIVE  ','  PERSON ','    PERSON ',
     *          ' TRANSIT ','   NON   ','    PERSON '/
     *       1X,'         ','  ALONE  ','  AUTO   ','     AUTO  ',
     *          '   TRIPS ',' MOTORIZED','    TRIPS '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','---------','----------')
      DO C=1,NCATS
      WRITE(26,8003) C,(OESUM(T,C),T=31,33),OESUM(2,C),
     *                  OESUM(36,C),OESUM(1,C)
 8003 FORMAT(5X,I1,2X,7F10.0)
      END DO
      WRITE(26,8004) (OESUM(T,CI),T=31,33),OESUM(2,CI),
     *                OESUM(36,CI),OESUM(1,CI)
 8004 FORMAT(/,2X,'TOTAL',1X,7F10.0)
      WRITE(26,8005)
 8005 FORMAT(//,10X,'R E P O R T   1B  (E S T I M A T E D  D A T A)')
      WRITE(26,8002)
      DO C=1,NCATS
      WRITE(26,8003) C,(TESUM(T,C),T=31,33),TESUM(2,C),
     *                  TESUM(36,C),TESUM(1,C)
      END DO
      WRITE(26,8004) (TESUM(T,CI),T=31,33),TESUM(2,CI),
     *                  TESUM(36,CI),TESUM(1,CI)
      WRITE(26,8006)
 8006 FORMAT(//,10X,'R E P O R T   1C  (ESTIMATED-OBSERVED DATA)')
      WRITE(26,8002)
      DO C=1,NCATS
      WRITE(26,8003) C,(DFSUM(T,C),T=31,33),DFSUM(2,C),
     *                  DFSUM(36,C),DFSUM(1,C)
      END DO
      WRITE(26,8004) (DFSUM(T,CI),T=31,33),DFSUM(2,CI),
     *                DFSUM(36,CI),DFSUM(1,CI)
C
      WRITE(26,7004)
 7004 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *         'SUMMARIZE OBSERVED TRANSIT TRIPS BY MARKET SEGMENT',/)
      WRITE(26,8007)
 8007 FORMAT(1X,' MARKET  '/
     *       1X,' SEGMENT ','    GO    ','   TTC    ','   GO     ',
     *          '   BUS   ','   RAPID  '/
     *       1X,'  LEVEL  ','    RAIL  ','  SUBWAY  ','   BUS    ',
     *          'STREETCAR','    BUS   ','  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *          '----------','----------','----------')
      DO C=1,NCATS
      WRITE(26,7005) C,OESUM(3,C),OESUM(4,C),OESUM(5,C),
     *                 OESUM(37,C),OESUM(38,C),OESUM(2,C)
 7005 FORMAT(3X,I1,4X,6F10.0)
      END DO
      WRITE(26,7006) OESUM(3,CI),OESUM(4,CI),OESUM(5,CI),
     *               OESUM(37,CI),OESUM(38,CI),OESUM(2,CI)
      WRITE(26,7020)
 7020 FORMAT(//,30X,'R E P O R T   2B',/,
     *          20X,
     *         'SUMMARIZE ESTIMATED TRANSIT TRIPS BY MARKET SEGMENT',/)
      WRITE(26,8007)
      DO C=1,NCATS
      WRITE(26,7005) C,TESUM(3,C),TESUM(4,C),TESUM(5,C),
     *                 TESUM(37,C),TESUM(38,C),TESUM(2,C)
      END DO
      WRITE(26,7006) TESUM(3,CI),TESUM(4,CI),TESUM(5,CI),
     *               TESUM(37,CI),TESUM(38,CI),TESUM(2,CI)          
 7006 FORMAT(/,2X,'TOTAL',1X,6F10.0)
      WRITE(26,7001)
 7001 FORMAT(//,30X,'R E P O R T   2C',/,
     *          20X,
     *         'SUMMARIZE ESTIMATED-OBSERVED BY MARKET SEGMENT',/)
      WRITE(26,8007)
      DO C=1,NCATS
      WRITE(26,7005) C,DFSUM(3,C),DFSUM(4,C),DFSUM(5,C),
     *                 DFSUM(37,C),DFSUM(38,C),DFSUM(2,C)
      END DO
      WRITE(26,7006) DFSUM(3,CI),DFSUM(4,CI),DFSUM(5,CI),
     *               DFSUM(37,CI),DFSUM(38,CI),DFSUM(2,CI)
C
C GO RAIL AND THEN TTC SUBWAY
C
      WRITE(26,7007)
 7007 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE OBSERVED GO RAIL BY MARKET SEGMENT',/)
      WRITE(26,8008)
 8008 FORMAT(1X,' MARKET  ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------','----------','----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(OESUM(K,C),K=9,12),OESUM(3,C)
 7009 FORMAT(5X,I1,2X,5F10.1)
      END DO
      WRITE(26,7010) (OESUM(K,CI),K=9,12),OESUM(3,CI)
 7010 FORMAT(/,2X,'TOTAL',1X,6F10.1)
      WRITE(26,7021)
 7021 FORMAT(//,30X,'R E P O R T   3B',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED GO RAIL BY MARKET SEGMENT',/)
      WRITE(26,8008)
      DO C=1,NCATS
      WRITE(26,7009) C,(TESUM(K,C),K=9,12),TESUM(3,C)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=9,12),TESUM(3,CI)
      WRITE(26,7022)
 7022 FORMAT(//,30X,'R E P O R T   3C',/,
     *          20X,
     *   'SUMMARIZE ESTIMATED-OBSERVED GO RAIL BY MARKET SEGMENT',/)
      WRITE(26,8008)      
      DO C=1,NCATS
      WRITE(26,7009) C,(DFSUM(K,C),K=9,12),DFSUM(3,C)
      END DO
      WRITE(26,7010) (DFSUM(K,CI),K=9,12),DFSUM(3,CI)
C
      WRITE(26,7011)
 7011 FORMAT(//,30X,'R E P O R T   4A',/,
     *          20X,
     *          'SUMMARIZE OBSERVED TTC SUBWAY BY MARKET SEGMENT',/)
      WRITE(26,8008)
      DO C=1,NCATS
      WRITE(26,7009) C,(OESUM(K,C),K=13,16),OESUM(4,C)
      END DO
      WRITE(26,7010) (OESUM(K,CI),K=13,16),OESUM(4,CI)
      WRITE(26,7023)
 7023 FORMAT(//,30X,'R E P O R T   4B',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED TTC SUBWAY BY MARKET SEGMENT',/)
      WRITE(26,8008)
      DO C=1,NCATS
      WRITE(26,7009) C,(TESUM(K,C),K=13,16),TESUM(4,C)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=13,16),TESUM(4,CI)
      WRITE(26,7003)
 7003 FORMAT(//,30X,'R E P O R T   4C',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED-OBSERVED TTC SUBWAY',/)
      WRITE(26,8008)
      DO C=1,NCATS
      WRITE(26,7009) C,(DFSUM(K,C),K=13,16),DFSUM(4,C)
      END DO
      WRITE(26,7010) (DFSUM(K,CI),K=13,16),DFSUM(4,CI)
C
      WRITE(26,7031)
 7031 FORMAT(//,30X,'R E P O R T   5A',/,
     *          20X,
     *          'SUMMARIZE OBSERVED GO BUS BY MARKET SEGMENT',/)
      WRITE(26,8009)
 8009 FORMAT(1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ','    WALK  ','   DRIVE  ',
     *          '  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(OESUM(K,C),K=7,8),OESUM(5,C)
      END DO
      WRITE(26,7010) (OESUM(K,CI),K=7,8),OESUM(5,CI)
      WRITE(26,7024)
 7024 FORMAT(//,30X,'R E P O R T   5B',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED GO BUS BY MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,(TESUM(K,C),K=7,8),TESUM(5,C)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=7,8),TESUM(5,CI)
      WRITE(26,7013)
 7013 FORMAT(//,30X,'R E P O R T   5C',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED - OBSERVED GO BUS BY',
     *          ' MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,(DFSUM(K,C),K=7,8),DFSUM(5,C)
      END DO
      WRITE(26,7010) (DFSUM(K,CI),K=7,8),DFSUM(5,CI)
C
      WRITE(26,7035)
 7035 FORMAT(//,30X,'R E P O R T   6A',/,
     *          20X,
     *          'SUMMARIZE OBSERVED BUS/STREETCAR BY MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,OESUM(17,C),OESUM(19,C),OESUM(37,C)
      END DO
      WRITE(26,7010) OESUM(17,CI),OESUM(19,CI),OESUM(37,CI)
      WRITE(26,7033)
 7033 FORMAT(//,30X,'R E P O R T   6B',/,
     *          20X,
     *        'SUMMARIZE ESTIMATED BUS/STREETCAR BY MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,TESUM(17,C),TESUM(19,C),TESUM(37,C)
      END DO
      WRITE(26,7010) TESUM(17,CI),TESUM(19,CI),TESUM(37,CI)
      WRITE(26,7034)
 7034 FORMAT(//,30X,'R E P O R T   6C',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED - OBSERVED BUS/STREETCAR BY',
     *          ' MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,DFSUM(17,C),DFSUM(19,C),DFSUM(37,C)
      END DO
      WRITE(26,7010) DFSUM(17,CI),DFSUM(19,CI),DFSUM(37,CI)
C
      WRITE(26,7036)
 7036 FORMAT(//,30X,'R E P O R T   7A',/,
     *          20X,
     *          'SUMMARIZE OBSERVED RAPID BUS BY MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,OESUM(18,C),OESUM(20,C),OESUM(38,C)
      END DO
      WRITE(26,7010) OESUM(18,CI),OESUM(20,CI),OESUM(38,CI)
      WRITE(26,7037)
 7037 FORMAT(//,30X,'R E P O R T   7B',/,
     *          20X,
     *        'SUMMARIZE ESTIMATED RAPID BUS BY MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,TESUM(18,C),TESUM(20,C),TESUM(38,C)
      END DO
      WRITE(26,7010) TESUM(18,CI),TESUM(20,CI),TESUM(38,CI)
      WRITE(26,7038)
 7038 FORMAT(//,30X,'R E P O R T   7C',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED - OBSERVED RAPID BUS BY',
     *          ' MARKET SEGMENT',/)
      WRITE(26,8009)
      DO C=1,NCATS
      WRITE(26,7009) C,DFSUM(18,C),DFSUM(20,C),DFSUM(38,C)
      END DO
      WRITE(26,7010) DFSUM(18,CI),DFSUM(20,CI),DFSUM(38,CI)
C
      WRITE(26,7039)
 7039 FORMAT(//,30X,'R E P O R T   8A',/,
     *          20X,
     *          'SUMMARIZE NON-MOTORIZED BY MARKET SEGMENT',/)
      WRITE(26,8010)
 8010 FORMAT(1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ','    WALK  ','   BIKE   ',
     *          '  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------')
      DO C=1,NCATS
      WRITE(26,7009) C,OESUM(34,C),OESUM(35,C),OESUM(36,C)
      END DO
      WRITE(26,7010) OESUM(34,CI),OESUM(35,CI),OESUM(36,CI)
      WRITE(26,7040)
 7040 FORMAT(//,30X,'R E P O R T   8B',/,
     *          20X,
     *        'SUMMARIZE ESTIMATED NON-MOTORIZED BY MARKET SEGMENT',/)
      WRITE(26,8010)
      DO C=1,NCATS
      WRITE(26,7009) C,TESUM(34,C),TESUM(35,C),TESUM(36,C)
      END DO
      WRITE(26,7010) TESUM(34,CI),TESUM(35,CI),TESUM(36,CI)
      WRITE(26,7041)
 7041 FORMAT(//,30X,'R E P O R T   8C',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED - OBSERVED NON-MOTORIZED BY',
     *          ' MARKET SEGMENT',/)
      WRITE(26,8010)
      DO C=1,NCATS
      WRITE(26,7009) C,DFSUM(34,C),DFSUM(35,C),DFSUM(36,C)
      END DO
      WRITE(26,7010) DFSUM(34,CI),DFSUM(35,CI),DFSUM(36,CI)
C
      IF(OESUM(6,CI).GT.0) THEN
      WRITE(26,7042)
 7042 FORMAT(//,30X,'R E P O R T   9',/,
     *          20X,
     *          'SUMMARIZE SCHOOL BUS BY MARKET SEGMENT',/)
      WRITE(26,8011)
 8011 FORMAT(1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ',' OBSERVED ',' ESTIMATED',
     *          '  DIFFERENCE'/
     *       1X,'---------','----------','----------',
     *          '------------')
      DO C=1,NCATS
      WRITE(26,7044) C,OESUM(6,C),TESUM(6,C),DFSUM(6,C)
 7044 FORMAT(5X,I1,2X,F10.1,2(2X,F10.1))
      END DO
      WRITE(26,7043) OESUM(6,CI),TESUM(6,CI),DFSUM(6,CI)
 7043 FORMAT(/,2X,'TOTAL',1X,F10.1,2(2X,F10.1))
      END IF
C..
      WRITE(26,7047)
 7047 FORMAT(//,30X,'R E P O R T   10A',/,
     *          20X,
     *          'SUMMARIZE TAXI TRIPS BY MARKET SEGMENT',/)
      WRITE(26,8071)
 8071 FORMAT(1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ','  TOTAL   '/
     *       1X,'---------','----------')
      DO C=1,NCATS
      WRITE(26,7009) C,OESUM(41,C)
      END DO
      WRITE(26,7010) OESUM(41,CI)
      WRITE(26,7045)
 7045 FORMAT(//,30X,'R E P O R T   10B',/,
     *          20X,
     *        'SUMMARIZE ESTIMATED TAXI TRIPS BY MARKET SEGMENT',/)
      WRITE(26,8071)
      DO C=1,NCATS
      WRITE(26,7009) C,TESUM(41,C)
      END DO
      WRITE(26,7010) TESUM(41,CI)
      WRITE(26,7046)
 7046 FORMAT(//,30X,'R E P O R T   10C',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED - OBSERVED TAXI TRIPS BY',
     *          ' MARKET SEGMENT',/)
      WRITE(26,8071)
      DO C=1,NCATS
      WRITE(26,7009) C,DFSUM(41,C)
      END DO
      WRITE(26,7010) DFSUM(41,CI)
C....................
      write(100,7032) iter
      write(26,7032) iter
 7032 format(/' Calibration Iteration ',i2/
     *        ' -------------------------'/)
C
C----------------------------------------------------------------------
C
C     Calculate Basic Set of Constants (Transit, Shared Ride & 3-Person Auto)
C
      IF(CCODE(7)) THEN
C
C     Calculate Transit Constants
C
      IF(CCODE(10)) THEN
      DO 4863 INC=1,NCATS
      WRITE(26,4853) INC
 4853 FORMAT(/10X,'Stratified Transit Constants-Market Segment ',I1/
     *        10X,'--------------------------------------------',/,
     *  1X,'  Trip       Obs     Est    Existing               New',
     *               '    Person    Obs'/
     *  1X,' Length     Value   Value   Constant Adjustment  Constant',
     *               '  Trips   Share'/
     *  1X,'---------  ------- ------- --------- ---------- ---------',
     *               ' ------- ------')
	DO 4864 T=1,25
	   POBS=0.0
	   PEST=0.0
C    IF(T.GE.MAXT) THEN
C	    IF(T.GT.MAXT) THEN
C	    TTRIP(MAXT,INC)=TTRIP(MAXT,INC)+TTRIP(T,INC)
C	    END IF
C	   IF(T.LT.25) GO TO 4864
C	   END IF
             POBS=otrip(T,INC)
             PEST=TTRIP(T,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
C        IF(T.LT.MAXT) THEN
         ECON=KTRNT(T,INC)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=-15.0
         NCON=DMAX1(NCON,-15.0)
         NCON=DMIN1(NCON,15.0)
         KTRNT(T,INC)=NCON
         OSHARE=0.0
         IF(PTRIP(T,INC).GT.0) OSHARE=(POBS/PTRIP(T,INC))*100.0
         T1=(T-1)*5
         T2=T*5
         WRITE(26,4873) T1,T2,POBS,PEST,ECON,RCON,NCON,
     *                  PTRIP(T,INC),OSHARE
 4873    FORMAT(1X,I3,'-',I3,3X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5,
     *          1X,F7.0,1X,F6.1)
         write(100,4883) T,INC,KTRNT(T,INC)
 4883    FORMAT(4X,'KTRNT(',I2,',',I1,')=',F9.5)
C        ELSE
C        ECON=KTRNT(MAXT,INC)
C        NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=-15.0
C        NCON=DMAX1(NCON,-15.0)
C        DO 4865 K=MAXT,25
C        KTRNT(K,INC)=NCON
C        T1=(K-1)*5
C        T2=K*5
C        WRITE(26,4873) T1,T2,POBS,PEST,ECON,RCON,NCON,
C    *                  PTRIP(T,INC),OSHARE
C        write(100,4883) K,INC,KTRNT(K,INC)
C4865 CONTINUE
C     END IF
 4864 CONTINUE
 4863 CONTINUE
      END IF
      WRITE(26,4850)
 4850 FORMAT(/15X,'Transit Constants',/,
     *        15X,'-----------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
	DO 4860 INC=1,NCATS
         POBS=OESUM(2,INC)
         PEST=TESUM(2,INC)
         RCON=0.0
         IF(POBS.GT.0.AND.PEST.GT.0) THEN
         RCON=DLOG(POBS/PEST)
         END IF
         ECON=KTRN(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KTRN(INC)=NCON
         IF(CCODE(10)) THEN
         KTRN(INC)=0.0
         RCON=0.0
         NCON=0.0
         END IF
         WRITE(26,4870) (INC),POBS,PEST,ECON,RCON,NCON
 4870    FORMAT(1X,' KTRN(',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      IF(.NOT.CCODE(10)) write(100,4880) INC,KTRN(INC)
 4880 FORMAT(4X,'KTRN(',I1,')=',F9.5)
 4860 CONTINUE
C
C----------------------------------------------------------------------
C     Calculate Shared Ride Constants
C
      WRITE(26,4750)
 4750 FORMAT(/15X,'Shared Ride Constants',/,
     *        15X,'---------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
         POBS=0.0
         PEST=0.0
         POBS=OESUM(32,INC)+OESUM(33,INC)
         PEST=TESUM(32,INC)+TESUM(33,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
         RCON=DLOG(POBS/PEST)
         END IF
         ECON=KSR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KSR(INC)=NCON
         WRITE(26,4770) INC,POBS,PEST,ECON,RCON,NCON
 4770    FORMAT(1X,' KSR(',I1,')',3X,2F8.0,1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4761) INC,KSR(INC)
 4761 FORMAT(4X,'KSR(',I1,')=',F9.5)
      END DO
C
C----------------------------------------------------------------------
C     Calculate 3+ Person Auto Constants
C
      WRITE(26,4550)
 4550 FORMAT(/15X,'3+ Person Auto Constants',/,
     *        15X,'------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
         POBS=0.0
         PEST=0.0
         POBS=OESUM(33,INC)
         PEST=TESUM(33,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=K3P(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         K3P(INC)=NCON
         WRITE(26,4570) INC,POBS,PEST,ECON,RCON,NCON
 4570    FORMAT(1X,' K3P(',I1,')',3X,2(1X,F7.0),1X,
     A                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4561) INC,K3P(INC)
 4561 FORMAT(4X,'K3P(',I1,')=',F9.5)
      END DO
      END IF
C
C----------------------------------------------------------------------
C     Calculate Non-Motorized Constants
C
      IF(CCODE(13)) THEN
      WRITE(26,4551)
 4551 FORMAT(/15X,'Non Motorized Travel',/,
     *        15X,'--------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
         POBS=0.0
         PEST=0.0
         POBS=OESUM(36,INC)
         PEST=TESUM(36,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KNMOT(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KNMOT(INC)=NCON
         WRITE(26,4571) INC,POBS,PEST,ECON,RCON,NCON
 4571    FORMAT(1X,'KNMOT(',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4562) INC,KNMOT(INC)
 4562 FORMAT(4X,'KNMOT(',I1,')=',F9.5)
      END DO
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bicycle Constants
C
      IF(CCODE(21)) THEN
      WRITE(26,4552)
 4552 FORMAT(/15X,'Bicycle Travel',/,
     *        15X,'--------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
         POBS=0.0
         PEST=0.0
         POBS=OESUM(35,INC)
         PEST=TESUM(35,INC)
         RCON=0.0
         IF(POBS.GT.0.0.AND.PEST.GT.0.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBIKE(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
         NCON=DMAX1(NCON,MINVAL)
         KBIKE(INC)=NCON
         WRITE(26,4572) INC,POBS,PEST,ECON,RCON,NCON
 4572    FORMAT(1X,'KBIKE(',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4563) INC,KBIKE(INC)
 4563 FORMAT(4X,'KBIKE(',I1,')=',F9.5)
      END DO
      END IF
C
C----------------------------------------------------------------------
C     Calculate Drive to Transit Constants
C
      IF(CCODE(1)) THEN
      WRITE(26,4601)
 4601 FORMAT(/15X,'Drive to Transit Constants',/,
     *        15X,'--------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(8,INC)+OESUM(11,INC)+OESUM(12,INC)+
     *           OESUM(15,INC)+OESUM(16,INC)+OESUM(19,INC)+
     *           OESUM(20,INC)
            PEST=TESUM(8,INC)+TESUM(11,INC)+TESUM(12,INC)+
     *           TESUM(15,INC)+TESUM(16,INC)+TESUM(19,INC)+
     *           TESUM(20,INC)
         RCON=0.0
         IF(POBS.GT.0.AND.PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KDTRN(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
         NCON=DMAX1(NCON,MINVAL)
         KDTRN(INC)=NCON
         WRITE(26,4602) INC,POBS,PEST,ECON,RCON,NCON
 4602    FORMAT(1X,'KDTRN(',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4603) INC,KDTRN(INC)
 4603 FORMAT(4X,'KDTRN(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Park-n-Ride to GO Rail and TTC Subway Constants
C
      IF(CCODE(2)) THEN
      WRITE(26,4604)
 4604 FORMAT(/15X,'Park-n-Ride to GO Rail/TTC Transit Constants',/,
     *        15X,'--------------------------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(11,INC)+OESUM(15,INC)
            PEST=TESUM(11,INC)+TESUM(15,INC)
         RCON=0.0
         IF(PEST.GT.0.AND.POBS.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KPNR(INC)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KPNR(INC)=NCON
         WRITE(26,4605) INC,POBS,PEST,ECON,RCON,NCON
 4605    FORMAT(1X,'KPNR(',I1,')',3X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4606) INC,KPNR(INC)
 4606 FORMAT(4X,'KPNR(',I1,')=',F9.5)
      END DO 
      END IF
C----------------------------------------------------------------------
C     Calculate Drive to GO Bus Constant
C
      IF(CCODE(3)) THEN
      WRITE(26,4607)
 4607 FORMAT(/15X,'Drive to GO Bus Constant',/,
     *        15X,'------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
         DO INC=1,NCATS
         TO=TO+OESUM(8,INC)
         TE=TE+TESUM(8,INC)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KGOBUSD
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KGOBUSD=NCON
         WRITE(26,4608) TO,TE,ECON,RCON,NCON
 4608    FORMAT(1X,'KGOBUSD  ',1X,2(1X,F7.1),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4609) KGOBUSD
 4609 FORMAT(4X,'KGOBUSD=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate GO Rail Constants
C
      IF(CCODE(4)) THEN
      WRITE(26,4610)
 4610 FORMAT(/15X,'GO Rail Constants',/,
     *        15X,'------------------',/,
     *  1X,' Market'/
     *  1X,' Segment    Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DENOBS=0.0
         DENEST=0.0
      DO INC=1,NCATS
         TO=TO+OESUM(3,INC)
         TE=TE+TESUM(3,INC)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KCR(1)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KCR(1)=NCON
         KCR(2)=NCON
         KCR(3)=NCON
         KCR(4)=NCON
         KCR(5)=NCON
         KCR(6)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(11)
         WRITE(26,4611) TO,TE,ECON,RCON,NCON,EQUIVT
 4611    FORMAT(1X,'KCR ' ' ',5X,2(1X,F7.0),1X,
     A                F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      write(100,4612) (KCR(K),K=1,NCATS)
 4612 FORMAT(4X,'KCR=',F9.5,5(',',F9.5))   
      END IF
C
C----------------------------------------------------------------------
C     Calculate Urban Rail Constants
C
      IF(CCODE(5)) THEN
      WRITE(26,4613)
 4613 FORMAT(/15X,'TTC Subway Constants',/,
     *        15X,'--------------------',/,
     *  1X,' Market'/
     *  1X,' Segment      Obs      Est    Existing               New'/
     *  1X,' Level       Value    Value   Constant Adjustment',
     *          '  Constant'/
     *  1X,'---------  -------- -------- --------- ----------',
     *          ' ---------')
         TO=0.0
         TE=0.0
      DO INC=1,NCATS
         TO=TO+OESUM(4,INC)
         TE=TE+TESUM(4,INC)
      END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KUR(1)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KUR(1)=NCON
         KUR(2)=NCON
         KUR(3)=NCON
         KUR(4)=NCON
         KUR(5)=NCON
         KUR(6)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(11)
         WRITE(26,4614) TO,TE,ECON,RCON,NCON,EQUIVT
 4614    FORMAT(1X,'KUR ' ' ',5X,2(1X,F8.0),1X,
     *      F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      write(100,4615) (KUR(K),K=1,NCATS)
 4615 FORMAT(4X,'KUR=',F9.5,5(',',F9.5))   
      END IF
C
C----------------------------------------------------------------------
C     Calculate GO Bus Constants
C
      IF(CCODE(6)) THEN
      WRITE(26,4616)
 4616 FORMAT(/15X,'GO Bus Constants',/,
     *        15X,'----------------',/,
     *  1X,' Market'/
     *  1X,' Segment    Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
      DO INC=1,NCATS
         TO=TO+OESUM(5,INC)
         TE=TE+TESUM(5,INC)
      END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KGBUS(1)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KGBUS(1)=NCON
         KGBUS(2)=NCON
         KGBUS(3)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(11)
         WRITE(26,4617) TO,TE,ECON,RCON,NCON,EQUIVT
 4617    FORMAT(1X,'KGBUS ',4X,2(1X,F7.1),1X,
     *      F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      write(100,4618) KGBUS
 4618 FORMAT(4X,'KGBUS(1)=',F9.5/
     *       4X,'KGBUS(2)=',F9.5/
     *       4X,'KGBUS(3)=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Rapid Bus Constants
C
      IF(CCODE(11)) THEN
      WRITE(26,4620)
 4620 FORMAT(/15X,'Rapid Bus Constants',/,
     *        15X,'-------------------',/,
     *  1X,' Market'/
     *  1X,' Segment    Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
      DO INC=1,NCATS
         TO=TO+OESUM(38,INC)
         TE=TE+TESUM(38,INC)
      END DO
         RCON=0.0
         IF(TE.GT.0.AND.TO.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KRBUS(1)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KRBUS(1)=NCON
         KRBUS(2)=NCON
         KRBUS(3)=NCON
         KRBUS(4)=NCON
         KRBUS(5)=NCON
         KRBUS(6)=NCON
         EQUIVT=(-1.0*NCON)/COEFF(11)
         WRITE(26,4655) TO,TE,ECON,RCON,NCON,EQUIVT
 4655    FORMAT(1X,'KRBUS ',4X,2(1X,F7.0),1X,
     *      F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      write(100,4656) (KRBUS(K),K=1,NCATS)
 4656 FORMAT(4X,'KRBUS=',6F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate Walk to TTC Subway Constants
C
      IF(CCODE(8)) THEN
      WRITE(26,4622)
 4622 FORMAT(/15X,'Walk to TTC Subway Constants',/,
     *        15X,'----------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(13,INC)
            PEST=TESUM(13,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWUR(INC)
         NCON=ECON+ADJFCT*RCON
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KWUR(INC)=NCON
         WRITE(26,4623) INC,POBS,PEST,ECON,RCON,NCON
 4623    FORMAT(1X,'KWUR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4624) INC,KWUR(INC)
 4624 FORMAT(4X,'KWUR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate CBD Constants
C
      IF(CCODE(9)) THEN
      WRITE(26,4625)
 4625 FORMAT(/15X,'CBD Constants',/,
     *        15X,'--------------',/,
     *  1X,' Market'/
     *  1X,' Segment    Obs     Est     Existing               New'/
     *  1X,' Level      Value   Value   Constant  Adjustment Constant'/
     *  1X,'---------  ------- -------- --------- ---------- ',
     *     '---------')
         TO=0.0
         TE=0.0
      DO K=1,5
         IF(CBDTRP(K).LE.0) CYCLE
         TO=CBDTRP(K)
         TE=ESTCBD(K)
         IF(TE.GT.0.AND.TO.GT.0) THEN
            PEST=TO/TE
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(PEST)
         END IF
         ECON=KCBD(K)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KCBD(K)=NCON
         WRITE(26,4626) K,TO,TE,ECON,RCON,NCON
 4626    FORMAT(1X,'KCBD(',I1, ')',3X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
         END DO
      write(100,4627) KCBD
 4627 FORMAT(4X,'KCBD=',F9.5,4(',',F9.5)) 
      END IF
C
C----------------------------------------------------------------------
C     Calculate PNR to GO Rail Constants
C
      IF(CCODE(12)) THEN
      WRITE(26,4634)
 4634 FORMAT(/15X,'PNR to GO Rail Constants',/,
     *        15X,'------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(11,INC)
            PEST=TESUM(11,INC)
         RCON=0.0
         IF(PEST.GT.0.AND.POBS.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KPCR(INC)
         NCON=ECON+ADJFCT*RCON
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KPCR(INC)=NCON
         WRITE(26,4635) INC,POBS,PEST,ECON,RCON,NCON
 4635    FORMAT(1X,'KPCR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4636) INC,KPCR(INC)
 4636 FORMAT(4X,'KPCR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate KNR to GO Rail Constants
C
      IF(CCODE(14)) THEN
      WRITE(26,4640)
 4640 FORMAT(/15X,'KNR to GO Rail Constants',/,
     *        15X,'------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(12,INC)
            PEST=TESUM(12,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KKCR(INC)
         NCON=ECON+ADJFCT*RCON
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KKCR(INC)=NCON
         WRITE(26,4641) INC,POBS,PEST,ECON,RCON,NCON
 4641    FORMAT(1X,'KKCR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4642) INC,KKCR(INC)
 4642 FORMAT(4X,'KKCR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate KNR to TTC Subway Constants
C
      IF(CCODE(15)) THEN
      WRITE(26,4643)
 4643 FORMAT(/15X,'KNR to TTC Subway Constants',/,
     *        15X,'---------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(16,INC)
            PEST=TESUM(16,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KKUR(INC)
         NCON=ECON+ADJFCT*RCON
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KKUR(INC)=NCON
         WRITE(26,4644) INC,POBS,PEST,ECON,RCON,NCON
 4644    FORMAT(1X,'KKUR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4645) INC,KKUR(INC)
 4645 FORMAT(4X,'KKUR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to GO Rail Constants
C
      IF(CCODE(16)) THEN
      WRITE(26,4628)
 4628 FORMAT(/15X,'Bus to Commuter Rail Constants',/,
     *        15X,'------------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(10,INC)
            PEST=TESUM(10,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBCR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         IF(NCON.GT.0) NCON=0.0
         KBCR(INC)=NCON
         WRITE(26,4646) INC,POBS,PEST,ECON,RCON,NCON
 4646    FORMAT(1X,'KBCR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4630) INC,KBCR(INC)
 4630 FORMAT(4X,'KBCR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to TTC Subway Constants
C
      IF(CCODE(17)) THEN
      WRITE(26,4631)
 4631 FORMAT(/15X,'Bus to Urban Rail Constants',/,
     *        15X,'---------------------------',/,
     *  1X,' Market'/
     *  1X,'Segment      Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(14,INC)
            PEST=TESUM(14,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBUR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         IF(NCON.GT.0) NCON=0.0
         KBUR(INC)=NCON
         WRITE(26,4647) INC,POBS,PEST,ECON,RCON,NCON
 4647    FORMAT(1X,'KBUR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4633) INC,KBUR(INC)
 4633 FORMAT(4X,'KBUR(',I1,')=',F9.5)
      END DO 
      END IF
C----------------------------------------------------------------------
C     Calculate Walk to GO Bus Constant
C
      IF(CCODE(18)) THEN
      WRITE(26,4648)
 4648 FORMAT(/15X,'Walk to GO Bus Constant',/,
     *        15X,'-----------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DO INC=1,NCATS
         TO=TO+OESUM(7,INC)
         TE=TE+TESUM(7,INC)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KGOBUSW
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KGOBUSW=NCON
         WRITE(26,4649) TO,TE,ECON,RCON,NCON
 4649    FORMAT(1X,'KGOBUSW  ',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4650) KGOBUSW
 4650 FORMAT(4X,'KGOBUSW=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to GO Rail Constants
C
      IF(CCODE(19)) THEN
      WRITE(26,4651)
 4651 FORMAT(/15X,'Walk to GO Rail Constants',/,
     *        15X,'-------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(9,INC)
            PEST=TESUM(9,INC)
         RCON=0.0
         IF(PEST.GT.0.AND.POBS.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KWCR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KWCR(INC)=NCON
         WRITE(26,4652) INC,POBS,PEST,ECON,RCON,NCON
 4652    FORMAT(1X,'KWCR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4621) INC,KWCR(INC)
 4621 FORMAT(4X,'KWCR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate PNR to TTC Subway Constants
C
      IF(CCODE(20)) THEN
      WRITE(26,4653)
 4653 FORMAT(/15X,'PNR to TTC Subway Constants',/,
     *        15X,'---------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      DO INC=1,NCATS
            POBS=OESUM(15,INC)
            PEST=TESUM(15,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KPUR(INC)
         NCON=ECON+ADJFCT*RCON
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KPUR(INC)=NCON
         WRITE(26,4654) INC,POBS,PEST,ECON,RCON,NCON
 4654    FORMAT(1X,'KPUR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,4639) INC,KPUR(INC)
 4639 FORMAT(4X,'KPUR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Informal Drive Access to Bus/Streetcar
C
      IF(CCODE(22)) THEN
      WRITE(26,5654)
 5654 FORMAT(/15X,'Informal Drive Access Bus Streetcar Constant',/,
     *        15X,'--------------------------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DO INC=1,NCATS
            POBS=POBS+OESUM(19,INC)
            PEST=PEST+TESUM(19,INC)
      END DO
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KINFLSTR
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KINFLSTR=NCON
         WRITE(26,5655) POBS,PEST,ECON,RCON,NCON
 5655    FORMAT(1X,'KINFLSTR',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,5640) KINFLSTR 
 5640 FORMAT(4X,'KINFLSTR=',F9.5) 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Informal Drive Access to Rapid Bus
C
      IF(CCODE(23)) THEN
      WRITE(26,5755)
 5755 FORMAT(/15X,'Informal Drive Access Rapid Bus Constant',/,
     *        15X,'----------------------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
      POBS=0.0
      PEST=0.0
      DO INC=1,NCATS
            POBS=POBS+OESUM(20,INC)
            PEST=PEST+TESUM(20,INC)
      END DO
         RCON=0.0
         IF(PEST.GT.0.AND.POBS.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KINFLRPD
         NCON=ECON+ADJFCT*RCON
         IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KINFLRPD=NCON
         WRITE(26,5756) POBS,PEST,ECON,RCON,NCON
 5756    FORMAT(1X,'KINFLRPD',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      write(100,5641) KINFLRPD 
 5641 FORMAT(4X,'KINFLRPD=',F9.5) 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Destination End Constants at District Level
C
      IF(CCODE(24)) THEN
      WRITE(26,4725)
 4725 FORMAT(/15X,'Destination End District Constants',/,
     *        15X,'----------------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment    Obs     Est     Existing               New'/
     *  1X,' Level      Value   Value   Constant  Adjustment Constant'/
     *  1X,'---------  ------- -------- --------- ---------- ',
     *     '---------')
         TO=0.0
         TE=0.0
      DO K=1,30
         IF(DSTTRP(K).LE.0) CYCLE
         TO=DSTTRP(K)
         TE=TOTTRN(31,K,1)
         IF(TE.GT.0.AND.TO.GT.0) THEN
            PEST=TO/TE
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(PEST)
         END IF
         ECON=KDIST(K)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KDIST(K)=NCON
         WRITE(26,4726) K,TO,TE,ECON,RCON,NCON
 4726    FORMAT(1X,'KDIST(',I2, ')',1X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
         IF(K.GT.9) THEN
         write(100,4727) K,KDIST(K)
 4727    FORMAT(4X,'KDIST(',I2,')=',5F9.5)
         ELSE
         write(100,4728) K,KDIST(K)
 4728    FORMAT(4X,'KDIST(',I1,')=',5F9.5)         
         END IF
         END DO
      END IF
C----------------------------------------------------------------------
C     Calculate Walk to Bus/Streetcar Constant
C
      IF(CCODE(25)) THEN
      WRITE(26,4678)
 4678 FORMAT(/15X,'Walk to Bus/Streecar Constant',/,
     *        15X,'-----------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DO INC=1,NCATS
         TO=TO+OESUM(17,INC)
         TE=TE+TESUM(17,INC)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KBUSTRW
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KBUSTRW=NCON
         WRITE(26,4679) TO,TE,ECON,RCON,NCON
 4679    FORMAT(1X,'KBUSTRW  ',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4680) KBUSTRW
 4680 FORMAT(4X,'KBUSTRW=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Destination End Constants at District Level
C
      IF(CCODE(26)) THEN
      WRITE(26,4730)
 4730 FORMAT(/15X,'Drive Alone Auto Constants',/,
     *        15X,'--------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment    Obs     Est     Existing               New'/
     *  1X,' Level      Value   Value   Constant  Adjustment Constant'/
     *  1X,'---------  ------- -------- --------- ---------- ',
     *     '---------')
         TO=0.0
         TE=0.0
      DO K=1,NCATS
         IF(.NOT.DAINDEX(K)) CYCLE
         TO=OESUM(31,K)
         TE=TESUM(31,K)
         IF(TE.GT.0.AND.TO.GT.0) THEN
            PEST=TO/TE
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(PEST)
         END IF
         ECON=KDA(K)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KDA(K)=NCON
         WRITE(26,4731) K,TO,TE,ECON,RCON,NCON
 4731    FORMAT(1X,'KDA(',I2, ')',1X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
         write(100,4732) K,KDA(K)
 4732    FORMAT(4X,'KDA(',I1,')=',5F9.5)         
         END DO
      END IF
C
C----------------------------------------------------------------------
C     Calculate GO Rail Line Specific Constants
C
      IF(CCODE(27)) THEN
      WRITE(26,4735)
 4735 FORMAT(/15X,'GO Rail Line Specific Constants',/,
     *        15X,'-------------------------------',/,
     *  1X,'  Line      Obs     Est     Existing               New'/
     *  1X,' Number     Value   Value   Constant  Adjustment Constant'/
     *  1X,'---------  ------- -------- --------- ---------- ',
     *     '---------')
         TO=0.0
         TE=0.0
      DO K=1,MAXGLN
         IF(GORLOBS(K).LE.0.0) CYCLE
         TO=GORLOBS(K)
         TE=GORAIL(K,(MAXGLN+1))
         IF(TE.GT.0.AND.TO.GT.0) THEN
            PEST=TO/TE
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(PEST)
         END IF
         ECON=KGLINE(K)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KGLINE(K)=NCON
         WRITE(26,4736) K,TO,TE,ECON,RCON,NCON
 4736    FORMAT(5X,I2,4X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
         write(100,4737) K,KGLINE(K)
 4737    FORMAT(4X,'KGLINE(',I2,')=',F9.5)         
         END DO
      END IF
C
C----------------------------------------------------------------------
C     Calculate TTC Subway Line Specific Constants
C
      IF(CCODE(28)) THEN
      WRITE(26,4745)
 4745 FORMAT(/15X,'TTC Subway Line Specific Constants',/,
     *        15X,'----------------------------------',/,
     *  1X,'  Line      Obs     Est     Existing               New'/
     *  1X,' Sequence   Value   Value   Constant  Adjustment Constant'/
     *  1X,'---------  ------- -------- --------- ---------- ',
     *     '---------')
         TO=0.0
         TE=0.0
         DO K=1,MAXTLN
         DO K1=1,MAXTLN
         IF(TTCLOBS(K,K1).LE.0.0) CYCLE
         TO=TTCLOBS(K,K1)
         TE=TTCSUBWAY(K,K1)
         IF(TE.GT.0.AND.TO.GT.0) THEN
            PEST=TO/TE
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(PEST)
         END IF
         ECON=KTLINE(K,K1)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KTLINE(K,K1)=NCON
         WRITE(26,4746) K,K1,TO,TE,ECON,RCON,NCON
 4746    FORMAT(3X,I2,'-',I2,3X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
         write(100,4747) K,K1,KTLINE(K,K1)
 4747    FORMAT(4X,'KTLINE(',I2,',',I2,')=',F9.5)        
         END DO
         END DO
      END IF
C----------------------------------------------------------------------
C     Calculate Finch Station Bus Access Constant
C
      IF(CCODE(29)) THEN
      WRITE(26,4753)
 4753 FORMAT(/15X,'Finch Station Bus Access Constant',/,
     *        15X,'---------------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         IF(FINCH.LE.0) THEN
         WRITE(26,4754)
         WRITE(*,4754)
 4754    FORMAT(/' FINCH STATION VALUE UNDEFINED')
         STOP
         END IF
         ORIGSTA=EQUIV(FINCH)-MAX_IZONES
         TO=0.0
         TE=0.0
         TO=FINCHOBS
         TE=STASUM(ORIGSTA,7)
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KFINCHBUS
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KFINCHBUS=NCON
         WRITE(26,4751) TO,TE,ECON,RCON,NCON
 4751    FORMAT(1X,'KFINCHBUS',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4752) KFINCHBUS
 4752 FORMAT(4X,'KFINCHBUS=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Non-Motorized CBD Constants
C
      IF(CCODE(30)) THEN
      WRITE(26,4755)
 4755 FORMAT(/15X,'Non-Motorized CBD Constants',/,
     *        15X,'---------------------------',/,
     *  1X,' District   Obs     Est     Existing               New'/
     *  1X,' Sequence   Value   Value   Constant  Adjustment Constant'/
     *  1X,'---------  ------- -------- --------- ---------- ',
     *     '---------')
         TO=0.0
         TE=0.0
         DO K=1,2
         DO K1=1,2
         IF(NMCBDOBS(K,K1).LE.0.0) CYCLE
         TO=NMCBDOBS(K,K1)
         TE=NONMOT(K,K1,3)
         IF(TE.GT.0.AND.TO.GT.0) THEN
            PEST=TO/TE
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(PEST)
         END IF
         ECON=KNMCBD(K,K1)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KNMCBD(K,K1)=NCON
         WRITE(26,4756) K,K1,TO,TE,ECON,RCON,NCON
 4756    FORMAT(3X,I2,'-',I2,3X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
         write(100,4757) K,K1,KNMCBD(K,K1)
 4757    FORMAT(4X,'KNMCBD(',I2,',',I2,')=',F9.5)        
         END DO
         END DO
      END IF
C----------------------------------------------------------------------
C     Calculate GO Rail Union Station Walk Constant
C
      IF(CCODE(31)) THEN
      WRITE(26,4763)
 4763 FORMAT(/15X,'GO Rail Union Station Walk Constant',/,
     *        15X,'-----------------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         ORIGSTA=EQUIV(9801)-MAX_IZONES
         TO=0.0
         TE=0.0
         TO=GORUNOBS
         TE=STASUM2(ORIGSTA,1)
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KCRWLK
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KCRWLK=NCON
         WRITE(26,4765) TO,TE,ECON,RCON,NCON
 4765    FORMAT(1X,'KCRWLK   ',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4762) KCRWLK
 4762 FORMAT(4X,'KCRWLK=',F9.5)
      END IF
C----------------------------------------------------------------------
C     Taxi Trip Constants
C
      IF(CCODE(32)) THEN
      WRITE(26,4771)
 4771 FORMAT(/15X,'Taxi Mode Constants',/,
     *        15X,'-------------------',/,
     *  1X,' Market'/
     *  1X,' Segment    Obs     Est     Existing               New'/
     *  1X,' Level      Value   Value   Constant  Adjustment Constant'/
     *  1X,'---------  ------- -------- --------- ---------- ',
     *     '---------')
         TO=0.0
         TE=0.0
      DO K=1,NCATS
         TO=OESUM(41,K)
         TE=TESUM(41,K)
         IF(TE.GT.0.AND.TO.GT.0) THEN
            PEST=TO/TE
         ELSE
            PEST=0.0
         END IF
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(PEST)
         END IF
         ECON=KTAXI(K)
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KTAXI(K)=NCON
         WRITE(26,4772) K,TO,TE,ECON,RCON,NCON
 4772    FORMAT(1X,'KTAXI(',I2, ')',1X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
         write(100,4773) K,KTAXI(K)
 4773    FORMAT(4X,'KTAXI(',I1,')=',5F9.5)         
         END DO
      END IF
C----------------------------------------------------------------------
C     Calculate TTC Subway Union Station Walk Constant
C
      IF(CCODE(33)) THEN
      WRITE(26,4766)
 4766 FORMAT(/15X,'TTC Subway Union Station Walk Constant',/,
     *        15X,'--------------------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         ORIGSTA=EQUIV(9771)-MAX_IZONES
         TO=0.0
         TE=0.0
         TO=TTCUNOBS
         TE=STASUM2(ORIGSTA,5)
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KURUNW
         NCON=ECON+STAFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KURUNW=NCON
         WRITE(26,4767) TO,TE,ECON,RCON,NCON
 4767    FORMAT(1X,'KURUNW   ',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4768) KURUNW
 4768 FORMAT(4X,'KURUNW=',F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate Selected Station Walk Egress Constant
C
      IF(CCODE(34)) THEN
      DO K=1,10
      IF(ASELSTA(K).LE.0) CYCLE
      WRITE(26,4776) ASELSTA(K)
 4776 FORMAT(/15X,'Selected TTC Station Walk Egress Constant for',
     *            ' Station=',I4/
     *        15X,'-----------------------------------------',
     *            '-------------',/,
     *  1X,'       '/
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'  Index     Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         ORIGSTA=EQUIV(ASELSTA(K))-MAX_IZONES
         TO=0.0
         TE=0.0
         TO=ASTAOBS(K)
         TE=STASUM2(ORIGSTA,5)
C        IF(TE.LE.TO) CYCLE
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KASTA(K)
         NCON=ECON+STAFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KASTA(K)=NCON
         WRITE(26,4777) K,TO,TE,ECON,RCON,NCON
 4777    FORMAT(1X,'KASTA(',I2,')',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4778) K,KASTA(K)
 4778 FORMAT(4X,'KASTA(',I2,')=',F9.5)
      END DO
      END IF
C----------------------------------------------------------------------
C     Calculate TTC Subway Egress Walk Constant
C
      IF(CCODE(35)) THEN
      WRITE(26,4866)
 4866 FORMAT(/15X,'TTC Subway Egress Walk Constant',/,
     *        15X,'-------------------------------',/,
     *  1X,'             Obs     Est    Existing               New'/
     *  1X,'            Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         TO=TTCEGR
         DESTSTA=EQUIV(9771)-MAX_IZONES
         DO K=1,MAX_STATIONS
         IF(STANUM(K).NE.2) CYCLE
         IF(K.EQ.DESTSTA) CYCLE
         TE=TE+STASUM2(K,5)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KURWLK
         NCON=ECON+STAFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KURWLK=NCON
         WRITE(26,4867) TO,TE,ECON,RCON,NCON
 4867    FORMAT(1X,'KURWLK   ',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4868) KURWLK 
 4868 FORMAT(4X,'KURWLK=',F9.5)
      END IF
C----------------------------------------------------------------------
C     Calculate Walk to Rapid Bus Constant
C
      IF(CCODE(36)) THEN
      WRITE(26,4681)
 4681 FORMAT(/15X,'Walk to Rapid Bus Constant',/,
     *        15X,'--------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
     *  1X,' Level      Value   Value   Constant Adjustment  Constant'/
     *  1X,'---------  ------- ------- --------- ---------- ---------')
         TO=0.0
         TE=0.0
         DO INC=1,NCATS
         TO=TO+OESUM(18,INC)
         TE=TE+TESUM(18,INC)
         END DO
         RCON=0.0
         IF(TE.GT.0) THEN
            RCON=LOG(TO/TE)
         END IF
         ECON=KRBUSW
         NCON=ECON+ADJFCT*RCON
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KRBUSW=NCON
         WRITE(26,4682) TO,TE,ECON,RCON,NCON
 4682    FORMAT(1X,'KRBUSW  ',1X,2(1X,F7.0),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      write(100,4683) KRBUSW
 4683 FORMAT(4X,'KRBUSW=',F9.5)
      END IF
      close(98,status='keep')
	    return
	    end 
