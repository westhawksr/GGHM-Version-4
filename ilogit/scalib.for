      subroutine scalib(iter,tesum)
      include 'ilogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
c     self calibration subroutine
c
      character*80   header,mkt
      integer*2      iter
      integer*4      nseg,ci,c
      real*8         values(14),pobs,pest,econ,ncon,rcon
      real*8         tesum(20,4),oesum(20,4),dfsum(20,4)
      real*8         maxval,minval
      logical        exists
      data           minval/-15.0/
      nseg=0
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
      read(98,1,end=100,err=50) header
    1 format(a80)
   10 read(98,*,end=100,err=50) mkt,values
      nseg=nseg+1
      oesum(9,nseg)=values(1)
      oesum(10,nseg)=values(2)
      oesum(11,nseg)=values(3)
      oesum(12,nseg)=values(4)
      oesum(3,nseg)=values(5)
      oesum(13,nseg)=values(6)
      oesum(14,nseg)=values(7)
      oesum(15,nseg)=values(8)
      oesum(16,nseg)=values(9)
      oesum(4,nseg)=values(10)
      oesum(7,nseg)=values(11)
      oesum(8,nseg)=values(12)
      oesum(5,nseg)=values(13)
      oesum(2,nseg)=values(14)
      go to 10
   50 write(26,51) nseg
   51 format(' scalib 51 (f) error reading calibration target',
     *       ' value file for record=',i1)
      stop 51
  100 continue
C
      CI=NCATS+1
      DO K=1,20
      DO C=1,CI
      DFSUM(K,C)=TESUM(K,C)-OESUM(K,C)
      END DO
      END DO
      WRITE(26,7004)
 7004 FORMAT(//,30X,'R E P O R T   2',/,
     *          20X,
     *         'SUMMARIZE OBSERVED TRANSIT TRIPS BY MARKET SEGMENT',//,
     *       1X,' MARKET  '/
     *       1X,' SEGMENT ','    GO    ','   TTC    ','   GO     '/
     *       1X,'  LEVEL  ','    RAIL  ','  SUBWAY  ','   BUS    ',
     *                      '  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,7005) C,OESUM(3,C),OESUM(4,C),OESUM(5,C),OESUM(2,C)
 7005 FORMAT(3X,I1,4X,4F10.1)
      END DO
      WRITE(26,7006) OESUM(3,CI),OESUM(4,CI),OESUM(5,CI),OESUM(2,CI)      
 7006 FORMAT(/,2X,'TOTAL',1X,4F10.1)
      WRITE(26,7001)
 7001 FORMAT(//,30X,'R E P O R T   2A',/,
     *          20X,
     *         'SUMMARIZE ESTIMATED-OBSERVED BY MARKET SEGMENT',//,
     *       1X,' MARKET  '/
     *       1X,' SEGMENT ','    GO    ','   TTC    ','   GO     '/
     *       1X,'  LEVEL  ','    RAIL  ','  SUBWAY  ','   BUS    ',
     *                      '  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,7005) C,DFSUM(3,C),DFSUM(4,C),DFSUM(5,C),DFSUM(2,C)
      END DO
      WRITE(26,7006) DFSUM(3,CI),DFSUM(4,CI),DFSUM(5,CI),DFSUM(2,CI)
C
C GO RAIL AND THEN TTC SUBWAY
C
      WRITE(26,7007)
 7007 FORMAT(//,30X,'R E P O R T   3',/,
     *          20X,
     *          'SUMMARIZE OBSERVED GO RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ',
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
      WRITE(26,7002)
 7002 FORMAT(//,30X,'R E P O R T   3A',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED-OBSERVED GO RAIL',//,
     *       1X,' MARKET  ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------','----------','----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(DFSUM(K,C),K=9,12),DFSUM(3,C)
      END DO
      WRITE(26,7010) (DFSUM(K,CI),K=9,12),DFSUM(3,CI)
      WRITE(26,7011)
 7011 FORMAT(//,30X,'R E P O R T   4',/,
     *          20X,
     *          'SUMMARIZE OBSERVED TTC SUBWAY BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------','----------','----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(OESUM(K,C),K=13,16),OESUM(4,C)
      END DO
      WRITE(26,7010) (OESUM(K,CI),K=13,16),OESUM(4,CI)
      WRITE(26,7003)
 7003 FORMAT(//,30X,'R E P O R T   4A',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED-OBSERVED TTC SUBWAY',//,
     *       1X,' MARKET  ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------','----------','----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(DFSUM(K,C),K=13,16),DFSUM(4,C)
      END DO
      WRITE(26,7010) (DFSUM(K,CI),K=13,16),DFSUM(4,CI)
C
      WRITE(26,7031)
 7031 FORMAT(//,30X,'R E P O R T   5',/,
     *          20X,
     *          'SUMMARIZE OBSERVED GO BUS BY MARKET SEGMENT',//,
     *       1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ','    WALK  ','   DRIVE  ',
     *          '  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------')
      TTOTAL=0.0
      TTPNR=0.0
      DO C=1,NCATS
      TTPNR=OESUM(7,C)+OESUM(8,C)
      TTOTAL=TTOTAL+TTPNR
      WRITE(26,7009) C,(OESUM(K,C),K=7,8),TTPNR
      END DO
      WRITE(26,7010) (OESUM(K,CI),K=7,8),TTOTAL
      WRITE(26,7013)
 7013 FORMAT(//,30X,'R E P O R T   5A',/,
     *          20X,
     *          'SUMMARIZE ESTIMATED - OBSERVED GO BUS BY',
     *          ' MARKET SEGMENT',//,
     *       1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ','    WALK  ','   DRIVE  ',
     *          '  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------')
      TTOTAL=0.0
      TTPNR=0.0
      DO C=1,NCATS
      TTPNR=DFSUM(7,C)+DFSUM(8,C)
      TTOTAL=TTOTAL+TTPNR
      WRITE(26,7009) C,(DFSUM(K,C),K=7,8),TTPNR
      END DO
      WRITE(26,7010) (DFSUM(K,CI),K=7,8),TTOTAL
      write(97,7032) iter
 7032 format(/' Calibration Iteration ',i2/
     *        ' -------------------------'/)
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
     *           OESUM(15,INC)+OESUM(16,INC)
            PEST=TESUM(8,INC)+TESUM(11,INC)+TESUM(12,INC)+
     *           TESUM(15,INC)+TESUM(16,INC)
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
      WRITE(97,4603) INC,KDTRN(INC)
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
         IF(PEST.GT.0) THEN
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
      WRITE(97,4606) INC,KPNR(INC)
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
      WRITE(97,4609) KGOBUSD
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
         EQUIVT=(-1.0*NCON)/COEFF(11)
         WRITE(26,4611) TO,TE,ECON,RCON,NCON,EQUIVT
 4611    FORMAT(1X,'KCR ' ' ',5X,2F8.0,1X,
     A                F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(97,4612) KCR
 4612 FORMAT(4X,'KCR(1)=',F9.5/
     *       4X,'KCR(2)=',F9.5/
     *       4X,'KCR(3)=',F9.5)
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
         EQUIVT=(-1.0*NCON)/COEFF(11)
         WRITE(26,4614) TO,TE,ECON,RCON,NCON,EQUIVT
 4614    FORMAT(1X,'KUR ' ' ',5X,2(1X,F8.0),1X,
     *      F9.5,1X,F10.5,1X,F9.5,1X,'(',F7.2,')')
      WRITE(97,4615) KUR
 4615 FORMAT(4X,'KUR(1)=',F9.5/
     *       4X,'KUR(2)=',F9.5/
     *       4X,'KUR(3)=',F9.5)
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
      WRITE(97,4618) KGBUS
 4618 FORMAT(4X,'KGBUS(1)=',F9.5/
     *       4X,'KGBUS(2)=',F9.5/
     *       4X,'KGBUS(3)=',F9.5)
      END IF
C
C----------------------------------------------------------------------
C     Calculate Walk to GO Rail Constants
C
      IF(CCODE(7)) THEN
      WRITE(26,4619)
 4619 FORMAT(/15X,'Walk to GO Rail Constants',/,
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
         WRITE(26,4620) INC,POBS,PEST,ECON,RCON,NCON
 4620    FORMAT(1X,'KWCR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4621) INC,KWCR(INC)
 4621 FORMAT(4X,'KWCR(',I1,')=',F9.5)
      END DO 
      END IF
C
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
      WRITE(97,4624) INC,KWUR(INC)
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
      DO K=1,3
         IF(CBDTRP(K).LE.0) CYCLE
         TO=CBDTRP(K)
         TE=ESTCBD(K)
         IF(TE.GT.0) THEN
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
         IF(NCON.LT.0) NCON=0.0
C        IF(POBS.EQ.0.0) NCON=MINVAL
C        IF(POBS.EQ.1.0) NCON=MAXVAL
C        NCON=DMAX1(NCON,MINVAL)
         KCBD(K)=NCON
         WRITE(26,4626) K,TO,TE,ECON,RCON,NCON
 4626    FORMAT(1X,'KCBD(',I1, ')',3X,F8.1,1X,F8.1,1X,
     *      F9.5,1X,F9.5,1X,F9.5)
      WRITE(97,4627) K,KCBD(K)
 4627 FORMAT(4X,'KCBD(',I1,')=',F9.5)
         END DO
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to GO Rail Constants
C
      IF(CCODE(10)) THEN
      WRITE(26,4628)
 4628 FORMAT(/15X,'Bus to GO Rail Constants',/,
     *        15X,'------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
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
         IF(NCON.GT.0) NCON=0.0
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KBCR(INC)=NCON
         WRITE(26,4629) INC,POBS,PEST,ECON,RCON,NCON
 4629    FORMAT(1X,'KBCR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4630) INC,KBCR(INC)
 4630 FORMAT(4X,'KBCR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to TTC Subway Constants
C
      IF(CCODE(11)) THEN
      WRITE(26,4631)
 4631 FORMAT(/15X,'Bus to TTC Subway Constants',/,
     *        15X,'---------------------------',/,
     *  1X,' Market'/
     *  1X,' Segment     Obs     Est    Existing               New'/
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
         IF(NCON.GT.0) NCON=0.0
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KBUR(INC)=NCON
         WRITE(26,4632) INC,POBS,PEST,ECON,RCON,NCON
 4632    FORMAT(1X,'KBUR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4633) INC,KBUR(INC)
 4633 FORMAT(4X,'KBUR(',I1,')=',F9.5)
      END DO 
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
         IF(PEST.GT.0) THEN
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
      WRITE(97,4636) INC,KPCR(INC)
 4636 FORMAT(4X,'KPCR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate PNR to TTC Subway Constants
C
      IF(CCODE(13)) THEN
      WRITE(26,4637)
 4637 FORMAT(/15X,'PNR to TTC Subway Constants',/,
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
         WRITE(26,4638) INC,POBS,PEST,ECON,RCON,NCON
 4638    FORMAT(1X,'KPUR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4639) INC,KPUR(INC)
 4639 FORMAT(4X,'KPUR(',I1,')=',F9.5)
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
 4641    FORMAT(1X,'KKCR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4642) INC,KKCR(INC)
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
 4644    FORMAT(1X,'KKUR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4645) INC,KKUR(INC)
 4645 FORMAT(4X,'KKUR(',I1,')=',F9.5)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to GO Rail Constants
C
      IF(CCODE(16)) THEN
      WRITE(26,4628)
      DO INC=1,NCATS
            POBS=OESUM(10,INC)
            PEST=TESUM(10,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
            RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBCR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(NCON.GT.0) NCON=0.0
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KBCR(INC)=NCON
         IF(KBCR(INC).GT.0) THEN
         KBCR(INC)=0.0
         NCON=0.0
         END IF
         WRITE(26,4646) INC,POBS,PEST,ECON,RCON,NCON
 4646    FORMAT(1X,'KBCR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4630) INC,KBCR(INC)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate Bus to TTC Subway Constants
C
      IF(CCODE(17)) THEN
      WRITE(26,4631)
      DO INC=1,NCATS
            POBS=OESUM(14,INC)
            PEST=TESUM(14,INC)
         RCON=0.0
         IF(PEST.GT.0) THEN
         RCON=DLOG(POBS/PEST)
         END IF
         ECON=KBUR(INC)
         NCON=ECON+ADJFCT*RCON
         IF(NCON.GT.0) NCON=0.0
c        IF(POBS.EQ.0.0) NCON=MINVAL
c        IF(POBS.EQ.1.0) NCON=MAXVAL
c        NCON=DMAX1(NCON,MINVAL)
         KBUR(INC)=NCON
         IF(KBUR(INC).GT.0) THEN
         KBUR(INC)=0.0
         NCON=0.0
         END IF
         WRITE(26,4647) INC,POBS,PEST,ECON,RCON,NCON
 4647    FORMAT(1X,'KBUR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4633) INC,KBUR(INC)
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
 4649    FORMAT(1X,'KGOBUSW  ',1X,2(1X,F7.1),1X,
     *        F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4650) KGOBUSW
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
 4652    FORMAT(1X,'KWCR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4621) INC,KWCR(INC)
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
 4654    FORMAT(1X,'KPUR (',I1,')',2X,2(1X,F7.1),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4639) INC,KPUR(INC)
      END DO 
      END IF
C
C----------------------------------------------------------------------
C     Calculate KNR to GO Rail Constants
C
      IF(CCODE(21)) THEN
      WRITE(26,4664)
 4664 FORMAT(/15X,'KNR to GO Rail Constants',/,
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
         WRITE(26,4665) INC,POBS,PEST,ECON,RCON,NCON
 4665    FORMAT(1X,'KKCR (',I1,')',2X,2(1X,F7.0),1X,
     *                             F9.5,1X,F10.5,1X,F9.5)
      WRITE(97,4666) INC,KKCR(INC)
 4666 FORMAT(4X,'KKCR(',I1,')=',F9.5)
      END DO 
      END IF
	    return
	    end 
