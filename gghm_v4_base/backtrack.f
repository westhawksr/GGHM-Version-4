C-------------------------------------------------------------------
C     EVALUATE TTC SUBWAY TO GO RAIL BACKTRACKING
C-------------------------------------------------------------------
      SUBROUTINE BACKTRACK(STASTA,STAZNE,ZONESTA,EGRIVT)
      INCLUDE 'stadat.inc'
      INCLUDE 'param.inc'
      include 'mlogitpar.inc'
      include 'dvalues.inc'
      integer*2     JZ,IMODE,OGOR,DGOR
      INTEGER*2     ZONESTA(MAX_IZONES)
      INTEGER*2     STA,DS,DSTA,ic,dc,sic,sdc
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(7,MAX_STATIONS,MAX_IZONES),
     *              EGRIVT(1000,2),TOTAL,INVEH(MAX_STATIONS,2)
      CHARACTER*3   ANS(2)
      CHARACTER*13  NAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TT Subway    '/
      DATA          ANS/'---','YES'/
      INVEH=0.0
C
      IF(LDEBUG) WRITE(26,9002)
 9002 FORMAT(//' TTC SUBWAY TO PEARSON AIRPORT IN-VEHICLE + ',
     *         'WAIT TIMES'/
     *       ' ---------------------------------------------',
     *         '----------'/
     *       ' OSTA       NAME            DSTA       NAME          ',
     *       '  STAIVT  STAWGT  EGRIVT  EGRWGT   TOTAL'/
     *       ' ---- --------------------  ---- --------------------',
     *       '  ------  ------  ------  ------  ------')
      JZ=EQUIV(4378)
      IMODE=2
      DO ISTA=1,MAX_STATIONS
      if(stanum(ista).ne.2) cycle
      if(stadata(ista,6).ne.1.0) cycle
      IZ=ISTA+MAX_IZONES
      CALL EGRSTA(JZ,IZ,STASTA,STAZNE,DSTA,IMODE,
     *                  ZONESTA)
      TOTAL=STASTA(3,ISTA,(DSTA-MAX_IZONES))+
     *      STASTA(5,ISTA,(DSTA-MAX_IZONES))+
     *      EGRIVT((DSTA-MAX_IZONES),1)+
     *      EGRIVT((DSTA-MAX_IZONES),2)
      INVEH(ISTA,1)=TOTAL
      IF(LDEBUG) WRITE(26,9001) IEQUIV(IZ),STANAME(ISTA),IEQUIV(DSTA),
     *    STANAME(DSTA-MAX_IZONES),STASTA(3,ISTA,(DSTA-MAX_IZONES)),
     *    STASTA(5,ISTA,(DSTA-MAX_IZONES)),
     *    EGRIVT((DSTA-MAX_IZONES),1),
     *    EGRIVT((DSTA-MAX_IZONES),2),
     *    TOTAL
 9001 FORMAT(1X,I4,1X,A20,2X,I4,1X,A20,5(2X,F6.2))
      END DO
C
      IF(LDEBUG) WRITE(26,9003)
 9003 FORMAT(//' TTC SUBWAY TO UNION STATION/PEARSON IN-VEHICLE + ',
     *         'WAIT TIMES'/
     *       ' ---------------------------------------------------',
     *       '----------'/
     *       ' OSTA       NAME            TTCIVT  TTCWGT  GORIVT',
     *       '  GORWGT   TOTAL'/
     *       ' ---- --------------------  ------  ------  ------',
     *       '  ------  ------')
      DSTA=EQUIV(TTCUNION)
      OGOR=EQUIV(GOUNION)-MAX_IZONES
      DGOR=EQUIV(PEARSON)-MAX_IZONES
      DO ISTA=1,MAX_STATIONS
      if(stanum(ista).ne.2) cycle
      if(stadata(ista,6).ne.1.0) cycle
      IZ=ISTA+MAX_IZONES
      TOTAL=STASTA(3,ISTA,(DSTA-MAX_IZONES))+STASTA(3,OGOR,DGOR)+
     *      STASTA(5,ISTA,(DSTA-MAX_IZONES))+STASTA(5,OGOR,DGOR)
      INVEH(ISTA,2)=TOTAL
      IF(LDEBUG) WRITE(26,9004) IEQUIV(IZ),STANAME(ISTA),
     *  STASTA(3,ISTA,(DSTA-MAX_IZONES)),
     *  STASTA(5,ISTA,(DSTA-MAX_IZONES)),
     *  STASTA(3,OGOR,DGOR),
     *  STASTA(5,OGOR,DGOR),
     *  TOTAL
 9004 FORMAT(1X,I4,1X,A20,5(2X,F6.2))
      END DO
C
      IF(LDEBUG) WRITE(26,9005)
 9005 FORMAT(//' TTC SUBWAY COMPARATIVE TO GO RAIL IN-VEHICLE +',
     *         'WAIT TIMES'/
     *       ' -------------------------------------------------',
     *       '----------'/
     *       ' OSTA       NAME             TTC    GORAIL   DIFF   ELM'/
     *       ' ---- --------------------  ------  ------  ------  ---')
      DO ISTA=1,MAX_STATIONS
      if(stanum(ista).ne.2) cycle
      if(stadata(ista,6).ne.1.0) cycle
      IZ=ISTA+MAX_IZONES
      TOTAL=INVEH(ISTA,2)-INVEH(ISTA,1)
      IF(TOTAL.GT.ACOEF(9)) TTCIND(ISTA)=1
      IF(LDEBUG) WRITE(26,9006) IEQUIV(IZ),STANAME(ISTA),
     *     INVEH(ISTA,1),INVEH(ISTA,2),TOTAL,ANS(TTCIND(ISTA)+1)
 9006 FORMAT(1X,I4,1X,A20,3(2X,F6.2),2X,A3)
      END DO
      RETURN
      END 
     