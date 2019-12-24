C-------------------------------------------------------------------
C        EGRESS STATION --> DESTINATION ZONE SELECTION SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE EGRSTA(JZ,STA,STASTA,STAZNE,DSTA,IMODE,
     *                  ZONESTA)
       INCLUDE 'stadat.inc'
       INCLUDE 'param.inc'
       include 'ilogitpar.inc'
       include 'dvalues.inc'
      integer*2     jz,imode,SMODE,SJZ
      INTEGER*2     ZONESTA(MAX_IZONES)
      INTEGER*2     STA,DS,DSTA,ic,dc,sic,sdc
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(6,MAX_STATIONS,MAX_IZONES),
     *              DDIST,UTIL,UTIL2,UTIL3
      REAL*4        WAIT1,WAIT2,
     *              TRANSF,FARE,INVEH,WALKTFR,STAUTL  
      REAL*4        INVEHL,INVEHG,INVEHS
      REAL*4        INVEHJ,WAIT1J,WAIT2J,
     *              TRANSFJ,FAREJ,WALKEGRJ,WALKTFRJ,STAUTLJ
      REAL*4        DISTEGR
      CHARACTER*13  NAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TT Subway    '/
      DDIST=-99999.9
      DSTA=MAX_ZONES
      IC=STA-MAX_IZONES
      if(sdetail) write(26,9002) iequiv(sta)
 9002 format(//' STATION=',i4)
      IF(IC.LT.0) RETURN
C
C
C DESTINATION STATION LOOP
C
      DO 100 DC=1,MAX_STATIONS
      DS=DC+MAX_IZONES
      IF(DC.EQ.IC) GOTO 100
      IF(DC.EQ.MAX_STATIONS) GO TO 100
c      IF(STANUM(DC).NE.IMODE) GO TO 100
c      IF(STADATA(DC,6).LE.0.0) GO TO 100
      IF(STASTA(2,IC,DC).EQ.0) GO TO 100
C
C COMPUTE TOTAL UTILITY EXPRESSION FOR COMPARISON
C
      UTIL=STASTA(2,IC,DC)+STAZNE(2,DC,JZ)
C.............................................................
      IF(SDETAIL) THEN
      WRITE(26,9001) IEQUIV(DS),STANAME(DC),STAIND(DC,JZ),UTIL,
     *               STASTA(2,IC,DC),
     *               STAZNE(2,DC,JZ),DDIST,IEQUIV(DSTA)
 9001 FORMAT(' STATION=',I4,1X,A37,' STAIND=',I1,
     *       ' UTIL=',F8.1,
     *       ' STA->STA=',F8.1,' STA->ZNE=',F8.1,
     *       ' CURRENT BEST=',F8.1,' FOR STATION=',I4)
      END IF
C..............................................................
      IF(UTIL.GT.DDIST) THEN
      DDIST=UTIL
      DSTA=DS
      END IF
 100  continue
      DC=DSTA-MAX_IZONES
      IF(DC.LT.0) DC=MAX_STATIONS
C....................................................................
C     IF(DEBUG.AND.(JOI(JZ))) THEN
      IF(DEBUG) THEN
      DC=DSTA-MAX_IZONES
      IF(DC.GT.0.AND.DC.LT.MAX_STATIONS) THEN
C 
C OBTAIN STATION-TO-STATION DETAILS 
C
      DC=DSTA-MAX_IZONES
      REWIND 91
  200 READ(91,END=210) SMODE,SIC,SDC,WAIT1,WAIT2,
     *                TRANSF,FARE,INVEH,WALKTFR,STAUTL
      IF(SMODE.EQ.IMODE.AND.IC.EQ.SIC.AND.DC.EQ.SDC) GO TO 250
      GO TO 200
  210 WRITE(26,9027) IMODE,IEQUIV(STA),
     *               IEQUIV(DSTA)
 9027 FORMAT(//' NO STA-STA MATCH FOR IMODE=',I1,' STA=',I4,
     *         ' DSTA=',I4/)
c      STOP 16
  250 CONTINUE
      WRITE(26,9029) IEQUIV(SIC+MAX_IZONES),IEQUIV(SDC+MAX_IZONES),
     *                WAIT1,WAIT2,
     *                TRANSF,FARE,INVEH,
     *                WALKTFR,STAUTL
 9029 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8/
     *       1X,'DESTINATION       STATION=',I8/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TOTAL WAIT           TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'PRIMARY MODE IN-VEHICLE  =',F8.2/
     *       1X,'TRANSFER WALK TIME       =',F8.2//
     *       1X,'UTILITY VALUE            =',F10.5/)
C
C OBTAIN STATION-TO-ZONE DETAILS
C
      REWIND 93
  300 READ(93,END=310) SMODE,SIC,SJZ,INVEHJ,WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKEGRJ,WALKTFRJ,STAUTLJ,
     *               INVEHL,INVEHG,INVEHS
      IF(SMODE.EQ.IMODE.AND.DC.EQ.SIC.AND.SJZ.EQ.JZ) GO TO 350
      GO TO 300
  310 WRITE(26,9028) IMODE,DC,JZ
 9028 FORMAT(//' NO STA-ZNE MATCH FOR IMODE=',I1,' DSTA=',I4,
     *         ' JZ=',I4/)
c      STOP 16
  350 CONTINUE                                        
      WRITE(26,9025) IEQUIV(SIC+MAX_IZONES),IEQUIV(SJZ),INVEHJ,
     *               INVEHL,INVEHG,INVEHS,
     *               WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKEGRJ,WALKTFRJ,
     *               STAUTLJ
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'IN-VEHICLE-LOCAL     TIME=',F8.2/
     *       1X,'IN-VEHICLE-GO BUS    TIME=',F8.2/
     *       1X,'IN-VEHICLE-STREETCAR TIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TOTAL WAIT           TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2//
     *       1X,'UTILITY VALUE            =',F10.5/)    
      ELSE                                                              
      WRITE(26,9026) NAME(IMODE),STA,JZ,DSTA                                        
 9026 FORMAT(/1X,'EGRESS STATION SELECTION -- ',A13/                            
     *       1X,'------------------------'/                             
     *       1X,'ACCESS STATION    =',I10/                              
     *       1X,'DESTINATION ZONE  =',I10/                              
     *       1X,'DEST   STATION    =',I10)                              
      ENDIF                                                             
      ENDIF
C.....................................................................
      RETURN
      END
