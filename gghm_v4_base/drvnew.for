C-------------------------------------------------------------------
C       DRIVE --> EXPRESS BUS/TRANSITWAY UTILITY COMPUTATION
C-------------------------------------------------------------------
       SUBROUTINE DRVNEW(JZ,UTIL,ADIST,ATIME,CSTA,IMODE,STAZNE,
     *        hwydst)
       include 'stadat.inc'
       include 'param.inc'
	     include 'dvalues.inc'
	     include 'mlogitpar.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2    IMODE,JZ,CSTA,ISTA,ic,SMODE,SIC,SDC,SJZ
      INTEGER*2    DC,dsta,IC2,ISTA2
      REAL*4       UTIL,ADIST(1000),ATIME(1000),CDIST,
     *             STAUTL,STAZNE(7,MAX_STATIONS,MAX_IZONES)
      real*4       hwydst(4000)
      REAL*4       UTILCR
      REAL*4       INVEHL,INVEHE,INVEHT,INVEHN,INVEHP
     *             WAIT1,TRWAIT,TRANSF,FARE5,
     *             INVEH2,WALKACC,WALKEGR,
     *             WALKTFR,SELUTL,STAUTL1
      REAL*4       INVEH2J,INVEHLJ,INVEHEJ,INVEHTJ,
     *             INVEHRJ,WAIT1J,TRWAITJ,
     *             TRANSFJ,FARE5J,WALKEGRJ,
     *             WALKTFRJ,WALKACCJ,
     *             STAZNE1,STAZNE2,DAR,
     *             NCAPAC,EAWT,CROWD,LUNREL
      CHARACTER*13 NAME
      LOGICAL      EXT
      DATA          NAME/'GO Bus       '/
C
C   BEST STATION SELECTION
C
      UTIL=0.0
      STAUTL=99999.9
      CDIST=-99999.9
      CSTA=MAX_ZONES
      CCSTA=MAX_ZONES
C
      DO 40,IC=1,MAX_STATIONS
	    ISTA=IC+MAX_IZONES
	    IF(STANUM(IC).NE.IMODE) GOTO 40
      if(stadata(ic,6).le.0.0) goto 40
      if(stadata(ic,3).le.0.0) go to 40
      if(stazne(2,ic,jz).eq.0.0) go to 40
C----------------------------------------------------------------------
      STAUTL=COEFF(16)*ATIME(IC)+ STAZNE(2,IC,JZ)
C.............................................................
      IF(SDETAIL) THEN
      WRITE(26,9001) IEQUIV(ISTA),STANAME(IC),ATIME(IC),
     *               STAZNE(2,IC,JZ),
     *               STAUTL,CDIST,IEQUIV(CSTA)
 9001 FORMAT(' STATION=',I4,1X,A37,' ACCESS TIME=',F8.2,
     *       ' STA->ZNE UTIL=',F8.1,' UTIL=',F8.1,
     *       ' CURRENT BEST=',F8.1,' FOR STATION=',I4)
      END IF
C..............................................................
      IF(STAUTL.GT.CDIST) THEN
      CDIST=STAUTL
      CSTA=ISTA
      ENDIF
  40  CONTINUE
C
      IF(STANUM((CSTA-MAX_IZONES)).EQ.IMODE) THEN
C
C   COMPUTE MODE CHOICE MODEL UTILITY VALUE - ADD AUTO TIME/COST
C
      IC=CSTA-MAX_IZONES
	    DAR=0.0
	    IF(ADIST(ic).GT.0.AND.HWYDST(JZ).GT.0) 
     *    DAR=(ADIST(ic)/hwydst(JZ))-0.5
	    DAR=AMAX1(DAR,0.0)
      UTIL=COEFF(16)*ATIME(ic)+opcost*COEFF(14)*ADIST(ic)+
     *      STAZNE(2,IC,JZ)+CDAR*DAR+COEFF(17)*STADATA(IC,11)+
     *      COEFF(11)*STADATA(IC,12)
C....................................................................
      IF(DEBUG) THEN
      IF(CSTA.NE.MAX_ZONES) THEN
      WRITE(26,9220) NAME
 9220 FORMAT(//1X,'DRIVE ACCESS TO ',A13/
     *         1X,'==========================================='/)
C
C OBTAIN STATION-TO-ZONE DETAILS
C
      REWIND 103
  300 READ(103,END=310) SMODE,SIC,SJZ,INVEHJ,WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKEGRJ,WALKTFRJ,STAUTLJ,
     *               INVEHL,INVEHG,INVEHS,INVEHN,INVEHP,
     *               NCAPAC,EAWT,CROWD,LUNREL
      IF(SMODE.EQ.IMODE.AND.IC.EQ.SIC.AND.SJZ.EQ.JZ) GO TO 350
      GO TO 300
  310 WRITE(26,9028) IMODE,CSTA,JZ
 9028 FORMAT(//' NO MATCH FOR IMODE=',I1,' CSTA=',I4,' JZ=',I4/)
      RETURN
  350 CONTINUE  
      WRITE(26,9025) IEQUIV(SIC+MAX_IZONES),IEQUIV(SJZ),INVEHJ,
     *               INVEHL,INVEHG,INVEHS,INVEHN,INVEHP,
     *               WAIT1J,WAIT2J,
     *               TRANSFJ,FAREJ,WALKEGRJ,WALKTFRJ,
     *               NCAPAC,EAWT,CROWD,LUNREL,
     *               STAUTLJ
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'IN-VEHICLE-LOCAL     TIME=',F8.2/
     *       1X,'IN-VEHICLE-GO BUS    TIME=',F8.2/
     *       1X,'IN-VEHICLE-STREETCAR TIME=',F8.2/
     *       1X,'IN-VEHICLE-RAPID BUS TIME=',F8.2/
     *       1X,'IN-VEHICLE-PREMIUM   TIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TOTAL WAIT           TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2//
     *       1X,'NODE CAPACITY PENALTY    =',F8.2/
     *       1X,'EXTRA ADDED WAIT TIME    =',F8.2/
     *       1X,'CROWDING TIME            =',F8.2/
     *       1X,'LINK UNRELIABILITY TIME  =',F8.2//
     *       1X,'UTILITY VALUE            =',F10.5/)
      IF(IC.LT.0) IC=MAX_STATIONS
      IF(DC.LT.0) DC=MAX_STATIONS
      WRITE(26,9047) NAME,IEQUIV(CSTA),STANAME(IC),ATIME(ic),
     *  ADIST(ic),hwydst(JZ),DAR,
     * 	STAZNE(2,IC,JZ),STADATA(IC,11),STADATA(IC,12),
     *  UTIL
 9047 FORMAT(//1X,' Drive Access Station LOS Data -- ',A13/
     *       1X,'------------------------------------------------'/
     *       1X,'ACCESS STATION NUMBER     =',I10,5X,A37/
     *       1X,'HIGHWAY TIME              =',F10.2/
     *       1X,'HIGHWAY DISTANCE (STA)    =',F10.2/
     *       1X,'HIGHWAY DISTANCE (JZ)     =',F10.2/
     *       1X,'DRIVE ACCESS RATIO        =',F10.2/
     *       1X,'STATION-DESTINATION UTIL  =',F10.4/
     *       1X,'STATION WALK ACCESS TIME  =',F10.4/
     *       1X,'STATION IVT  ACCESS TIME  =',F10.4/
     *       1X,'MODE CHOICE MODEL UTILITY =',F10.4/)
      END IF
      END IF
      ELSE
      IF(DEBUG) THEN
      WRITE(26,9220) NAME
      WRITE(26,9221)
 9221 FORMAT(' ***** NO PATH AVAILABLE ******')
      END IF
      END IF
C -----------------------------------------------------------------
	    RETURN
	    END
