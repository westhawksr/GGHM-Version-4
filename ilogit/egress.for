C-------------------------------------------------------------------
C        STATION --> DESTINATION ZONE UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE EGRESS(STAZNE,IMODE,ZONESTA,STAZNEI)
       include 'stadat.inc'
       include 'param.inc'
	     include 'ilogitpar.inc'
	     include 'dvalues.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,JZ,ZONESTA(MAX_IZONES)
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,3,4)
      INTEGER*2     LBUS,GBUS,STRC,TTC
      integer*2     unit,type,orgzn,destzn,iz,bunit
	    real*4        frow(max_izones)
	    real*4        prow(max_stations)
	    real*4        srow(max_stations,max_stations)
      REAL*4        STAZNE(6,MAX_STATIONS,MAX_IZONES)
      REAL*4        FARE(max_stations,max_zones),
     *              WAIT2(max_stations,max_zones),
     *              INVEHL(max_stations,max_zones),
     *              INVEHG(max_stations,max_zones),
     *              INVEHS(max_stations,max_zones),
     *              INVEHT(max_stations,max_zones),
     *              TRANSF(max_stations,max_zones),
     *              WAIT1(max_stations,max_zones),
     *              WALKEGR(max_stations,max_zones),
     *              WALKTFR(max_stations,max_zones),
     *              INVEH(max_stations,max_zones),
     *              TTCFAC(max_stations,max_zones),
     *              GORFAC(max_stations,max_zones)
      real*4        wait1a,wait1b
      CHARACTER*13  NAME(3)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   ',
     *                   'GO Bus       '/
C
      WRITE(*,8000) NAME(IMODE)
 8000 FORMAT(1X,'Egress Station --> Destination Zone',
     *            ' Utility Computations for ',a13)
      IF(IMODE.EQ.3) WRITE(*,8001)
 8001 FORMAT(/)
C
C OBTAIN EGRESS PORTION OF PATH DATA
C
C
      type=4
      orgzn=max_izones+1
      destzn=max_zones
      bunit=70
      if(imode.ge.2) bunit=10
C.....1ST WAIT TIME
      unit=bunit+5
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,wait1)
C.....TOTAL WAIT TIME
      unit=bunit+6
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,wait2)
C.....NUMBER OF BOARDINGS
      unit=bunit+7
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,transf)
C.....FARE
      if(imode.eq.1) then
      unit=65
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,fare)      
      unit=66
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,ttcfac)   
      unit=67
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,gorfac)
      else
      unit=62
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,fare)      
      unit=63
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,ttcfac)   
      unit=64
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,gorfac)
      end if       
C...LOCAL BUS IN-VEHICLE TIME
      unit=bunit+1
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,invehl)
C...GO BUS IN-VEHICLE TIME
      unit=bunit+2
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,invehg)
C...STREETCAR IN-VEHICLE TIME
      unit=bunit+3
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,invehs)
C....WALK TIME EGRESS
      unit=bunit+8
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,walkegr)
C...WALK TIME TRANSFER
      unit=bunit+4
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,walktfr)
C...TTC SUBWAY IN-VEHICLE TIME
      if(imode.eq.1) then
      unit=80
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,inveht)
      end if
C 
C DESTINATION STATION LOOP 
C
      DO 100 SC=1,MAX_STATIONS
      IC=SC+MAX_IZONES
      IF(STANUM(SC).NE.IMODE) GO TO 100
      IF(STADATA(SC,6).LE.0.0) GOTO 100
C
C EGRESS ZONE LOOP
C
      DO 200 JZ=1,MAX_IZONES
      STAZNE(2,SC,JZ)=0.0   
C
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
      IF(WALKEGR(SC,JZ).LE.0.0) then
      write(100,300) walkegr(sc,jz),iequiv(ic),iequiv(jz),name(imode)
  300 format(' egress 300 (w) station to destination zone walk time ',
     *       '(',f4.2,
     *       ') for station ',i4,' to destination zone=',i4,
     *       ' for ',a13,' must be greater than zero')
      GO TO 200
      end if
C
C COMPUTE TOTAL IN-VEHICLE TIME
C
      INVEH(SC,JZ)=INVEHL(SC,JZ)+INVEHG(SC,JZ)+INVEHS(SC,JZ)
      IF(IMODE.EQ.1) INVEH(SC,JZ)=INVEH(SC,JZ)+INVEHT(SC,JZ)
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      STAZNE(2,SC,JZ)=COEFF(1)*INVEH(SC,JZ) + COEFF(4)*WAIT1(SC,JZ) +
     *       COEFF(4)*(WAIT2(SC,JZ)-WAIT1(SC,JZ)) + 
     *       COEFF(5)*(TRANSF(SC,JZ)-1.0) +
     *       COEFF(7)*WALKTFR(SC,JZ)
      IF(IMODE.EQ.3) THEN
      WAIT1A=0.0
      WAIT1B=0.0
      WAIT1A=AMIN1(WAIT1(SC,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(SC,JZ),WAITLT)
      IF(INVEHG(SC,JZ).LE.0) THEN
      STAZNE(2,SC,JZ)=0.0
      GO TO 200
      END IF
      STAZNE(2,SC,JZ)=COEFF(11)*INVEH(SC,JZ) + COEFF(12)*WAIT1A +
     *       COEFF(13)*WAIT1B +
     *       COEFF(15)*(WAIT2(SC,JZ)-WAIT1(SC,JZ)) + 
     *       COEFF(20)*(TRANSF(SC,JZ)-1.0) +
     *       COEFF(17)*WALKTFR(SC,JZ)
      END IF
C.....STORE WALK TIME AT DESTINATION
      STAZNE(3,SC,JZ)=WALKEGR(SC,JZ)
C.....STORE TRANSIT FARE
      FARE(SC,JZ)=FARE(SC,JZ)*100.0
      STAZNE(4,SC,JZ)=FARE(SC,JZ)
      STAZNE(5,SC,JZ)=TTCFAC(SC,JZ)
      STAZNE(6,SC,JZ)=GORFAC(SC,JZ)
C.....STORE NUMBER OF TRANSFERS
      STAZNE(1,SC,JZ)=TRANSF(SC,JZ)
      IF(INVEH(SC,JZ).LE.0) THEN
      ZONESTA(JZ)=IC
      STAIND(SC,JZ)=1
      ELSE
      STAIND(SC,JZ)=2
      END IF
C.....STORE SUBMODE USAGE
      LBUS=0
      GBUS=0
      SRTC=0
      TTC=0
      IF(INVEHL(SC,JZ).GT.0) LBUS=1
      IF(INVEHG(SC,JZ).GT.0) GBUS=1
      IF(INVEHS(SC,JZ).GT.0) SRTC=1
      IF(INVEHT(SC,JZ).GT.0) TTC=1
      STAZNEI(SC,JZ,IMODE,1)=LBUS
      STAZNEI(SC,JZ,IMODE,2)=GBUS
      STAZNEI(SC,JZ,IMODE,3)=SRTC
      STAZNEI(SC,JZ,IMODE,4)=TTC
C....................................................................
      IF((DEBUG).AND.(STAZNE(2,SC,JZ).NE.0.0)) THEN
      WRITE(93) IMODE,SC,JZ,INVEH(SC,JZ),WAIT1(SC,JZ),WAIT2(SC,JZ),
     *               TRANSF(SC,JZ),FARE(SC,JZ),WALKEGR(SC,JZ),
     *               WALKTFR(SC,JZ),STAZNE(2,SC,JZ),
     *               INVEHL(SC,JZ),INVEHG(SC,JZ),INVEHS(SC,JZ)
      IF(SDETAIL) THEN
      WRITE(26,9025) NAME(IMODE),IEQUIV(IC),STANAME(SC),IEQUIV(JZ),
     *               INVEH(SC,JZ),
     *               INVEHL(SC,JZ),INVEHG(SC,JZ),INVEHS(SC,JZ),
     *               INVEHT(SC,JZ),
     *               WAIT1(SC,JZ),WAIT2(SC,JZ),
     *               TRANSF(SC,JZ),FARE(SC,JZ),WALKEGR(SC,JZ),
     *               WALKTFR(SC,JZ),STAZNE(2,SC,JZ),STAIND(SC,JZ)
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'IN-VEHICLE-LOCAL     TIME=',F8.2/
     *       1X,'IN-VEHICLE-GO BUS    TIME=',F8.2/
     *       1X,'IN-VEHICLE-STREETCAR TIME=',F8.2/
     *       1X,'IN-VEHICLE-TTC SUBWAYTIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TOTAL WAIT           TIME=',F8.2/
     *       1X,'NUMBER OF BOARDINGS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2//
     *       1X,'UTILITY VALUE            =',F10.5/
     *       1X,'STATION INDICATOR        =',I10/)
      END IF
      END IF
C.......................................................................
  200 CONTINUE
  100 CONTINUE
      RETURN
      END
