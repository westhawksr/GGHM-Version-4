C-------------------------------------------------------------------
C        STATION --> DESTINATION ZONE UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE EGRESS(STAZNE,IMODE,ZONESTA,STAZNEI,EGRIVT)
       include 'stadat.inc'
       include 'param.inc'
	     include 'mlogitpar.inc'
	     include 'dvalues.inc'
C
C DATA DECLARATIONS
C
      INTEGER*2     IMODE,SC,IC,JZ,ZONESTA(MAX_IZONES)
      INTEGER*2     STAZNEI(MAX_STATIONS,MAX_IZONES,3,5)
      INTEGER*2     LBUS,GBUS,STRC,RBUS,PBUS
      integer*2     unit,type,orgzn,destzn,iz,bunit
	    real*4        frow(4000)
	    real*4        prow(1000)
	    real*4        srow(1000,1000)
      REAL*4        STAZNE(7,MAX_STATIONS,MAX_IZONES)
      REAL*4        FARE(1000,4000),
     *              WAIT2(1000,4000),
     *              INVEHL(1000,4000),
     *              INVEHG(1000,4000),
     *              INVEHS(1000,4000),
     *              INVEHN(1000,4000),
     *              INVEHP(1000,4000),
     *              TRANSF(1000,4000),
     *              WAIT1(1000,4000),
     *              WALKEGR(1000,4000),
     *              WALKTFR(1000,4000),
     *              INVEH(1000,4000),
     *              TTCFAC(1000,4000),
     *              GORFAC(1000,4000),
     *              DIRWALK(1000,4000)
      REAL*4        NCAPAC(1000,4000),
     *              EAWT(1000,4000),
     *              CROWD(1000,4000),
     *              LUNREL(1000,4000)
      real*4        egrivt(1000,2)
      real*4        wait1a,wait1b,invrat,rativt
      CHARACTER*13  NAME(3)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   ',
     *                   'GO Bus       '/
      ncapac=0.0
      eawt=0.0
      crowd=0.0
      lunrel=0.0
      invrat=0.0
      rativt=0.0
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
      bunit=10
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
      unit=62
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,fare)      
      unit=63
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,ttcfac)   
      unit=64
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,gorfac)   
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
C...RAPID BUS IN-VEHICLE TIME
      unit=65
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,invehn)
C...PREMIUM BUS IN-VEHICLE TIME
      unit=66
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,invehp)
C...CAPACITY, CROWDING & RELIABILITY VARAIBLES
      IF(CCR) THEN
C.....NODE CAPACITY PENALTY
      unit=108
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,ncapac)
C.....EXTRA ADDED WAIT TIME
      unit=109
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,eawt)      
C.....LINK CROWDING TIME
      unit=110
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,crowd)
C.....LINK UNRELIABILITY
      unit=111
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,lunrel)      
      END IF
C.....DIRECT WALK FROM STATION
      unit=19
      call mfread(unit,type,orgzn,destzn,frow,srow,prow,dirwalk)       
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
C UNION STATION WALK EGRESS 
C
C...GO RAIL
      IF(IMODE.EQ.1.AND.IEQUIV(IC).EQ.gounion.AND.
     *   DIRWALK(SC,JZ).LE.UNIONGR) THEN
      STAZNE(3,SC,JZ)=DIRWALK(SC,JZ)
      FARE(SC,JZ)=0.0
      STAZNE(4,SC,JZ)=0.0
      STAZNE(5,SC,JZ)=0.0
      STAZNE(6,SC,JZ)=0.0
      STAZNE(1,SC,JZ)=0.0
      ZONESTA(JZ)=IC
      STAIND(SC,JZ)=1
C....................................................................
      IF(DEBUG.AND.SDETAIL) THEN
      WRITE(26,9026) NAME(IMODE),IEQUIV(IC),STANAME(SC),IEQUIV(JZ),
     *               DIRWALK(SC,JZ),
     *               STAZNE(2,SC,JZ),STAIND(SC,JZ)
 9026 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'DIRECT WALK       TIME   =',F8.2/
     *       1X,'UTILITY VALUE            =',F10.5/
     *       1X,'STATION INDICATOR        =',I10/)
      END IF
C....................................................................
      GO TO 200
      END IF
C...TTC SUBWAY
      IF(IMODE.EQ.2.AND.IEQUIV(IC).EQ.ttcunion.AND.
     *   DIRWALK(SC,JZ).LE.UNIONTTC) THEN
      STAZNE(3,SC,JZ)=DIRWALK(SC,JZ)
      FARE(SC,JZ)=0.0
      STAZNE(4,SC,JZ)=0.0
      STAZNE(5,SC,JZ)=0.0
      STAZNE(6,SC,JZ)=0.0
      STAZNE(1,SC,JZ)=0.0
      ZONESTA(JZ)=IC
      STAIND(SC,JZ)=1
C....................................................................
      IF(DEBUG.AND.SDETAIL) THEN
      WRITE(26,9026) NAME(IMODE),IEQUIV(IC),STANAME(SC),IEQUIV(JZ),
     *               DIRWALK(SC,JZ),
     *               STAZNE(2,SC,JZ),STAIND(SC,JZ)
      END IF
C....................................................................
      GO TO 200
      END IF
C
C DESTINATION STATION --> EGRESS ZONE VALIDITY CHECKS
C
      IF(WALKEGR(SC,JZ).LE.0.0) then
      write(41,300) walkegr(sc,jz),iequiv(ic),iequiv(jz),name(imode)
  300 format(' egress 300 (w) station to destination zone walk time ',
     *       '(',f4.2,
     *       ') for station ',i4,' to destination zone=',i4,
     *       ' for ',a13,' must be greater than zero')
      stazne(2,sc,jz)=-999.99
      GO TO 200
      end if
C
C COMPUTE TOTAL IN-VEHICLE TIME
C
      INVEH(SC,JZ)=INVEHL(SC,JZ)+INVEHG(SC,JZ)+INVEHS(SC,JZ)+
     *             INVEHN(SC,JZ)+INVEHP(SC,JZ)
      IF(AIRPASS.AND.JZ.EQ.EQUIV(4378).AND.IMODE.EQ.2) THEN
      EGRIVT(SC,1)=INVEH(SC,JZ)
      EGRIVT(SC,2)=WAIT2(SC,JZ)
      END IF
C
C COMPUTE EGRESS PORTION OF UTILITY
C
C....USING MODEL COEFFICIENTS
      STAZNE(2,SC,JZ)=COEFF(1)*INVEH(SC,JZ) + COEFF(4)*WAIT1(SC,JZ) +
     *       COEFF(4)*(WAIT2(SC,JZ)-WAIT1(SC,JZ)) + 
     *       COEFF(5)*(TRANSF(SC,JZ)-1.0) +
     *       COEFF(7)*WALKTFR(SC,JZ) +
     *       COEFF(20)*NCAPAC(sc,jz)/(LSUM2CR*LSUM1TRN*LSUM3CW) + 
     *       COEFF(21)*EAWT(sc,jz)/(LSUM2CR*LSUM1TRN*LSUM3CW) +
     *       COEFF(22)*CROWD(sc,jz)/(LSUM2CR*LSUM1TRN*LSUM3CW) + 
     *       COEFF(23)*LUNREL(sc,jz)/(LSUM2CR*LSUM1TRN*LSUM3CW)
C     IF(AIR.AND.IEQUIV(IC).EQ.9701) THEN
C     STAZNE(2,SC,JZ)=STAZNE(2,SC,JZ)-COEFF(5)*(TRANSF(SC,JZ)-1.0)
C     END IF
      IF(AIRPASS.AND.IMODE.EQ.2) THEN
      STAZNE(2,SC,JZ)=STAZNE(2,SC,JZ)+COEFF(1)*INVEH(SC,JZ)*COEFF(80)
      END IF
      IF(IMODE.EQ.3) THEN
      WAIT1A=0.0
      WAIT1B=0.0
      WAIT1A=AMIN1(WAIT1(SC,JZ),WAITLT)
      WAIT1B=DIM(WAIT1(SC,JZ),WAITLT)
      IF(INVEHG(SC,JZ).LE.0.AND.INVEHP(SC,JZ).LE.0) THEN
      STAZNE(2,SC,JZ)=0.0
      GO TO 200
      END IF
      INVRAT=1.0-((INVEHG(SC,JZ)+INVEHP(SC,JZ))/INVEH(SC,JZ))
      RATIVT=LOG((INVEHG(SC,JZ)+INVEHP(SC,JZ))/INVEH(SC,JZ))
      STAZNE(7,SC,JZ)=(INVEHG(SC,JZ)+INVEHP(SC,JZ))/INVEH(SC,JZ)
      STAZNE(2,SC,JZ)=COEFF(11)*INVEH(SC,JZ) + COEFF(12)*WAIT1A +
     *       COEFF(13)*WAIT1B +
     *       COEFF(15)*(WAIT2(SC,JZ)-WAIT1(SC,JZ)) + 
     *       COEFF(24)*(TRANSF(SC,JZ)-1.0) +
     *       COEFF(17)*WALKTFR(SC,JZ) +
     *       COEFF(20)*NCAPAC(sc,jz) + 
     *       COEFF(21)*EAWT(sc,jz) +
     *       COEFF(22)*CROWD(sc,jz) + 
     *       COEFF(23)*LUNREL(sc,jz) +
     *       COEFF(37)*INVRAT +
     *       COEFF(41)*RATIVT
      END IF
C.....STORE WALK TIME AT DESTINATION
      STAZNE(3,SC,JZ)=WALKEGR(SC,JZ)
C.....STORE TRANSIT FARE
      FARE(SC,JZ)=FARE(SC,JZ)*100.0
      STAZNE(4,SC,JZ)=FARE(SC,JZ)
      STAZNE(5,SC,JZ)=TTCFAC(SC,JZ)
      STAZNE(6,SC,JZ)=GORFAC(SC,JZ)
C.....STORE NUMBER OF BOARDINGS
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
      RBUS=0
      PBUS=0
      IF(INVEHL(SC,JZ).GT.0) LBUS=1
      IF(INVEHG(SC,JZ).GT.0) GBUS=1
      IF(INVEHS(SC,JZ).GT.0) SRTC=1
      IF(INVEHN(SC,JZ).GT.0) RBUS=1
      IF(INVEHP(SC,JZ).GT.0) PBUS=1
      STAZNEI(SC,JZ,IMODE,1)=LBUS
      STAZNEI(SC,JZ,IMODE,2)=GBUS
      STAZNEI(SC,JZ,IMODE,3)=SRTC
      STAZNEI(SC,JZ,IMODE,4)=RBUS
      STAZNEI(SC,JZ,IMODE,5)=PBUS
C....................................................................
      IF((DEBUG).AND.(STAZNE(2,SC,JZ).NE.0.0)) THEN
      WRITE(103) IMODE,SC,JZ,INVEH(SC,JZ),WAIT1(SC,JZ),WAIT2(SC,JZ),
     *               TRANSF(SC,JZ),FARE(SC,JZ),WALKEGR(SC,JZ),
     *               WALKTFR(SC,JZ),STAZNE(2,SC,JZ),
     *               INVEHL(SC,JZ),INVEHG(SC,JZ),INVEHS(SC,JZ),
     *               INVEHN(SC,JZ),INVEHP(SC,JZ),NCAPAC(sc,jz), 
     *               EAWT(sc,jz),CROWD(sc,jz),LUNREL(sc,jz)
      IF(SDETAIL) THEN
      WRITE(336,9025) NAME(IMODE),IEQUIV(IC),STANAME(SC),IEQUIV(JZ),
     *               DIRWALK(SC,JZ),INVEH(SC,JZ),
     *               INVEHL(SC,JZ),INVEHG(SC,JZ),INVEHS(SC,JZ),
     *               INVEHN(SC,JZ),INVEHP(SC,JZ),
     *               WAIT1(SC,JZ),WAIT2(SC,JZ),
     *               TRANSF(SC,JZ),FARE(SC,JZ),WALKEGR(SC,JZ),
     *               WALKTFR(SC,JZ),NCAPAC(sc,jz), 
     *               EAWT(sc,jz),CROWD(sc,jz),LUNREL(sc,jz),
     *               STAZNE(5,SC,JZ),STAZNE(6,SC,JZ),
     *               STAZNE(2,SC,JZ),STAIND(SC,JZ)
 9025 FORMAT(1X,'DESTINATION STATION --> EGRESS ZONE COMPUTATIONS',
     *       1X,'(',A13,')'/
     *       1X,'------------------------------------------------'//
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'EGRESS            ZONE   =',I8/
     *       1X,'DIRECT WALK          TIME=',F8.2/
     *       1X,'IN-VEHICLE           TIME=',F8.2/
     *       1X,'IN-VEHICLE-LOCAL     TIME=',F8.2/
     *       1X,'IN-VEHICLE-GO BUS    TIME=',F8.2/
     *       1X,'IN-VEHICLE-STREETCAR TIME=',F8.2/
     *       1X,'IN-VEHICLE-RAPID BUS TIME=',F8.2/
     *       1X,'IN-VEHICLE-PREMIUM   TIME=',F8.2/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TOTAL WAIT           TIME=',F8.2/
     *       1X,'NUMBER OF BOARDINGS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'WALK EGRESS          TIME=',F8.2/
     *       1X,'WALK TRANSFER        TIME=',F8.2//
     *       1X,'NODE CAPACITY PENALTY    =',F8.2/
     *       1X,'EXTRA ADDED WAIT TIME    =',F8.2/
     *       1X,'CROWDING TIME            =',F8.2/
     *       1X,'LINK UNRELIABILITY TIME  =',F8.2//
     *       1X,'TTC FARE FACTOR          =',F8.2/
     *       1X,'GO RAIL FARE FACTOR      =',F8.2//
     *       1X,'UTILITY VALUE            =',F10.5/
     *       1X,'STATION INDICATOR        =',I10/)
      END IF
      END IF
C.......................................................................
  200 CONTINUE
  100 CONTINUE
C
C     OPEN ZONE-TO-STATION & STATION-TO-ZONE FILE NAMES
C
      close(11,status='keep')
      close(12,status='keep')
      close(13,status='keep')
      close(14,status='keep')
      close(15,status='keep')
      close(16,status='keep')
      close(17,status='keep')
      close(18,status='keep')
      close(19,status='keep')
      close(32,status='keep')
      close(62,status='keep')
      close(63,status='keep')
      close(64,status='keep')
      close(65,status='keep')
      close(66,status='keep')
      if(ccr) then
      close(108,status='keep')
      close(109,status='keep')
      close(110,status='keep')
      close(111,status='keep')
      end if  
      if(imode.lt.3) then   
      open(11,file=fzsta(1),
     *       status='old',form='binary')
      open(12,file=fzsta(2),
     *       status='old',form='binary')
      open(13,file=fzsta(3),
     *       status='old',form='binary')
      open(14,file=fzsta(4),
     *       status='old',form='binary')
      open(15,file=fzsta(5),
     *       status='old',form='binary')
      open(16,file=fzsta(6),
     *       status='old',form='binary')
      open(17,file=fzsta(7),
     *       status='old',form='binary')
      open(18,file=fzsta(8),
     *       status='old',form='binary')
      open(19,file=fwlksta,
     *       status='old',form='binary')
      open(32,file=fzsta(9),
     *       status='old',form='binary')
      open(65,file=fzsta(10),
     *       status='old',form='binary')
      open(66,file=fzsta(11),
     *       status='old',form='binary')
C.....FARE MATRICES
      open(62,file=fzstafr(1),
     *       status='old',form='binary')
      open(63,file=fzstafr(2),
     *       status='old',form='binary')
      open(64,file=fzstafr(3),
     *       status='old',form='binary')
      if(ccr) then
      open(108,file=fzsta(12),
     *       status='old',form='binary')
      open(109,file=fzsta(13),
     *       status='old',form='binary')
      open(110,file=fzsta(14),
     *       status='old',form='binary')
      open(111,file=fzsta(15),
     *       status='old',form='binary')
      end if
      else
      open(11,file=szsta(1),
     *       status='old',form='binary')
      open(12,file=szsta(2),
     *       status='old',form='binary')
      open(13,file=szsta(3),
     *       status='old',form='binary')
      open(14,file=szsta(4),
     *       status='old',form='binary')
      open(15,file=szsta(5),
     *       status='old',form='binary')
      open(16,file=szsta(6),
     *       status='old',form='binary')
      open(17,file=szsta(7),
     *       status='old',form='binary')
      open(18,file=szsta(8),
     *       status='old',form='binary')
      open(19,file=swlksta,
     *       status='old',form='binary')
      open(32,file=szsta(9),
     *       status='old',form='binary')
      open(65,file=szsta(10),
     *       status='old',form='binary')
      open(66,file=szsta(11),
     *       status='old',form='binary')
C.....FARE MATRICES
      open(62,file=szstafr(1),
     *       status='old',form='binary')
      open(63,file=szstafr(2),
     *       status='old',form='binary')
      open(64,file=szstafr(3),
     *       status='old',form='binary')
      if(ccr) then
      open(108,file=szsta(12),
     *       status='old',form='binary')
      open(109,file=szsta(13),
     *       status='old',form='binary')
      open(110,file=szsta(14),
     *       status='old',form='binary')
      open(111,file=szsta(15),
     *       status='old',form='binary')
      end if
      end if      
      
      
      RETURN
      END
