C-------------------------------------------------------------------
C        STATION --> STATION UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STATION(STASTA,IMODE)
       INCLUDE 'stadat.inc'
       INCLUDE 'param.inc'
	     include 'ilogitpar.inc'
	     include 'dvalues.inc'
C
C DATA DECLARATIONS
C
      integer*2     IMODE,ic,sc,sc2,dc,T,unit,orgzn,destzn,type
	    real*4        frow(max_izones)
	    real*4        prow(max_stations)
	    real*4        arow(max_stations,max_izones)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS)
      REAL*4        INVEH(max_stations,max_stations),
     *              FARE(max_stations,max_stations),
     *              TRANSF(max_stations,max_stations),
     *              WAIT1(max_stations,max_stations),
     *              WAIT2(max_stations,max_stations),
     *              WALKTFR(max_stations,max_stations),
     *              SDIST(max_stations,max_stations),
     *              WAIT1OP(max_stations,max_stations)
      REAL*4        STAUTL,WAIT1A,WAIT1B,TXWAIT
      REAL*4        TXFERS,OFPKHD
      CHARACTER*13  NAME(2)
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   '/
      waitlt=5.0
      wait1op=0.0
C
      WRITE(*,8000) NAME(IMODE)
 8000 FORMAT(1X,'Station --> Station',
     *            ' Utility Computations for ',a13)
      IF(IMODE.EQ.2) WRITE(*,8002)
 8002 FORMAT(/)
C
C  OPEN INPUT MATRICES
C
      if(imode.eq.1) then
      open(1,file=fcrss(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(2,file=fcrss(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(3,file=fcrss(3),recl=4,access='direct',
     *       status='old',form='binary')
      open(4,file=fcrss(4),recl=4,access='direct',
     *       status='old',form='binary')
      open(5,file=fcrss(5),recl=4,access='direct',
     *       status='old',form='binary')
      open(6,file=fcrss(6),recl=4,access='direct',
     *       status='old',form='binary')
       if(mfcrss(7).gt.0) then
       open(7,file=fcrss(7),recl=4,access='direct',
     *        status='old',form='binary')
       end if
      else
      open(1,file=furss(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(2,file=furss(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(3,file=furss(3),recl=4,access='direct',
     *       status='old',form='binary')
      open(4,file=furss(4),recl=4,access='direct',
     *       status='old',form='binary')
      open(5,file=furss(5),recl=4,access='direct',
     *       status='old',form='binary')      
      end if
C
C    OBTAIN LEVEL-OF-SERVICE VALUES
C
      type=2
      orgzn=max_izones+1
      destzn=max_zones
C.....1ST WAIT TIME
      unit=2
      call mfread(unit,type,orgzn,destzn,frow,wait1,prow,arow)
C.....TOTAL WAIT TIME
      unit=3
      call mfread(unit,type,orgzn,destzn,frow,wait2,prow,arow)
C.....NUMBER OF BOARDINGS
      unit=4
      call mfread(unit,type,orgzn,destzn,frow,transf,prow,arow)
C.....FARE
      if(imode.eq.1) then
      unit=6
      call mfread(unit,type,orgzn,destzn,frow,sdist,prow,arow)      
      end if
C.....COMMUTER RAIL/URBAN RAIL IN-VEHICLE TIME
      unit=1
      call mfread(unit,type,orgzn,destzn,frow,inveh,prow,arow)
C...TRANSFER WALK TIME
      unit=5
      call mfread(unit,type,orgzn,destzn,frow,walktfr,prow,arow)
C...1ST WAIT TIME - OFF PEAK
      if(imode.eq.1.and.mfcrss(7).gt.0) then
      unit=7
      call mfread(unit,type,orgzn,destzn,frow,wait1op,prow,arow)
      end if
C
C  ORIGIN STATION LOOP 
C
      DO 50 SC=1,MAX_STATIONS
      IC=iequiv(sc+max_izones)
      IF(STANUM(SC).NE.IMODE) GO TO 50
      IF(STADATA(SC,6).NE.1.0) GOTO 50
C
C   DESTINATION STATION LOOP
C
      DO 60 SC2=1,MAX_STATIONS
      DC=iequiv(sc2+max_izones)
      IF(sc.EQ.sc2) GOTO 60
      IF(STANUM(SC2).NE.IMODE) GO TO 60
      IF(INVEH(sc,sc2).LE.0.0) THEN
      STAUTL=0.0
      ELSE
      WAIT1A=AMIN1(WAIT1(sc,sc2),WAITLT)
      WAIT1B=DIM(WAIT1(sc,sc2),WAITLT)
      IF(IMODE.EQ.1.AND.MFCRSS(7).GT.0.AND.WAIT1OP(SC,SC2).GT.0) THEN
C      WAIT1OP(SC,SC2)=WAIT1OP(SC,SC2)*0.50
      OFPKHD=COEFF(31)+COEFF(32)*EXP((-2.0*WAIT1OP(SC,SC2))/COEFF(33))
      ELSE
      OFPKHD=0.0
      END IF
      STAUTL=COEFF(1)*INVEH(sc,sc2) + COEFF(3)*WAIT1A +
     *       COEFF(4)*(WAIT2(sc,sc2)-WAIT1(sc,sc2)) + 
     *       COEFF(5)*(TRANSF(sc,sc2)-1.0) +
     *       COEFF(7)*WALKTFR(sc,sc2) + COEFF(1)*WAIT1B +
     *       COEFF(1)*OFPKHD*(-1.0)
      STASTA(1,SC,SC2)=TRANSF(sc,sc2)-1.0
      STASTA(2,SC,SC2)=STAUTL
      STASTA(3,SC,SC2)=INVEH(sc,sc2)
C
C     COMPUTE FARE
C
      IF(IMODE.EQ.1) THEN
      FARE(SC,SC2)=(BFAREGR+DFAREGR*SDIST(SC,SC2))*100.0
      ELSE
      FARE(SC,SC2)=BFARETTC*100.0
      END IF
      STASTA(4,SC,SC2)=FARE(sc,sc2)
      STASTA(5,SC,SC2)=WAIT2(sc,sc2)-WAIT1(sc,sc2)
      TXWAIT=WAIT2(sc,sc2)-WAIT1(sc,sc2)
      TXFERS=TRANSF(SC,SC2)-1.0
C....................................................................
      IF((DEBUG).AND.(STAUTL.NE.0.0)) THEN
      WRITE(91) IMODE,SC,SC2,WAIT1(sc,sc2),WAIT2(sc,sc2),
     *                TRANSF(sc,sc2),FARE(sc,sc2),INVEH(sc,sc2),
     *                WALKTFR(sc,sc2),
     *                STAUTL
      IF(SDETAIL) THEN
      TXWAIT=WAIT2(sc,sc2)-WAIT1(sc,sc2)
      TXFERS=TRANSF(SC,SC2)-1.0
      WRITE(26,9029)  IC,STANAME(SC),NAME(IMODE),DC,STANAME(SC2),
     *                WAIT1(sc,sc2),TXWAIT,
     *                TXFERS,FARE(sc,sc2),INVEH(sc,sc2),
     *                WALKTFR(sc,sc2),WAIT1OP(sc,sc2),OFPKHD,STAUTL
 9029 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8,1X,A37,
     *           A13/
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'PRIMARY MODE IN-VEHICLE  =',F8.2/
     *       1X,'TRANSFER WALK TIME       =',F8.2/
     *       1X,'OFF PEAK 1ST WAIT TIME   =',F8.2/
     *       1X,'OFF PEAK HEADWAY BENEFIT =',F8.2//
     *       1X,'UTILITY VALUE            =',F10.5/)
      END IF
      END IF
C.......................................................................
      END IF
   60 CONTINUE
   50 CONTINUE
      close(1,status='keep')
      close(2,status='keep')
      close(3,status='keep')
      close(4,status='keep')
      close(5,status='keep')
      RETURN
      END
