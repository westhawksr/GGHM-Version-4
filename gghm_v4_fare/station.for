C-------------------------------------------------------------------
C        STATION --> STATION UTILITY COMPUTATION SUBROUTINE
C-------------------------------------------------------------------
       SUBROUTINE STATION(STASTA,IMODE,UPXIVT,IVTCHK)
       INCLUDE 'stadat.inc'
       INCLUDE 'param.inc'
	     include 'mlogitpar.inc'
	     include 'dvalues.inc'
C
C DATA DECLARATIONS
C
      integer*2     IMODE,ic,sc,sc2,dc,T,unit,orgzn,destzn,type
      integer*4     origline,destline
	    real*4        frow(4000)
	    real*4        prow(1000)
	    real*4        arow(1000,4000)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS)
      REAL*4        INVEH(1000,1000),
     *              FARE(1000,1000),
     *              STSFARE(1000,1000),
     *              TRANSF(1000,1000),
     *              WAIT1(1000,1000),
     *              WAIT2(1000,1000),
     *              WALKTFR(1000,1000),
     *              SDIST(1000,1000),
     *              WAIT1OP(1000,1000),
     *              UPXIVT(1000,1000)
      REAL*4        NCAPAC(1000,1000),
     *              EAWT(1000,1000),
     *              CROWD(1000,1000),
     *              LUNREL(1000,1000)
      REAL*4        STAUTL,WAIT1A,WAIT1B,TXWAIT
      REAL*4        TXFERS,OFPKHD,LNECNST,PEARCNST,STACNST
      CHARACTER*13  NAME(2)
      LOGICAL       IVTCHK
      DATA          NAME/'GO Rail      ',
     *                   'TTC Subway   '/
      waitlt=5.0
      ncapac=0.0
      eawt=0.0
      crowd=0.0
      lunrel=0.0
      upxivt=0.0
      pearcnst=0.0
      stacnst=0.0
      ivtchk=.true.
C
      WRITE(*,8000) NAME(IMODE)
 8000 FORMAT(1X,'Station --> Station',
     *            ' Utility Computations for ',a13)
      IF(IMODE.EQ.2) WRITE(*,8002)
 8002 FORMAT(/)
C     IF(IMODE.EQ.1) WRITE(26,9000)
 9000 FORMAT(/
     * '                    GO RAIL STATION-TO-STATION TRAVEL TIMES'/
     * ' STATION        NAME            STATION        NAME        ',
     * '      IVT       UPX'/
     * ' -------  --------------------  -------  --------------------',
     * '  --------  --------')
C
C  OPEN INPUT MATRICES
C
      if(imode.eq.1) then
      open(1,file=fcrss(1),
     *       status='old',form='binary')
      open(2,file=fcrss(2),
     *       status='old',form='binary')
      open(3,file=fcrss(3),
     *       status='old',form='binary')
      open(4,file=fcrss(4),
     *       status='old',form='binary')
      open(5,file=fcrss(5),
     *       status='old',form='binary')
      open(6,file=fcrss(6),
     *       status='old',form='binary')
       if(mfcrss(11).gt.0) then
       open(130,file=fcrss(11),
     *        status='old',form='binary')
       end if
       if(mfcrss(12).gt.0) then
       open(193,file=fcrss(12),
     *        status='old',form='binary')
       end if
       if(mfcrss(13).gt.0) then
       open(326,file=fcrss(13),
     *        status='old',form='binary')
       end if
      if(ccr) then
      open(104,file=fcrss(7),
     *       status='old',form='binary')
      open(105,file=fcrss(8),
     *       status='old',form='binary')
      open(106,file=fcrss(9),
     *       status='old',form='binary')
      open(107,file=fcrss(10),
     *       status='old',form='binary')
      end if
      else
      open(1,file=furss(1),
     *       status='old',form='binary')
      open(2,file=furss(2),
     *       status='old',form='binary')
      open(3,file=furss(3),
     *       status='old',form='binary')
      open(4,file=furss(4),
     *       status='old',form='binary')
      open(5,file=furss(5),
     *       status='old',form='binary') 
       if(mfurss(10).gt.0) then
       open(327,file=furss(10),
     *        status='old',form='binary')
       end if  
      if(ccr) then
      open(104,file=furss(6),
     *       status='old',form='binary')
      open(105,file=furss(7),
     *       status='old',form='binary')
      open(106,file=furss(8),
     *       status='old',form='binary')
      open(107,file=furss(9),
     *       status='old',form='binary')
      end if   
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
C.....GO RAIL STATION-TO-STATION DISTANCE
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
      if(imode.eq.1.and.mfcrss(11).gt.0) then
      unit=130
      call mfread(unit,type,orgzn,destzn,frow,wait1op,prow,arow)
      end if
C...UPX IN-VEHICLE TIME
      if(imode.eq.1.and.mfcrss(12).gt.0) then
      unit=193
      call mfread(unit,type,orgzn,destzn,frow,upxivt,prow,arow)
      end if
C...CAPACITY, CROWDING & RELIABILITY VARAIBLES
      IF(CCR) THEN
C.....NODE CAPACITY PENALTY
      unit=104
      call mfread(unit,type,orgzn,destzn,frow,ncapac,prow,arow)
C.....EXTRA ADDED WAIT TIME
      unit=105
      call mfread(unit,type,orgzn,destzn,frow,eawt,prow,arow)      
C.....LINK CROWDING TIME
      unit=106
      call mfread(unit,type,orgzn,destzn,frow,crowd,prow,arow)
C.....LINK UNRELIABILITY
      unit=107
      call mfread(unit,type,orgzn,destzn,frow,lunrel,prow,arow)      
      END IF
C...GO RAIL STATION-TO-STATION FARE
      if(imode.eq.1.and.mfcrss(13).gt.0) then
      unit=326
      call mfread(unit,type,orgzn,destzn,frow,stsfare,prow,arow)
      end if
C...TTC SUBWAY STATION-TO-STATION FARE
      if(imode.eq.2.and.mfurss(10).gt.0) then
      unit=327
      call mfread(unit,type,orgzn,destzn,frow,stsfare,prow,arow)
      end if
C
C  ORIGIN STATION LOOP 
C
      DO 50 SC=1,MAX_STATIONS
      IC=iequiv(sc+max_izones)
      IF(STANUM(SC).NE.IMODE) GO TO 50
      IF(STADATA(SC,6).NE.1.0) GOTO 50
      IF(IMODE.EQ.1) THEN
      ORIGLINE=STADATA(SC,8)
      DESTLINE=10
      IF(ORIGLINE.LE.0.OR.ORIGLINE.GT.20) THEN
      WRITE(26,9001) IC,STANAME(SC),ORIGLINE
      WRITE(*,9001) IC,STANAME(SC),ORIGLINE
 9001 FORMAT(/' STATION 9001 (F) ENCOUNTERED ILLEGAL LINE',
     *       ' NUMBER FOR STATION=',I4,1X,A37,' LINE NUMMBER=',I2/)
      STOP 9001
      END IF
      LNECNST=KGLINE(ORIGLINE)/(LSUM1TRN*LSUM2CR*LSUM2CR)
      ELSE
      LNECNST=0.0
      END IF
      IF(AIRPASS.AND.(IC.EQ.GOUNION)) THEN
      STACNST=ACNST(13)/(LSUM1TRN*LSUM2CR*LSUM2CR)
      ELSE
      STACNST=0.0
      END IF
C
C   DESTINATION STATION LOOP
C
      DO 60 SC2=1,MAX_STATIONS
      DC=iequiv(sc2+max_izones)
      IF(sc.EQ.sc2) GOTO 60
      IF(STANUM(SC2).NE.IMODE) GO TO 60
      IF(STADATA(SC,18).GT.0.AND.STADATA(SC2,18).GT.0.AND.
     *   (STADATA(SC,18).EQ.STADATA(SC2,18))) THEN
C....................................................................
      IF(DEBUG.AND.SDETAIL) WRITE(26,9030)  IC,STANAME(SC),NAME(IMODE),
     *                      DC,STANAME(SC2),
     *                      STADATA(SC,18),STADATA(SC2,18)
 9030 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN             STATION=',I8,1X,A37,
     *           A13/
     *       1X,'DESTINATION        STATION=',I8,1X,A37/
     *       1X,'ADJOINING STATION ORG CODE=',F8.0/
     *       1X,'ADJOINING STATION DST CODE=',F8.0/) 
C....................................................................  
      GO TO 60
      END IF
C..................................................................
      IF(IMODE.EQ.1) THEN
C     WRITE(26,9002) IC,STANAME(SC),DC,STANAME(SC2),INVEH(SC,SC2),
C    *               UPXIVT(SC,SC2)
 9002 FORMAT(2X,I4,4X,A20,4X,I4,3X,A20,2X,F8.2,2X,F8.2)  
      END IF
C..................................................................
C ----------------------------------------------------------------
      PEARCNST=0.0
      IF(UPXIVT(SC,SC2).GT.0.0) THEN
      UPXIND(SC,SC2)=1
      INVEH(SC,SC2)=INVEH(SC,SC2)+UPXIVT(SC,SC2)
       IF(DC.EQ.PEARSON) THEN
       PEARCNST=UPXCNST/(LSUM1TRN*LSUM2CR*LSUM3CP)
       LNECNST=0.0
       ELSE  
       LNECNST=KGLINE(11)/(LSUM1TRN*LSUM2CR*LSUM2CR)
       END IF
      END IF
C ----------------------------------------------------------------   
      IF(IMODE.EQ.2) THEN
      ORIGLINE=STADATA(SC,8)
      DESTLINE=STADATA(SC2,8)
      LNECNST=KTLINE(ORIGLINE,DESTLINE)/(LSUM1TRN*LSUM2UR*LSUM2UR)
      END IF
      IF(INVEH(sc,sc2).LE.0.0) THEN
      STAUTL=0.0
      ELSE
      WAIT1A=AMIN1(WAIT1(sc,sc2),WAITLT)
      WAIT1B=DIM(WAIT1(sc,sc2),WAITLT)
      IF(IMODE.EQ.1.AND.MFCRSS(11).GT.0.AND.WAIT1OP(SC,SC2).GT.0) THEN
      OFPKHD=COEFF(31)+COEFF(32)*EXP((-2.0*WAIT1OP(SC,SC2))/COEFF(33))
      ELSE
      OFPKHD=0.0
      END IF
      STAUTL=COEFF(1)*INVEH(sc,sc2) + COEFF(3)*WAIT1A +
     *       COEFF(4)*(WAIT2(sc,sc2)-WAIT1(sc,sc2)) + 
     *       COEFF(5)*(TRANSF(sc,sc2)-1.0) +
     *       COEFF(7)*WALKTFR(sc,sc2) + COEFF(1)*WAIT1B +
     *       COEFF(1)*OFPKHD*(-1.0) + LNECNST + PEARCNST + STACNST +
     *       COEFF(20)*NCAPAC(sc,sc2)/(LSUM2CR*LSUM1TRN*LSUM3CW) + 
     *       COEFF(21)*EAWT(sc,sc2)/(LSUM2CR*LSUM1TRN*LSUM3CW) +
     *       COEFF(22)*CROWD(sc,sc2)/(LSUM2CR*LSUM1TRN*LSUM3CW) + 
     *       COEFF(23)*LUNREL(sc,sc2)/(LSUM2CR*LSUM1TRN*LSUM3CW)
      STASTA(1,SC,SC2)=TRANSF(sc,sc2)-1.0
      STASTA(2,SC,SC2)=STAUTL
      STASTA(3,SC,SC2)=INVEH(sc,sc2)
      IVTCHK=.FALSE.
C
C     COMPUTE FARE
C
      IF(IMODE.EQ.1) THEN
      FARE(SC,SC2)=(BFAREGR+DFAREGR*SDIST(SC,SC2))*100.0
      FARE(SC,SC2)=FARE(SC,SC2)*(1.0-(UPXIVT(SC,SC2)/INVEH(SC,SC2)))
        IF(UPXIVT(SC,SC2).GT.0.0) THEN
        IF(DC.EQ.PEARSON) THEN
        FARE(SC,SC2)=FARE(SC,SC2)+AIRFR*100.0
        ELSE
        FARE(SC,SC2)=FARE(SC,SC2)+NAIRFR*100.0
        END IF
        END IF
      ELSE
      FARE(SC,SC2)=BFARETTC*100.0
      END IF
      FARE(SC,SC2)=FARE(SC,SC2)+STSFARE(SC,SC2)*100.0
      IF(FARE(SC,SC2).LT.0) THEN
      WRITE(41,9003) IC,STANAME(SC),DC,STANAME(SC2),
     *               FARE(SC,SC2),STSFARE(SC,SC2)
 9003 FORMAT(' STATION 9003 (W) ORIGIN STATION=',I4,1X,A20,
     *       ' DESTINATION STATION=',I4,1X,A20,' NEGATIVE ',
     *       ' COMPUTED FARE=',F8.2,' STATION-TO-STATION FARE=',F8.2)
      END IF
      STASTA(4,SC,SC2)=FARE(sc,sc2)
      STASTA(5,SC,SC2)=WAIT2(sc,sc2)-WAIT1(sc,sc2)
      TXWAIT=WAIT2(sc,sc2)-WAIT1(sc,sc2)
      TXFERS=TRANSF(SC,SC2)-1.0
C....................................................................
      IF((DEBUG).AND.(STAUTL.NE.0.0)) THEN
      WRITE(101) IMODE,SC,SC2,WAIT1(sc,sc2),WAIT2(sc,sc2),
     *                TRANSF(sc,sc2),FARE(sc,sc2),INVEH(sc,sc2),
     *                WALKTFR(sc,sc2),NCAPAC(sc,sc2),EAWT(sc,sc2),
     *                CROWD(sc,sc2),LUNREL(sc,sc2),
     *                STAUTL
      IF(SDETAIL) THEN
      TXWAIT=WAIT2(sc,sc2)-WAIT1(sc,sc2)
      TXFERS=TRANSF(SC,SC2)-1.0
      WRITE(26,9029)  IC,STANAME(SC),NAME(IMODE),DC,STANAME(SC2),
     *                STADATA(SC,8),STADATA(SC2,8),SDIST(SC,SC2),
     *                KGLINE(ORIGLINE),
     *                KTLINE(ORIGLINE,DESTLINE),LNECNST,UPXCNST,
     *                WAIT1(sc,sc2),TXWAIT,
     *                TXFERS,FARE(sc,sc2),STSFARE(SC,SC2),
     *                INVEH(sc,sc2),UPXIVT(sc,sc2),
     *                WALKTFR(sc,sc2),WAIT1OP(sc,sc2),OFPKHD,
     *                NCAPAC(sc,sc2),EAWT(sc,sc2),
     *                CROWD(sc,sc2),LUNREL(sc,sc2),STAUTL
 9029 FORMAT(/1X,'STATION --> STATION UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'//
     *       1X,'ORIGIN            STATION=',I8,1X,A37,
     *           A13/
     *       1X,'DESTINATION       STATION=',I8,1X,A37/
     *       1X,'ORIGIN            LINE   =',F8.0/
     *       1X,'DESTINATION       LINE   =',F8.0/
     *       1X,'DISTANCE                 =',F8.2/
     *       1X,'GO RAIL  CONSTANT        =',F8.4/
     *       1X,'TTC SUBWAY CONSTANT      =',F8.4/
     *       1X,'LINE CONSTANT            =',F8.4/
     *       1X,'UPX  CONSTANT            =',F8.4/
     *       1X,'1ST WAIT             TIME=',F8.2/
     *       1X,'TRANSFER WAIT        TIME=',F8.2/
     *       1X,'NUMBER OF TRANSFERS      =',F8.2/
     *       1X,'FARE                     =',F8.2/
     *       1X,'STATION-TO-STATION FARE  =',F8.2/
     *       1X,'PRIMARY MODE IN-VEHICLE  =',F8.2/
     *       1X,'UPX          IN-VEHICLE  =',F8.2/
     *       1X,'TRANSFER WALK TIME       =',F8.2/
     *       1X,'OFF PEAK WAIT TIME       =',F8.2/
     *       1X,'OFF PEAK HEADWAY BENEFIT =',F8.2//
     *       1X,'NODE CAPACITY PENALTY    =',F8.2/
     *       1X,'EXTRA ADDED WAIT TIME    =',F8.2/
     *       1X,'CROWDING TIME            =',F8.2/
     *       1X,'LINK UNRELIABILITY TIME  =',F8.2//
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
      if(imode.eq.1) then
      close(6,status='keep')
      close(130,status='keep')
      end if
      if(ccr) then
      close(104,status='keep')
      close(105,status='keep')
      close(106,status='keep')
      close(107,status='keep')      
      end if
      if(mfcrss(13).gt.0) close(326,status='keep')
      if(mfurss(10).gt.0) close(327,status='keep')
      RETURN
      END
