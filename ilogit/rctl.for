C***********************************************************
C     OBTAIN FILE AND PARAMETER VALUES                     *
C***********************************************************
      SUBROUTINE RCTL
      include 'stadat.inc'
      include 'param.inc'
	    include 'ilogitpar.inc'
	    include 'dvalues.inc'
      INTEGER*2    TIME(4),NERR,STAT1,I(20),J(20)
      INTEGER*4    IT,ST,TT,UT,VT,XT,RLEN,DATE(3)
      INTEGER*4    T,PD,RERPD
      CHARACTER*1  COMMA,BLANK
      CHARACTER*7  NAMERR(7)
      CHARACTER*9  EMAT
      CHARACTER*35 DNAME
      CHARACTER*200 FILENAME
      CHARACTER*200 CTLFILE
      LOGICAL      EXISTS,CTLERR(7)
      DATA         FRPT/'ILOGIT.RPT'/
      DATA         NAMERR/'OBS    ','PARAMS ','OPTIONS','SELECTS',
     *                    'PARMS  ','MATX   ','FILES'/
      DATA         EMAT/'\emmemat\'/

C
C WRITE PROGRAM HEADER INFORMATION
C
      CALL GETTIM(TIME(1),TIME(2),TIME(3),TIME(4))
      CALL GETDAT(DATE(3),DATE(1),DATE(2))
      WRITE(*,1000) DATE,(TIME(T),T=1,3)
 1000 FORMAT(/'              PROGRAM ILOGIT'/
     *        '            INCREMENTAL LOGIT '/
     *        '           RER EXPRESS ANALYSIS'/
     *        '          [Version Date: 23Mar16]'/
     *        '          [Update  Date: 31Jul17]'//
     *        5X,'       Date: ',I2,'/',I2,'/',I4/
     *        5X,'       Time: ',I2,':',I2,':',I2,///)
C
C     NAMELIST DEFINITION..........
C
      NAMELIST  /FILES/  FZINDEX,FSTAFILE,FRPT,
     *                   DGOBUS,DGOTRAIN,DTTC,DTOTAL,
     *                   FDSHARE,FTLF,FDEQUIV,FCTVINP,
     *                   FSTAOUT,FSTPKIN,FPREFIX,FSDEQUIV,
     *                   FDESCP,FDTRIPS,FTRNTLF,FSTASUM
      NAMELIST  /MATX/   MFURSS,MFCRSS,MFZSTA,MFWLKSTA,
     *                   MFDRV0,MFWKGOBUS,MFTRNPER,MFLSUM,
     *                   MFSTATRP,MFTRIPS,MFZSTAGR,MFZSTAFR
      NAMELIST  /PARAMS/ NCATS,MXZONES,CITER,FRPT
      NAMELIST  /OPTIONS/DEBUG,SDETAIL,CALIB,LSBASE,TRIPSOUT,
     *                   PEAK,SHARES,CAPRES
      NAMELIST  /SELECTS/I,J
      NAMELIST  /PARMS/  COEFF,WAITLT,LSUM1TRN,LSUM2GB,LSUM2CR,
     *                   LSUM2UR,LSUM3CW,LSUM3CB,LSUM3CP,
     *                   LSUM3CK,LSUM3UW,LSUM3UB,LSUM3UP,
     *                   LSUM3UK,KDTRN,KPNR,OPCOST,
     *                   BPFACE,BUSPNROCC,
     *                   KWCR,KBCR,KPCR,KKCR,
     *                   KWUR,KBUR,KPUR,KKUR,
     *                   KCBD,KCR,KUR,KGBUS,
     *                   CCRPNRT,CCRPNRD,CDAR,KGOBUSW,
     *                   KGOBUSD,BFAREGR,DFAREGR,BFARETTC
      NAMELIST  /OBS/    NITER,ADJFCT,CCODE,CBDTRP
      NERR=0
      CAPRES=.FALSE.
      SDNAME(31)='TOTAL'
      COEFF(21)=1.25
      COEFF(22)=0.75
      COEFF(31)=0.493502
      COEFF(32)=9.67239
      COEFF(33)=43.29598
C
C     READ CONTROL FILE NAME FROM COMMAND LINE
C
      CALL GETARG(1,CTLFILE,STAT1)
      IF(STAT1.LE.0) THEN
      WRITE(*,9005)
 9005 FORMAT(/' ILOGIT 9005 (F): CONTROL FILE NAME NOT ',
     *         ' PROVIDED ON COMMAND LINE')
      STOP 9005
      ENDIF    
C
C     OPEN AND READ THE CONTROL FILE
C
      INQUIRE(FILE=CTLFILE,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(*,9018)  CTLFILE
 9018 FORMAT(//' RCTL 9018 (F) CONTROL FILE=',A200,' NOT FOUND'//)
      STOP 9018
      ELSE
      OPEN(1,FILE=CTLFILE,STATUS='OLD')
      END IF
      READ(1,FILES,ERR=5)
      REWIND 1
      GO TO 10
    5 CTLERR(7)=.TRUE.
   10 READ(1,PARAMS ,ERR=15)
      REWIND 1
      GO TO 20
   15 CTLERR(2)=.TRUE.
      REWIND 1
   20 READ(1,OPTIONS,ERR=25)
      REWIND 1
      GO TO 30
   25 CTLERR(3)=.TRUE.
      REWIND 1
   30 READ(1,SELECTS,ERR=35)
      REWIND 1
      GO TO 40
   35 CTLERR(4)=.TRUE.
      REWIND 1
   40 READ(1,PARMS  ,ERR=45)
      REWIND 1
      GO TO 50
   45 CTLERR(5)=.TRUE.
      REWIND 1
   50 READ(1,MATX  ,ERR=55)
      REWIND 1
      GO TO 60
   55 CTLERR(6)=.TRUE.
   60 IF(CALIB) READ(1,OBS, ERR=65)
      GO TO 70
   65 CTLERR(1)=.TRUE.
   70 CONTINUE
C
C     CHECK FOR ERRORS IN THE READS; WRITE ANY MESSAGES TO SCREEN
C
      DO E=1,7
        IF(CTLERR(E)) THEN
          WRITE(*,9002) NAMERR(E)
 9002     FORMAT(/' RCTL 9002 (F) ERROR(S) READING &',A7)
          NERR=NERR+1
        ENDIF
      END DO
C 
C SUMMARIZE FILE NAMES
C
      OPEN(26,FILE=FRPT,STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(26,1000) DATE,(TIME(T),T=1,3)
C 
C PRINT FILE INPUT/OUTPUT SUMMARY
C
      WRITE(26,500) FZINDEX,FSTAFILE,FRPT,
     *             DGOBUS,DGOTRAIN,DTTC,DTOTAL,
     *             FDSHARE,FTLF,FDEQUIV,FCTVINP,
     *             FSTAOUT,FSTPKIN,FPREFIX
  500 FORMAT(1X,'SUMMARY OF FILE INPUTS & OUTPUTS'/
     *       1X,'--------------------------------------------------'/
     *       1X,'FZINDEX = ',a50,' EXTERNAL/INTERNAL ZONE INDEX FILE'/
     *       1X,'FSTAFILE= ',a50,' STATION ATTRIBUTE FILE'/
     *       1X,'FRPT    = ',a50,' PROGRAM REPORT FILE'/
     *       1X,'DGOBUS  = ',a50,' GO BUS DISTRICT LEVEL FILE'/
     *       1X,'DGOTRAIN= ',a50,' GO TRAIN DISTRICT TRIP FILE'/
     *       1X,'DTTC    = ',a50,' TTC SUBWAY DISTRICT TRIP FILE'/
     *       1X,'DTOTAL  = ',a50,' TOTAL PERSON TRIP DISTRICT FILE'/
     *       1X,'FDSHARE = ',a50,' DISTRICT SHARE FILE (OUTPUT)'/
     *       1X,'FTLF    = ',a50,' SHARE FREQUENCY FILE (OUTPUT)'/
     *       1X,'FDEQUIV = ',a50,' ZONE/DISTRICT EQUIVALENCE FILE'/
     *       1X,'FCTVINP = ',a50,' CALIBRATION TARGET VALUE FILE'/
     *       1X,'FSTAOUT = ',a50,' STATION USAGE OUTPUT FILE'/
     *       1X,'FSTPKIN = ',a50,' STATION USAGE INPUT FILE'/
     *       1X,'FPREFIX = ',a50,' EMMEMAT DIRECTORY LOCATION')
      IF(CALIB) THEN
      WRITE(26,501) FSDEQUIV,FDESCP,FDTRIPS,FTRNTLF,FSTASUM
  501 FORMAT(
     *       1X,'FSDEQUIV= ',a50,' SUPER DISTRICT EQUIVALENCE'/
     *       1X,'FDESCP  = ',a50,' SUPER DISTRICT NAMES'/
     *       1X,'FDTRIPS = ',a50,' SUPER DISTRICT TRIPS SUMMARY'/
     *       1X,'FTRNTLF = ',a50,' TRANSIT TRIP LENGTH DISTRIBUTION'/
     *       1X,'FSTASUM = ',a50,' GO RAIL STATION SEGMENT SUMMARY'/)
      END IF
C
C     PLAY BACK VALUES FROM THE CONTROL FILE
C
      WRITE(26,9001) MFCRSS,MFURSS,MFZSTA,MFWLKSTA,
     *               MFDRV0,MFWKGOBUS,MFTRNPER,MFLSUM,
     *               MFSTATRP,MFTRIPS,MFZSTAGR,MFZSTAFR,
     *               NCATS,MXZONES,CITER,SDETAIL,DEBUG,CALIB,LSBASE,
     *               TRIPSOUT,PEAK,SHARES,CAPRES,
     *               I,J,(COEFF(K),K=1,20),COEFF(23),
     *               (COEFF(K2),K2=31,33),(COEFF(K1),K1=51,53),
     *               WAITLT,OPCOST,
     *               BFAREGR,DFAREGR,BFARETTC,
     *               CCRPNRT,CCRPNRD,CDAR,
     *               BPFACE,BUSPNROCC,LSUM1TRN,
     *               LSUM2GB,LSUM2CR,LSUM2UR,LSUM3CW,
     *               LSUM3CB,LSUM3CP,LSUM3CK,LSUM3UW,
     *               LSUM3UB,LSUM3UP,LSUM3UK,KDTRN,KPNR,
     *               KGOBUSD,KGOBUSW,
     *               KCR,KUR,KGBUS,KCBD,KWCR,KWUR,
     *               KBCR,KBUR,KPCR,KPUR,KKCR,KKUR
 9001 FORMAT(//' RCTL 9001 (I) CONTENTS OF CONTROL FILE: '/
     *         ' ----------------------------------------'/
     *    '&MATX'   /'  MFCRSS   = ',7I4 /'  MFURSS   = ',5I4/
     *               '  MFZSTA   = ',9I4/'  MFWLKSTA = ',I4/
     *               '  MFDRV0   = ',2I4/ '  MFWKGOBUS= ',10I4/
     *               '  MFTRNPER = ',3I4/ '  MFLSUM   = ',3I4/
     *               '  MFSTATRP = ',8I4/ '  MFTRIPS  = ',11I4/
     *               '  MFZSTAGR = ',10I4/'  MFZSTAFR = ',6I4/
     *    '&PARAMS' /'  NCATS   = ',I4/   '  MXZONES  = ',I4/
     *               '  CITER   = ',I4/
     *    '&OPTIONS'/'  SDETAIL = ',L1  /'  DEBUG   = ',L1/
     *               '  CALIB   = ',L1  /'  LSBASE  = ',L1/
     *               '  TRIPSOUT= ',L1  /'  PEAK    = ',L1/
     *               '  SHARES  = ',L1  /'  CAPRES  = ',L1/
     *    '&SELECTS'/'  I       = ',20I5/'  J       = ',20I5/
     *    '&PARMS  '/'  COEFF( 1-10)= ',10(1X,F8.5)/
     *               '  COEFF(11-20)= ',10(1X,F8.5)/
     *               '  COEFF(   23)= ',   1X,F8.5/
     *               '  COEFF(31-33)= ', 3(1X,F8.5)/
     *               '  COEFF(51-53)= ',3(1X,F8.5)/
     *               '  WAITLT      = ',F9.5/
     *               '  OPCOST      = ',F9.2/
     *               '  BFAREGR     = ',F9.2/
     *               '  DFAREGR     = ',F9.2/
     *               '  BFARETTC    = ',F9.2/
     *               '  CCRPNRT     = ',F9.5/
     *               '  CCRPNRD     = ',F9.5/
     *               '  CDAR        = ',F9.5/
     *               '  BPFACE      = ',F9.5/
     *               '  BUSPNROCC   = ',F9.5//
     *               '  LSUM1TRN    = ',F9.5/
     *               '  LSUM2GB     = ',F9.5/
     *               '  LSUM2CR     = ',F9.5/
     *               '  LSUM2UR     = ',F9.5/
     *               '  LSUM3CW     = ',F9.5/
     *               '  LSUM3CB     = ',F9.5/
     *               '  LSUM3CP     = ',F9.5/
     *               '  LSUM3CK     = ',F9.5/
     *               '  LSUM3UW     = ',F9.5/
     *               '  LSUM3UB     = ',F9.5/
     *               '  LSUM3UP     = ',F9.5/
     *               '  LSUM3UK     = ',F9.5//
     *               '  KDTRN       = ',3F9.5/
     *               '  KPNR        = ',3F9.5/
     *               '  KGOBUSD     = ',F9.5/
     *               '  KGOBUSW     = ',F9.5/
     *               '  KCR         = ',3F9.5/
     *               '  KUR         = ',3F9.5/
     *               '  KGBUS       = ',3F9.5/    
     *               '  KCBD        = ',3F9.5/
     *               '  KWCR        = ',3F9.5/
     *               '  KWUR        = ',3F9.5/
     *               '  KBCR        = ',3F9.5/
     *               '  KBUR        = ',3F9.5/
     *               '  KPCR        = ',3F9.5/
     *               '  KPUR        = ',3F9.5/
     *               '  KKCR        = ',3F9.5/
     *               '  KKUR        = ',3F9.5/) 
      IF(CALIB) THEN
      WRITE(26,9011) NITER,ADJFCT,CCODE
 9011 FORMAT('&OBS   ' /'  NITER   = ',I4  /'  ADJFCT   = ',F4.2/
     * '  CCODE(1) = ',3X,L1,' DRIVE TO TRANSIT (KDTRN)'/
     * '  CCODE(2) = ',3X,L1,' PNR   TO    RAIL (KPNR)'/
     * '  CCODE(3) = ',3X,L1,' DRIVE TO GO BUS  (KGOBUSD)'/
     * '  CCODE(4) = ',3X,L1,' GO RAIL CONSTANT (KCR)'/
     * '  CCODE(5) = ',3X,L1,' TTC SUBWAY CONSTANT (KUR)'/
     * '  CCODE(6) = ',3X,L1,' GO BUS CONSTANT(KGBUS)'/
     * '  CCODE(7) = ',3X,L1,' WALK TO GO RAIL (KWCR)'/
     * '  CCODE(8) = ',3X,L1,' WALK TO TTC SUBWAY (KWUR)'/
     * '  CCODE(9) = ',3X,L1,' CBD CONSTANTS (KCBD)'/
     * '  CCODE(10)= ',3X,L1,' BUS TO GO RAIL (KBCR)'/
     * '  CCODE(11)= ',3X,L1,' BUS TO TTC SUBWAY (KBUR)'/
     * '  CCODE(12)= ',3X,L1,' PNR TO GO RAIL (KPCR)'/
     * '  CCODE(13)= ',3X,L1,' PNR TO TTC SUBWAY (KPUR)'/
     * '  CCODE(14)= ',3X,L1,' KNR TO GO RAIL (KKCR)'/
     * '  CCODE(15)= ',3X,L1,' KNR TO TTC SUBWAY (KKUR)'/
     * '  CCODE(16)= ',3X,L1,' BUS TO GO RAIL (KBCR)-DIRECT'/
     * '  CCODE(17)= ',3X,L1,' BUS TO TTC SUBWAY (KBUR) - DIRECT'/
     * '  CCODE(18)= ',3X,L1,' WALK TO GO BUS (KGOBUSW) - DIRECT'/
     * '  CCODE(19)= ',3X,L1,' WALK TO GO RAIL (KWCR) - DIRECT'/
     * '  CCODE(20)= ',3X,L1,' PNR TO TTC SUBWAY (KPUR) - DIRECT'/
     * '  CCODE(21)= ',3X,L1,' KNR TO GO RAIL (KKCR) - DIRECT'/)
      IF(CCODE(10).AND.CCODE(16)) THEN
      WRITE(26,9012) 
      WRITE(*,9012)
 9012 FORMAT(/' RCTL 9012 (F) CCODE 10 & 16 BOTH =T'/)
      STOP 9012
      END IF
      IF(CCODE(11).AND.CCODE(17)) THEN
      WRITE(26,9013) 
      WRITE(*,9013)
 9013 FORMAT(/' RCTL 9013 (F) CCODE 11 & 17 BOTH =T'/)
      STOP 9013
      END IF
      IF(CCODE(7).AND.CCODE(19)) THEN
      WRITE(26,9014) 
      WRITE(*,9014)
 9014 FORMAT(/' RCTL 9014 (F) CCODE 7 & 19 BOTH =T'/)
      STOP 9014
      END IF
      IF(CCODE(13).AND.CCODE(20)) THEN
      WRITE(26,9015) 
      WRITE(*,9015)
 9015 FORMAT(/' RCTL 9015 (F) CCODE 13 & 20 BOTH =T'/)
      STOP 9015
      END IF
      END IF
      IF(MXZONES.LE.0) THEN
      WRITE(26,9016)
      WRITE(*,9016)
 9016 FORMAT(/' RCTL 9016 (W) MXZONES VALUE NOT PROVIDED'/)
      STOP 9016
      END IF
c
c     READ & STORE DISTRICT/EQUIVALENCE FILE
c
      open(99,file=fdequiv,status='old',form='formatted')
      read(99,9,end=325,err=321) header
  322 read(99,*,end=325,err=321) t,pd,rerpd
      if(rerpd.gt.46) rerpd=rerpd-254
      if(rerpd.gt.60) then
      write(26,320) rerpd,t
  320 format(' RCTL 320 (W) DISTRICT NUMBER (',I3,') FOR ZONE ',I4,
     *       ' EXCEEDS MAXIMUM ALLOWABLE VALUE OF 60')
      stop 320
      end if
      dequiv(t)=rerpd
      go to 322
  321 write(26,323) t
  323 format(' RCTL 321 (F) ERROR READING DISTRICT EQUIVALENCE FILE',
     *       ' RECORD=',i10)
      stop 321
  325 close(99,status='keep')
C
C     READ & STORE INTERNAL/EXTERNAL ZONE CORRESPONDENCE
C
      inquire(file=fzindex,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9019)  fzindex
 9019 FORMAT(//' RCTL 9019 (F) FZINDEX FILE=',A90,' NOT FOUND'//)
      STOP 9019
      else
      open(99,file=fzindex,status='old',form='formatted')
      read(99,9) header
    9 format(a80)
    8 read(99,*,err=97,end=96) index,zone
      iequiv(index+1)=zone
      equiv(zone)=index+1
      go to 8
   97 write(26,95) index,zone
   95 format(/' ilogit 97 (f) error reading zone index file',
     *        ' near index=',i4,' zone=',i5)
      stop
   96 close(99,status='keep')
      end if
C
C     READ & STORE SUPER DISTRICT/ZONE CORRESPONDENCE
C
      inquire(file=fsdequiv,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9020)  fsdequiv
 9020 FORMAT(//' RCTL 9020 (F) FSDEQUIV FILE=',A90,' NOT FOUND'//)
      STOP 9020
      else
      open(99,file=fsdequiv,status='old',form='formatted')
   18 read(99,*,err=197,end=196) zone,index
      sdequiv(zone)=index
      go to 18
  197 write(26,195) index,zone
  195 format(/' ilogit 197 (f) error reading super district file',
     *        ' near district=',i4,' zone=',i5)
      stop
  196 close(99,status='keep')
      end if      
      inquire(file=fdescp,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9021)  fsdescp
 9021 FORMAT(//' RCTL 9021 (F) FDESCP FILE=',A90,' NOT FOUND'//)
      STOP 9021
      else
      open(99,file=fdescp,status='old',form='formatted')
      read(99,9) header
   28 read(99,*,err=297,end=296) zone,index,dname
      sdname(zone)=dname
      go to 28
  297 write(26,295) zone
  295 format(/' ilogit 297 (f) error reading super district file',
     *        ' near district=',i4)
      stop
  296 close(99,status='keep')
      end if 
      if(calib) then    
      open(87,file=fdtrips,status='unknown',form='formatted')
      open(88,file=ftrntlf,status='unknown',form='formatted')
      end if
C
C     FIGURE OUT LENGTH OF EMMEMAT PATH DIRECTORY
C
       do st=1,200
       tt=st+8
       if(fprefix(st:tt).eq.emat) then
       prelen=tt
       go to 98
       end if
       end do
       write(26,99) FPREFIX
   99  format(' RCTL 99 (F) UNABLE TO DETERMINE EMMEMAT',
     *        ' DIRECTORY PATH INFORMATION:'/
     *        1X,A200)
       stop
   98  continue
C
C     CONVERT I AND J ZONE INPUTS TO INTERNAL ZONES
C
       DO IN=1,20     
       IF(I(IN).GT.0) THEN
       I(IN)=EQUIV(I(IN))
       ELSE
        IF(I(IN).LT.0) THEN
        I(IN)=IIABS(I(IN))
        I(IN)=EQUIV(I(IN))*(-1.0)
        END IF
       END IF
       IF(J(IN).GT.0) THEN
       J(IN)=EQUIV(J(IN))
       ELSE
        IF(J(IN).LT.0) THEN
        J(IN)=IIABS(J(IN))
        J(IN)=EQUIV(J(IN))*(-1.0)
        END IF
       END IF
       END DO
C.............................................
       IF(DEBUG) THEN
       WRITE(26,110) I,J
  110 FORMAT(' I=',20I5/
     *       ' J=',20I5)
       END IF
C.............................................
C
C     SET THE I-ZONES CONTROLS
C
      DO IN=1,20
        IF(I(IN).GT.0) THEN
          NI=NI+1
          IF(I(IN).LE.MXZONES) THEN
            IOI(I(IN))=.TRUE.
          ENDIF
        ELSEIF(I(IN).LT.0) THEN
          NI=NI+1
          IEND=I(IN)*(-1)
          IBEG=I(IN-1)+1
          DO INF=IBEG,IEND
          IOI(INF)=.TRUE.
          END DO
        ENDIF
      END DO
C
C     SET THE J-ZONES CONTROLS
C
      DO JN=1,20
        IF(J(JN).GT.0) THEN
          NJ=NJ+1
          IF(J(JN).LE.MXZONES) THEN
            JOI(J(JN))=.TRUE.
          ENDIF
        ELSEIF(J(JN).LT.0) THEN
          NJ=NJ+1
          JEND=J(JN)*(-1)
          JBEG=J(JN-1)+1
          DO INF=JBEG,JEND
          JOI(INF)=.TRUE.
        END DO
        ENDIF
      END DO
C
C     OPEN WORKING FILES FOR DEBUG FEATURE
C
      if(debug) then
      open(91,file='stafile.bin',status='unknown',form='binary')
      open(93,file='egrfile.bin',status='unknown',form='binary')
      end if
C
C     OPEN CALIBRATED CONSTANT FILE
C
      if(calib) then
      open(97,file='constant.dat',status='unknown',form='formatted')
      end if
C
C     OPEN STATION ATTRIBUTE FILE
C
      inquire (file=fstafile,exist=exists)
      if(exists) then
      open(42,file=fstafile,status='old',form='formatted')
      else
      write(26,7001) fstafile
 7001 format(/' RCTL 7001 (F) STATION ATTRIBUTE FILE ',A40,
     *       ' NOT FOUND'/)
      write(*,7001) fstafile
      stop 7001
      end if
C
C     SET STATION-TO-STATION FILE NAMES
C
      DO IT=1,5
      CALL MFNAME(MFURSS(IT),FILENAME)
      FURSS(IT)=FILENAME
      CALL MFNAME(MFCRSS(IT),FILENAME)
      FCRSS(IT)=FILENAME
      END DO
      CALL MFNAME(MFCRSS(6),FILENAME)
      FCRSS(6)=FILENAME
      IF(MFCRSS(7).GT.0) THEN
      CALL MFNAME(MFCRSS(7),FILENAME)
      FCRSS(7)=FILENAME
      END IF
C
C     SET ZONE-TO-STATION & STATION-TO-ZONE FILE NAMES
C
      DO IT=1,10
      IF(IT.LE.9) THEN
      CALL MFNAME(MFZSTA(IT),FILENAME)
      FZSTA(IT)=FILENAME
      END IF
      CALL MFNAME(MFZSTAGR(IT),FILENAME)
      FZSTAGR(IT)=FILENAME
      END DO
      DO IT=1,6
      CALL MFNAME(MFZSTAFR(IT),FILENAME)
      FZSTAFR(IT)=FILENAME
      END DO
C
C     OPEN ZONE-TO-STATION & STATION-TO-ZONE FILE NAMES
C
C.....TTC SUBWAY
      open(11,file=fzsta(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(12,file=fzsta(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(13,file=fzsta(3),recl=4,access='direct',
     *       status='old',form='binary')
      open(14,file=fzsta(4),recl=4,access='direct',
     *       status='old',form='binary')
      open(15,file=fzsta(5),recl=4,access='direct',
     *       status='old',form='binary')
      open(16,file=fzsta(6),recl=4,access='direct',
     *       status='old',form='binary')
      open(17,file=fzsta(7),recl=4,access='direct',
     *       status='old',form='binary')
      open(18,file=fzsta(8),recl=4,access='direct',
     *       status='old',form='binary')
      open(32,file=fzsta(9),recl=4,access='direct',
     *       status='old',form='binary')
C.....GO RAIL
      open(71,file=fzstagr(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(72,file=fzstagr(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(73,file=fzstagr(3),recl=4,access='direct',
     *       status='old',form='binary')
      open(74,file=fzstagr(4),recl=4,access='direct',
     *       status='old',form='binary')
      open(75,file=fzstagr(5),recl=4,access='direct',
     *       status='old',form='binary')
      open(76,file=fzstagr(6),recl=4,access='direct',
     *       status='old',form='binary')
      open(77,file=fzstagr(7),recl=4,access='direct',
     *       status='old',form='binary')
      open(78,file=fzstagr(8),recl=4,access='direct',
     *       status='old',form='binary')
      open(79,file=fzstagr(9),recl=4,access='direct',
     *       status='old',form='binary')
      open(80,file=fzstagr(10),recl=4,access='direct',
     *       status='old',form='binary')
C.....FARE MATRICES
      open(62,file=fzstafr(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(63,file=fzstafr(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(64,file=fzstafr(3),recl=4,access='direct',
     *       status='old',form='binary')
      open(65,file=fzstafr(4),recl=4,access='direct',
     *       status='old',form='binary')
      open(66,file=fzstafr(5),recl=4,access='direct',
     *       status='old',form='binary')
      open(67,file=fzstafr(6),recl=4,access='direct',
     *       status='old',form='binary')
C
C DIRECT WALK TO STATION
C
      CALL MFNAME(MFWLKSTA,FILENAME)
      FWLKSTA=FILENAME
      open(19,file=fwlksta,recl=4,access='direct',
     *       status='old',form='binary')
C
C  HIGHWAY TIME & DISTANCE
C
      DO IT=1,2
      CALL MFNAME(MFDRV0(IT),FILENAME)
      FDRV0(IT)=FILENAME
      END DO
      open(20,file=fdrv0(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(21,file=fdrv0(2),recl=4,access='direct',
     *       status='old',form='binary')
C
C  WALK TO GO BUS SKIMS
C
      DO IT=1,10
      CALL MFNAME(MFWKGOBUS(IT),FILENAME)
      FWKGOBUS(IT)=FILENAME
      END DO
      open(22,file=fwkgobus(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(23,file=fwkgobus(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(24,file=fwkgobus(3),recl=4,access='direct',
     *       status='old',form='binary')
      open(27,file=fwkgobus(4),recl=4,access='direct',
     *       status='old',form='binary')
      open(28,file=fwkgobus(5),recl=4,access='direct',
     *       status='old',form='binary')
      open(29,file=fwkgobus(6),recl=4,access='direct',
     *       status='old',form='binary')
      open(30,file=fwkgobus(7),recl=4,access='direct',
     *       status='old',form='binary')
      open(31,file=fwkgobus(8),recl=4,access='direct',
     *       status='old',form='binary')
      open(33,file=fwkgobus(9),recl=4,access='direct',
     *       status='old',form='binary')
      open(25,file=fwkgobus(10),recl=4,access='direct',
     *       status='old',form='binary')
C
C     INPUT BASE CASE TRANSIT TRIPS
C
      DO IT=1,3
      CALL MFNAME(MFTRNPER(IT),FILENAME)
      FTRNPER(IT)=FILENAME
      END DO
      open(34,file=ftrnper(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(35,file=ftrnper(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(36,file=ftrnper(3),recl=4,access='direct',
     *       status='old',form='binary')
C
C     OPEN LOGSUM VALUE MATRICES
C
      IF(.NOT.CALIB) THEN
      DO IT=1,3
      CALL MFNAME(MFLSUM(IT),FILENAME)
      FLSUM(IT)=FILENAME
      END DO
      IF(LSBASE) THEN
      open(37,file=flsum(1),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(38,file=flsum(2),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(39,file=flsum(3),recl=4,access='direct',
     *       status='unknown',form='binary')      
      ELSE
      open(37,file=flsum(1),recl=4,access='direct',
     *       status='old',form='binary')
      open(38,file=flsum(2),recl=4,access='direct',
     *       status='old',form='binary')
      open(39,file=flsum(3),recl=4,access='direct',
     *       status='old',form='binary')
      END IF
      END IF
C
C     OPEN OUTPUT TRIP MATRICES
C
      IF(TRIPSOUT) THEN
C....STATION LEVEL TRIPS
      DO IT=1,8
      CALL MFNAME(MFSTATRP(IT),FILENAME)
      FSTATRP(IT)=FILENAME
      END DO
      open(43,file=fstatrp(1),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(44,file=fstatrp(2),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(45,file=fstatrp(3),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(46,file=fstatrp(4),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(47,file=fstatrp(5),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(48,file=fstatrp(6),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(49,file=fstatrp(7),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(50,file=fstatrp(8),recl=4,access='direct',
     *       status='unknown',form='binary')
C...ZONE TO ZONE TRIPS
      DO IT=1,11
      CALL MFNAME(MFTRIPS(IT),FILENAME)
      FTRIPS(IT)=FILENAME
      END DO
      open(51,file=ftrips(1),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(52,file=ftrips(2),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(53,file=ftrips(3),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(54,file=ftrips(4),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(55,file=ftrips(5),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(56,file=ftrips(6),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(57,file=ftrips(7),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(58,file=ftrips(8),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(59,file=ftrips(9),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(60,file=ftrips(10),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(61,file=ftrips(11),recl=4,access='direct',
     *       status='unknown',form='binary')
      END IF
C
C     CHECK FOR ERRORS
C
      IF(NERR.GT.0) THEN
      WRITE(26,102) NERR
  102 FORMAT(' RCTL 102 (F) ENCOUNTERED ',I2,' CONTROL FILE ERRORS')
      STOP 102
      END IF
      OPEN(100,FILE='ILOGIT.ERR',STATUS='UNKNOWN',FORM='FORMATTED')
      RETURN
      END
