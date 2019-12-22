C***********************************************************
C     OBTAIN FILE AND PARAMETER VALUES                     *
C***********************************************************
      SUBROUTINE RCTL
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
      INTEGER*2    TIME(4),NERR,STAT1,I(20),J(20),LINDEX,JCOUNT
      INTEGER*4    IT,ST,TT,UT,VT,XT,RLEN,DATE(3),NUM,NZONES,ALLOC_VAL
      INTEGER*4    T,PD,RERPD,AREA1,AREA2,AREA3,ZONE,SUPERPD,IZ,JZ
      REAL*4       PCOST,WALK1,WALK2,PCOSTFCT,HOTEL,MCOST
      REAL*4       SHDROPP,SHDROPE,RVALUE(5),XCORD,YCORD
      REAL*4       MEDINC,HHLD,POP,EMPLY(30),TEMPLY,ECOST
      REAL*4       AREAM2,ZPOP,ZEMP,AVALUE(24)
	    REAL*4       FROW(4000),TEMP,TPOP,TAREA,POPEMPDEN
	    REAL*4, ALLOCATABLE : : EMATX(:)
      CHARACTER*1  COMMA,BLANK,SELECT
      CHARACTER*2  EGTYPE,WALK,SHUTTLE,TRNONLY,ITF
      CHARACTER*5  CODE
      CHARACTER*6  TPURP,TDAY
      CHARACTER*7  NAMERR(7)
      CHARACTER*9  EMAT
      CHARACTER*10 RENTNAME(10)
      CHARACTER*35 DNAMEIN,SNAME
      CHARACTER*60 ALTNAME
      CHARACTER*80 HEADER
      CHARACTER*200 FILENAME
      CHARACTER*200 CTLFILE
      LOGICAL      EXISTS,CTLERR(7),TXERR
      DATA         FRPT/'MLOGIT.RPT'/,FERRFILE/'MLOGIT.ERR'/
      DATA         FBUCKET/'u140.TMP'/
      DATA         NAMERR/'OBS    ','PARAMS ','OPTIONS','SELECTS',
     *                    'PARMS  ','MATX   ','FILES'/
      DATA         EMAT/'\emmebin\'/
      DATA         WALK/'W'/,SHUTTLE/'ST'/,TRNONLY/'T'/,ITF/'I'/
C
C WRITE PROGRAM HEADER INFORMATION
C
      CALL GETTIM(TIME(1),TIME(2),TIME(3),TIME(4))
      CALL GETDAT(DATE(3),DATE(1),DATE(2))
      WRITE(*,1000) DATE,(TIME(T),T=1,3)
 1000 FORMAT(/'              PROGRAM MLOGIT'/
     *        '    MODE CHOICE MODEL APPLICATION PROGRAM '/
     *        '          [Version Date: 16Aug19]'/
     *        '          [ 4,000 Zone Version ]'/
     *        '       [ Provincial Model Version ]'//
     *        5X,'       Date: ',I2,'/',I2,'/',I4/
     *        5X,'       Time: ',I2,':',I2,':',I2,///)
C
C     NAMELIST DEFINITION..........
C
      NAMELIST  /FILES/  FZINDEX,FSTAFILE,FRPT,
     *                   FDEQUIV,FCTVINP,FSTASUM,
     *                   FSTAOUT,FSTPKIN,FPREFIX,
     *                   FPRKCST,FPWALK,FDNAME,FSTAACC,
     *                   FCONST,FGOBUS,FDISTSUM,FRAPDRAT,
     *                   FBUSTXFR,AIRPARK,RENTAL,XYCOORD,
     *                   FERRFILE,FBUCKET,FRPTCSV,
     *                   FPOPSYN,FSPFILE,FHOTEL,FWATERLOO,
     *                   FUSERBEN,U190TMP,FAIRCALIB,FZDEN,
     *                   FTRESODAT,FTRESOSTA,FTRESOEGR,
     *                   FVEHATTR,FTAXISUM,FTAXIWAIT,
     *                   FTAXITLF,FEGRWALK,FAVCVPROP,
     *                   FVSKIM
      NAMELIST  /MATX/   MFURSS,MFCRSS,MFZSTA,MFWLKSTA,
     *                   MFWKGOBUS,MFTOTPER,MFLSUM,
     *                   MFSTATRP,MFTRIPS,MFZSTAGR,MFZSTAFR,
     *                   MFWKBUSTR,MFWKRAP,MFHWY0,MFHWY2P,
     *                   MFNMOTOR,MFATRIPS,MFAIR,MFHWY3P,
     *                   SFZSTA,SFZSTAFR,SFWLKSTA,MFVSKIM,
     *                   AVHWY0,AVHWY2P,AVHWY3P,MFAPARK
      NAMELIST  /PARAMS/ NCATS,MXZONES,CITER,TVALUE,PVALUE,
     *                   CTAZNE,LVALUE,FINCH,GOUNION,TTCUNION,
     *                   BDMAIN,PEARSON,ASELSTA,
     *                   MXACCTTC,MXACCGOR,MXEGRTTC,MXEGRGOR
      NAMELIST  /OPTIONS/DEBUG,SDETAIL,CALIB,LSBASE,TRIPSOUT,
     *                   PEAK,CAPRES,ZEROCAR,NDRVGOB,NDRVRAP,
     *                   NDRVBUS,CCR,PTEST,AIR,ITFPRK,LDEBUG,
     *                   AIRPASS,TRNLOT,CONRAC,NMOT,VEHOUT,
     *                   NOTRANSIT,CSVRPT,EMPPRK,SPEVENT,
     *                   VISITOR,SPACESUM,WATERLOO,TTCACC,
     *                   USERBEN,CARPOOL,HWYREL,UBERTRN,UBERMODE,
     *                   TAXIMODE,TRNEGR,PRKMDL,AVMDL,TRESO,
     *                   VEHATR,TDEBUG,ALTPARK,INITEMX,BACKVER,
     *                   VSKIM,UBERIN,FOURDETAIL
      NAMELIST  /SELECTS/I,J
      NAMELIST  /PARMS/  COEFF,WAITLT,LSUM1TRN,LSUM2GB,LSUM2CR,
     *                   LSUM2UR,LSUM3CW,LSUM3CB,LSUM3CP,LSUM1NM,
     *                   LSUM3CK,LSUM3UW,LSUM3UB,LSUM3UP,PCOSTFCT,
     *                   LSUM2SB,LSUM2RB,LSUM1AUTO,LSUM2AUTO,LSUM3AUTO,
     *                   LSUM3P2,LSUM3P3,K2P,K3P,LSUM2SR,KVIS,
     *                   LSUM2DA,LSUM3DA,KDA,KSR,KAUT,KTRN,KTRNT,
     *                   LSUM3UK,KDTRN,KPNR,OPCOST,DSCT2P,DSCT3P,
     *                   BPFACE,BUSPNROCC,SWALK,LWALK,UPXCNST,
     *                   KWCR,KBCR,KPCR,KKCR,SCHBUS,ATTFCT,AIRFCTR,
     *                   KWUR,KBUR,KPUR,KKUR,KCRURTX,KUBERMODE,
     *                   KCBD,KCR,KUR,KGBUS,KRBUS,KDIST,
     *                   KINFLSTR,KINFLRPD,KPUBPRK,KLOTTRN,
     *                   CCRPNRT,CCRPNRD,CDAR,KGOBUSW,KBUSTRW,
     *                   KGOBUSD,BFAREGR,DFAREGR,BFARETTC,
     *                   HOV2P,HOV3P,YINTER,YSLOPE,NAIRFR,AIRFR,
     *                   KTOLL,KTOLL2,KTOLL3,KHOV2,KHOV3,KRBUSW,
     *                   INTRADA,INTRA2P,KBIKE,KNMOT,KGLINE,KTLINE,
     *                   INTRA3P,INTRABIKE,INTRAWLK,INTRASCHBUS,
     *                   MWALKT,MWALK1,MWALK2,MBIKET,MBIKE1,MBIKE2,
     *                   ACOEF,ACNST,RNTWAIT,AIROCC,KCRUNW,KURWLK,
     *                   KFINCHBUS,KNMCBD,UNIONGR,UNIONTTC,KURUNW,
     *                   COEFFCP,KCP,KCPMSEG,KCPTPER,KMAAS,KTAXI,
     *                   TXCOEF,UBCOEF,KASTA,RELFCTR,KCRWLK
      NAMELIST  /OBS/    NITER,ADJFCT,CCODE,CBDTRP,DSTTRP,DAINDEX,
     *                   GORLOBS,TTCLOBS,FINCHOBS,NMCBDOBS,
     *                   GORUNOBS,TTCUNOBS,ASTAOBS,STAFCT,TTCEGR
      NERR=0
      DSCT2P=1.0
      DSCT3P=1.0
      SWALK=5.0
      LWALK=10.0
      CBDTRP=0.0
      KINFLSTR=0.0
      KINFLRPD=0.0
      PCOSTFCT=1.0
      MAXPD=0
      STALINE=0
      CCR=.FALSE.
      SCHBUS=0.0
      KGOBUSW=0.0
      KGOBUSD=0.0
      COEFF(25)=1.25
      COEFF(26)=0.75
      COEFF(31)=0.493502
      COEFF(32)=9.67239
      COEFF(33)=43.29598
      TVALUE=0.0
      LVALUE(1)=0.25
      LVALUE(2)=0.25
      LVALUE(3)=0.25
      LVALUE(4)=0.25
      LVALUE(5)=0.25
      LVALUE(6)=0.25
      PVALUE(1)=26
      PVALUE(2)=26
      PVALUE(3)=26
      PVALUE(4)=26
      PVALUE(5)=26
      PVALUE(6)=26
      LINDEX=0
      JCOUNT=0
      AIRPASS=.FALSE.
      TRNLOT=.FALSE.
      CONRAC=.FALSE.
      NMOT=.TRUE.
      ACOEF=0.0
      ACNST=0.0
      RNTWAIT=0.0
      AIROCC=1.0
      LCPI=1.0
      KDIST=0.0
      KLOTTRN=0.0
      KBUSTRW=0.0
      KRBUSW=0.0
      KGLINE=0.0
      KTLINE=0.0
      FINCH=9708
      GOUNION=9801
      TTCUNION=9771
      BDMAIN=9763
      PEARSON=9863
      KFINCHBUS=0.0
      FINCHOBS=0.0
      DAINDEX=.FALSE.
      VEHOUT=.FALSE.
      CHKOUT=.TRUE.
      CTLERR=.FALSE.
      UNIONGR=15.0
      UNIONTTC=10.0
      NOTRANSIT=.FALSE.
      CSVRPT=.FALSE.  
      EMPPRK=.FALSE.
      SPEVENT=.FALSE.
      MXZONES=4000
      ATTFCT=1.0
      WATERLOO=.FALSE.
      TTCACC=.FALSE.
      TPURP='HBW'
      TDAY='PEAK'
      ALTNAME='2011 BASE'
      AIRFCTR=1.52381
      NAIRFR=0.0
      AIRFR=0.0
      COEFFCP=0.0
      ZDEN=0.0
      UBERTRN=.FALSE.
      UBERMODE=.FALSE.
      TAXIMODE=.FALSE.
      KMAAS=0.0
      KTAXI=0.0
      KUBERMODE=0.0
      TRNEGR=.FALSE.
      PRKMDL=.FALSE.
      AVMDL=.FALSE.
      TRESO=.FALSE.
      VEHATR=.FALSE.
      ALTPARK=.FALSE.
      TXCOEF=0.0
      UBCOEF=0.0
      TXERR=.FALSE.
      ZHHD=0.0
      HWY3P=.FALSE.
      ASELSTA=0
      KASTA=0.0
      ASTAOBS=0.0
      STAFCT=0.05
      TTCEGR=0.0
      INITEMX=.FALSE.
      BACKVER=.FALSE.
      RELFCTR=1.0
      KCRWLK=0.0
      VSKIM=.FALSE.          
      UBERIN=.TRUE.
      UPXCNST=0.0
      FDIST=0.0
      UPXIND=0
      MXACCTTC=99.0
      MXACCGOR=99.0
      MXEGRTTC=99.0
      MXEGRGOR=99.0
      FOURDETAIL=.FALSE.
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
C SET GO RAIL DISTANCE FARE
C
      CALL RDFILES
C 
C SUMMARIZE FILE NAMES
C
      OPEN(26,FILE=FRPT,STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(26,1000) DATE,(TIME(T),T=1,3)
C 
C PRINT FILE INPUT/OUTPUT SUMMARY
C
      WRITE(26,500) FZINDEX,FSTAFILE,FRPT,FRPTCSV,
     *             FDEQUIV,FDNAME,FCTVINP,
     *             FSTAOUT,FSTPKIN,FPRKCST,FPREFIX,FPWALK,
     *             FZDEN,FSTASUM,FSTAACC,FUSERBEN,
     *             FCONST,FGOBUS,FDISTSUM,FVSKIM,
     *             FBUSTXFR,FRAPDRAT,FTRESODAT,FTRESOSTA,FTRESOEGR,
     *             FVEHATTR,FTAXISUM,FTAXIWAIT,FTAXITLF,FEGRWALK
      IF(AIR) WRITE(26,501) AIRPARK,RENTAL,XYCOORD
      IF(SPEVENT) WRITE(26,502)  FPOPSYN,FSPFILE
      IF(VISITOR) WRITE(26,503)  FHOTEL
  500 FORMAT(1X,'SUMMARY OF FILE INPUTS & OUTPUTS'/
     *       1X,'--------------------------------------------------'/
     *       1X,'FZINDEX = ',a50,' EXTERNAL/INTERNAL ZONE INDEX FILE'/
     *       1X,'FSTAFILE= ',a50,' STATION ATTRIBUTE FILE'/
     *       1X,'FRPT    = ',a50,' PROGRAM REPORT FILE'/
     *       1X,'FRPTCSV = ',a50,' PROGRAM REPORT FILE (CSV)'/
     *       1X,'FDEQUIV = ',a50,' ZONE/DISTRICT EQUIVALENCE FILE'/
     *       1X,'FDNAME  = ',a50,' DISTRICT DESCRIPTION FILE'/
     *       1X,'FCTVINP = ',a50,' CALIBRATION TARGET VALUE FILE'/
     *       1X,'FSTAOUT = ',a50,' STATION USAGE OUTPUT FILE'/
     *       1X,'FSTPKIN = ',a50,' STATION USAGE INPUT FILE'/
     *       1X,'FPRKCST = ',a50,' PARKING COST FILE'/
     *       1X,'FPREFIX = ',a50,' EMMEMAT DIRECTORY LOCATION'/
     *       1X,'FPWALK  = ',a50,' ZONAL PERCENT WALK FILE'/
     *       1X,'FZDEN   = ',a50,' ZONAL DENSITIES FILE'/
     *       1X,'FSTASUM = ',a50,' RAIL LINE SEGMENT SUMMARY'/
     *       1X,'FSTAACC = ',a50,' RAIL STATION ACCESS SUMMARY'/
     *       1X,'FUSERBEN= ',a50,' USER BENEFIT FILE OUTPUT'/
     *       1X,'FCONST  = ',a50,' CALIBRATED CONSTANT FILE'/
     *       1X,'FGOBUS  = ',a50,' GO BUS SUMMARY FILE'/
     *       1X,'FDISTSUM= ',a50,' TRANSIT DISTRICT SUMMARY FILE'/
     *       1X,'FVSKIM=   ',a50,' VIRTUAL PATH STATION OUTPUT'/
     *       1X,'FBUSTXFR= ',a50,' BUS TRANSFER INCIDENCE DATA'/
     *       1X,'FRAPDRAT= ',a50,' RAPID BUS IN-VEHICLE RATIO'/
     *       1X,'FTRESODAT=',a50,' TRESO PROBABILITY FILE'/
     *       1X,'FTRESOSTA=',a50,' TRESO STATION FILE'/
     *       1X,'FTRESOEGR=',a50,' TRESO EGRESS PROBABILITY FILE'/
     *       1X,'FVEHATTR =',a50,' AUTO PERSON ATTRACTION FILE'/
     *       1X,'FTAXISUM =',a50,' TAXI DISTRICT LEVEL SUMMARY'/
     *       1X,'FTAXIWAIT=',a50,' TAXI WAIT TIME DISTRIBUTIONS'/
     *       1X,'FTAXITLF =',a50,' TAXI TRIP LENGTH FREQUENCY'/
     *       1X,'FEGRWALK =',a50,' STATION EGRESS WALK DISTRIBUTION'/
     *       1X,'FAVCVPROP=',a50,' AV/CV PROPORTIONS')
  501 FORMAT(1X,'AIRPARK = ',a50,' PEARSON PARKING LOT FILE'/
     *       1X,'RENTAL  = ',a50,' RENTAL CAR FACILITY FILE'/
     *       1X,'XYCOORD = ',a50,' X-Y CENTROID COORDINATE FILE'/)
  502 FORMAT(1X,'FPOPSYN = ',a50,' POPSYN SUMMARY FILE'/
     *       1X,'FSPFILE = ',a50,' SPECIAL EVENT INPUT FILE'/)
  503 FORMAT(1X,'FHOTEL  = ',a50,' HOTEL ROOM FILE'/)
C
C     PLAY BACK VALUES FROM THE CONTROL FILE
C
      WRITE(26,9001) MFCRSS,MFURSS,MFZSTA,MFWLKSTA,
     *               MFWKGOBUS,MFWKBUSTR,MFWKRAP,
     *               MFHWY0,MFHWY2P,MFHWY3P,MFTOTPER,MFLSUM,
     *               MFSTATRP,MFTRIPS,MFATRIPS,MFVSKIM,
     *               MFZSTAFR,MFNMOTOR,SFZSTA,SFZSTAFR,SFWLKSTA,
     *               MFAIR,AVHWY0,AVHWY2P,AVHWY3P,MFAPARK,
     *               NCATS,MXZONES,CITER,TVALUE,PVALUE,LVALUE,CTAZNE,
     *               FINCH,GOUNION,TTCUNION,PEARSON,ASELSTA,
     *               MXACCTTC,MXACCGOR,MXEGRTTC,MXEGRGOR,
     *               SDETAIL,DEBUG,CALIB,LSBASE,
     *               TRIPSOUT,PEAK,CAPRES,ZEROCAR,
     *               NDRVGOB,NDRVRAP,NDRVBUS,CCR,AIR,AIRPASS,TRNLOT,
     *               CONRAC,NMOT,VEHOUT,CSVRPT,EMPPRK,SPEVENT,VISITOR,
     *               SPACESUM,USERBEN,VSKIM,UBERIN,TTCACC,
     *               CARPOOL,HWYREL,UBERTRN,UBERMODE,
     *               TAXIMODE,TRNEGR,PRKMDL,AVMDL,TRESO,VEHATR,ALTPARK,
     *               INITEMX,BACKVER,
     *               I,J,(COEFF(K),K=1,30),(COEFF(K3),K3=31,40),
     *               (COEFF(K4),K4=41,50),
     *               (COEFF(K1),K1=51,59),
     *               (COEFF(K2),K2=71,76),
     *               (COEFFCP(K5),K5=1,30),
     *               ((TXCOEF(K6,K5),K5=1,2),K6=1,6),
     *               ((UBCOEF(K6,K5),K5=1,2),K6=1,6),
     *               WAITLT,
     *               SWALK,LWALK,OPCOST,PCOSTFCT,
     *               BFAREGR,DFAREGR,BFARETTC,NAIRFR,AIRFR,
     *               CCRPNRT,CCRPNRD,CDAR,
     *               BPFACE,BUSPNROCC,HOV2P,HOV3P,
     *               DSCT2P,DSCT3P,YINTER,YSLOPE,
     *               INTRADA,INTRA2P,INTRA3P,
     *               INTRABIKE,INTRAWLK,INTRASCHBUS,SCHBUS,
     *               MWALKT,MWALK1,MWALK2,MBIKET,MBIKE1,MBIKE2,
     *               UNIONGR,UNIONTTC,LSUM1TRN,LSUM1NM,
     *               LSUM2GB,LSUM2CR,LSUM2UR,LSUM2SB,LSUM2RB,
     *               LSUM3CW,
     *               LSUM3CB,LSUM3CP,LSUM3CK,LSUM3UW,
     *               LSUM3UB,LSUM3UP,LSUM3UK,
     *               LSUM1AUTO,LSUM2AUTO,LSUM3AUTO,
     *               LSUM2DA,LSUM3DA,
     *               LSUM2SR,LSUM3P2,LSUM3P3,KTRN,
     *               KDTRN,KPNR,KGOBUSD,KGOBUSW,
     *               KRBUS,KLBUS,KBUSTRW,KRBUSW,
     *               KCR,KUR,KGBUS,KCBD,KWCR,KWUR,
     *               KBCR,KBUR,KPCR,KPUR,KKCR,KKUR,
     *               K2P,K3P,KDA,KSR,KAUT,KBIKE,KNMOT,KMAAS,KTAXI,
     *               KUBERMODE,KTOLL,KTOLL2,KTOLL3,KHOV2,KHOV3,
     *               KINFLSTR,KINFLRPD,KGLINE,KFINCHBUS,
     *               KCRURTX,KCRUNW,KURUNW,KCRWLK,KURWLK,KPUBPRK,
     *               KLOTTRN,KASTA,ATTFCT,
     *               KCP(1),KCP(2),KCP(3),(KCPMSEG(1,K6),K6=1,6),
     *               (KCPMSEG(2,K6),K6=1,6),(KCPMSEG(3,K7),K7=1,6),
     *               (KCPTPER(K8),K8=1,3)
      IF(AIRPASS)    WRITE(26,9013) ACOEF,RNTWAIT,AIROCC,
     *               ACNST,AWUR,UPXCNST,AIRFCTR
 9001 FORMAT(//' RCTL 9001 (I) CONTENTS OF CONTROL FILE: '/
     *         ' ----------------------------------------'/
     *    '&MATX'   /'  MFCRSS   = ',12I5/'  MFURSS   = ',9I5/
     *               '  MFZSTA   = ',15I5/'  MFWLKSTA = ',I5/
     *               '  MFWKGOBUS= ',16I5/
     *               '  MFWKBUSTR= ',12I5/'  MFWKRAP  = ',13I5/
     *               '  MFHWY0   = ',8I5/ '  MFHWY2P  = ',10I5/
     *               '  MFHWY3P  = ',10I5/
     *               '  MFTOTPER = ',6I5/ '  MFLSUM   = ',6I5/
     *               '  MFSTATRP = ',16I5/'  MFTRIPS  = ',17I5/
     *               '  MFATRIPS = ',23I5/'  MFVSKIM  = ',25I5/
     *               '  MFZSTAFR = ',3I5/ '  MFNMOTOR = ',4I5/
     *               '  SFZSTA   = ',15I5/'  SFSTAFR  = ',3I5/
     *               '  SFWLKSTA = ',I5/  '  MFAIR    = ',6I5/
     *               '  AVHWY0   = ',8I5/ '  AVHWY2P  = ',10I5/
     *               '  AVHWY3P  = ',10I5/'  MFAPARK  = ',6I5/  
     *    '&PARAMS' /'  NCATS   = ',I4/   '  MXZONES  = ',I4/
     *               '  CITER   = ',I4/   '  TVALUE   = ',6(1X,F5.2)/
     *               '  PVALUE  = ',6I4/  '  LVALUE   = ',6(1X,F5.2)/
     *               '  CTAZNE   = ',5I5/ '  FINCH    = ',I4/
     *               '  GOUNION  = ',I4/  '  TTCUNION = ',I4/
     *               '  PEARSON  = ',I4/  '  ASELSTA  = ',10I5/
     *               '  MXACCTTC = ',F4.1/'  MXACCGOR = ',F4.1/
     *               '  MXEGRTTC = ',F4.1/'  MXEGRGOR = ',F4.1/
     *    '&OPTIONS'/'  SDETAIL = ',L1  /'  DEBUG   = ',L1/
     *               '  CALIB   = ',L1  /'  LSBASE  = ',L1/
     *               '  TRIPSOUT= ',L1  /'  PEAK    = ',L1/
     *               '  CAPRES  = ',L1  /'  ZEROCAR = ',L1/
     *               '  NDRVGOB = ',L1  /'  NDRVRAP = ',L1/
     *               '  NDRVBUS = ',L1  /'  CCR     = ',L1/
     *               '  AIR     = ',L1  /'  AIRPASS = ',L1/
     *               '  TRNLOT  = ',L1  /'  CONRAC  = ',L1/
     *               '  NMOT    = ',L1  /'  VEHOUT  = ',L1/
     *               '  CSVRPT  = ',L1  /'  EMPPRK  = ',L1/
     *               '  SPEVENT = ',L1  /'  VISITOR = ',L1/
     *               '  SPACESUM= ',L1  /'  USERBEN = ',L1/
     *               '  VSKIM=    ',L1  /'  UBERIN  = ',L1/
     *               '  TTCACC    ',L1  /
     *               '  CARPOOL = ',L1  /'  HWYREL  = ',L1/
     *               '  UBERTRN = ',L1  /'  UBERMODE= ',L1/
     *               '  TAXIMODE= ',L1  /'  TRNEGR  = ',L1/
     *               '  PRKMDL  = ',L1  /'  AVMDL   = ',L1/
     *               '  TRESO   = ',L1  /'  VEHATR  = ',L1/
     *               '  ALTPARK = ',L1  /'  INITEMX = ',L1/
     *               '  BACKVER = ',L1  /
     *    '&SELECTS'/'  I       = ',20I5/'  J       = ',20I5/
     *    '&PARMS  '/'  COEFF( 1-10)= ',10(1X,F8.5)/
     *               '  COEFF(11-20)= ',10(1X,F8.5)/
     *               '  COEFF(21-30)= ',10(1X,F8.5)/
     *               '  COEFF(31-40)= ',10(1X,F8.5)/
     *               '  COEFF(41-50)= ',10(1X,F8.5)/
     *               '  COEFF(51-59)= ',9(1X,F8.5)/
     *               '  COEFF(71-76)= ',6(1X,F8.5)/
     *               '  COEFFCP( 1-10)= ',10(1X,F8.5)/
     *               '  COEFFCP(11-20)= ',10(1X,F8.5)/
     *               '  COEFFCP(11-30)= ',10(1X,F8.5)/
     *               '  TXCOEF(1,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(2,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(3,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(4,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(5,1-2) = ', 2(1X,F12.5)/
     *               '  TXCOEF(6,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(1,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(2,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(3,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(4,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(5,1-2) = ', 2(1X,F12.5)/
     *               '  UBCOEF(6,1-2) = ', 2(1X,F12.5)/
     *               '  WAITLT      = ',F9.5/
     *               '  SWALK       = ',F9.2/
     *               '  LWALK       = ',F9.2/
     *               '  OPCOST      = ',F9.2/
     *               '  PCOSTFCT    = ',F9.2/
     *               '  BFAREGR     = ',F9.2/
     *               '  DFAREGR     = ',F9.2/
     *               '  BFARETTC    = ',F9.2/
     *               '  NAIRFR      = ',F9.2/
     *               '  AIRFR       = ',F9.2/
     *               '  CCRPNRT     = ',F9.5/
     *               '  CCRPNRD     = ',F9.5/
     *               '  CDAR        = ',F9.5/
     *               '  BPFACE      = ',F9.5/
     *               '  BUSPNROCC   = ',F9.5/
     *               '  HOV2P       = ',F9.5/
     *               '  HOV3P       = ',F9.5/
     *               '  DSCT2P      = ',F9.5/
     *               '  DSCT3P      = ',F9.5/
     *               '  YINTER      = ',F9.5/
     *               '  YSLOPE      = ',F9.5/
     *               '  INTRADA     = ',6F9.5/
     *               '  INTRA2P     = ',6F9.5/
     *               '  INTRA3P     = ',6F9.5/
     *               '  INTRABIKE   = ',6F9.5/
     *               '  INTRAWLK    = ',6F9.5/
     *               '  INTRASCHBUS = ',6F9.5/
     *               '  SCHBUS      = ',F9.5/
     *               '  MWALKT      = ',F9.5/
     *               '  MWALK1      = ',F9.5/
     *               '  MWALK2      = ',F9.5/
     *               '  MBIKET      = ',F9.5/
     *               '  MBIKE1      = ',F9.5/
     *               '  MBIKE2      = ',F9.5/
     *               '  UNIONGR     = ',F9.1/
     *               '  UNIONTTC    = ',F9.1//
     *               '  LSUM1TRN    = ',F9.5/
     *               '  LSUM1NM     = ',F9.5/
     *               '  LSUM2GB     = ',F9.5/
     *               '  LSUM2CR     = ',F9.5/
     *               '  LSUM2UR     = ',F9.5/
     *               '  LSUM2SB     = ',F9.5/
     *               '  LSUM2RB     = ',F9.5/
     *               '  LSUM3CW     = ',F9.5/
     *               '  LSUM3CB     = ',F9.5/
     *               '  LSUM3CP     = ',F9.5/
     *               '  LSUM3CK     = ',F9.5/
     *               '  LSUM3UW     = ',F9.5/
     *               '  LSUM3UB     = ',F9.5/
     *               '  LSUM3UP     = ',F9.5/
     *               '  LSUM3UK     = ',F9.5/
     *               '  LSUM1AUTO   = ',F9.5/
     *               '  LSUM2AUTO   = ',F9.5/
     *               '  LSUM3AUTO   = ',F9.5/
     *               '  LSUM2DA     = ',F9.5/
     *               '  LSUM3DA     = ',F9.5/
     *               '  LSUM2SR     = ',F9.5/
     *               '  LSUM3P2     = ',F9.5/
     *               '  LSUM3P3     = ',F9.5//
     *               '  KTRN        = ',6F9.5/
     *               '  KDTRN       = ',6F9.5/
     *               '  KPNR        = ',6F9.5/
     *               '  KGOBUSD     = ',F9.5/
     *               '  KGOBUSW     = ',F9.5/
     *               '  KRBUS       = ',6F9.5/
     *               '  KLBUS       = ',6F9.5/
     *               '  KBUSTRW     = ',F9.5/
     *               '  KRBUSW      = ',F9.5/
     *               '  KCR         = ',6F9.5/
     *               '  KUR         = ',6F9.5/
     *               '  KGBUS       = ',6F9.5/    
     *               '  KCBD        = ',5F9.5/
     *               '  KWCR        = ',6F9.5/
     *               '  KWUR        = ',6F9.5/
     *               '  KBCR        = ',6F9.5/
     *               '  KBUR        = ',6F9.5/
     *               '  KPCR        = ',6F9.5/
     *               '  KPUR        = ',6F9.5/
     *               '  KKCR        = ',6F9.5/
     *               '  KKUR        = ',6F9.5/
     *               '  K2P         = ',6F9.5/
     *               '  K3P         = ',6F9.5/
     *               '  KDA         = ',6F9.5/
     *               '  KSR         = ',6F9.5/
     *               '  KAUT        = ',6F9.5/
     *               '  KBIKE       = ',6F9.5/ 
     *               '  KNMOT       = ',6F9.5/
     *               '  KMAAS       = ',6F9.5/ 
     *               '  KTAXI       = ',6F9.5/ 
     *               '  KUBERMODE   = ',6F9.5/       
     *               '  KTOLL       = ',F9.5/
     *               '  KTOLL2      = ',F9.5/
     *               '  KTOLL3      = ',F9.5/
     *               '  KHOV2       = ',F9.5/
     *               '  KHOV3       = ',F9.5/
     *               '  KINFLSTR    = ',F9.5/
     *               '  KINFLRPD    = ',F9.5/
     *               '  KGLINE      = ',10F9.5/
     *               '              = ',10F9.5/
     *               '  KFINCHBUS   = ',F9.5/
     *               '  KCRURTX     = ',F9.5/
     *               '  KCRUNW      = ',F9.5/
     *               '  KURUNW      = ',F9.5/
     *               '  KCRWLK      = ',F9.5/
     *               '  KURWLK      = ',F9.5/
     *               '  KPUBPRK     = ',F9.5/
     *               '  KLOTTRN     = ',F9.5/
     *               '  KASTA       = ',10F9.5/
     *               '  ATTFCT      = ',F9.5/
     *               '  KCP         = ',3F9.5/
     *               '  KCPMSEG(1)  = ',6F9.5/
     *               '  KCPMSEG(2)  = ',6F9.5/
     *               '  KCPMSEG(3)  = ',6F9.5/
     *               '  KCPTPER     = ',3F9.5)
 9013 FORMAT(//1X,'AIR PASSENGER MODEL COEFFICIENTS --'/
     *       1X,'--------------------------------'//
     *  1X,'IN-VEHICLE COEFFICIENT             =',F8.5,' ACOEF(1)'/
     *  1X,'OUT-OF-VEHICLE COEFFICIENT         =',F8.5,' ACOEF(2)'/
     *  1X,'COST COEFFICIENT                   =',F8.5,' ACOEF(3)'/
     *  1X,'RENTAL CAR AUTO OPERATING COST/KM  =',F8.5,' ACOEF(4)'/
     *  1X,'RENTAL CAR FACILITY WAIT TIME      =',F8.5,' RNTWAIT'/
     *  1X,'PEARSON PARKING LOT AUTO OCCUPANCY =',F8.5,' AIROCC'//
     *  1X,'TAXI CONSTANT                      =',F8.4,' ACNST(1)'/
     *  1X,'RENTAL CONSTANT                    =',F8.4,' ACNST(2)'/
     *  1X,'LIMO/TOWN CAR CONSTANT             =',F8.4,' ACNST(3)'/
     *  1X,'PRIVATE VEHICLE DROP-OFF CONSTANT  =',F8.4,' ACNST(4)'/
     *  1X,'PRIVATE VEHICLE PARKED CONSTANT    =',F8.4,' ACNST(5)'/
     *  1X,'ON-CALL SHUTTLE/VAN CONSTANT       =',F8.4,' ACNST(6)'/
     *  1X,'PUBLIC TRANSIT CONSTANT            =',F8.4,' ACNST(7)'/
     *  1X,'NOT USED                           =',F8.4,' ACNST(8)'/
     *  1X,'PUBLIC SERVICES CONSTANT           =',F8.4,' ACNST(9)'/
     *  1X,'TNC (UBER) CONSTANT                =',F8.4,' ACNST(10)'/
     *  1X,'TNC (UBER) UPX ACCESS CONSTANT     =',F8.4,' ACNST(11)'/
     *  1X,'TTC SUBWAY ACCESS TO GO RAIL       =',F8.4,' ACNST(12)'/
     *  1X,'GO UNION STATION CONSTANT          =',F8.4,' ACNST(13)'/
     *  1X,'URBAN RAIL WALK ACCESS CONSTANT    =',F8.4,' AWUR'/
     *  1X,'UP EXPRESS CONSTANT                =',F8.4,' UPXCNST'/
     *  1X,'AIR PASSENGER ENPLANEMENT FACTOR   =',F8.4,' AIRFCTR'//)
      IF(CALIB) THEN
      WRITE(26,9011) NITER,ADJFCT,CCODE
 9011 FORMAT('&OBS   ' /'  NITER   = ',I4  /'  ADJFCT   = ',F4.2/
     * '  CCODE(1) = ',3X,L1,' DRIVE TO TRANSIT (KDTRN)'/
     * '  CCODE(2) = ',3X,L1,' PNR   TO    RAIL (KPNR)'/
     * '  CCODE(3) = ',3X,L1,' DRIVE TO GO BUS  (KGOBUSD)'/
     * '  CCODE(4) = ',3X,L1,' GO RAIL CONSTANT (KCR)'/
     * '  CCODE(5) = ',3X,L1,' TTC SUBWAY CONSTANT (KUR)'/
     * '  CCODE(6) = ',3X,L1,' GO BUS CONSTANT(KGBUS)'/
     * '  CCODE(7) = ',3X,L1,' BASIC SET OF CONTANTS',
     *                       ' (KTRN,KTRNT,KSR,K3P)'/
     * '  CCODE(8) = ',3X,L1,' WALK TO TTC SUBWAY (KWUR)'/
     * '  CCODE(9) = ',3X,L1,' CBD CONSTANTS (KBCBD)'/
     * '  CCODE(10)= ',3X,L1,' STRATIFIED TRANSIT CONSTANTS'/
     * '  CCODE(11)= ',3X,L1,' RAPID BUS CONSTANT (KRBUS)'/
     * '  CCODE(12)= ',3X,L1,' PNR TO GO RAIL (KPCR)'/
     * '  CCODE(13)= ',3X,L1,' NON-MOTORIZED (KNMOT)'/
     * '  CCODE(14)= ',3X,L1,' KNR TO GO RAIL (KKCR)'/
     * '  CCODE(15)= ',3X,L1,' KNR TO TTC SUBWAY (KKUR)'/
     * '  CCODE(16)= ',3X,L1,' BUS TO GO RAIL (KBCR)'/
     * '  CCODE(17)= ',3X,L1,' BUS TO TTC SUBWAY (KBUR)'/
     * '  CCODE(18)= ',3X,L1,' WALK TO GO BUS (KGOBUSW)'/
     * '  CCODE(19)= ',3X,L1,' WALK TO GO RAIL (KWCR)'/
     * '  CCODE(20)= ',3X,L1,' PNR TO TTC SUBWAY (KPUR)'/
     * '  CCODE(21)= ',3X,L1,' BICYCLE (KBIKE)'/
     * '  CCODE(22)= ',3X,L1,' INFORMAL DRIVE - BUS/STREETCAR',
     *                       ' (KINFLSTR)'/
     * '  CCODE(23)= ',3X,L1,' INFORMAL DRIVE - RAPID BUS (KINFLRPD)'/
     * '  CCODE(24)= ',3X,L1,' DISTRICT LEVEL DEST. CONSTANT',
     *                       ' (KDIST)'/
     * '  CCODE(25)= ',3X,L1,' WALK TO BUS/STREETCAR (KBUSTRW)'/
     * '  CCODE(26)= ',3X,L1,' DRIVE ALONE (KDA)'/
     * '  CCODE(27)= ',3X,L1,' GO RAIL LINE CONSTANTS'/
     * '  CCODE(28)= ',3X,L1,' TTC SUBWAY RAIL LINE CONSTANTS'/
     * '  COODE(29)= ',3X,L1,' FINCH BUS ACCESS CONSTANT'/
     * '  CCODE(30)= ',3X,L1,' NON-MOTORIZED CBD CONSTANT'/
     * '  CCODE(31)= ',3X,L1,' GO RAIL UNION STATION WALK'/
     * '  CCODE(32)= ',3X,L1,' TAXI'/
     * '  CCODE(33)= ',3X,L1,' TTC SUBWAY UNION STATION WALK'/
     * '  CCODE(34)= ',3X,L1,' SELECTED STATION CONSTANTS'/
     * '  CCODE(35)= ',3X,L1,' TTC SUBWAY EGRESS CONSTANT'/
     * '  CCODE(36)= ',3X,L1,' WALK TO RAPID BUS (KRBUSW)'/)
      END IF
C     IF(MXZONES.LE.0) THEN
C     WRITE(26,9016)
C     WRITE(*,9016)
C9016 FORMAT(/' RCTL 9016 (W) MXZONES VALUE NOT PROVIDED'/)
C     STOP 9016
C     END IF
      IF((VEHOUT.OR.TRIPSOUT).AND.CALIB) THEN
      WRITE(26,9017)
      WRITE(*,9017)
 9017 FORMAT(/' RCTL 9017 (F) TRIPS CANNOT BE WRITTEN OUT ',
     *        ' WHEN IN CALIBRATION MODE'/)
      STOP 9017
      END IF
      IF(WATERLOO.AND.(.NOT.LSBASE)) LSBASE=.TRUE.
      IF(BACKVER) KCRURTX=0.0
C
C CHECK TAXI WAIT TIME COEFFICIENTS
C
      IF(TAXIMODE) THEN
      DO K=1,6
      DO K1=1,2
      IF(TXCOEF(K,K1).EQ.0.0) THEN
      WRITE(26,9030) K,K1
      WRITE(*,9030) K,K1
 9030 FORMAT(' RCTL 9030 (F) MISSING COEFFICIENT VALUE FOR ',
     *       ' TXCOEF(',I1,',',I1,')')
      TXERR=.TRUE.
      END IF
      END DO
      END DO
      IF(TXERR) STOP 9030
      END IF
C
C CHECK UBER WAIT TIME COEFFICIENTS
C
      IF(UBERMODE.OR.UBERTRN.OR.AIR) THEN
      DO K=1,6
      DO K1=1,2
      IF(UBCOEF(K,K1).EQ.0.0) THEN
      WRITE(26,9031) K,K1
      WRITE(*,9031) K,K1
 9031 FORMAT(' RCTL 9031 (F) MISSING COEFFICIENT VALUE FOR ',
     *       ' UBCOEF(',I1,',',I1,')')
      TXERR=.TRUE.
      END IF
      END DO
      END DO
      IF(TXERR) STOP 9031
      END IF
c
c     OPEN CSV REPORT FILE, IF DESIRED
C
      IF(CSVRPT) THEN
      OPEN(173,FILE=FRPTCSV,STATUS='UNKNOWN',FORM='FORMATTED')
      END IF
C
C     OPEN VIRTUAL PATH BUILDING STATION FILE
C
      IF(VSKIM) THEN
      OPEN(179,FILE=FVSKIM,STATUS='UNKNOWN',FORM='BINARY')
      END IF
c
c     READ & STORE DISTRICT/EQUIVALENCE FILE
c
      open(99,file=fdequiv,status='old',form='formatted')
      read(99,9,end=325,err=321) header
  322 read(99,*,end=325,err=321) t,pd,rerpd,superpd
      if(rerpd.gt.46) rerpd=rerpd-254
      if(rerpd.gt.60) then
      write(26,320) rerpd,t
  320 format(' RCTL 320 (W) DISTRICT NUMBER (',I3,') FOR ZONE ',I4,
     *       ' EXCEEDS MAXIMUM ALLOWABLE VALUE OF 60')
      stop 320
      end if
      if(superpd.le.0) superpd=999
      dequiv(t)=superpd
      go to 322
  321 write(26,323) t
  323 format(' RCTL 321 (F) ERROR READING DISTRICT EQUIVALENCE FILE',
     *       ' RECORD=',i10)
      stop 321
  325 close(99,status='keep')
c
c     READ & STORE DESCRIPTION FILE
c
      open(99,file=fdname,status='old',form='formatted')
      read(99,9,end=625,err=621) header
  622 read(99,*,end=625,err=621) t,superpd,dnamein
      if(t.gt.60) then
      write(26,620) t
  620 format(' RCTL 620 (W) INTERNAL DISTRICT NUMBER (',I3,')',
     *       ' EXCEEDS MAXIMUM ALLOWABLE VALUE OF 60')
      stop 620
      end if
      spdequiv(superpd)=t
      dname(t)=dnamein
      if(t.gt.maxpd) maxpd=t
      go to 622
  621 write(26,623) t
  623 format(' RCTL 621 (F) ERROR READING DISTRICT DESCRIPTION FILE',
     *       ' RECORD=',i10)
      stop 621
  625 close(99,status='keep')
      dname(maxpd+1)='Total'
c
c     RESET DISTRICT EQUIVALENCE VALUE TO BE INTERNAL DISTRICT NUMBER
c
      do t=1,10000
      if(dequiv(t).gt.0) dequiv(t)=spdequiv(dequiv(t))
      end do
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
c
c     READ & STORE PARKING COST VALUES
c
      if(.not.spevent) then
      inquire(file=fprkcst,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9029)  fprkcst
 9029 FORMAT(//' RCTL 9029 (F) FPRKCST FILE=',A90,' NOT FOUND'//)
      STOP 9029
      else
      open(7,file=fprkcst,status='old',form='formatted')
  422 continue
      if(prkmdl) then
      read(7,*,end=425,err=421) t,pcost,mcost
      else
      read(7,*,end=425,err=421) t,pcost
      mcost=pcost      
      end if
      zone=equiv(t)
      if(zone.gt.max_zones) then
      write(26,420) zone
  420 format(' RCTL 420 (W) ZONE NUMBER (',I5,')',
     *       ' EXCEEDS MAXIMUM ALLOWABLE VALUE')
      stop 420
      end if
      prkcst(zone)=pcost*100.0
      mdlprk(zone)=mcost*100.0
      go to 422
  421 write(26,423)
  423 format(' RCTL 421 (F) ERROR READING PARKING COST FILE')
      stop 421
  425 close(7,status='keep')
      end if
      end if
c
c     READ & STORE PERCENT WALK VALUES
c
      inquire(file=fpwalk,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9021)  fpwalk
      WRITE(26,9021) fpwalk
 9021 FORMAT(//' RCTL 9021 (F) FPWALK FILE=',A90,' NOT FOUND'//)
      STOP 9021
      else
      open(20,file=fpwalk,status='old',form='formatted')
      read(20,9,end=525,err=521) header
  522 read(20,*,end=525,err=521) t,area1,area2,area3,walk1,walk2
      zone=equiv(t)
      if(zone.gt.max_zones) then
      write(26,520) zone
  520 format(' RCTL 520 (W) ZONE NUMBER (',I5,')',
     *       ' IN FPWALK EXCEEDS MAXIMUM ALLOWABLE VALUE')
      stop 520
      end if
      if(zone.le.0) then
      write(26,526) t
  526 format(' RCTL 526 (W) ZONE NUMBER (',I5,') NOT CONTAINED',
     *       ' IN EQUIVALENCY FILE')
      go to 522
      end if
      pwalk(zone,1)=walk1
      pwalk(zone,2)=walk2-walk1
      if(pwalk(zone,2).lt.0.0) write(26,9022) walk2,walk1,t
 9022 format(' RCTL 9022 (W) LONG WALK VALUE (',F6.4,') NOT GREATER ',
     *       ' OR EQUAL TO SHORT WALK VALUE (',F6.4,') FOR ZONE=',
     *       I5)
      go to 522
  521 write(26,523)
  523 format(' RCTL 521 (F) ERROR READING PERCENT WALK FILE')
      stop 521
  525 close(20,status='keep')
      end if
C
C     READ & STORE ZONE POPULATION AND EMPLOYMENT DENSITIES
C
      if(carpool.or.prkmdl.or.taximode.or.ubermode.or.airpass) then
      inquire(file=fzden,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9023)  fzden
 9023 FORMAT(//' RCTL 9023 (F) FZDEN FILE=',A90,' NOT FOUND'//)
      STOP 9023
      else
      open(99,file=fzden,status='old',form='formatted')
      read(99,9) header
    7 read(99,*,err=87,end=86) t,aream2,zpop,zemp
      zone=equiv(t)
      zden(zone,1)=aream2/1000000
      zden(zone,2)=zpop
      zden(zone,3)=zemp
      if(aream2.gt.0) then
      zden(zone,4)=zpop/zden(zone,1)/1000
      zden(zone,5)=zemp/zden(zone,1)/1000
      else
      zden(zone,4)=0
      zden(zone,5)=0
      end if
      zden(zone,6)=aream2*0.000247105
      go to 7
   87 write(26,85) t
   85 format(/' MLOGIT 87 (F) ERROR READING ZONE DENSITY FILE',
     *        ' NEAR ZONE=',i5)
      stop
   86 close(99,status='keep')
      end if
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
  110 FORMAT(/' I=',20I5/
     *       ' J=',20I5/)
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
            JCOUNT=JCOUNT+1
            IF(AIR) AEQUIV(JCOUNT)=J(JN)
          ENDIF
        ELSEIF(J(JN).LT.0) THEN
          NJ=NJ+1
          JEND=J(JN)*(-1)
          JBEG=J(JN-1)+1
          DO INF=JBEG,JEND
          JOI(INF)=.TRUE.
          JCOUNT=JCOUNT+1
          IF(AIR) AEQUIV(JCOUNT)=INF
        END DO
        ENDIF
      END DO
C
C     OPEN WORKING FILES FOR DEBUG FEATURE
C
      if(debug) then
      open(101,file='stafile.bin',status='unknown',form='binary')
      open(103,file='egrfile.bin',status='unknown',form='binary')
      end if
C
C     OPEN CALIBRATED CONSTANT FILE
C
      if(calib) then
      open(100,file=fconst,status='unknown',form='formatted')
      open(102,file=fgobus,status='unknown',
     *         form='formatted')
      open(129,file=fdistsum,status='unknown',
     *         form='formatted')
      open(137,file=fstasum,status='unknown',
     *        form='formatted')
      open(138,file=fstaacc,status='unknown',
     *        form='formatted')
      open(139,file=fbustxfr,status='unknown',
     *        form='formatted')
      open(150,file=frapdrat,status='unknown',
     *        form='formatted')
      open(154,file='..\outputs\avail_person.csv',status='unknown',
     *        form='formatted')
      open(158,file='..\outputs\nonmot.csv',status='unknown',
     *        form='formatted')
      open(214,file=ftaxisum,status='unknown',form='formatted')
      open(217,file=fegrwalk,status='unknown',form='formatted')
      write(217,3020)
 3020 format('Walk_Time,GO_Rail_Union,TTC_Subway_Union',
     *       ',GO_Rail_All,TTC_Subway_ALL')
      if(capres) then
      open(155,file='stacap.csv',status='unknown',
     *        form='formatted')
      write(155,9750) 
 9750 format(',,,,space,adjusted,prev,overall,remain,revised,shadow'/
     *       'station,station_name,mode,line,demand,spaces,used,',
     *       'v_c,v_c,constant,price')
      end if
      inquire(file=fctvinp,exist=exists)
      if(.not.exists) then
      write(*,9003)  fctvinp
      write(26,9003) fctvinp
 9003 FORMAT(//' RCTL 9003 (F) CALIBRATION TARGET VALUE FILE=',
     *         A40,' NOT FOUND'//)
      STOP 9003
      end if
      end if
C
C     OPEN ERROR FILE
C
      OPEN(41,FILE=FERRFILE,STATUS='UNKNOWN',FORM='FORMATTED')
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
      ST=5
      IF(CCR) ST=9
      DO IT=1,ST
      CALL MFNAME(MFURSS(IT),FILENAME)
      FURSS(IT)=FILENAME
      CALL MFNAME(MFCRSS(IT),FILENAME)
      FCRSS(IT)=FILENAME
      END DO
      CALL MFNAME(MFCRSS(ST+1),FILENAME)
      FCRSS(ST+1)=FILENAME
      IF(MFCRSS(11).GT.0) THEN
      CALL MFNAME(MFCRSS(11),FILENAME)
      FCRSS(11)=FILENAME
      END IF
      IF(MFCRSS(12).GT.0) THEN
      CALL MFNAME(MFCRSS(12),FILENAME)
      FCRSS(12)=FILENAME
      END IF
C
C     SET ZONE-TO-STATION & STATION-TO-ZONE FILE NAMES
C
      ST=11
      IF(CCR) ST=15
      DO IT=1,ST
      CALL MFNAME(MFZSTA(IT),FILENAME)
      FZSTA(IT)=FILENAME
      CALL MFNAME(SFZSTA(IT),FILENAME)
      SZSTA(IT)=FILENAME
      END DO
      DO IT=1,3
      CALL MFNAME(MFZSTAFR(IT),FILENAME)
      FZSTAFR(IT)=FILENAME
      CALL MFNAME(SFZSTAFR(IT),FILENAME)
      SZSTAFR(IT)=FILENAME
      END DO
C
C DIRECT WALK TO STATION
C
      CALL MFNAME(MFWLKSTA,FILENAME)
      FWLKSTA=FILENAME
      CALL MFNAME(SFWLKSTA,FILENAME)
      SWLKSTA=FILENAME
C
C  DRIVE ALONE SKIMS
C
      ST=6
      IF(HWYREL) ST=8
      DO IT=1,ST
      CALL MFNAME(MFHWY0(IT),FILENAME)
      FHWY0(IT)=FILENAME
      END DO
C
C  HOV 2-PERSON SKIMS
C
      ST=8
      IF(HWYREL) ST=10
      DO IT=1,ST
      CALL MFNAME(MFHWY2P(IT),FILENAME)
      FHWY2P(IT)=FILENAME
      END DO
C
C  HOV 3+ PERSON SKIMS
C
      IF(MFHWY3P(1).GT.0) THEN
      ST=8
      IF(HWYREL) ST=10
      HWY3P=.TRUE.
      DO IT=1,ST
      CALL MFNAME(MFHWY3P(IT),FILENAME)
      FHWY3P(IT)=FILENAME
      END DO
      ELSE
      HWY3P=.FALSE.
      WRITE(26,9401)
 9401 FORMAT(/' 3+ PERSON SKIMS NOT PROVIDED - SET TO 2 PERSON SKIMS'/)
      END IF
C----- AV HIGHWAY SKIMS -----------
      IF(AVMDL) THEN
C
C  DRIVE ALONE SKIMS
C
      ST=6
      IF(HWYREL) ST=8
      DO IT=1,ST
      CALL MFNAME(AVHWY0(IT),FILENAME)
      VHWY0(IT)=FILENAME
      END DO
C
C  HOV 2-PERSON SKIMS
C
      ST=8
      IF(HWYREL) ST=10
      DO IT=1,ST
      CALL MFNAME(AVHWY2P(IT),FILENAME)
      VHWY2P(IT)=FILENAME
      END DO
C
C  HOV 3+ PERSON SKIMS
C
      IF(AVHWY3P(1).GT.0) THEN
      ST=8
      IF(HWYREL) ST=10
      DO IT=1,ST
      CALL MFNAME(AVHWY3P(IT),FILENAME)
      VHWY3P(IT)=FILENAME
      END DO
      ELSE
       IF(HWY3P) THEN
       WRITE(26,9402)
       WRITE(*,9402)
 9402  FORMAT(/' RCTL 9402 (F) AV 3+ SKIM MATRICES NOT PROVIDED,',
     *         ' BUT 3+ PERSON SKIMS PROVIDED'/)
C      STOP 9402
       END IF
      END IF      
      END IF
C
C  WALK TO GO BUS SKIMS
C
      DO IT=1,11
      CALL MFNAME(MFWKGOBUS(IT),FILENAME)
      FWKGOBUS(IT)=FILENAME
      END DO
      CALL MFNAME(MFWKGOBUS(16),FILENAME)
      FWKGOBUS(16)=FILENAME
      IF(CCR) THEN
      DO IT=12,15
      CALL MFNAME(MFWKGOBUS(IT),FILENAME)
      FWKGOBUS(IT)=FILENAME
      END DO
      END IF
C
C  WALK TO BUS/STREETCAR SKIMS
C
      DO IT=1,8
      CALL MFNAME(MFWKBUSTR(IT),FILENAME)
      FWKBUSTR(IT)=FILENAME
      END DO
      IF(CCR) THEN
      DO IT=9,12
      CALL MFNAME(MFWKBUSTR(IT),FILENAME)
      FWKBUSTR(IT)=FILENAME
      END DO
      END IF
C
C  WALK TO RAPID BUS SKIMS
C
      DO IT=1,9
      CALL MFNAME(MFWKRAP(IT),FILENAME)
      FWKRAP(IT)=FILENAME
      END DO
      IF(CCR) THEN
      DO IT=10,13
      CALL MFNAME(MFWKRAP(IT),FILENAME)
      FWKRAP(IT)=FILENAME
      END DO
      END IF
C
C     INPUT PERSON TRIP MATRICES
C
      IF(.NOT.SPEVENT) THEN
      DO IT=1,6
      CALL MFNAME(MFTOTPER(IT),FILENAME)
      FTOTPER(IT)=FILENAME
      END DO
      END IF
C
C     OPEN WALK AND BICYLCE LEVEL OF SERVICE MATRICES
C
      IF(NMOT) THEN
      DO IT=1,4
      CALL MFNAME(MFNMOTOR(IT),FILENAME)
      FNMOTOR(IT)=FILENAME
      END DO
      END IF
C
C     COMPUTE FLOATING DENSITY FOR EACH ZONE
C     DETERMINE CATEGORY
C
      if(taximode.or.ubermode.or.airpass) then
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      do iz=1,max_izones
      read(85,end=300) iiz,frow
      temp=0.0
      tarea=0.0
      tpop=0.0
      do jz=1,max_izones
      if(frow(jz).lt.2.0) then
      temp=temp+zden(jz,3)
      tarea=tarea+zden(jz,1)*1000000
      tpop=tpop+zden(jz,2)
c     write(26,200) iequiv(iz),iequiv(jz),temp,tarea,tpop,zden(jz,3),
c    *               zden(jz,1),zden(jz,2),frow(jz)
  200 format(' iz=',i4,' jz=',i4,' temp=',f12.2,
     *      ' tarea=',f12.2,' tpop=',f12.2,
     *       ' zemp=',f12.2,' zarea=',f12.2,' zpop=',f12.2,
     *       ' dist=',f12.2)
      end if
      end do
      popempden=((tpop+2*temp)/tarea)*1000.0
      zhhd(6,iz)=popempden
      zhhd(7,iz)=6.0
      if(zhhd(6,iz).gt.5.0.and.zhhd(6,iz).lt.10.0) zhhd(7,iz)=5.0
      if(zhhd(6,iz).gt.10.0.and.zhhd(6,iz).lt.25.0) zhhd(7,iz)=4.0
      if(zhhd(6,iz).gt.25.0.and.zhhd(6,iz).lt.50.0) zhhd(7,iz)=3.0
      if(zhhd(6,iz).gt.50.0.and.zhhd(6,iz).lt.100.0) zhhd(7,iz)=2.0 
      if(zhhd(6,iz).gt.100.0) zhhd(7,iz)=1.0        
c     write(26,201) iequiv(iz),popempden,zhhd(7,iz)
  201 format(' iz=',i4,' popempden=',f12.2,' zhhd(7)=',f3.1)
      end do
  300 continue
      close(85,status='keep')
      end if
C
C     CALL TO OPEN INPUT FILES
C
      CALL FILEOPEN
C
C     COMPUTE PARKING COST VALUES FROM MODEL
C
      call parkcost
C
C     OPEN OUTPUT TRIP MATRICES
C
      IF(TRIPSOUT) THEN
C....STATION LEVEL TRIPS
      chkout=.false.
      DO IT=1,14
      CALL MFNAME(MFSTATRP(IT),FILENAME)
      FSTATRP(IT)=FILENAME
      END DO
      open(184,file=fstatrp(1),
     *       status='unknown',form='binary')
      open(44,file=fstatrp(2),
     *       status='unknown',form='binary')
      open(45,file=fstatrp(3),
     *       status='unknown',form='binary')
      open(46,file=fstatrp(4),
     *       status='unknown',form='binary')
      open(47,file=fstatrp(5),
     *       status='unknown',form='binary')
      open(48,file=fstatrp(6),
     *       status='unknown',form='binary')
      open(49,file=fstatrp(7),
     *       status='unknown',form='binary')
      open(50,file=fstatrp(8),
     *       status='unknown',form='binary')
      open(21,file=fstatrp(9),
     *       status='unknown',form='binary')
      open(171,file=fstatrp(10),
     *       status='unknown',form='binary')
      open(185,file=fstatrp(11),
     *       status='unknown',form='binary')
      open(186,file=fstatrp(12),
     *       status='unknown',form='binary')
      open(187,file=fstatrp(13),
     *       status='unknown',form='binary')
      open(188,file=fstatrp(14),
     *       status='unknown',form='binary')
      if(ubertrn) then
      CALL MFNAME(MFSTATRP(15),FILENAME)
      FSTATRP(15)=FILENAME      
      open(199,file=fstatrp(15),
     *       status='unknown',form='binary')     
      end if
      if(trnegr.and.ubertrn) then
      CALL MFNAME(MFSTATRP(16),FILENAME)
      FSTATRP(16)=FILENAME      
      open(200,file=fstatrp(16),
     *       status='unknown',form='binary')     
      end if
C...ZONE TO ZONE TRANSIT TRIPS
      DO IT=1,17
      CALL MFNAME(MFTRIPS(IT),FILENAME)
      FTRIPS(IT)=FILENAME
      END DO
      chkout=.true.
      open(51,file=ftrips(1),
     *       status='unknown',form='binary')
      open(52,file=ftrips(2),
     *       status='unknown',form='binary')
      open(53,file=ftrips(3),
     *       status='unknown',form='binary')
      open(54,file=ftrips(4),
     *       status='unknown',form='binary')
      open(55,file=ftrips(5),
     *       status='unknown',form='binary')
      open(56,file=ftrips(6),
     *       status='unknown',form='binary')
      open(57,file=ftrips(7),
     *       status='unknown',form='binary')
      open(58,file=ftrips(8),
     *       status='unknown',form='binary')
      open(59,file=ftrips(9),
     *       status='unknown',form='binary')
      open(60,file=ftrips(10),
     *       status='unknown',form='binary')
      open(124,file=ftrips(11),
     *       status='unknown',form='binary')
      open(125,file=ftrips(12),
     *       status='unknown',form='binary')
      open(126,file=ftrips(13),
     *       status='unknown',form='binary')
      open(127,file=ftrips(14),
     *       status='unknown',form='binary')
      open(61,file=ftrips(15),
     *       status='unknown',form='binary')
      if(mftrips(16).gt.0) then
      open(180,file=ftrips(16),
     *       status='unknown',form='binary')
      end if
      if(mftrips(17).gt.0) then
      open(181,file=ftrips(17),
     *       status='unknown',form='binary')
      end if
      END IF
      IF(VEHOUT) THEN
C...ZONE TO ZONE AUTO TRIPS
      chkout=.false.
      DO IT=1,10
      CALL MFNAME(MFATRIPS(IT),FILENAME)
      FATRIPS(IT)=FILENAME
      END DO
      chkout=.true.
      open(161,file=fatrips(1),
     *       status='unknown',form='binary')
      open(162,file=fatrips(2),
     *       status='unknown',form='binary')
      open(163,file=fatrips(3),
     *       status='unknown',form='binary')
      open(164,file=fatrips(4),
     *       status='unknown',form='binary')
      open(165,file=fatrips(5),
     *       status='unknown',form='binary')
      open(166,file=fatrips(6),
     *       status='unknown',form='binary')
      open(167,file=fatrips(7),
     *       status='unknown',form='binary')
      open(168,file=fatrips(8),
     *       status='unknown',form='binary')
      open(169,file=fatrips(9),
     *       status='unknown',form='binary')
      open(170,file=fatrips(10),
     *       status='unknown',form='binary')       
      IF(TAXIMODE) THEN
      chkout=.false.
      CALL MFNAME(MFATRIPS(11),FILENAME)
      FATRIPS(11)=FILENAME 
      open(206,file=fatrips(11),
     *       status='unknown',form='binary')
      chkout=.true.
      END IF  
      IF(UBERMODE) THEN
      chkout=.false.
      CALL MFNAME(MFATRIPS(12),FILENAME)
      FATRIPS(12)=FILENAME 
      open(207,file=fatrips(12),
     *       status='unknown',form='binary')
      chkout=.true.
      END IF
      IF(AVMDL) THEN
      chkout=.false.
      DO IT=13,22
      CALL MFNAME(MFATRIPS(IT),FILENAME)
      FATRIPS(IT)=FILENAME
      END DO
      chkout=.true.
      open(261,file=fatrips(13),
     *       status='unknown',form='binary')
      open(262,file=fatrips(14),
     *       status='unknown',form='binary')
      open(263,file=fatrips(15),
     *       status='unknown',form='binary')
      open(264,file=fatrips(16),
     *       status='unknown',form='binary')
      open(265,file=fatrips(17),
     *       status='unknown',form='binary')
      open(266,file=fatrips(18),
     *       status='unknown',form='binary')
      open(267,file=fatrips(19),
     *       status='unknown',form='binary')
      open(268,file=fatrips(20),
     *       status='unknown',form='binary')
      open(269,file=fatrips(21),
     *       status='unknown',form='binary')
      open(270,file=fatrips(22),
     *       status='unknown',form='binary')
      END IF
      IF(UBERMODE) THEN
      chkout=.false.
      CALL MFNAME(MFATRIPS(23),FILENAME)
      FATRIPS(23)=FILENAME 
      open(271,file=fatrips(23),
     *       status='unknown',form='binary')
      chkout=.true.
      END IF
      END IF
C
C     OPEN OUTPUT ALTERNATIVE AND RETURN HOME PARKING MATRICES
C
      IF(AVMDL.AND.ALTPARK) THEN
      chkout=.false.
      DO IT=1,6
      CALL MFNAME2(MFAPARK(IT),FILENAME)
      FAPARK(IT)=FILENAME
      END DO
      chkout=.true.
      if(initemx) then
      allocate(ematx(12250000)) 
      ematx=0.0
      open(272,file=fapark(1),
     *       status='unknown',form='binary')
      open(273,file=fapark(2),
     *       status='unknown',form='binary')
      open(274,file=fapark(3),
     *       status='unknown',form='binary')
      open(275,file=fapark(4),
     *       status='unknown',form='binary')
      open(276,file=fapark(5),
     *       status='unknown',form='binary')
      open(277,file=fapark(6),
     *       status='unknown',form='binary')
      do it=1,6
      write(271+it) (ematx(k),k=1,12250000)
      close((271+it),status='keep')
      end do
      deallocate(ematx,stat=alloc_val)
      end if     
      open(272,file=fapark(1),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(273,file=fapark(2),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(274,file=fapark(3),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(275,file=fapark(4),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(276,file=fapark(5),recl=4,access='direct',
     *       status='unknown',form='binary')
      open(277,file=fapark(6),recl=4,access='direct',
     *       status='unknown',form='binary')
      END IF
C...DESTINATION CHOICE LOGSUM VALUES
      chkout=.false.
      IF(LSBASE) THEN
      DO IT=1,6
      CALL MFNAME(MFLSUM(IT),FILENAME)
      FLSUM(IT)=FILENAME
      END DO
      chkout=.true.
      open(131,file=flsum(1),
     *       status='unknown',form='binary')
      if(ncats.gt.1) then
      open(132,file=flsum(2),
     *       status='unknown',form='binary')
      open(133,file=flsum(3),
     *       status='unknown',form='binary')
      open(134,file=flsum(4),
     *       status='unknown',form='binary')
      open(135,file=flsum(5),
     *       status='unknown',form='binary')
      open(136,file=flsum(6),
     *       status='unknown',form='binary')
      end if
      END IF
C...AIR PASSENGER TRIPS
      chkout=.false.
      IF(AIRPASS.AND.TRIPSOUT.AND.VEHOUT) THEN
      DO IT=1,6
      CALL MFNAME(MFAIR(IT),FILENAME)
      FAIR(IT)=FILENAME
      END DO
      chkout=.true.
      open(201,file=fair(1),
     *       status='unknown',form='binary')
      open(202,file=fair(2),
     *       status='unknown',form='binary')
      open(203,file=fair(3),
     *       status='unknown',form='binary')
      open(204,file=fair(4),
     *       status='unknown',form='binary')
      open(205,file=fair(5),
     *       status='unknown',form='binary')
      open(278,file=fair(6),
     *       status='unknown',form='binary')
      END IF
C...ZONE TO ZONE TRANSIT TRIPS FOR VSKIM ONLY
      chkout=.false.
      IF(VSKIM) THEN
      DO IT=1,25
      CALL MFNAME(MFVSKIM(IT),FILENAME)
      FVSKIMT(IT)=FILENAME
      END DO
      chkout=.true.
      do it=1,25
      open((it+300),file=fvskimt(it),
     *       status='unknown',form='binary')
      end do
      END IF
C
C     READ & STORE PEARSON PARKING LOT FILE
C
      IF(AIR) THEN
      INQUIRE (FILE=AIRPARK,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9351) AIRPARK
 9351 FORMAT(1X,'RCTL 9351 (F) PEARSON PARKING LOT INPUT FILE=',A40,
     *       ' DOES NOT EXIST')
      WRITE(*,9351) AIRPARK
      STOP 8
      ELSE
      OPEN(147,FILE=AIRPARK,
     *        STATUS='OLD',FORM='FORMATTED')
      END IF
      READ(147,*) HEADER,LCPI
      READ(147,*) HEADER,SHDROPP
      READ(147,*) HEADER,SHDROPE
      READ(147,*) HEADER
 9352 LINDEX=LINDEX+1
      IF(LINDEX.GT.50) THEN
      WRITE(26,9353)
      WRITE(*,9353) 
 9353 FORMAT(1X,'RCTL 9353 (F) NUMBER OF PEARSON PARKING LOTS EXCEED',
     *          ' MAXIMUM ALLOWABLE VALUE (50)')
      STOP 9353
      END IF
 9355 READ(147,*,END=9356) ZONE,PRKDATA(LINDEX,1),EGTYPE,
     *                   (PRKDATA(LINDEX,K),K=3,19)
      IF(ZONE.LE.0.OR.ZONE.GT.10000) THEN
      WRITE(26,9358) ZONE
      WRITE(*,9358)  ZONE
 9358 FORMAT(1X,'RCTL 9358 (F) ENCOUNTERED INVALID ZONE NUMBER=',I6,
     *          ' IN PARKING LOT INPUT FILE')
      STOP 9358
      END IF 
      PEQUIV(LINDEX)=EQUIV(ZONE)
      IF(EGTYPE.EQ.WALK) PRKDATA(LINDEX,2)=1.0
      IF(EGTYPE.EQ.SHUTTLE) PRKDATA(LINDEX,2)=2.0
      IF(EGTYPE.EQ.TRNONLY) PRKDATA(LINDEX,2)=3.0
      IF(EGTYPE.EQ.ITF) PRKDATA(LINDEX,2)=4.0
      IF(PRKDATA(LINDEX,2).LE.0.OR.PRKDATA(LINDEX,2).GT.4.0) THEN
      WRITE(26,9354) EGTYPE,ZONE
      WRITE(*,9354)  EGTYPE,ZONE
 9354 FORMAT(1X,'RCTL 9354 (F) INVALID EGRESS TYPE (',A2,') FOR',
     *          ' PARKING LOT ZONE=',I5) 
      STOP 9354
      END IF
      IF(PRKDATA(LINDEX,2).EQ.4.AND.(.NOT.ITFPRK)) THEN
      WRITE(26,9361) EGTYPE,ZONE
      WRITE(*,9361)  EGTYPE,ZONE
 9361 FORMAT(/1X,'RCTL 9361 (F) INVALID EGRESS TYPE (',A2,') FOR',
     *          ' PARKING LOT ZONE=',I4,' AND ITFPRK IS TRUE'/)
      STOP 9361
      END IF 
      PRKDATA(LINDEX,5)=(PRKDATA(LINDEX,5)*100.0*LCPI)/2.0
      PRKDATA(LINDEX,6)=(PRKDATA(LINDEX,6)*100.0*LCPI)/44.0
      PRKDATA(LINDEX,10)=PRKDATA(LINDEX,10)*100.0*LCPI
      PRKDATA(LINDEX,11)=PRKDATA(LINDEX,11)*100.0*LCPI
      GO TO 9352
 9356 LINDEX=LINDEX-1
      WRITE(26,9357) LINDEX,LCPI,SHDROPP,SHDROPE
 9357 FORMAT(//,1X,'PEARSON PARKING LOT INPUT DATA'/
     *          1X,'------------------------------'/
     *          1X,'NUMBER OF PARKING LOTS          =',I6/
     *          1X,'CPI CONVERSION FACTOR           =',F6.3/
     *          1X,'SHUTTLE DROP-OFF TIME - PUBLIC  =',F6.3/
     *          1X,'SHUTTLE DROP-OFF TIME - EMPLOYEE=',F6.3//
     *          1X,'                                                 ',
     *              '                                                ',
     *              '                  EMPL   PUB'/
     *          1X,'                                                 ',
     *              'PUB   EMPL         PUB   EMPLY   PUB    EMPLY  ',
     *              'INTRA   PUB  EMPL  TURN  TURN'/
     *          1X,'       USE  EGRESS PUBLIC  EMPLOY  PUBLIC EMPLY  ',
     *              'SHLT  SHLT  SHLT   SHLT   SHLT   SHLT    SHLT   ',
     *              'TERM    SEARCH    OVER  OVER'/
     *          1X,' ZONE  TYPE  TYPE  SPACES  SPACES   COST   COST  ',
     *              'WAIT  WAIT  WALK   COST   COST  FACTOR  FACTOR  ',
     *              'WALK  TIME  TIME RATIO RATIO'/
     *          1X,' ----  ----  ----  ------  ------  -----  -----  ',
     *             '----  ----  ----  -----  -----  ------  ------  ',
     *             '----  ----  ----  ----  ----')
      DO 9359 K=1,LINDEX
      WRITE(26,9360) IEQUIV(PEQUIV(K)),(PRKDATA(K,K1),K1=1,18)
 9360 FORMAT(1X,I5,2(2X,F4.0),2(2X,F6.0),2(2X,F5.0),3(2X,F4.0),
     *             2(2X,F5.0),2(2X,F6.2),3(2X,F4.0),2(2X,F4.1))
 9359 CONTINUE
      OPEN(159,FILE='AIRPARK.CSV',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(159,9362)
 9362 FORMAT('PARKING,TOTAL'/
     *       'LOT,VEHICLES,WALK,SHUTTLE,TRANSIT,SPACE,',
     *       'VC_RATIO,SHD_PRICE')
C
C...OBTAIN RENTAL FACILITY FILE DATA
C
      IF(AIRPASS) THEN
      INQUIRE(FILE=RENTAL,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9745) RENTAL
 9745 FORMAT(1X,'RCTL 9745 (F) RENTAL FACILITY INPUT FILE=',A40/
     *       ' DOES NOT EXIST')
      WRITE(*,9745) RENTAL
      STOP 9745
      ELSE
      OPEN(148,file=RENTAL,FORM='FORMATTED',STATUS='OLD')
      END IF
      LINDEX=0
      READ(148,*) HEADER,RVALUE(1)
      READ(148,*) HEADER,RVALUE(2)
      READ(148,*) HEADER,RVALUE(3)
      READ(148,*) HEADER,RVALUE(4)
      READ(148,*) HEADER
 9746 LINDEX=LINDEX+1
      READ(148,*,END=9747) (RNTLDATA(LINDEX,K),K=1,2),RENTNAME(LINDEX)
      RNTLDATA(LINDEX,3)=RVALUE(1)
      RNTLDATA(LINDEX,4)=RVALUE(2)*100.0
      RNTLDATA(LINDEX,5)=RVALUE(3)
      RNTLDATA(LINDEX,6)=RVALUE(4)
      GO TO 9746
 9747 LINDEX=LINDEX-1
      WRITE(26,9767) LINDEX
 9767 FORMAT(//,1X,'RENTAL FACILITY DATA'/
     *          1X,'--------------------'/
     *          1X,'NUMBER OF RENTAL FACILITIES     =',I6//
     *  1X,'                 DROP           SHUTTLE'/
     *  1X,'          MKT     OFF          WAIT   IVT'/
     *  1X,' TAZ     SHARE   TIME   COST   TIME   FCTR     NAME'/
     *  1X,'------  -------  -----  -----  -----  ----- ----------')
      DO NI=1,LINDEX
      WRITE(26,9768) (RNTLDATA(NI,K),K=1,6),RENTNAME(NI)
 9768 FORMAT(2X,F5.0,2X,F7.5,4(2X,F5.1),1X,A10)
      RVALUE(5)=RVALUE(5)+RNTLDATA(NI,2)
      END DO
      IF(RVALUE(5).LT.0.99.OR.RVALUE(5).GT.1.01) THEN
      WRITE(26,9769) RVALUE(5)
 9769 FORMAT(/,' MARKET SHARE PROPORTIONS (',F7.4,') DOES NOT'
     *         ' ADD TO 100%')
      STOP 9769
      END IF
      END IF
C...CONVERT CTA ZONES TO INTERNAL ZONE NUMBERS
      DO NI=1,5
      IF(CTAZNE(NI).GT.0) CTAZNE(NI)=EQUIV(CTAZNE(NI))
      END DO
      END IF
C...X-Y COORDINATE FILE
      IF(AIRPASS) THEN
      INQUIRE(FILE=XYCOORD,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
      WRITE(26,9748) XYCOORD
 9748 FORMAT(1X,'RCTL 9748 (F) X-Y COORDINATE FILE=',A40/
     *       ' DOES NOT EXIST')
      WRITE(*,9748) XYCOORD
      STOP 9748
      ELSE
      OPEN(149,file=XYCOORD,FORM='FORMATTED',STATUS='OLD')
      END IF
  600 READ(149,*,END=610,ERR=601) ZONE,XCORD,YCORD
      ZONE=EQUIV(ZONE)
      SXCOORD(ZONE)=XCORD
      SYCOORD(ZONE)=YCORD
      GO TO 600
  601 WRITE(26,9749) IEQUIV(ZONE)
 9749 FORMAT(' RCTL 9749 (F) ERROR READING X=Y COORDINATE FILE ',
     *       ' AFTER ZONE =',I5)
      STOP 9749
  610 CONTINUE
      END IF
C
C     READ & STORE POPSYN DATA
C
      IF(SPEVENT) THEN
      inquire(file=fpopsyn,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9139)  fpopsyn 
 9139 FORMAT(//' RCTL 9139 (F) POPSYN DATA FILE=',A90,' NOT FOUND'//)
      STOP 9139
      else
      open(174,file=fpopsyn,status='old',form='formatted')
      read(174,9) header
  301 read(174,*,err=302,end=303) zone,medinc,hhld,pop,emply
      index=equiv(zone)
      if(index.gt.0.and.index.lt.4000) then
      zhhd(8,index)=medinc
      zhhd(9,index)=hhld
      zhhd(10,index)=pop
      temply=0.0
      do k=1,30
      temply=temply+emply(k)
      end do
      zhhd(11,index)=temply
      else
      write(26,9142) ZONE
 9142 format(//' RCTL 9142 (W) INVALID ZONE NUMBER (',I5,
     *         ' ENCOUNTERED IN POPSYN DATA FILE'/)
      end if
      go to 301
  302 write(26,9141)
 9141 format(//' RCTL 9141 (F) ERROR READING POPSYN DATA',
     *         ' FILE'/)
      STOP 9141
  303 continue
      end if
      end if
C.....SPECIAL EVENT DATA
      if(spevent.or.visitor) then
      prkcst=0.0
      inquire(file=fspfile,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9143)  fspfile
 9143 FORMAT(//' RCTL 9143 (F) SP DATA FILE=',A90,' NOT FOUND'//)
      STOP 9143
      else
      maxsp=0
      open(175,file=fspfile,status='old',form='formatted')
      read(175,9) header
  305 read(175,*,err=306,end=307) num,select,code,sname,gattend,events,
     *                 attend,ecost,zone
      maxsp=maxsp+1
      if(maxsp.gt.50) then
      write(26,9149) 
      write(*,9149)
 9149 format(//' RCTL 9149 (F) SPECIAL EVENT FILE CONTAINS',
     *         ' MORE THAN MAXIMUM ALLOWABLE NUMBER OF EVENTS '/)
      stop 9149
      end if
      index=equiv(zone)
      if(index.gt.0.and.index.lt.6000) then
      spdata(maxsp,1)=attend
      spdata(maxsp,2)=ecost*100.0
      spname(maxsp)=sname
      spzone(maxsp)=zone
      prkcst(index)=ecost*100.0
      else
      write(26,9142) ZONE
 9144 format(//' RCTL 9144 (W) INVALID ZONE NUMBER (',I5,
     *         ' ENCOUNTERED IN SPECIAL EVENT DATA FILE'/)
      end if
      go to 305
  306 write(26,9145) zone
      write(*,9145) zone
 9145 format(//' RCTL 9145 (F) ERROR READING SP DATA',
     *         ' FILE AFTER ZONE ',I4/)
      STOP 9145
  307 continue
      end if
      WRITE(26,9147)
 9147 FORMAT(//
     *       '          SPECIAL EVENT INPUT DATA'/
     *      1X,65('-')/
     *       ' VENUE                                                 ',
     *       '   PARKING'/
     *       ' NUMBER   ZONE       VENUE NAME               ',
     *       '   ATTEND     COST'/
     *       ' ------  -----  ------------------------------',
     *       '  --------  --------')
      attend=0.0
      do k=1,maxsp
      WRITE(26,9146) k,spzone(k),spname(k),(spdata(k,k1),k1=1,2)
 9146 FORMAT(3X,I2,3X,I5,2X,A30,2X,F8.0,2X,F8.0)
      attend=attend+spdata(k,1)
      END DO
      write(26,9148) attend
 9148 format(/8x,'TOTAL',32x,f10.0)
      open(176,file='spevent_dist.csv',status='unknown',
     *         form='formatted')
      end if
C
C     RESET ATTRACTION ZONES FOR SPEVENT
C
      IF(SPEVENT.OR.VISITOR) THEN
      JOI=.FALSE.
      DO JN=1,50
      IF(SPZONE(JN).GT.0) THEN
      JBEG=EQUIV(SPZONE(JN))
      JOI(JBEG)=.TRUE.
      END IF
      END DO
      END IF
C
C     READ & STORE HOTEL DATA
C
      IF(VISITOR) THEN
      inquire(file=fhotel,exist=exists)
      IF(.not.exists) THEN
      WRITE(*,9169)  fhotel
 9169 FORMAT(//' RCTL 9169 (F) HOTEL DATA FILE=',A90,' NOT FOUND'//)
      STOP 9169
      else
      open(177,file=fhotel,status='old',form='formatted')
      read(177,9) header
  401 read(177,*,err=402,end=403) hotel,zone
      index=equiv(zone)
      if(index.gt.0.and.index.lt.4000) then
      zhhd(5,index)=hotel+zhhd(5,index)
      else
      write(26,9162) ZONE
 9162 format(//' RCTL 9162 (W) INVALID ZONE NUMBER (',I5,
     *         ' ENCOUNTERED IN HOTEL DATA FILE'/)
      end if
      go to 401
  402 write(26,9161)
 9161 format(//' RCTL 9161 (F) ERROR READING HOTEL DATA',
     *         ' FILE'/)
      STOP 9161
  403 continue
      end if
      end if
C
C     OPEN WATERLOO UTILITY VALUE FILE
C
      if(waterloo) then
      open(182,file=fwaterloo,status='unknown',
     *         form='formatted')
      end if
C
C     OPEN TRESO OUTPUT FILE
C
      if(treso) then
      open(208,file=ftresodat,status='unknown',
     *         form='binary')
      open(209,file=ftresosta,status='unknown',
     *         form='binary')
      open(210,file=ftresoegr,status='unknown',
     *         form='binary')
      if(tdebug) open(211,file='treso_debug.csv',status='unknown',
     *         form='formatted')
      end if
C
C     OPEN AUTO PERSON ATTRACTION FILE
C
      if(vehatr) then
      open(212,file=fvehattr,
     *         status='unknown',form='formatted')
      write(212,9070) 
 9070 format('zone,tdrv0n,tdrv0t,tdrv2nh,tdrv2nn,tdrv2th,tdrv2tn',
     *       ',tdrv3nh,tdrv3nn,tdrv3th,tdrv3tn,pertrp')
      end if
C
C     OPEN TAXI WAIT TIME DISTRIBUTION
C
      IF(TAXIMODE.AND.CALIB) THEN
      OPEN(215,FILE=FTAXIWAIT,STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      OPEN(216,FILE=FTAXITLF,STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      END IF
C
C     OPEN FTA USER BENEFIT FILE (IF REQUESTED)
C
      IF(USERBEN) THEN
       OPEN(178,FILE=FUSERBEN,STATUS='UNKNOWN',FORM='BINARY')
       NZONES=MAX_IZONES
       WRITE(178) NZONES,NCATS,COEFF(11),COEFF(11),TPURP,TDAY,ALTNAME
      END IF
C
C     CONVENTIONAL/AUTOMATED VEHICLE ZONAL LEVEL DATA
C
      if(avmdl) then
      open(218,file=favcvprop,status='old',form='formatted')
      read(218,9) header
  150 read(218,*,end=152,err=153) zone,avalue
      iz=equiv(zone)
      do k=1,24
      zhhd((k+16),iz)=avalue(k)
      end do
      go to 150
  153 write(26,154) zone
  154 format(' RCTL 153 (F) ERROR READING AV/CV PROPORTION FILE ',
     *       ' NEAR ZONE=',I4)
      STOP 153
  152 continue
      else
      do iz=1,max_zones
      zhhd(17,iz)=1.0
      zhhd(21,iz)=1.0
      zhhd(25,iz)=1.0
      zhhd(29,iz)=1.0
      zhhd(33,iz)=1.0
      zhhd(37,iz)=1.0
      end do
      end if
      if(treso) then
      do iz=1,max_zones
      do k=17,40
      zhhd(k,iz)=1.0
      end do
      end do
      end if
C
C     SDETAIL FILES
C
      IF(DEBUG.AND.SDETAIL) THEN
      OPEN(335,FILE='STA_TO_STA.RPT',STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      OPEN(336,FILE='STA_TO_ZONE.RPT',STATUS='UNKNOWN',
     *         FORM='FORMATTED')
      END IF
C
C     CHECK FOR ERRORS
C
      IF(NERR.GT.0) THEN
      WRITE(26,102) NERR
  102 FORMAT(/' RCTL 102 (F) ENCOUNTERED ',I2,' CONTROL FILE ERRORS')
C     STOP 102
      END IF
      OPEN(190,FILE=U190TMP,STATUS='UNKNOWN',FORM='FORMATTED')
      RETURN
      END
