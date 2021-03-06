C
C DECLARATIONS REQUIRED FOR PARAMETER VALUES
C
      INTEGER*4    MFCRSS(14),MFURSS(11),MFZSTA(15),MFWLKSTA
      INTEGER*4    MFWKGOBUS(16),MFTOTPER(6),SFZSTA(15),SFWLKSTA
      INTEGER*4    MFLSUM(6),MFSTATRP(14),MFTRIPS(17),MFATRIPS(12)
      INTEGER*4    MFZSTAFR(3),MFWKBUSTR(13),MFWKRAP(14),SFZSTAFR(3)
      INTEGER*4    MFHWY0(6),MFHWY2P(8),MFNMOTOR(4),PVALUE(6)
      INTEGER*4    NITER,NCATS,DEQUIV(10000),MXZONES,CITER
      INTEGER*4    PRELEN,SPDEQUIV(1000),MAXPD,MAXGLN,MAXTLN
      INTEGER*4    FINCH,MAXSP,SPZONE(50),ZNEREF(50,2)
      INTEGER*4    GOUNION,TTCUNION,BDMAIN,PEARSON
      INTEGER*4    WESTON,BLOOR
      INTEGER*4    MFAIR(6),MFVSKIM(25),MFLOT(3)
      REAL*4       COEFF(100),LSUM1TRN,LSUM2GB,LSUM1NM
      REAL*4       LSUM3CW,LSUM3CB,LSUM3CP,LSUM3CK
      REAL*4       LSUM3UW,LSUM3UB,LSUM3UP,LSUM3UK
      REAL*4       LSUM2CR,LSUM2UR,LSUM2SB,LSUM2RB
      REAL*4       LSUM1AUTO,LSUM2AUTO,LSUM3AUTO
      REAL*4       LSUM3P2,LSUM3P3,LSUM2SR,LSUM2DA,LSUM3DA
      REAL*4       KDTRN(6),KPNR(6),KBIKE(6),KNMOT(6)
      REAL*4       KWCR(6),KBCR(6),KPCR(6),KKCR(6)
      REAL*4       KWUR(6),KBUR(6),KPUR(6),KKUR(6)
      REAL*4       KCBD(5),KCR(6),KUR(6),KGBUS(6),KVIS(50)
      REAL*4       K2P(6),K3P(6),KDA(6),KSR(6),KAUT(6)
      REAL*4       KTRNT(25,6),KTRN(6),KRBUS(6),KLBUS(6)
      REAL*4       KDIST(30),KBUSTRW,KFINCHBUS,FINCHOBS
      REAL*4       KPUBPRK,KLOTTRN,KGLINE(20),KTLINE(20,20)
      REAL*4       KNMCBD(2,2),NMCBDOBS(2,2),GORUNOBS,UPXCNST
      REAL*4       KINFLSTR,KINFLRPD,SCHBUS,TVALUE(6),LVALUE(6)
      REAL*4       CBDTRP(5),ESTCBD(5),DSTTRP(30),GORLOBS(10)
      REAL*4       ADJFCT,INTRADA(6),INTRA2P(6),INTRA3P(6)
      REAL*4       INTRABIKE(6),INTRAWLK(6),INTRASCHBUS(6)
      REAL*4       PWALK(4000,2),PRKCST(4000),TTCLOBS(10,10)
      REAL*4       CCRPNRT,CCRPNRD,CDAR,KGOBUSD,WAITLT
      REAL*4       BFAREGR,DFAREGR,BFARETTC,KGOBUSW,NAIRFR,AIRFR
      REAL*4       BPFACE,BUSPNROCC,OPCOST,SWALK,LWALK
      REAL*4       HOV2P,HOV3P,YINTER,YSLOPE,DSCT2P,DSCT3P
      REAL*4       KTOLL,KTOLL2,KTOLL3,KHOV2,KHOV3,TTCDSCV
      REAL*4       MWALKT,MWALK1,MWALK2,MBIKET,MBIKE1,MBIKE2
      REAL*4       ACOEF(10),ACNST(13),RNTWAIT,AIROCC,AWCR,LCPI
      REAL*4       UNIONGR,UNIONTTC,KCRURTX,KCRWLK,TTCDSCF
      REAL*4       ZHHD(16,4000),SPDATA(50,2),ATTFCT,AIRFCTR
      REAL*4       UBCOEF(6,2),FDIST(1000,6),ZDEN(4000,7)
      REAL*4       DURDIST(1000),DURCOEF(2),MAXPCOST,TIMEFACT
      REAL*4       AIRSEG(3),KCURB(3),KPARK(3),TXCOEF(6,2)
      REAL*4       TXDIST(1000,6),TTCNST
      LOGICAL      DEBUG,SDETAIL,IOI(4000),JOI(4000),BOI(4000)
      LOGICAL      CALIB,LSBASE,TRIPSOUT,CCODE(31),PEAK
      LOGICAL      CAPRES,HSR,ZEROCAR,NDRVGOB,NMOT,VEHOUT
      LOGICAL      NDRVRAP,NDRVBUS,CCR,PTEST,TRNLOT,DAINDEX(6)
      LOGICAL      AIR,AIRTRN,CNVRGE,LDEBUG,AIRPASS,CONRAC
      LOGICAL      CHKOUT,NOTRANSIT,CSVRPT,EMPPRK,SPEVENT
      LOGICAL      VISITOR,SPACESUM,WATERLOO,TTCACC
      LOGICAL      USERBEN,VSKIM,UBERIN,SPRICE,AIRCALIB
      LOGICAL      UNION_ONLY,SDEBUG,AIRPNR,CHKPART4
      LOGICAL      NOBUSCR
      CHARACTER*2  TYPEEG(50)
      CHARACTER*35 DNAME(61),SPNAME(50)
      CHARACTER*200 FZINDEX,FRPT,FCTVINP,FDEQUIV,
     *             FSTAFILE,FSTAOUT,FDNAME,FSTASUM,
     *             FSTPKIN,FPREFIX,FPRKCST,FPWALK,
     *             FSTAACC,FCONST,FGOBUS,FDISTSUM,
     *             FBUSTXFR,AIRPARK,RENTAL,XYCOORD,
     *             FRAPDRAT,FERRFILE,FBUCKET,FRPTCSV,
     *             FPOPSYN,FSPFILE,FHOTEL,FWATERLOO,
     *             FUSERBEN,U190TMP,FAIRCALIB,
     *             FVSKIM,FZDEN,FAIRPARK,FSPEVENT,
     *             FAIRDIST
      CHARACTER*200 FLSUM(6),FCRSS(14),FAIR(6),
     *             FURSS(11),FZSTA(15),FWLKSTA,FLOT(3),
     *             FDRV0(2),FWKGOBUS(16),FTOTPER(6),
     *             FSTATRP(14),FTRIPS(17),FATRIPS(12),
     *             FZSTAFR(3),FWKBUSTR(13),FWKRAP(14),
     *             FHWY0(6),FHWY2P(8),FNMOTOR(4),
     *             SZSTA(15),SWLKSTA,SZSTAFR(3),FVSKIMT(25)
C
C DEFINE COMMON
C
      COMMON/PARAM/COEFF,DEBUG,SDETAIL,IOI,JOI,CALIB,PEAK,NMOT,VEHOUT,
     *             NDRVGOB,NDRVRAP,NDRVBUS,CCR,PTEST,AIR,EMPPRK,TTCACC,
     *             AIRTRN,CNVRGE,LDEBUG,AIRPASS,TRNLOT,CONRAC,CSVRPT,
     *             BOI,UNION_ONLY,CHKPART4,NOBUSCR,
     *             CAPRES,HSR,ZEROCAR,SPEVENT,VISITOR,SPACESUM,USERBEN,
     *             BPFACE,BUSPNROCC,PRELEN,FINCH,GOUNION,TTCUNION,
     *             WESTON,BLOOR,
     *             SPZONE,ZNEREF,BDMAIN,PEARSON,TTCDSCV,TTCDSCF,
     *             LSUM1TRN,LSUM2GB,LSUM2CR,LSUM2UR,MXZONES,PVALUE,
     *             LSUM3CW,LSUM3CB,LSUM3CP,LSUM3CK,ZHHD,SPDATA,
     *             LSUM3UW,LSUM3UB,LSUM3UP,LSUM3UK,LSUM1NM,
     *             LSUM2SB,LSUM2RB,LSUM1AUTO,LSUM2AUTO,LSUM3AUTO,
     *             LSUM3P2,LSUM3P3,LSUM2SR,LSUM2DA,LSUM3DA,
     *             KDTRN,KPNR,KWCR,KBCR,KPCR,KKCR,PRKCST,KVIS,
     *             SWALK,LWALK,KDA,KSR,KTRN,KAUT,KTRNT,KRBUS,KLBUS,
     *             KWUR,KBUR,KPUR,KKUR,KCBD,KCR,KUR,K2P,K3P,KBIKE,
     *             KINFLSTR,KINFLRPD,SCHBUS,TVALUE,KPUBPRK,KLOTTRN,
     *             KGBUS,MFCRSS,MFURSS,NITER,CITER,LVALUE,KDIST,
     *             ADJFCT,MFZSTA,NCATS,MFWLKSTA,MFLSUM,KBUSTRW,
     *             MFWKGOBUS,MFTRNPER,MFSTATRP,MFNMOTOR,DAINDEX,
     *             MFWKBUSTR,MFWKRAP,PWALK,MFHWY0,MFHWY2P,MFVSKIM,
     *             SFZSTA,SFZSTAFR,SFWLKSTA,ATTFCT,AIRFCTR,
     *             OPCOST,CCRPNRT,CCRPNRD,CDAR,MFTRIPS,MFATRIPS,
     *             KNMCBD,NMCBDOBS,MFTOTPER,UNIONGR,UNIONTTC,
     *             KGOBUSD,DEQUIV,SPDEQUIV,WAITLT,LSBASE,TRIPSOUT,
     *             CCODE,CBDTRP,ESTCBD,MFZSTAGR,MFZSTAFR,
     *             DSTTRP,KGLINE,GORLOBS,KTLINE,TTCLOBS,KCRWLK,
     *             BFAREGR,DFAREGR,BFARETTC,KGOBUSW,KNMOT,VSKIM,
     *             NAIRFR,AIRFR,UPXCNST,MFAIR,UBERIN,SPRICE,MFLOT,
     *             HOV2P,HOV3P,YINTER,YSLOPE,DSCT2P,DSCT3P,TTCNST,
     *             KTOLL,KTOLL2,KTOLL3,KHOV2,KHOV3,KCRURTX
     *             INTRADA,INTRA2P,INTRA3P,MAXGLN,MAXTLN,MAXSP,
     *             INTRABIKE,INTRAWLK,INTRASCHBUS,MAXPD,GORUNOBS,
     *             MWALKT,MWALK1,MWALK2,MBIKET,MBIKE1,MBIKE2,
     *             KFINCHBUS,FINCHOBS,CHKOUT,NOTRANSIT,WATERLOO
      COMMON/APARM/ACOEF,ACNST,RNTWAIT,AIROCC,AWCR,LCPI,
     *             UBCOEF,FDIST,ZDEN,TYPEEG,DURDIST,DURCOEF,
     *             MAXPCOST,TIMEFACT,AIRSEG,KCURB,KPARK,
     *             TXCOEF,TXDIST,AIRCALIB,SDEBUG,AIRPNR
      COMMON/FILES/FZINDEX,FTRNOUT,FRPT,FCTVINP,FSTAOUT,
     *             FSTPKIN,FSTAFILE,FCRSS,FPRKCST,FSTASUM,
     *             FURSS,FZSTA,FWLKSTA,FDRV0,FPWALK,FSTAACC,
     *             FWKGOBUS,FTRNPER,FDEQUIV,FPREFIX,FATRIPS,
     *             FLSUM,FSTATRP,FTRIPS,FZSTAGR,FZSTAFR,FAIR,
     *             FWKBUSTR,FWKRAP,FHWY0,FHWY2P,FDNAME,DNAME,
     *             FCONST,FGOBUS,FDISTSUM,FBUSTXFR,FNMOTOR,
     *             AIRPARK,RENTAL,XYCOORD,FRAPDRAT,FTOTPER,
     *             SZSTA,SWLKSTA,SZSTAFR,FERRFILE,FBUCKET,
     *             FRPTCSV,FPOPSYN,FSPFILE,SPNAME,FHOTEL,
     *             FWATERLOO,FUSERBEN,U190TMP,FAIRCALIB,FVSKIM,
     *             FVSKIMT,FZDEN,FAIRPARK,FSPEVENT,FLOT,FAIRDIST


