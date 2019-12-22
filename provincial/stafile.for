C-------------------------------------------------------------------
C        READ & STORE STATION DATA SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE STAFILE
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
C
C DATA DECLARATIONS
C
      character*37  header,namesta,sta1,sta2
      character*1   stype,parktype
      integer*2     useflag,sc,sused(max_stations),utype,xsta
      integer*4     pzone,rzone1,rzone2,minsta,nmnsta,index
      real*4        awalk,parksize,dpcost,opkcost,prwalk,kwalk,xc,yc
      real*4        pivt,kivt,fpark,ppark,payprop,shadprice
      logical       exists
      data          pivt/0.0/,kivt/0.0/
c
      WRITE(26,9006)
 9006 FORMAT(/20X,'STATION ATTRIBUTE INPUT FILE SUMMARY'/
     *        20X,'------------------------------------'//
     *    52X,'     STA        FREE   PAID   DLY  OFPK',
     * '  PNR  PNR  KNR  KNR   STATION'/
     *       ' PSUEDO',2X,16X,'STATION',20X,       
     * '     PLAT  PRK   PRK   PRK    PRK   PRK',
     * ' WALK  IVT WALK  IVT EQUIVALENCE'/
     *       '  ZONE ',2X,16X,'NAME',18X,'MODE',1X,
     * 'LINE TIME TYPE  CAPC  CAPC    COST  COST',
     * ' TIME TIME TIME TIME   1    2'/
     *       ' ------',2X,37('-'),1X,'----',1X,    
     * '---- ---- ---- ------ ------ ----- -----',
     * ' ---- ---- ---- ---- ---- ----')

c
c     read station attribute file
c
      read(42,1) header
    1 format(a37)
  100 read(42,*,end=200,err=150) pzone,namesta,useflag,stype,utype,
     *         awalk,parktype,fpark,ppark,dpcost,opkcost,prwalk,kwalk,
     *         rzone1,rzone2,xc,yc,xsta
      sc=equiv(pzone)-max_izones
      parksize=fpark+ppark
      payprop=1.0
      if(parksize.gt.0) payprop=ppark/parksize
      if(sc.gt.0) then
      if(sused(sc).gt.0) then
      write(26,9008) pzone
 9008 format(' STAFILE 9008 (W) STATION ZONE NUMBER (',i4,
     *       ') HAS ALREADY BEEN USED  RECORD SKIPPED')
      go to 100
      end if
      end if
      if(parktype.eq.'0') parktype=' '
      if(sc.le.0) then
      write(26,9001) pzone
 9001 format(' stafile 9001 (w) Station Zone Number (',I4,
     *       ') is out of range',
     *       ' error in Station Parking File')
      go to 100
      end if
      if(pzone.eq.max_zones) then
      write(26,9002) pzone,max_zones
 9002 format(1x,'stafile 9002 (F)--- The Last Station Zone ',I4,
     *          ' Must be Less Than',
     *          ' The Maximum Number Of Zones ',I4)
      stop
      end if
      staname(sc)=namesta
      sused(sc)=1
      write(26,9007) iequiv(sc+max_izones),staname(sc),
     *               stype,utype,awalk,parktype,fpark,ppark,
     *               dpcost,opkcost,prwalk,pivt,kwalk,kivt,
     *               rzone1,rzone2
 9007 FORMAT(2X,I4,3X,A37,3X,A1,3X,I2,1X,1X,F4.2,
     *       3X,A1,2X,F6.0,1X,F6.0,F6.2,F6.2,4F5.2,5(1X,I4),
     *       7(2X,F2.0,2X),5(1X,F5.1))
	    IF(RZONE1.EQ.0) THEN
	    RZONE1=MAX_ZONES
	    ELSE
	    RZONE1=EQUIV(RZONE1)
	    END IF
	    IF(RZONE2.EQ.0) THEN
	    RZONE2=MAX_ZONES
	    ELSE
	    RZONE2=EQUIV(RZONE2)
	    END IF
	    IF(RZONE1.LT.MAX_IZONES) THEN
	    WRITE(26,9003) RZONE1,PZONE,MAX_IZONES
 9003 FORMAT(/1X,'STAFILE 9003 (W) EQUIVALENT STATION VALUE=',I4,
     *            ' FOR STATION=',I4,
     *            ' IS LESS THAN THE NUMBER OF INTERNAL ZONES (',
     *            I4,') VALUE RESET')
      RZONE1=MAX_ZONES
      END IF
      IF(RZONE2.LT.MAX_IZONES) THEN
	    WRITE(26,9003) RZONE2,PZONE,MAX_IZONES
      RZONE2=MAX_ZONES
      END IF
      MINSTA=MIN(EQUIV(PZONE),RZONE1,RZONE2)
      NMNSTA=MINSTA-MAX_IZONES
      IF(STYPE.EQ.'M') STADATA(SC,2)=2.0
      IF(STYPE.EQ.'R') STADATA(SC,2)=1.0
      IF(STYPE.EQ.'B') STADATA(SC,2)=3.0
      IF(STYPE.EQ.'M'.AND.USEFLAG.EQ.1) STANUM(SC)=2
      IF(STYPE.EQ.'R'.AND.USEFLAG.EQ.1) STANUM(SC)=1
      IF(STYPE.EQ.'B'.AND.USEFLAG.EQ.1) STANUM(SC)=3
      EQUIV3(SC,1)=RZONE1
      EQUIV3(SC,2)=RZONE2
      SEQUIV(SC)=NMNSTA
      STADATA(SC,1)=DPCOST*PAYPROP
      STADATA(SC,4)=PARKSIZE
      STADATA(SC,3)=PARKSIZE
      STADATA(SC,6)=USEFLAG
      STADATA(SC,8)=UTYPE
      STADATA(SC,9)=AWALK
      STADATA(SC,10)=opkcost*PAYPROP
      STADATA(SC,11)=prwalk
      STADATA(SC,12)=PIVT
      STADATA(SC,13)=KWALK
      STADATA(SC,14)=KIVT
      STADATA(SC,15)=FPARK
      STADATA(SC,16)=PPARK
      STADATA(SC,18)=XSTA
      STADATA(SC,7)=3
      IF(PARKTYPE.EQ.'P') STADATA(SC,7)=1
      IF(PARKTYPE.EQ.'K') STADATA(SC,7)=2
      IF(STADATA(SC,7).EQ.2.AND.PARKSIZE.GT.0) THEN
      WRITE(26,9004) PZONE,NAMESTA
 9004 FORMAT(1X,'STAFILE 9004 (W) STATION (',I4,') ',A25,
     *       ' IS DEFINED AS KNR, BUT CONTAINS SPACES?')
      END IF  
      IF(STADATA(SC,7).EQ.1.AND.PARKSIZE.LE.0) THEN
      WRITE(26,9005) PZONE,NAMESTA
 9005 FORMAT(1X,'STAFILE 9005 (W) STATION (',I4,') ',A25,
     *       ' IS DEFINED AS PNR, BUT CONTAINS NO SPACES?')
      END IF
      IF(STANUM(SC).EQ.1.AND.UTYPE.GT.MAXGLN) MAXGLN=UTYPE
      IF(STANUM(SC).EQ.2.AND.UTYPE.GT.MAXTLN) MAXTLN=UTYPE
      IF(STANUM(SC).EQ.1.AND.(UTYPE.LE.0.OR.UTYPE.GT.20)) THEN
      WRITE(*,9101) PZONE,STANAME(SC),UTYPE
      WRITE(26,9101) PZONE,STANAME(SC),UTYPE
 9101 FORMAT(/' STAFILE 9101 (F) ENCOUNTERED ILLEGAL LINE',
     *       ' NUMBER FOR STATION=',I4,1X,A37,' LINE NUMMBER=',I2/)
      STOP 9101
      END IF
      IF(STANUM(SC).EQ.2.AND.(UTYPE.LE.0.OR.UTYPE.GT.20)) THEN
      WRITE(*,9102) PZONE,STANAME(SC),UTYPE
      WRITE(26,9102) PZONE,STANAME(SC),UTYPE
 9102 FORMAT(/' STAFILE 9102 (F) ENCOUNTERED ILLEGAL LINE',
     *       ' NUMBER FOR STATION=',I4,1X,A37,' LINE NUMMBER=',I2/)
      STOP 9102
      END IF
      go to 100
  150 write(26,151) pzone
  151 format(' stafile (f) error reading station file for',
     *       ' pseudo zone=',i4)
      stop 
  200 close(43,status='keep')
C
C  READ IN FILE CONTAINING PARKING UTILIZATION FROM PREVIOUS
C  RUNS AND SUBTRACT OUT USED SPACES FROM PARKING CAPACITY
C  IF PARKING RESTRAINT IN EFFECT
C
C     IF(CAPRES) THEN
      INQUIRE (FILE=FSTPKIN,EXIST=EXISTS)
      IF(EXISTS) THEN
      OPEN(9,FILE=FSTPKIN,FORM='FORMATTED',STATUS='OLD')
      READ(9,1) HEADER
      WRITE(26,9011)
 9011 FORMAT(//)
  250 READ(9,*,END=300,ERR=301) PZONE,PARKSIZE,SHADPRICE
      SC=EQUIV(PZONE)-MAX_IZONES
      STADATA(SC,3)=STADATA(SC,4) - PARKSIZE
      STADATA(SC,17)=PARKSIZE
      STADATA(SC,5)=SHADPRICE
      if(STADATA(SC,3).LT.0.0) THEN
      write(26,9010) PZONE,PARKSIZE,STADATA(SC,4)
 9010 FORMAT(1X,'STAFILE 9010 (W) STATION ',I4,' HAS ',F5.0,
     *       ' USED SPACES WHICH ',
     *       'EXCEEDS THE STATION CAPACITY OF ',F5.0)
      STADATA(SC,3)=0.0
      STADATA(SC,17)=STADATA(SC,4)
      end if
      GO TO 250
  301 WRITE(26,9009) 
 9009 FORMAT(' STAFILE 9009 (F) ERROR IN READING STATION',
     *       ' PARKING INPUT FILE (FSTPKIN)')
      STOP
  300 CONTINUE
      END IF
C     END IF
      CLOSE(9,STATUS='KEEP')
C
C     CREATE URBAN RAIL - COMMUTER RAIL CORRESPONDENCE
C
      INDEX=0
      DO SC=1,MAX_STATIONS
      IF(STANUM(SC).EQ.0) CYCLE
      IF(STANUM(SC).EQ.2.AND.STANUM(EQUIV3(SC,1)-MAX_IZONES).EQ.1) THEN
      INDEX=INDEX+1
      ZNEREF(INDEX,1)=EQUIV3(SC,1)
      ZNEREF(INDEX,2)=SC+MAX_IZONES
      END IF
      IF(STANUM(SC).EQ.2.AND.STANUM(EQUIV3(SC,2)-MAX_IZONES).EQ.1) THEN
      INDEX=INDEX+1
      ZNEREF(INDEX,1)=EQUIV3(SC,2)
      ZNEREF(INDEX,2)=SC+MAX_IZONES
      END IF
      END DO
C ------------------------------------------------------------
      IF(TTCACC) THEN
      WRITE(26,9013) 
 9013 FORMAT(/10X,' TTC SUBWAY - GO RAIL STATION CORRESPONDENCE'/
     *   10X,' ==========================================='//
     *       '  <--------- GO RAIL ------------>   ',
     *       '<-------- TTC SUBWAY ------>'/
     *       '  NO STATION           NAME          ',
     *       'STATION         NAME'/
     *       '  -- -------   --------------------  ',
     *       '-------  -------------------')
      DO SC=1,50
      IF(ZNEREF(SC,1).LE.0) CYCLE
      STA1=STANAME(ZNEREF(SC,1)-MAX_IZONES)
      STA2=STANAME(ZNEREF(SC,2)-MAX_IZONES)
      WRITE(26,9012) SC,IEQUIV(ZNEREF(SC,1)),STA1,
     *               IEQUIV(ZNEREF(SC,2)),STA2
 9012 FORMAT(2X,I2,2X,I4,5X,A20,4X,I4,3X,A20)
      END DO
      END IF
C -----------------------------------------------------------------
      return
      end
