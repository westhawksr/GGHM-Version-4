      PROGRAM ILOGIT
C      
C      
C---------------------------------------------------------------------
C            INCREMENTAL LOGIT MODE CHOICE MODEL APPLICATION PROGRAM                                                          
C            WITH STATION CHOICE FOR TTC & GO TRAIN
C
C            PARSONS BRINCKERHOFF,INC.
C            TORONTO, ONTARIO
C               CANADA
C
C
C---------------------------------------------------------------------
      include 'ilogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
      integer*2         time(4),iz,jz,ista,ix,itemp,itemp2
      integer*2         unit,orgzn,destzn,type,imode,iter
      integer*2         staznei(max_stations,max_izones,3,4)
      integer*2         zonesta(max_izones)
      integer*2         bsta(2,2),btxfer(2,2),busmode(2,3,2)
      integer*2         wsta(2,5),wdsta(2,5),bdsta(2,2)
      integer*2         zindur(max_stations),zindcr(max_stations)
      integer*2         osta(2,12),asta(2,12),tsta,tdsta,dsta(2,10)
      integer*2         psta(2,10),pdsta(2,10),ksta(2,10),kdsta(2,10)
      integer*2         cstat,asta2(2,12),prosta(2,12),prasta(2,12)
      integer*2         ist,sc,sc2,c,t1,t2,jst,maxiz,maxjz
      integer*4         recno,nfilerr,recl,fileno,ich
      integer*4         index,zone,diz,djz,ci,sdiz,sdjz
      integer*4         orista,dessta,date(3)
	    real*4            frow(max_izones,max_izones)
	    real*4            srow(max_stations,max_stations)
	    real*4            prow(max_izones,max_stations)
	    real*4            arow(max_stations,max_izones)
      real*4            stasta(5,max_stations,max_stations)
      real*4            stazne(6,max_stations,max_izones)
      real*4            wdist(2,5),bdist(2,2),twalk(2,2),skim(2,13)
      real*4            ewalk(2,2)
      real*4            hdist(max_stations),htime(max_stations)
      real*4            wait1(max_izones),wait2(max_izones)
      real*4            invehl(max_izones),invehg(max_izones)
      real*4            invehs(max_izones),inveh(max_izones)
      real*4            transf(max_izones),wlkacc(max_izones)
      real*4            wlkegr(max_izones)
      real*4            walktfr(max_izones),fare(max_izones)
      real*4            trntrp0(max_izones),trntrp1(max_izones)
      real*4            trntrp2(max_izones),tperin(3),tperson
      real*4            wutil(2,5),tutil,tdist,butil(2,2)
      real*4            pnrrat,pnrrat2,putil(2,10),hwydst(max_izones)
      real*4            kutil(2,10),wait1a,wait1b
      real*4            wlkbacc,wlkbegr,wgutl,dbutl,walkdt
      real*4            tcrw,tcrb,tcrp,tcrk,tcrw1,tcrw2,tcrb1,tcrb2
      real*4            tcrp1,tcrp2,tcrp3,tcrp4,tcrk1,tcrk2,tcrk3,tcrk4
      real*4            turw,turb,turp,turk,turw1,turw2,turb1,turb2
      real*4            turp1,turp2,turp3,turp4,turk1,turk2,turk3,turk4
      real*4            tgb,tgbw,tgbd,wlkm2,util(50),eutil(50)
      real*4            cwprob(3),cbprob(3),cpprob(5),ckprob(5)
      real*4            lscwlk,lscbus,lscpr,lsckr,lscr,lsum2rl
      real*4            uwprob(3),ubprob(3),upprob(5),ukprob(5)
      real*4            lsuwlk,lsubus,lsupr,lsukr,lsurb
      real*4            gobprob(3),lsgob,trnprob(3),lstrn(3),tshare(3)
      real*4            lsbas(3),peduh(3),pedut(3),tprob(3)
      real*4            tfac,dut,edut,mfval,cfare,outtrp(11),cumval
      real*4            crss(max_stations,max_stations)
      real*4            urss(max_stations,max_stations)
      real*4            bcr(max_izones,max_stations)
      real*4            bur(max_izones,max_stations)
      real*4            crstaz(max_stations,max_izones)
      real*4            urstaz(max_stations,max_izones)
      real*4            gbstaz(max_stations,max_izones)
      real*4            dtran(max_izones,max_stations)
      real*4            kkcbd(3),ntxfer
      real*8            denom,tesum(20,4),stasum(1000,12)
      real*8            stasum2(max_stations,7),maxvalue
      real*8            dtrips(31,31,3),gortlf(151),totgor
      real*8            gordar(51),pnrgor,gorail(21,21),txfers(2,6)
      logical           csort,converg
      character*13      name(3)
      character*25      ttcname(5),gorname(10)
      character*80      header
      data              coeff/-0.16650,-0.41292,-0.35298,-0.35798,
     *                    -1.44400,
     *                    -0.00883,-0.35964,0.00023,-0.41292,-0.14153,
     *                    -0.02500,-0.06250,-0.02500,0,-0.06250,
     *                    -0.06250,-0.06250,-0.09375,82*0.0/
      data              staname/1000*'No Station'/
      data              name/'GO Rail      ',
     *                       'TTC Subway   ',
     *                       'GO Bus       '/
      data              ttcname/'Yonge-University-Spadina',
     *                          'Bloor-Danforth',
     *                          'Scarborough-RT','Sheppard','Not Used'/
      data              gorname/'Lakeshore West','Milton','Kitchener',
     *                          'Not Used','Barrie','Richmond Hill',
     *                          'Stouffville','Not Used',
     *                          'Lakeshore East',
     *                          'Union Station'/
      maxvalue=0.0
      iter=0
      dtrips=0.0
      gortlf=0.0
      totgor=0.0
      pnrgor=0.0
      gorail=0.0
      stasum=0.0
c
c     initiate program and obtain input file information
c
      call rctl
c
c     read & compute district level shares
c
      call dshare
c
c     read station file
c
      call stafile
c -------------------------------------------------------------
c     create station-to-station utility values
c -------------------------------------------------------------
      imode=1
      call station(stasta,imode)
      imode=2
      call station(stasta,imode)
c ---------------------------------------------------------------
c     create station-to-zone utility values
c ---------------------------------------------------------------
      do imode=1,3
      call egress(stazne,imode,zonesta,staznei)
      end do
c ===============================================================
c     origin zone loop
c ===============================================================
 1000 continue
      dtrips=0.0
      gortlf=0.0
      estcbd=0.0
      gordar=0.0
      gorail=0.0
      crss=0.0
      urss=0.0
      bcr=0.0
      bur=0.0
      dtran=0.0
      crstaz=0.0
      urstaz=0.0
      gbstaz=0.0
      do iz=1,max_izones
      nk=IZ
      nk=mod(nk,100)
      if(nk.EQ.0) WRITE(*,8001) iz,iequiv(iz)
 8001 FORMAT(' Processing Origin Zone=',I5,' (',i5,')')
      if(.not.ioi(iz)) cycle
c ----------------------------------------------------
c     walk and bus access to GO Rail & TTC Subway
c ----------------------------------------------------     
      do imode=1,2
      call stabus(iz,bsta,bdist,twalk,btxfer,imode,skim,busmode,
     *                   wdist,wsta,ewalk)
      end do
c -----------------------------------------------------------
c     top ten drive access stations for GO Rail & TTC Subway
c -----------------------------------------------------------
      type=3
      orgzn=iz
      destzn=max_zones
      unit=20
      call mfread(unit,type,orgzn,destzn,frow,srow,hdist,arow)
      unit=21
      call mfread(unit,type,orgzn,destzn,frow,srow,htime,arow)     
      imode=1
      call stadrv(iz,zindcr,imode,hdist)
      imode=2
      call stadrv(iz,zindur,imode,hdist)
c -----------------------------------------------------------
c     obtain walk access GO Bus skim matrices
c -----------------------------------------------------------
      type=1
      orgzn=iz
C.....1ST WAIT TIME
      unit=27
      call mfread(unit,type,orgzn,destzn,wait1,srow,prow,arow)
C.....TOTAL WAIT TIME
      unit=28
      call mfread(unit,type,orgzn,destzn,wait2,srow,prow,arow)
C.....NUMBER OF BOARDINGS
      unit=29
      call mfread(unit,type,orgzn,destzn,transf,srow,prow,arow)
C.....FARE
      unit=25
      call mfread(unit,type,orgzn,destzn,fare,srow,prow,arow)
C...LOCAL BUS IN-VEHICLE TIME
      unit=23
      call mfread(unit,type,orgzn,destzn,invehl,srow,prow,arow)
C...GO BUS IN-VEHICLE TIME
      unit=22
      call mfread(unit,type,orgzn,destzn,invehg,srow,prow,arow)
C...STREETCAR IN-VEHICLE TIME
      unit=24
      call mfread(unit,type,orgzn,destzn,invehs,srow,prow,arow)
C....CENTROID WALK ACCESS TIME 
      unit=33
      call mfread(unit,type,orgzn,destzn,wlkacc,srow,prow,arow)
C....CENTROID WALK EGRESS TIME 
      unit=30
      call mfread(unit,type,orgzn,destzn,wlkegr,srow,prow,arow)
C...WALK TIME TRANSFER
      unit=31
      call mfread(unit,type,orgzn,destzn,walktfr,srow,prow,arow)
C---------------------------------------------------------------
C    INPUT TRANSIT TRIPS BY AUTO OWNERSHIP LEVEL
C---------------------------------------------------------------
C...ZERO CAR HOUSEHOLDS
      unit=34
      call mfread(unit,type,orgzn,destzn,trntrp0,srow,prow,arow)
C....ONE CAR HOUSEHOLDS 
      unit=35
      call mfread(unit,type,orgzn,destzn,trntrp1,srow,prow,arow)
C...2+ CAR HOUSEHOLDS
      unit=36
      call mfread(unit,type,orgzn,destzn,trntrp2,srow,prow,arow)
c ===============================================================
c     destination zone loop
c ===============================================================
      do jz=1,max_izones
      if(iz.eq.jz) cycle
	    if(.not.joi(jz)) cycle
	    outtrp=0.0
C
C    STORE INPUT TRANSIT TRIPS
C
      tperin(1)=TRNTRP0(JZ)
      tperin(2)=TRNTRP1(JZ)
      tperin(3)=TRNTRP2(JZ)
      tperson=tperin(1)+tperin(2)+tperin(3)
C....................................................................
      IF(DEBUG) THEN
      tperin(1)=100.0
      tperin(2)=100.0
      tperin(3)=100.0
      tperson=tperin(1)+tperin(2)+tperin(3)
      WRITE(26,9021) tperson,tperin(1),tperin(2),tperin(3)
 9021 FORMAT(/1X,'MARKET SEGMENTATION COMPUTATIONS'/
     *       1X,'----------------------------------'/
     *       1X,'PERSON TRIPS    TOTAL=',F10.2/
     *       1X,'PERSON TRIPS ZERO CAR=',F10.2/
     *       1X,'PERSON TRIPS ONE  CAR=',F10.2/
     *       1X,'PERSON TRIPS TWO+ CAR=',F10.2/)
      END IF
C....................................................................
      if(tperson.le.0.0) cycle
C---------------------------------------------------------------
C....WALK ACCESS STATION UTILITY COMPUTATION -- 	TTC Subway
c---------------------------------------------------------------
      do ista=1,5
	    imode=2
	    call egrsta(jz,wsta(imode,ista),stasta,stazne,
     *   wdsta(imode,ista),imode,zonesta)
	    CALL WUTL(ista,IZ,JZ,WSTA(IMODE,ista),WDSTA(imode,ista),
     *   WUTIL(imode,ista),STASTA,STAZNE,IMODE)
      end do
C...SORT & RETAIN BEST 2 ORIGIN STATIONS
   33 CSORT=.FALSE.
      GO TO 34
      DO 32 ISTA=2,5
      IF(WUTIL(IMODE,ISTA).EQ.0.0) GO TO 32
      IF(WUTIL(IMODE,ISTA).GT.WUTIL(IMODE,(ISTA-1))) THEN
      TSTA=WSTA(IMODE,ISTA)
      TDSTA=WDSTA(IMODE,ISTA)
      TUTIL=WUTIL(IMODE,ISTA)
      TDIST=WDIST(IMODE,ISTA)
      WSTA(IMODE,ISTA)=WSTA(IMODE,(ISTA-1))
      WDSTA(IMODE,ISTA)=WDSTA(IMODE,(ISTA-1))
      WUTIL(IMODE,ISTA)=WUTIL(IMODE,(ISTA-1))
      WDIST(IMODE,ISTA)=WDIST(IMODE,(ISTA-1))
      WSTA(IMODE,(ISTA-1))=TSTA
      WDSTA(IMODE,(ISTA-1))=TDSTA
      WUTIL(IMODE,(ISTA-1))=TUTIL
      WDIST(IMODE,(ISTA-1))=TDIST
      CSORT=.TRUE.
      END IF
   32 CONTINUE
      IF(CSORT) GO TO 33
   34 CONTINUE
      OSTA(IMODE,1)=WSTA(IMODE,1)
      OSTA(IMODE,2)=WSTA(IMODE,2)
      ASTA(IMODE,1)=WDSTA(IMODE,1)
      ASTA(IMODE,2)=WDSTA(IMODE,2)  
c----------------------------------------------------------
C....WALK ACCESS STATION UTILITY COMPUTATION -- 	GO Rail
c----------------------------------------------------------
      do ista=1,2
	    imode=1
	    call egrsta(jz,wsta(imode,ista),stasta,stazne,
     *   wdsta(imode,ista),imode,zonesta)
	    CALL WUTL(ista,IZ,JZ,WSTA(IMODE,ista),WDSTA(imode,ista),
     *   WUTIL(imode,ista),STASTA,STAZNE,IMODE)
      OSTA(imode,ista)=WSTA(imode,ista)
	    ASTA(imode,ista)=WDSTA(imode,ista)
	    end do
C-------------------------------------------------
C....BUS ACCESS STATION UTILITY COMPUTATION
c-------------------------------------------------
       do ista=1,2
       IMODE=1
       IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
   41  FORMAT(/' BUS ACCESS #',I1,' ---> ',a13/
     *         ' =================================')
	    CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,ZONESTA)
      CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE)
       OSTA(IMODE,(ISTA+2))=BSTA(IMODE,ISTA)
       ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
	    IMODE=2
	    IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
	    CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,ZONESTA)
      CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE)
       OSTA(IMODE,(ISTA+2))=BSTA(IMODE,ISTA)
       ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
      end do
c---------------------------------------------------
C....DRIVE ACCESS STATION UTILITY COMPUTATION
c---------------------------------------------------
      type=1
      orgzn=iz
C.....HIGHWAY DISTANCE TO DESTINATION ZONE
      unit=20
      call mfread(unit,type,orgzn,destzn,hwydst,srow,prow,arow)
      DO IX=1,10
C..COMMUTER RAIL
      IMODE=1
      IF(DEBUG) WRITE(26,341) IX,NAME(IMODE)
  341  FORMAT(/' DRIVE ACCESS #',I2,' ---> ',a13/
     *         ' =================================')
	    ITEMP=ZINDCR(IX)+MAX_IZONES
      CALL EGRSTA(JZ,ITEMP,STASTA,STAZNE,ITEMP2,
     *          IMODE,ZONESTA)
      DSTA(IMODE,IX)=ITEMP2
      CALL PUTL(IX,IZ,JZ,ZINDCR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,HTIME,HDIST,
     *  IMODE,PNRRAT,PNRRAT2,hwydst)
c     CRPNR(IX)=PNRRAT
c     CRPNR2(IX)=PNRRAT2
      CALL KUTL(IX,IZ,JZ,ZINDCR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,HTIME,HDIST,
     *  IMODE)
C...URBAN RAIL
      IMODE=2
      IF(DEBUG) WRITE(26,341) IX,NAME(IMODE)
      ITEMP=ZINDUR(IX)+MAX_IZONES
      CALL EGRSTA(JZ,ITEMP,STASTA,STAZNE,ITEMP2,
     *          IMODE,ZONESTA)
      DSTA(IMODE,IX)=ITEMP2
      CALL PUTL(IX,IZ,JZ,ZINDUR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,HTIME,HDIST,
     *  IMODE,PNRRAT,PNRRAT2,hwydst)
      CALL KUTL(IX,IZ,JZ,ZINDUR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,HTIME,HDIST,
     *  IMODE)
      end do
C.....SORT DRIVE ACCESS STATIONS - GO RAIL
      IMODE=1
      CALL USORT(1,PSTA,ZINDCR,DSTA,PDSTA,PUTIL,IMODE)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      CALL USORT(2,KSTA,ZINDCR,DSTA,KDSTA,KUTIL,IMODE)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
C.....SORT DRIVE ACCESS STATIONS - TTC SUBWAY
      IMODE=2
      CALL USORT(1,PSTA,ZINDUR,DSTA,PDSTA,PUTIL,IMODE)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      CALL USORT(2,KSTA,ZINDUR,DSTA,KDSTA,KUTIL,IMODE)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
C-------------------------------------------------------------------
C            WALK --> GO BUS UTILITY COMPUTATION                   |
C-------------------------------------------------------------------
      WAIT1A=0.0
      WAIT1B=0.0
      INVEH(JZ)=INVEHL(JZ)+INVEHG(JZ)+INVEHS(JZ)
      WLKBACC=WLKACC(JZ)
      WLKBEGR=WLKEGR(JZ)
      FARE(JZ)=FARE(JZ)*100.0
      IF(INVEHG(JZ).GT.0.0) THEN
      WAIT1A=AMIN1(WAIT1(JZ),WAITLT)
      WAIT1B=DIM(WAIT1(JZ),WAITLT)
      NTXFER=TRANSF(JZ)-1.0
      IF(NTXFER.GT.0.0) NTXFER=NTXFER+COEFF(22)
      WGUTL=COEFF(11)*INVEH(JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*(WAIT2(JZ)-WAIT1(JZ)) +
     *        COEFF(20)*(NTXFER**COEFF(21)) +
     *        COEFF(17)*WALKTFR(JZ) + KGOBUSW
      WGUTL=WGUTL/(LSUM1TRN*LSUM2GB)
      ELSE
      WGUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9043) INVEH(JZ),INVEHG(JZ),INVEHL(JZ),INVEHS(JZ),
     *               FARE(JZ),TRANSF(JZ),WAIT1(JZ),
     *               WAIT1A,WAIT1B,WAIT2(JZ),WALKTFR(JZ),
     *               WLKBACC,WLKBEGR,WGUTL
 9043 FORMAT(//1X,'Walk --> GO Bus Level-Of-Service Data'/
     *       1X,  '-------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'GO BUS IN-VEHICLE    =',F8.2/
     *       1X,'LOCAL BUS IN-VEHICLE =',F8.2/
     *       1X,'STREETCAR IN-VEHICLE =',F8.2/
     *       1X,'BUS   FARE           =',F8.2/
     *       1X,'BUS   BOARDINGS      =',F8.2/
     *       1X,'BUS   1ST WAIT       =',F8.2/
     *       1X,'BUS   1ST WAIT (<5)  =',F8.2/
     *       1X,'BUS   1ST WAIT (>5)  =',F8.2/
     *       1X,'BUS   TOTAL WAIT     =',F8.2/
     *       1X,'BUS   TRANSFER WALK  =',F8.2/
     *       1X,'BUS   ACCESS   WALK  =',F8.2/
     *       1X,'BUS   EGRESS   WALK  =',F8.2/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C.................................................................
C---------------------------------------------------------------------
C            DRIVE --> GO BUS UTILITY COMPUTATION                    |
C---------------------------------------------------------------------
      CSTAT=MAX_ZONES
      IMODE=3
	    CALL DRVNEW(JZ,DBUTL,HDIST,HTIME,CSTAT,IMODE,
     *            STAZNE,HWYDST)
	    IT=CSTAT-MAX_IZONES
	    WALKDT=STAZNE(3,IT,JZ)
      NTXFER=STAZNE(1,IT,JZ)-1.0
      IF(NTXFER.GT.0.0) NTXFER=NTXFER+COEFF(22)
 	    IF(DBUTL.NE.0.0) THEN
      DBUTL=DBUTL+COEFF(23)*(NTXFER**COEFF(21))
 	    DBUTL=(DBUTL+KGOBUSD)/(LSUM1TRN*LSUM2GB)
 	    END IF
C
C----------------------------------------------------------------------
C ADJUST PARK&RIDE UTILITY VALUES TO INCLUDE SHADOW PRICE
C----------------------------------------------------------------------
C
      IF(CAPRES) THEN
C..................................................................
      IF(DEBUG) THEN
      WRITE(26,9002)
 9002 FORMAT(/1X,'REVISED PARK&RIDE UTILITY VALUES'/
     *        1X,'--------------------------------------'//)
      END IF
C..................................................................
      DO 15,IMODE=1,2
      DO 15,T=1,4
      KT=T+4
      ORISTA=OSTA(IMODE,KT)-MAX_IZONES
      IF(ASTA(IMODE,KT).GT.0.AND.PUTIL(IMODE,T).NE.0.0) THEN
      PUTIL(IMODE,T)=PUTIL(IMODE,T)+COEFF(6)*
     *               STADATA((OSTA(IMODE,KT)-MAX_IZONES),5)
C....................................................................
      IF(DEBUG) THEN
      IC=OSTA(IMODE,KT)-MAX_IZONES
      WRITE(26,9001) IEQUIV(OSTA(IMODE,KT)),STANAME(IC),
     *               STADATA((OSTA(IMODE,KT)-MAX_IZONES),5),
     *               PUTIL(IMODE,T)
 9001 FORMAT(1X,'P&R STATION=',I4,5X,A37,' SHADOW PRICE=',F5.0,
     *          ' REVISED UTILITY=',F10.3)
      END IF
C.....................................................................
      END IF
   15 CONTINUE
C
C  ADJUST UTILITIES FOR DRIVE TO GO BUS
C
      IF(CSTAT.GT.0.AND.DBUTL.NE.0.0) THEN
      DBUTL=DBUTL + COEFF(6)*STADATA((CSTAT-MAX_IZONES),5)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9001) IEQUIV(CSTAT),STANAME(CSTAT-MAX_IZONES),
     *               STADATA((CSTAT-MAX_IZONES),5),DBUTL
      END IF
C.....................................................................  
      END IF
	    END IF
C
C  CHECK STATION NUMBER VALUES
C
      DO 18 K=1,12
      DO 18 L=1,2
      IF(OSTA(L,K).LE.0) OSTA(L,K)=MXZONES
      IF(ASTA(L,K).LE.0) ASTA(L,K)=MXZONES
      DC=ASTA(L,K)-MAX_IZONES
      IC=STAIND(DC,JZ)
      IF(IC.LT.1) IC=1
      ASTA2(L,K)=IC+(L-1)*4
      PROSTA(L,K)=IEQUIV(OSTA(L,K))
      PRASTA(L,K)=IEQUIV(ASTA(L,K))
   18 CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9000) IEQUIV(IZ),IEQUIV(JZ),WUTIL(1,1),WUTIL(1,2),
     *               BUTIL(1,1),BUTIL(1,2),(PUTIL(1,K1),K1=1,4),
     *               (KUTIL(1,K2),K2=1,4),WUTIL(2,1),WUTIL(2,2),
     *               BUTIL(2,1),BUTIL(2,2),(PUTIL(2,K3),K3=1,4),
     *               (KUTIL(2,K4),K4=1,4),
     *               WGUTL,DBUTL
 9000 FORMAT(/1X,'SUMMARY OF LOWER LEVEL UTILITY VALUES'/
     *       1X,'---------------------------------------'/
     *       1X,'ORIGIN ZONE         =',I10/
     *       1X,'DESTINATION ZONE    =',I10//
     *       1X,'GO       RAIL  WALK-->STA #1 UTILITY=',F10.5/
     *       1X,'GO       RAIL  WALK-->STA #2 UTILITY=',F10.5/
     *       1X,'GO       RAIL  BUS -->STA #1 UTILITY=',F10.5/
     *       1X,'GO       RAIL  BUS -->STA #2 UTILITY=',F10.5/
     *       1X,'GO       RAIL  P&R -->STA #1 UTILITY=',F10.5/
     *       1X,'GO       RAIL  P&R -->STA #2 UTILITY=',F10.5/
     *       1X,'GO       RAIL  P&R -->STA #3 UTILITY=',F10.5/
     *       1X,'GO       RAIL  P&R -->STA #4 UTILITY=',F10.5/
     *       1X,'GO       RAIL  K&R -->STA #1 UTILITY=',F10.5/
     *       1X,'GO       RAIL  K&R -->STA #2 UTILITY=',F10.5/
     *       1X,'GO       RAIL  K&R -->STA #3 UTILITY=',F10.5/
     *       1X,'GO       RAIL  K&R -->STA #4 UTILITY=',F10.5//
     *       1X,'TTC      RAIL  WALK-->STA #1 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  WALK-->STA #2 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  BUS -->STA #1 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  BUS -->STA #2 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  P&R -->STA #1 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  P&R -->STA #2 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  P&R -->STA #3 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  P&R -->STA #4 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  K&R -->STA #1 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  K&R -->STA #2 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  K&R -->STA #3 UTILITY=',F10.5/
     *       1X,'TTC      RAIL  K&R -->STA #4 UTILITY=',F10.5//
     *       1X,'WALK     -> GO BUS           UTILITY=',F10.4/
     *       1X,'DRIVE    -> GO BUS           UTILITY=',F10.4/)
C
      WRITE(26,9003) (PROSTA(1,K),PRASTA(1,K),K=1,12)
 9003 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *       1X,'GO       RAIL  WALK-->STA #1 ',I4,'-->',I4/
     *       1X,'GO       RAIL  WALK-->STA #2 ',I4,'-->',I4/
     *       1X,'GO       RAIL  BUS -->STA #1 ',I4,'-->',I4/
     *       1X,'GO       RAIL  BUS -->STA #2 ',I4,'-->',I4/
     *       1X,'GO       RAIL  P&R -->STA #1 ',I4,'-->',I4/
     *       1X,'GO       RAIL  P&R -->STA #2 ',I4,'-->',I4/
     *       1X,'GO       RAIL  P&R -->STA #3 ',I4,'-->',I4/
     *       1X,'GO       RAIL  P&R -->STA #4 ',I4,'-->',I4/
     *       1X,'GO       RAIL  K&R -->STA #1 ',I4,'-->',I4/
     *       1X,'GO       RAIL  K&R -->STA #2 ',I4,'-->',I4/
     *       1X,'GO       RAIL  K&R -->STA #3 ',I4,'-->',I4/
     *       1X,'GO       RAIL  K&R -->STA #4 ',I4,'-->',I4/)
C
      WRITE(26,9004) (PROSTA(2,K),PRASTA(2,K),K=1,12)
 9004 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY'/
     *       1X,'--------------------------------------'/
     *       1X,'TTC   RAIL     WALK-->STA #1 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     WALK-->STA #2 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     BUS -->STA #1 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     BUS -->STA #2 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     P&R -->STA #1 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     P&R -->STA #2 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     P&R -->STA #3 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     P&R -->STA #4 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     K&R -->STA #1 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     K&R -->STA #2 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     K&R -->STA #3 ',I4,'-->',I4/
     *       1X,'TTC   RAIL     K&R -->STA #4 ',I4,'-->',I4/)
      END IF
C.....................................................................
C
C  MARKET SEGMENT LOOP
C
      DO 100 C=1,NCATS
      if(tperin(c).le.0.0) cycle
C
C  INITIALIZE TRIP VALUES
C
      TCRW=0.0
      TCRB=0.0
      TCRP=0.0
      TCRK=0.0
      TCRW1=0.0
      TCRW2=0.0
      TCRB1=0.0
      TCRB2=0.0
      TCRP1=0.0
      TCRP2=0.0
      TCRP3=0.0
      TCRP4=0.0
      TCRK1=0.0
      TCRK2=0.0
      TCRK3=0.0
      TCRK4=0.0
      TURW=0.0
      TURB=0.0
      TURP=0.0
      TURK=0.0
      TURW1=0.0
      TURW2=0.0
      TURB1=0.0
      TURB2=0.0
      TURP1=0.0
      TURP2=0.0
      TURP3=0.0
      TURP4=0.0
      TURK1=0.0
      TURK2=0.0
      TURK3=0.0
      TURK4=0.0
      TGBW=0.0
      TGBD=0.0
      DO K=1,50
      UTIL(K)=0.0
      EUTIL(K)=0.0
      END DO
C
C  COMPUTE STATION LEVEL UTILITIES FOR GO RAIL & TTC SUBWAY
C
      DO IMODE=1,2
      LSUM2RL=LSUM2CR
      IF(IMODE.EQ.2) LSUM2RL=LSUM2UR
C
      t1=1+(imode-1)*12
      t2=2+(imode-1)*12
      DO ICH=t1,t2
C..WALK STATION #1/2
      IST=ICH
      if(imode.eq.2) ist=ich-12
      IF(WUTIL(imode,IST).NE.0.0) THEN
      SC=OSTA(imode,IST)-MAX_IZONES
      SC2=ASTA(imode,IST)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      CFARE=STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)
      IF(IMODE.EQ.1) THEN
      CFARE=CFARE-(STAZNE(6,SC2,JZ)*BFAREGR*100.0)
      ELSE
      CFARE=CFARE-(STAZNE(5,SC2,JZ)*BFARETTC*100.0)     
      END IF
      WLKM1=(60.0*WDIST(imode,IST))/(3.0*1.60934)
      WLKM2=STAZNE(3,SC2,JZ)
      UTIL(ICH)=WUTIL(imode,IST) + COEFF(7)*(WLKM2 + WLKM1) +
     *   (COEFF(50+C)*CFARE)/(LSUM1TRN*LSUM2RL)
      IF(UTIL(ICH).LE.(-100.0)) THEN
      EUTIL(ICH)=0.0
      ELSE
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
      END IF
      end do
C
C..BUS STATION #1/2
      t1=3+(imode-1)*12
      t2=4+(imode-1)*12
      DO ICH=t1,t2
      IST=ICH-2
      jst=ich
      if(imode.eq.2) ist=ich-14
      if(imode.eq.2) jst=ich-12
      IF(BUTIL(imode,IST).NE.0.0) THEN
      SC=OSTA(imode,jst)-MAX_IZONES
      SC2=ASTA(imode,jst)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      CFARE=STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)+SKIM(IMODE,6)
      IF(IMODE.EQ.1) THEN
      CFARE=CFARE-(STAZNE(6,SC2,JZ)*BFAREGR*100.0)
     *           -(SKIM(1,13)*BFAREGR*100.0)
      ELSE
      CFARE=CFARE-(STAZNE(5,SC2,JZ)*BFARETTC*100.0)
     *           -(SKIM(2,12)*BFARETTC*100.0)    
      END IF
      WLKM1=TWALK(imode,IST)
      WLKM2=STAZNE(3,SC2,JZ)
      UTIL(ICH)=BUTIL(imode,IST) + COEFF(7)*(WLKM1 + WLKM2) +
     *   (COEFF(50+C)*CFARE)/(LSUM1TRN*LSUM2RL)
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
      END DO
C
C..P&R STATION #1-4
C
      t1=5+(imode-1)*12
      t2=8+(imode-1)*12
      DO ICH=t1,t2
      IST=ICH-4
      jst=ich
      if(imode.eq.2) ist=ich-16
      if(imode.eq.2) jst=ich-12
      IF(PUTIL(imode,IST).NE.0.0) THEN
      SC=OSTA(imode,jst)-MAX_IZONES
      SC2=ASTA(imode,jst)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      CFARE=STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)
      IF(IMODE.EQ.1) THEN
      CFARE=CFARE-(STAZNE(6,SC2,JZ)*BFAREGR*100.0)
      ELSE
      CFARE=CFARE-(STAZNE(5,SC2,JZ)*BFARETTC*100.0)     
      END IF
      WLKM2=STAZNE(3,SC2,JZ)
      UTIL(ICH)=PUTIL(imode,IST) + COEFF(7)*WLKM2+
     *   (COEFF(50+C)*CFARE)/(LSUM1TRN*LSUM2RL)+
     *          (KDTRN(C)+KPNR(C))/(LSUM1TRN*LSUM2RL)
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
      END DO
C
C..K&R STATION #1-4
      t1=9+(imode-1)*12
      t2=12+(imode-1)*12
      DO ICH=t1,t2
      IST=ICH-8
      jst=ich
      if(imode.eq.2) ist=ich-20
      if(imode.eq.2) jst=ich-12
      IF(KUTIL(imode,IST).NE.0.0) THEN
      SC=OSTA(imode,jst)-MAX_IZONES
      SC2=ASTA(imode,jst)-MAX_IZONES
      IF(SC.LT.0) SC=MAX_STATIONS
      IF(SC2.LT.0) SC2=MAX_STATIONS
      CFARE=STASTA(4,SC,SC2)+STAZNE(4,SC2,JZ)
      IF(IMODE.EQ.1) THEN
      CFARE=CFARE-(STAZNE(6,SC2,JZ)*BFAREGR*100.0)
      ELSE
      CFARE=CFARE-(STAZNE(5,SC2,JZ)*BFARETTC*100.0)     
      END IF
      WLKM2=STAZNE(3,SC2,JZ)
      UTIL(ICH)=KUTIL(imode,IST) + COEFF(7)*WLKM2 +
     *    KDTRN(C)/(LSUM1TRN*LSUM2RL)+
     *   (COEFF(50+C)*CFARE)/(LSUM1TRN*LSUM2RL)
      EUTIL(ICH)=EXP(UTIL(ICH))
      END IF
      END DO
C....................................................................
      IF(DEBUG) THEN
      IF(IMODE.EQ.1) THEN
      WRITE(26,9005) C,(UTIL(K),EUTIL(K),K=1,12)
 9005 FORMAT(/1X,'SUMMARY OF GO RAIL UTILITIES FOR MARKET SEGMENT=',I1/
     *       1X, '-------------------------------------------------'/
     *       1X,'                     ',5X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',2X,'----------',5X,
     *          '----------'/
     *       1X,'GO RAIL  WALK-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  WALK-->STA #2 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  BUS -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  BUS -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  P&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  P&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  P&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  P&R -->STA #4 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  K&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  K&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  K&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'GO RAIL  K&R -->STA #4 ',F10.5,3X,E12.5/) 
      ELSE
      WRITE(26,9015) C,(UTIL(K),EUTIL(K),K=13,24)
 9015 FORMAT(/1X,'SUMMARY OF TTC SUBWAY UTILITIES FOR MARKET SEGMENT=',
     *        I1/
     *       1X, '---------------------------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',5X,'----------',5X,
     *          '----------'/
     *       1X,'TTC SUBWAY  WALK-->STA #1 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  WALK-->STA #2 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  BUS -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  BUS -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  P&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  P&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  P&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  P&R -->STA #4 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  K&R -->STA #1 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  K&R -->STA #2 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  K&R -->STA #3 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY  K&R -->STA #4 ',F10.5,3X,E12.5/)       
      END IF
      END IF
C...................................................................
      END DO
C..WALK ACCESS TO GO BUS
      IF(WGUTL.NE.0.0) THEN
      UTIL(25)=WGUTL +  (COEFF(17)*(WLKACC(JZ) + WLKEGR(JZ)) +
     *         COEFF(50+C)*FARE(JZ))/(LSUM2GB*LSUM1TRN)
      EUTIL(25)=EXP(UTIL(25))
      END IF
C..DRIVE ACCESS TO GO BUS
      IF(DBUTL.NE.0.0) THEN
      IT=CSTAT-MAX_IZONES
      UTIL(26)=DBUTL + (COEFF(17)*STAZNE(3,IT,JZ) +
     *          COEFF(50+C)*STAZNE(4,IT,JZ))/(LSUM2GB*LSUM1TRN)
      EUTIL(26)=EXP(UTIL(26))
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9306) UTIL(25),EUTIL(25),UTIL(26),EUTIL(26)
 9306 FORMAT(/1X,'SUMMARY OF TRANSIT MODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',5X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'WALK  ACCESS TO GO BUS=',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO GO BUS=',F10.5,3X,E12.5/)
      END IF
C.....................................................................
C
C   CALCULATE STATION LEVEL PROBABILITIES
C
C..WALK ACCESS - GO RAIL
      CWPROB(1)=1.0
      CWPROB(2)=0.0
      LSCWLK=0.0
      DENOM=EUTIL(1)+EUTIL(2)
      IF(DENOM.GT.0.0) THEN
      LSCWLK=DLOG(DENOM)
      CWPROB(1)=EUTIL(1)/DENOM
      CWPROB(2)=1.0-CWPROB(1)
      END IF
C..BUS ACCESS - GO RAIL
      CBPROB(1)=1.0
      CBPROB(2)=0.0
      LSCBUS=0.0
      DENOM=EUTIL(3)+EUTIL(4)
      IF(DENOM.GT.0.0) THEN
      LSCBUS=DLOG(DENOM)
      CBPROB(1)=EUTIL(3)/DENOM
      CBPROB(2)=1.0-CBPROB(1)
      END IF
C..P&R ACCESS - GO RAIL
      CPPROB(1)=1.0
      CPPROB(2)=0.0
      CPPROB(3)=0.0
      CPPROB(4)=0.0
      LSCPR=0.0
      DENOM=EUTIL(5)+EUTIL(6)+EUTIL(7)+EUTIL(8)
      IF(C.EQ.1) DENOM=0.0
      IF(DENOM.GT.0.0) THEN
      LSCPR=DLOG(DENOM)
      CPPROB(1)=EUTIL(5)/DENOM
      CPPROB(2)=EUTIL(6)/DENOM
      CPPROB(3)=EUTIL(7)/DENOM
      CPPROB(4)=1.0-CPPROB(1)-CPPROB(2)-CPPROB(3)
      END IF
C..K&R ACCESS - GO RAIL
      CKPROB(1)=1.0
      CKPROB(2)=0.0
      CKPROB(3)=0.0
      CKPROB(4)=0.0
      LSCKR=0.0
      DENOM=EUTIL(9)+EUTIL(10)+EUTIL(11)+EUTIL(12)
      IF(DENOM.GT.0.0) THEN
      LSCKR=DLOG(DENOM)
      CKPROB(1)=EUTIL(9)/DENOM
      CKPROB(2)=EUTIL(10)/DENOM
      CKPROB(3)=EUTIL(11)/DENOM
      CKPROB(4)=1.0-CKPROB(1)-CKPROB(2)-CKPROB(3)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9008) CWPROB(1),CWPROB(2),CBPROB(1),CBPROB(2),
     *               CPPROB(1),CPPROB(2),CPPROB(3),CPPROB(4),
     *               CKPROB(1),CKPROB(2),CKPROB(3),CKPROB(4),
     *               LSCWLK,LSCBUS,LSCPR,LSCKR
 9008 FORMAT(/1X,'SUMMARY OF GO RAIL STATION CHOICE PROB: '/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS STATION #1=',F8.5/ 
     *       1X,'WALK  ACCESS STATION #2=',F8.5/
     *       1X,'BUS   ACCESS STATION #1=',F8.5/
     *       1X,'BUS   ACCESS STATION #2=',F8.5/
     *       1X,'P&R   ACCESS STATION #1=',F8.5/
     *       1X,'P&R   ACCESS STATION #2=',F8.5/
     *       1X,'P&R   ACCESS STATION #3=',F8.5/
     *       1X,'P&R   ACCESS STATION #4=',F8.5/
     *       1X,'K&R   ACCESS STATION #1=',F8.5/
     *       1X,'K&R   ACCESS STATION #2=',F8.5/
     *       1X,'K&R   ACCESS STATION #3=',F8.5/
     *       1X,'K&R   ACCESS STATION #4=',F8.5//
     *       1X,'WALK  ACCESS LOGSUM    =',F8.3/
     *       1X,'BUS   ACCESS LOGSUM    =',F8.3/
     *       1X,'P&R   ACCESS LOGSUM    =',F8.3/
     *       1X,'K&R   ACCESS LOGSUM    =',F8.3)
      END IF
C....................................................................
C
C  GO RAIL ACCESS UTILITIES & PROBABILITIES
C
C...WALK ACCESS
      IF(LSCWLK.NE.0.0) THEN
      UTIL(27)=LSUM3CW*LSCWLK +
     *         KWCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(27)=EXP(UTIL(27))
      END IF
C...BUS ACCESS
      IF(LSCBUS.NE.0.0) THEN
      UTIL(28)=LSUM3CB*LSCBUS  +
     *         KBCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(28)=EXP(UTIL(28))
      END IF
C...PARK&RIDE ACCESS
      IF(LSCPR.NE.0.0) THEN
      UTIL(29)=LSUM3CP*LSCPR   +
     *         KPCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(29)=EXP(UTIL(29))
      END IF
C...KISS&RIDE ACCESS
      IF(LSCKR.NE.0.0) THEN
      UTIL(30)=LSUM3CK*LSCKR   +
     *         KKCR(C)/(LSUM1TRN*LSUM2CR)
      EUTIL(30)=EXP(UTIL(30))
      END IF
C...PROBABILITIES
      CWPROB(3)=0.0
      CBPROB(3)=0.0
      CPPROB(5)=0.0
      CKPROB(5)=0.0
      LSCR=0.0
      DENOM=EUTIL(27)+EUTIL(28)+EUTIL(29)+EUTIL(30)
      IF(DENOM.GT.0.0) THEN
      CWPROB(3)=EUTIL(27)/DENOM
      CBPROB(3)=EUTIL(28)/DENOM
      CPPROB(5)=EUTIL(29)/DENOM
      CKPROB(5)=1.0-CWPROB(3)-CBPROB(3)-CPPROB(5)
      CKPROB(5)=AMAX1(CKPROB(5),0.0)
      LSCR=DLOG(DENOM)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9009) (UTIL(K),EUTIL(K),K=27,30)
 9009 FORMAT(/1X,'SUMMARY OF GO RAIL ACCESS UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'WALK ACCESS TO GO RAIL ',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO GO RAIL ',F10.5,3X,E12.5//
     *       1X,'P&R  ACCESS TO GO RAIL ',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO GO RAIL ',F10.5,3X,E12.5)
      WRITE(26,9010) CWPROB(3),CBPROB(3),CPPROB(5),CKPROB(5),LSCR
 9010 FORMAT(/1X,'SUMMARY OF GO RAIL ACCESS CHOICE PROB:'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'GO RAIL LOGSUM=',F10.5)
      END IF
C.......................................................................
C
C  TTC SUBWAY ACCESS UTILITIES & PROBABILITIES
C
C..WALK ACCESS - TTC SUBWAY
      UWPROB(1)=1.0
      UWPROB(2)=0.0
      LSUWLK=0.0
      DENOM=EUTIL(13)+EUTIL(14)
      IF(DENOM.GT.0.0) THEN
      LSUWLK=DLOG(DENOM)
      UWPROB(1)=EUTIL(13)/DENOM
      UWPROB(2)=1.0-UWPROB(1)
      END IF
C..BUS ACCESS - TTC SUBWAY
      UBPROB(1)=1.0
      UBPROB(2)=0.0
      LSUBUS=0.0
      DENOM=EUTIL(15)+EUTIL(16)
      IF(DENOM.GT.0.0) THEN
      LSUBUS=DLOG(DENOM)
      UBPROB(1)=EUTIL(15)/DENOM
      UBPROB(2)=1.0-UBPROB(1)
      END IF
C..P&R ACCESS - TTC SUBWAY
      UPPROB(1)=1.0
      UPPROB(2)=0.0
      UPPROB(3)=0.0
      UPPROB(4)=0.0
      LSUPR=0.0
      DENOM=EUTIL(17)+EUTIL(18)+EUTIL(19)+EUTIL(20)
      IF(C.EQ.1) DENOM=0.0
      IF(DENOM.GT.0.0) THEN
      LSUPR=DLOG(DENOM)
      UPPROB(1)=EUTIL(17)/DENOM
      UPPROB(2)=EUTIL(18)/DENOM
      UPPROB(3)=EUTIL(19)/DENOM
      UPPROB(4)=1.0-UPPROB(1)-UPPROB(2)-UPPROB(3)
      END IF
C..K&R ACCESS - TTC SUBWAY
      UKPROB(1)=1.0
      UKPROB(2)=0.0
      UKPROB(3)=0.0
      UKPROB(4)=0.0
      LSUKR=0.0
      DENOM=EUTIL(21)+EUTIL(22)+EUTIL(23)+EUTIL(24)
      IF(DENOM.GT.0.0) THEN
      LSUKR=DLOG(DENOM)
      UKPROB(1)=EUTIL(21)/DENOM
      UKPROB(2)=EUTIL(22)/DENOM
      UKPROB(3)=EUTIL(23)/DENOM
      UKPROB(4)=1.0-UKPROB(1)-UKPROB(2)-UKPROB(3)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9108) UWPROB(1),UWPROB(2),UBPROB(1),UBPROB(2),
     *               UPPROB(1),UPPROB(2),UPPROB(3),UPPROB(4),
     *               UKPROB(1),UKPROB(2),UKPROB(3),UKPROB(4),
     *               LSUWLK,LSUBUS,LSUPR,LSUKR
 9108 FORMAT(/1X,'SUMMARY OF TTC SUBWAY STATION CHOICE PROB: '/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS STATION #1=',F8.5/ 
     *       1X,'WALK  ACCESS STATION #2=',F8.5/
     *       1X,'BUS   ACCESS STATION #1=',F8.5/
     *       1X,'BUS   ACCESS STATION #2=',F8.5/
     *       1X,'P&R   ACCESS STATION #1=',F8.5/
     *       1X,'P&R   ACCESS STATION #2=',F8.5/
     *       1X,'P&R   ACCESS STATION #3=',F8.5/
     *       1X,'P&R   ACCESS STATION #4=',F8.5/
     *       1X,'K&R   ACCESS STATION #1=',F8.5/
     *       1X,'K&R   ACCESS STATION #2=',F8.5/
     *       1X,'K&R   ACCESS STATION #3=',F8.5/
     *       1X,'K&R   ACCESS STATION #4=',F8.5//
     *       1X,'WALK  ACCESS LOGSUM    =',F10.5/
     *       1X,'BUS   ACCESS LOGSUM    =',F10.5/
     *       1X,'P&R   ACCESS LOGSUM    =',F10.5/
     *       1X,'K&R   ACCESS LOGSUM    =',F10.5)
      END IF
C....................................................................
C
C  TTC SUBWAY ACCESS UTILITIES & PROBABILITIES
C
C...WALK ACCESS
      IF(LSUWLK.NE.0.0) THEN
      UTIL(31)=LSUM3UW*LSUWLK  +
     *         KWUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(31)=EXP(UTIL(31))
      END IF
C...BUS ACCESS
      IF(LSUBUS.NE.0.0) THEN
      UTIL(32)=LSUM3UB*LSUBUS  +
     *         KBUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(32)=EXP(UTIL(32))
      END IF
C...PARK&RIDE ACCESS
      IF(LSUPR.NE.0.0) THEN
      UTIL(33)=LSUM3UP*LSUPR   +
     *         KPUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(33)=EXP(UTIL(33))
      END IF
C...KISS&RIDE ACCESS
      IF(LSUKR.NE.0.0) THEN
      UTIL(34)=LSUM3UK*LSUKR   +
     *         KKUR(C)/(LSUM1TRN*LSUM2UR)
      EUTIL(34)=EXP(UTIL(34))
      END IF
C...PROBABILITIES
      UWPROB(3)=0.0
      UBPROB(3)=0.0
      UPPROB(5)=0.0
      UKPROB(5)=0.0
      LSURB=0.0
      DENOM=EUTIL(31)+EUTIL(32)+EUTIL(33)+EUTIL(34)
      IF(DENOM.GT.0.0) THEN
      UWPROB(3)=EUTIL(31)/DENOM
      UBPROB(3)=EUTIL(32)/DENOM
      UPPROB(5)=EUTIL(33)/DENOM
      UKPROB(5)=1.0-UWPROB(3)-UBPROB(3)-UPPROB(5)
      UKPROB(5)=AMAX1(UKPROB(5),0.0)
      LSURB=DLOG(DENOM)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9109) (UTIL(K),EUTIL(K),K=31,34)
 9109 FORMAT(/1X,'SUMMARY OF TTC SUBWAY ACCESS UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',5X,'----------',5X,
     *          '----------'/
     *       1X,'WALK ACCESS TO TTC SUBWAY ',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO TTC SUBWAY ',F10.5,3X,E12.5//
     *       1X,'P&R  ACCESS TO TTC SUBWAY ',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO TTC SUBWAY ',F10.5,3X,E12.5)
      WRITE(26,9110) UWPROB(3),UBPROB(3),UPPROB(5),UKPROB(5),
     *               LSURB
 9110 FORMAT(/1X,'SUMMARY OF TTC SUBWAY ACCESS CHOICE PROB:'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'TTC SUBWAY LOGSUM=',F10.5)
      END IF
C................................................................
C..GO BUS
      GOBPROB(1)=0.0
      GOBPROB(2)=0.0
      LSGOB=0.0
      DENOM=EUTIL(25)+EUTIL(26)
      IF(DENOM.GT.0.0) THEN
      LSGOB=DLOG(DENOM)
      GOBPROB(1)=EUTIL(25)/DENOM
      GOBPROB(2)=1.0-GOBPROB(1)
      END IF
C........................................................................
      IF(DEBUG) THEN
      WRITE(26,9011) GOBPROB(1),GOBPROB(2),LSGOB
 9011 FORMAT(/1X,'SUMMARY OF GO BUS CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------'/
     *       1X,'WALK  ACCESS --> GO BUS      =',F8.5/
     *       1X,'DRIVE ACCESS --> GO BUS      =',F8.5/
     *       1X,'GO BUS LOGSUM                =',F8.2/)
      END IF
C.......................................................................
C
C  CALCULATE PRIMARY TRANSIT MODE PROBABILITIES
C
      DIZ=IEQUIV(IZ)
      DJZ=IEQUIV(JZ)
      DIZ=DEQUIV(DIZ)
      DJZ=DEQUIV(DJZ)
      SDIZ=SDEQUIV(IEQUIV(IZ))
      SDJZ=SDEQUIV(IEQUIV(JZ))
      KKCBD=0.0
      IF(DJZ.EQ.1) THEN
      KKCBD(1)=KCBD(1)
      KKCBD(2)=KCBD(2)
      KKCBD(3)=KCBD(3)
      IF(SDIZ.LE.4) KKCBD(1)=0.0
      IF(SDIZ.GT.4) KKCBD(2)=0.0
      END IF
C..GO RAIL
      IF(LSCR.NE.0.0) THEN
      CALL DISTFUNCTION(HWYDST(JZ),MFVAL)
      UTIL(35)=LSUM2CR*LSCR + KCR(C)/(LSUM1TRN) + KKCBD(1)/LSUM1TRN
     *        + COEFF(19)*MFVAL
      EUTIL(35)=EXP(UTIL(35))
      END IF
C
C..TTC SUBWAY
      IF(LSURB.NE.0.0) THEN
      UTIL(36)=LSUM2UR*LSURB + KUR(C)/(LSUM1TRN)+ KKCBD(2)/LSUM1TRN
      EUTIL(36)=EXP(UTIL(36))
      END IF
C..GO BUS
      IF(LSGOB.NE.0.0) THEN
      UTIL(37)=LSUM2GB*LSGOB + KGBUS(C)/(LSUM1TRN)+ KKCBD(3)/LSUM1TRN
      EUTIL(37)=EXP(UTIL(37))
      END IF
C..TRANSIT PROBABILITIES
      TRNPROB(1)=0.0
      TRNPROB(2)=0.0
      TRNPROB(3)=0.0
      LSTRN(C)=0.0
      DENOM=EUTIL(35)+EUTIL(36)+EUTIL(37)
      IF(DENOM.GT.0.0) THEN
      LSTRN(C)=DLOG(DENOM)
      TRNPROB(1)=EUTIL(35)/DENOM
      TRNPROB(2)=EUTIL(36)/DENOM
      TRNPROB(3)=1.0-TRNPROB(1)-TRNPROB(2)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9013) C,(UTIL(K),EUTIL(K),K=35,37)
 9013 FORMAT(/1X,'SUMMARY OF PRIMARY TRANSIT UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,' MARKET SEGMENT=',I1/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'GO RAIL                 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY              ',F10.5,3X,E12.5/
     *       1X,'GO BUS                  ',F10.5,3X,E12.5)
      WRITE(26,9014) TRNPROB(1),TRNPROB(2),TRNPROB(3),LSTRN(C)
 9014 FORMAT(/1X,'SUMMARY OF TRANSIT CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'GO RAIL        =',F8.5/
     *       1X,'TTC SUBWAY     =',F8.5/
     *       1X,'GO BUS         =',F8.5/
     *       1X,'TRANSIT LOGSUM =',F10.5/)
      END IF
C.....................................................................   
C ====================================================================
C     INCREMENTAL LOGIT COMPUTATIONS
C ====================================================================
C
C       OUTPUT OR READ BASE LOGSUM VALUES
C
        IF(.NOT.CALIB) THEN
        FILENO=36+C
        RECNO = ((IZ-1)*MXZONES) + JZ
        IF(LSBASE) THEN
	      MFVAL=LSTRN(C)     
        WRITE(FILENO,REC=RECNO) MFVAL
        ELSE
        READ(FILENO,REC=RECNO) MFVAL
        LSBAS(C)=MFVAL
        END IF
        END IF
C
C       OBTAIN THE TRANSIT SHARE
C
          TSHARE(C)=DSHARES(DIZ,DJZ,C)
C
C       COMPUTE THE CHANGE IN THE TRANSIT UTILITIES, SHARES, AND TRIPS
C
          PEDUH(C)=1.0-TSHARE(C)
          DUT=LSUM1TRN*(LSTRN(C)-LSBAS(C))
          IF(CALIB) DUT=0.0
          IF(LSBAS(C).EQ.0) DUT=0.0
          EDUT=EXP(DUT)
          PEDUT(C)=TSHARE(C)*EDUT
          TPROB(C)=PEDUT(C)/(PEDUT(C)+PEDUH(C))
          TFAC=1.0
          IF(TSHARE(C).GT.0.0) THEN
          TFAC=TPROB(C)/TSHARE(C)
          END IF
C........................................................................
      IF(DEBUG) THEN
      WRITE(26,9016) IZ,JZ,C,PEDUH(C),LSTRN(C),DUT,LSBAS(C),
     *                EDUT,PEDUT(C),
     *                TSHARE(C),TPROB(C),TFAC
 9016 FORMAT(' INCREMENTAL LOGIT COMPUTATIONS'/
     *       ' ------------------------------'/
     *       ' PRODUCTION ZONE              =',I4/
     *       ' ATTRACTION ZONE              =',I4/
     *       ' MARKET SEGMENT               =',I2/
     *       ' AUTO SHARE * EXP(AUTO LOGSUM)=',F8.4/
     *       ' NEW TRANSIT LOGSUM           =',F8.4/
     *       ' CHANGE IN TRANSIT LOGSUM     =',F8.4/
     *       ' BASE TRANSIT LOGSUM          =',F8.4/
     *       ' EXPONENTIATED TRANSIT UTILITY=',F8.4/
     *       ' TRANSIT SHARE * EXP(TR LSUM) =',F8.4/
     *       ' TRANSIT SHARE BASE           =',F8.4/
     *       ' NEW TRANSIT PROBABILITY      =',F8.4/
     *       ' TRANSIT TRIP FACTOR          =',F8.4/)
      END IF
C..........................................................................
C
C     COMPUTE TRIP VALUES
C
      TTRAN=TFAC*TPERIN(C)
      TCR=TTRAN*TRNPROB(1)
      TUR=TTRAN*TRNPROB(2)
      TGB=TTRAN-TCR-TUR
C
      TGBW=TGB*GOBPROB(1)
      TGBD=TGB-TGBW
      TCRK=AMAX1(TGBD,0.0)
C
      TCRW=TCR*CWPROB(3)
      TCRB=TCR*CBPROB(3)
      TCRP=TCR*CPPROB(5)
      TCRK=TCR-(TCRW+TCRB+TCRP)
      TCRK=AMAX1(TCRK,0.0)
C
      TURW=TUR*UWPROB(3)
      TURB=TUR*UBPROB(3)
      TURP=TUR*UPPROB(5)
      TURK=TUR-(TURW+TURB+TURP)
      TURK=AMAX1(TURK,0.0)
C
      TCRW1=TCRW*CWPROB(1)
      TCRW2=TCRW-TCRW1
      TCRB1=TCRB*CBPROB(1)
      TCRB2=TCRB-TCRB1
      TCRP1=TCRP*CPPROB(1)
      TCRP2=TCRP*CPPROB(2)
      TCRP3=TCRP*CPPROB(3)
      TCRP4=TCRP-(TCRP1+TCRP2+TCRP3)
      TCRP4=AMAX1(TCRP4,0.0)
      TCRK1=TCRK*CKPROB(1)
      TCRK2=TCRK*CKPROB(2)
      TCRK3=TCRK*CKPROB(3)
      TCRK4=TCRK-(TCRK1+TCRK2+TCRK3)
      TCRK4=AMAX1(TCRK4,0.0)
C
      TURW1=TURW*UWPROB(1)
      TURW2=TURW-TURW1
      TURB1=TURB*UBPROB(1)
      TURB2=TURB-TURB1
      TURP1=TURP*UPPROB(1)
      TURP2=TURP*UPPROB(2)
      TURP3=TURP*UPPROB(3)
      TURP4=TURP-(TURP1+TURP2+TURP3)
      TURP4=AMAX1(TURP4,0.0)
      TURK1=TURK*UKPROB(1)
      TURK2=TURK*UKPROB(2)
      TURK3=TURK*UKPROB(3)
      TURK4=TURK-(TURK1+TURK2+TURK3)
      TURK4=AMAX1(TURK4,0.0)
C
      IF(SDJZ.EQ.1) THEN
      ESTCBD(1)=ESTCBD(1)+TCR
      ESTCBD(2)=ESTCBD(2)+TUR
      ESTCBD(3)=ESTCBD(3)+TGB
      END IF
C
      DTRIPS(SDIZ,SDJZ,1)=DTRIPS(SDIZ,SDJZ,1)+TCR
      DTRIPS(SDIZ,SDJZ,2)=DTRIPS(SDIZ,SDJZ,2)+TUR
      DTRIPS(SDIZ,SDJZ,3)=DTRIPS(SDIZ,SDJZ,3)+TGB
C     
      INDEX=IFIX(HWYDST(JZ))+1
      INDEX=MIN0(INDEX,151)
      GORTLF(INDEX)=GORTLF(INDEX)+TCR
C...GO BUS WALK ACCESS TRANSFERS
      IF(TRANSF(JZ).GT.0) THEN
      ICH=IFIX(TRANSF(JZ)+0.01)
      ICH=MIN0(ICH,5)
      TXFERS(1,ICH)=TXFERS(1,ICH)+TGBW
      END IF
C...GO BUS DRIVE ACCESS TRANSFERS
      IT=CSTAT-MAX_IZONES
      IF(IT.GT.0) THEN
      ICH=IFIX(STAZNE(1,IT,JZ)+0.01)
      ICH=MIN0(ICH,5)
      IF(ICH.GT.0) THEN
      TXFERS(2,ICH)=TXFERS(2,ICH)+TGBD
      ELSE
      IF(TGBD.GT.0) WRITE(26,9117) IEQUIV(IZ),IEQUIV(JZ),
     *   IEQUIV(CSTAT),STAZNE(1,IT,JZ),ICH,TGBD
 9117 FORMAT(' ILOGIT 9117 (W) GO BUS TRANSFER SUMMARY ERROR ',
     *       ' IZ=',I4,' JZ=',I4,' CSTAT=',I4,
     *       ' STAZNE=',F8.2,
     *       ' ICH=',I3,' TGBD=',F12.4)
      END IF
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9421) C,TTRAN,TCR,TUR,TGB,TGBW,TGBD,
     *               TCRW,TCRB,TCRP,TCRK,TCRW1,TCRW2,
     *               TCRB1,TCRB2,
     *               TCRP1,TCRP2,TCRP3,TCRP4,
     *               TCRK1,TCRK2,TCRK3,TCRK4,
     *               TURW,TURB,TURP,TURK,TURW1,TURW2,
     *               TURB1,TURB2,
     *               TURP1,TURP2,TURP3,TURP4,
     *               TURK1,TURK2,TURK3,TURK4
 9421 FORMAT(/1X,'SUMMARY OF MODAL TRIP VALUES'/
     *       1X,'-----------------------------------'/
     *       1X,' MARKET SEGMENT=',I2//
     *       1X,'TRANSIT                        =',E12.5/
     *       1X,'GO RAIL                        =',E12.5/
     *       1X,'TTC SUBWAY                     =',E12.5/
     *       1X,'GO BUS                         =',E12.5//
     *       1X,'GO BUS WALK                    =',E12.5/
     *       1X,'GO BUS DRIVE                   =',E12.5//
     *       1X,'WALK  --> GO RAIL              =',E12.5/
     *       1X,'BUS   --> GO RAIL              =',E12.5/
     *       1X,'P&R   --> GO RAIL              =',E12.5/
     *       1X,'K&R   --> GO RAIL              =',E12.5//
     *       1X,'GO RAIL       WALK  -STATION #1=',E12.5/
     *       1X,'GO RAIL       WALK  -STATION #2=',E12.5/
     *       1X,'GO RAIL       BUS   -STATION #1=',E12.5/
     *       1X,'GO RAIL       BUS   -STATION #2=',E12.5/
     *       1X,'GO RAIL       PNR   -STATION #1=',E12.5/
     *       1X,'GO RAIL       PNR   -STATION #2=',E12.5/
     *       1X,'GO RAIL       PNR   -STATION #3=',E12.5/
     *       1X,'GO RAIL       PNR   -STATION #4=',E12.5/
     *       1X,'GO RAIL       PNR   -STATION #1=',E12.5/
     *       1X,'GO RAIL       PNR   -STATION #2=',E12.5/
     *       1X,'GO RAIL       KNR   -STATION #3=',E12.5/
     *       1X,'GO RAIL       KNR   -STATION #4=',E12.5//
     *       1X,'WALK  --> TTC SUBWAY              =',E12.5/
     *       1X,'BUS   --> TTC SUBWAY              =',E12.5/
     *       1X,'P&R   --> TTC SUBWAY              =',E12.5/
     *       1X,'K&R   --> TTC SUBWAY              =',E12.5//
     *       1X,'TTC SUBWAY    WALK     -STATION #1=',E12.5/
     *       1X,'TTC SUBWAY    WALK     -STATION #2=',E12.5/
     *       1X,'TTC SUBWAY    BUS      -STATION #1=',E12.5/
     *       1X,'TTC SUBWAY    BUS      -STATION #2=',E12.5/
     *       1X,'TTC SUBWAY    PNR      -STATION #1=',E12.5/
     *       1X,'TTC SUBWAY    PNR      -STATION #2=',E12.5/
     *       1X,'TTC SUBWAY    PNR      -STATION #3=',E12.5/
     *       1X,'TTC SUBWAY    PNR      -STATION #4=',E12.5/
     *       1X,'TTC SUBWAY    KNR      -STATION #1=',E12.5/
     *       1X,'TTC SUBWAY    KNR      -STATION #2=',E12.5/
     *       1X,'TTC SUBWAY    KNR      -STATION #3=',E12.5/
     *       1X,'TTC SUBWAY    KNR      -STATION #4=',E12.5/)
      END IF
C....................................................................
C
C  STORE TRIP END SUMMARY INFORMATION
C
C..UPPER LEVEL
      TESUM(1,C)=TESUM(1,C) + TPERIN(C)
      TESUM(2,C)=TESUM(2,C) + TTRAN
      TESUM(3,C)=TESUM(3,C) + TCR
      TESUM(4,C)=TESUM(4,C) + TUR
      TESUM(5,C)=TESUM(5,C) + TGB
      TESUM(6,C)=TESUM(6,C) + TTRAN-TPERIN(C)

C..TRANSIT PRIMARY/SUBMODE LEVEL
      TESUM(7,C)=TESUM(7,C) + TGBW
      TESUM(8,C)=TESUM(8,C) + TGBD
      TESUM(9,C)=TESUM(9,C) + TCRW
      TESUM(10,C)=TESUM(10,C) + TCRB
      TESUM(11,C)=TESUM(11,C) + TCRP
      TESUM(12,C)=TESUM(12,C) + TCRK
      TESUM(13,C)=TESUM(13,C) + TURW
      TESUM(14,C)=TESUM(14,C) + TURB
      TESUM(15,C)=TESUM(15,C) + TURP
      TESUM(16,C)=TESUM(16,C) + TURK
C
C  SAVE TRIPS FOR ZONE-TO-ZONE OUTPUT
C
      OUTTRP(1)=OUTTRP(1)+TCRW
      OUTTRP(2)=OUTTRP(2)+TCRB
      OUTTRP(3)=OUTTRP(3)+TCRP
      OUTTRP(4)=OUTTRP(4)+TCRK
      OUTTRP(5)=OUTTRP(5)+TURW
      OUTTRP(6)=OUTTRP(6)+TURB
      OUTTRP(7)=OUTTRP(7)+TURP
      OUTTRP(8)=OUTTRP(8)+TURK
      OUTTRP(9)=OUTTRP(9)+TGBW
      OUTTRP(10)=OUTTRP(10)+TGBD
      OUTTRP(11)=OUTTRP(11)+TTRAN
C..GO RAIL STATION SUMMARY MATRIX
      STASUM((OSTA(1,1)-MAX_IZONES),1)=STASUM((OSTA(1,1)-MAX_IZONES),1)+
     *                                 TCRW1
      STASUM((OSTA(1,2)-MAX_IZONES),1)=STASUM((OSTA(1,2)-MAX_IZONES),1)+
     *                                 TCRW2
      STASUM((OSTA(1,3)-MAX_IZONES),2)=STASUM((OSTA(1,3)-MAX_IZONES),2)+
     *                                 TCRB1
      STASUM((OSTA(1,4)-MAX_IZONES),2)=STASUM((OSTA(1,4)-MAX_IZONES),2)+
     *                                 TCRB2
      STASUM((OSTA(1,5)-MAX_IZONES),3)=STASUM((OSTA(1,5)-MAX_IZONES),3)+
     *                                 TCRP1
      STASUM((OSTA(1,6)-MAX_IZONES),3)=STASUM((OSTA(1,6)-MAX_IZONES),3)+
     *                                 TCRP2
      STASUM((OSTA(1,7)-MAX_IZONES),3)=STASUM((OSTA(1,7)-MAX_IZONES),3)+
     *                                 TCRP3
      STASUM((OSTA(1,8)-MAX_IZONES),3)=STASUM((OSTA(1,8)-MAX_IZONES),3)+
     *                                 TCRP4
      STASUM((OSTA(1,9)-MAX_IZONES),4)=STASUM((OSTA(1,9)-MAX_IZONES),4)+
     *                                 TCRK1
      STASUM((OSTA(1,10)-MAX_IZONES),4)=
     *                    STASUM((OSTA(1,10)-MAX_IZONES),4)+TCRK2
      STASUM((OSTA(1,11)-MAX_IZONES),4)=
     *                    STASUM((OSTA(1,11)-MAX_IZONES),4)+TCRK3
      STASUM((OSTA(1,12)-MAX_IZONES),4)=
     *                    STASUM((OSTA(1,12)-MAX_IZONES),4)+TCRK4
C..GO RAIL STATION EGRESS SUMMARY MATRIX
      STASUM2((ASTA(1,1)-MAX_IZONES),ASTA2(1,1))=
     *   STASUM2((ASTA(1,1)-MAX_IZONES),ASTA2(1,1))+TCRW1
      STASUM2((ASTA(1,2)-MAX_IZONES),ASTA2(1,2))=
     *   STASUM2((ASTA(1,2)-MAX_IZONES),ASTA2(1,2))+TCRW2
      STASUM2((ASTA(1,3)-MAX_IZONES),ASTA2(1,3))=
     *   STASUM2((ASTA(1,3)-MAX_IZONES),ASTA2(1,3))+TCRB1
      STASUM2((ASTA(1,4)-MAX_IZONES),ASTA2(1,4))=
     *   STASUM2((ASTA(1,4)-MAX_IZONES),ASTA2(1,4))+TCRB2
      STASUM2((ASTA(1,5)-MAX_IZONES),ASTA2(1,5))=
     *   STASUM2((ASTA(1,5)-MAX_IZONES),ASTA2(1,5))+TCRP1
      STASUM2((ASTA(1,6)-MAX_IZONES),ASTA2(1,6))=
     *   STASUM2((ASTA(1,6)-MAX_IZONES),ASTA2(1,6))+TCRP2
      STASUM2((ASTA(1,7)-MAX_IZONES),ASTA2(1,7))=
     *   STASUM2((ASTA(1,7)-MAX_IZONES),ASTA2(1,7))+TCRP3
      STASUM2((ASTA(1,8)-MAX_IZONES),ASTA2(1,8))=
     *   STASUM2((ASTA(1,8)-MAX_IZONES),ASTA2(1,8))+TCRP4
      STASUM2((ASTA(1,9)-MAX_IZONES),ASTA2(1,9))=
     *   STASUM2((ASTA(1,9)-MAX_IZONES),ASTA2(1,9))+TCRK1
      STASUM2((ASTA(1,10)-MAX_IZONES),ASTA2(1,10))=
     *   STASUM2((ASTA(1,10)-MAX_IZONES),ASTA2(1,10))+TCRK2
      STASUM2((ASTA(1,11)-MAX_IZONES),ASTA2(1,11))=
     *   STASUM2((ASTA(1,11)-MAX_IZONES),ASTA2(1,11))+TCRK3
      STASUM2((ASTA(1,12)-MAX_IZONES),ASTA2(1,12))=
     *   STASUM2((ASTA(1,12)-MAX_IZONES),ASTA2(1,12))+TCRK4
C..TTC SUBWAY STATION SUMMARY MATRIX
      STASUM((OSTA(2,1)-MAX_IZONES),6)=
     *                    STASUM((OSTA(2,1)-MAX_IZONES),6)+TURW1
      STASUM((OSTA(2,2)-MAX_IZONES),6)=
     *                    STASUM((OSTA(2,2)-MAX_IZONES),6)+TURW2
      STASUM((OSTA(2,3)-MAX_IZONES),7)=
     *                    STASUM((OSTA(2,3)-MAX_IZONES),7)+TURB1
      STASUM((OSTA(2,4)-MAX_IZONES),7)=
     *                    STASUM((OSTA(2,4)-MAX_IZONES),7)+TURB2
      STASUM((OSTA(2,5)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,5)-MAX_IZONES),8)+TURP1
      STASUM((OSTA(2,6)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,6)-MAX_IZONES),8)+TURP2
      STASUM((OSTA(2,7)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,7)-MAX_IZONES),8)+TURP3
      STASUM((OSTA(2,8)-MAX_IZONES),8)=
     *                    STASUM((OSTA(2,8)-MAX_IZONES),8)+TURP4
      STASUM((OSTA(2,9)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,9)-MAX_IZONES),9)+TURK1
      STASUM((OSTA(2,10)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,10)-MAX_IZONES),9)+TURK2
      STASUM((OSTA(2,11)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,11)-MAX_IZONES),9)+TURK3
      STASUM((OSTA(2,12)-MAX_IZONES),9)=
     *                    STASUM((OSTA(2,12)-MAX_IZONES),9)+TURK4
C..TTC SUBWAY STATION EGRESS SUMMARY MATRIX
      STASUM2((ASTA(2,1)-MAX_IZONES),ASTA2(2,1))=
     *   STASUM2((ASTA(2,1)-MAX_IZONES),ASTA2(2,1))+TURW1
      STASUM2((ASTA(2,2)-MAX_IZONES),ASTA2(2,2))=
     *   STASUM2((ASTA(2,2)-MAX_IZONES),ASTA2(2,2))+TURW2
      STASUM2((ASTA(2,3)-MAX_IZONES),ASTA2(2,3))=
     *   STASUM2((ASTA(2,3)-MAX_IZONES),ASTA2(2,3))+TURB1
      STASUM2((ASTA(2,4)-MAX_IZONES),ASTA2(2,4))=
     *   STASUM2((ASTA(2,4)-MAX_IZONES),ASTA2(2,4))+TURB2
      STASUM2((ASTA(2,5)-MAX_IZONES),ASTA2(2,5))=
     *   STASUM2((ASTA(2,5)-MAX_IZONES),ASTA2(2,5))+TURP1
      STASUM2((ASTA(2,6)-MAX_IZONES),ASTA2(2,6))=
     *   STASUM2((ASTA(2,6)-MAX_IZONES),ASTA2(2,6))+TURP2
      STASUM2((ASTA(2,7)-MAX_IZONES),ASTA2(2,7))=
     *   STASUM2((ASTA(2,7)-MAX_IZONES),ASTA2(2,7))+TURP3
      STASUM2((ASTA(2,8)-MAX_IZONES),ASTA2(2,8))=
     *   STASUM2((ASTA(2,8)-MAX_IZONES),ASTA2(2,8))+TURP4
      STASUM2((ASTA(2,9)-MAX_IZONES),ASTA2(2,9))=
     *   STASUM2((ASTA(2,9)-MAX_IZONES),ASTA2(2,9))+TURK1
      STASUM2((ASTA(2,10)-MAX_IZONES),ASTA2(2,10))=
     *   STASUM2((ASTA(2,10)-MAX_IZONES),ASTA2(2,10))+TURK2
      STASUM2((ASTA(2,11)-MAX_IZONES),ASTA2(2,11))=
     *   STASUM2((ASTA(2,11)-MAX_IZONES),ASTA2(2,11))+TURK3
      STASUM2((ASTA(2,12)-MAX_IZONES),ASTA2(2,12))=
     *   STASUM2((ASTA(2,12)-MAX_IZONES),ASTA2(2,12))+TURK4
C GO BUS DRIVE
      IF(CSTAT.GT.0.AND.CSTAT.LT.MAX_ZONES) THEN
      STASUM((CSTAT-MAX_IZONES),11)=
     *                    STASUM((CSTAT-MAX_IZONES),11) + TGBD
      END IF
C
C     STORE STATION LEVEL TRIP MATRICES FOR OUTPUT
C
C......GO RAIL STATION-TO-STATION
      orista=osta(1,1)-MAX_IZONES
      dessta=asta(1,1)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrw1)
      call statsum(maxgln,orista,dessta,tcrw1,gorail)
      orista=osta(1,2)-MAX_IZONES
      dessta=asta(1,2)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrw2)
      call statsum(maxgln,orista,dessta,tcrw2,gorail)
      orista=osta(1,3)-MAX_IZONES
      dessta=asta(1,3)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrb1)
      call statsum(maxgln,orista,dessta,tcrb1,gorail)
      orista=osta(1,4)-MAX_IZONES
      dessta=asta(1,4)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrb2)
      call statsum(maxgln,orista,dessta,tcrb2,gorail)
      orista=osta(1,5)-MAX_IZONES
      dessta=asta(1,5)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp1)
      call statsum(maxgln,orista,dessta,tcrp1,gorail)
      call darsum(orista,jz,tcrp1,hdist,hwydst,gordar)
      orista=osta(1,6)-MAX_IZONES
      dessta=asta(1,6)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp2)
      call statsum(maxgln,orista,dessta,tcrp2,gorail)
      call darsum(orista,jz,tcrp2,hdist,hwydst,gordar)
      orista=osta(1,7)-MAX_IZONES
      dessta=asta(1,7)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp3)
      call statsum(maxgln,orista,dessta,tcrp3,gorail)
      call darsum(orista,jz,tcrp3,hdist,hwydst,gordar)
      orista=osta(1,8)-MAX_IZONES
      dessta=asta(1,8)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp4)
      call statsum(maxgln,orista,dessta,tcrp4,gorail)
      call darsum(orista,jz,tcrp4,hdist,hwydst,gordar)
      orista=osta(1,9)-MAX_IZONES
      dessta=asta(1,9)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk1)
      call statsum(maxgln,orista,dessta,tcrk1,gorail)
      orista=osta(1,10)-MAX_IZONES
      dessta=asta(1,10)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk2)
      call statsum(maxgln,orista,dessta,tcrk2,gorail)
      orista=osta(1,11)-MAX_IZONES
      dessta=asta(1,11)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk3)
      call statsum(maxgln,orista,dessta,tcrk3,gorail)
      orista=osta(1,12)-MAX_IZONES
      dessta=asta(1,12)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk4)
      call statsum(maxgln,orista,dessta,tcrk4,gorail)
C.....TTC SUBWAY STATION-TO-STATION
      orista=osta(2,1)-MAX_IZONES
      dessta=asta(2,1)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turw1)
      orista=osta(2,2)-MAX_IZONES
      dessta=asta(2,2)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turw2)
      orista=osta(2,3)-MAX_IZONES
      dessta=asta(2,3)-MAX_IZONES 
      if(orista.gt.max_stations) then
      write(26,99999) iequiv(iz),iequiv(jz),c,m,osta(2,3)
99999 format(' iz=',i5,' jz=',i5,' c=',i2,' m=',i2,' osta(2,3)=',i5)
      end if
      urss(orista,dessta)=urss(orista,dessta) + sngl(turb1)
      orista=osta(2,4)-MAX_IZONES
      dessta=asta(2,4)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turb2)
      orista=osta(2,5)-MAX_IZONES
      dessta=asta(2,5)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp1)
      orista=osta(2,6)-MAX_IZONES
      dessta=asta(2,6)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp2)
      orista=osta(2,7)-MAX_IZONES
      dessta=asta(2,7)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp3)
      orista=osta(2,8)-MAX_IZONES
      dessta=asta(2,8)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp4)
      orista=osta(2,9)-MAX_IZONES
      dessta=asta(2,9)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk1)
      orista=osta(2,10)-MAX_IZONES
      dessta=asta(2,10)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk2)
      orista=osta(2,11)-MAX_IZONES
      dessta=asta(2,11)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk3)
      orista=osta(2,12)-MAX_IZONES
      dessta=asta(2,12)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk4)
C.....GO RAIL STATION TO ZONE
      dessta=asta(1,1)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrw1)
      dessta=asta(1,2)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrw2)
      dessta=asta(1,3)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrb1)
      dessta=asta(1,4)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrb2)
      dessta=asta(1,5)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp1)
      dessta=asta(1,6)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp2)
      dessta=asta(1,7)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp3)
      dessta=asta(1,8)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp4)
      dessta=asta(1,9)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk1)
      dessta=asta(1,10)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk2)
      dessta=asta(1,11)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk3)
      dessta=asta(1,12)-MAX_IZONES
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk4)
C.....TTC SUBWAY STATION-T0-DESTINATION ZONE
      dessta=asta(2,1)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turw1)
      dessta=asta(2,2)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turw2)
      dessta=asta(2,3)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turb1)
      dessta=asta(2,4)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turb2)
      dessta=asta(2,5)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp1)
      dessta=asta(2,6)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp2)
      dessta=asta(2,7)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp3)
      dessta=asta(2,8)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp4)
      dessta=asta(2,9)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk1)
      dessta=asta(2,10)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk2)
      dessta=asta(2,11)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk3)
      dessta=asta(2,12)-MAX_IZONES
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk4)
c...BUS TO GO RAIL
      orista=osta(1,3)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb1)
      orista=osta(1,4)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb2)
c...BUS TO TTC SUBWAY
      orista=osta(2,3)-MAX_IZONES
      bur(iz,orista)=bur(iz,orista) + sngl(turb1)
      orista=osta(2,4)-MAX_IZONES
      bur(iz,orista)=bur(iz,orista) + sngl(turb2)
c...DRIVE TO GO RAIL
      orista=osta(1,5)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp1)
      orista=osta(1,6)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp2)
      orista=osta(1,7)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp3)
      orista=osta(1,8)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrp4)
      orista=osta(1,9)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk1)
      orista=osta(1,10)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk2)
      orista=osta(1,11)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk3)
      orista=osta(1,12)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(tcrk4)
c....DRIVE TO TTC SUBWAY
      orista=osta(2,5)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp1)
      orista=osta(2,6)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp2)
      orista=osta(2,7)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp3)
      orista=osta(2,8)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turp4)
      orista=osta(2,9)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk1)
      orista=osta(2,10)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk2)
      orista=osta(2,11)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk3)
      orista=osta(2,12)-MAX_IZONES
      dtran(iz,orista)=dtran(iz,orista) + sngl(turk4)
c...DRIVE TO GO BUS
      orista=cstat-MAX_IZONES
      if(orista.gt.0) then
      dtran(iz,orista)=dtran(iz,orista) + sngl(tgbd)
      end if
C...GO BUS STATION TO DESTINATION ZONE
      orista=cstat-MAX_IZONES
      if(orista.gt.0) then
      gbstaz(orista,jz)=gbstaz(orista,jz) + sngl(tgbd)
      end if
c ----- Market Segmentation Loop End
  100 CONTINUE
C
C  OUTPUT ZONE LEVEL MATRICES
C
      IF(TRIPSOUT) THEN
      RECNO = ((IZ-1)*MXZONES) + JZ
      DO T=1,11
      FILENO=50+T
      WRITE(FILENO,REC=RECNO) OUTTRP(T)
      END DO
      END IF
c ----- Destination & Origin Zone Loop End
      end do
      end do
C ===================================================================
C     TRIP REPORTING
C ===================================================================
      DO C=1,NCATS
      DO K=1,20
      TESUM(K,4)=TESUM(K,4)+TESUM(K,C)
      END DO 
      END DO
      CI=NCATS+1
      WRITE(26,7001)
 7001 FORMAT(//,25X,'R E P O R T   1',//,
     *       10X,'         ','  INPUT  ',' OUTPUT  ','         ',
     *           '         ','         ','         ','         ',/,
     *       10X,' MARKET  ',' TRANSIT ',' TRANSIT ',' ABSOLUTE',
     *           ' PERCENT ','         ','         ','         ',/,
     *       10X,' SEGMENT ','  TRIPS  ','  TRIPS  ','  CHANGE ',
     *           '  CHANGE ','         ','         ','         ',/,
     *       10X,'---------','---------','---------','----------',
     *           '---------')
      IF(NCATS.GT.1) THEN
      DO C=1,NCATS
      ABSDIF=TESUM(2,C)-TESUM(1,C)
      IF(TESUM(1,C).GT.0) THEN
      PERDIF=(ABSDIF/TESUM(1,C))*100.0
      ELSE
      PERDIF=0.0
      END IF
      WRITE(26,7002) C,TESUM(1,C),TESUM(2,C),ABSDIF,PERDIF
 7002 FORMAT(15X,I1,2X,4F9.1)
      END DO
      END IF
      ABSDIF=TESUM(2,CI)-TESUM(1,CI)
      IF(TESUM(1,CI).GT.0) THEN
      PERDIF=(ABSDIF/TESUM(1,CI))*100.0
      ELSE
      PERDIF=0.0
      END IF
      WRITE(26,7003) TESUM(1,CI),TESUM(2,CI),ABSDIF,PERDIF
 7003 FORMAT(/,12X,'TOTAL',1X,4F9.1)
C
      WRITE(26,7004)
 7004 FORMAT(//,30X,'R E P O R T   2',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY MARKET SEGMENT',//,
     *       1X,' MARKET  '/
     *       1X,' SEGMENT ','    GO    ','   TTC    ','   GO     '/
     *       1X,'  LEVEL  ','    RAIL  ','  SUBWAY  ','   BUS    ',
     *                      '  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,7005) C,TESUM(3,C),TESUM(4,C),TESUM(5,C),TESUM(2,C)
 7005 FORMAT(3X,I1,4X,4F10.1)
      END DO
      WRITE(26,7006) TESUM(3,CI),TESUM(4,CI),TESUM(5,CI),TESUM(2,CI)      
 7006 FORMAT(/,2X,'TOTAL',1X,4F10.1)
C
C GO RAIL AND THEN TTC SUBWAY
C
      WRITE(26,7007)
 7007 FORMAT(//,30X,'R E P O R T   3',/,
     *          20X,
     *          'SUMMARIZE GO RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------','----------','----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(TESUM(K,C),K=9,12),TESUM(3,C)
 7009 FORMAT(5X,I1,2X,5F10.1)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=9,12),TESUM(3,CI)
 7010 FORMAT(/,2X,'TOTAL',1X,6F10.1)
      WRITE(26,7011)
 7011 FORMAT(//,30X,'R E P O R T   4',/,
     *          20X,
     *          'SUMMARIZE TTC SUBWAY BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------','----------','----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(TESUM(K,C),K=13,16),TESUM(4,C)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=13,16),TESUM(4,CI)
C
      WRITE(26,7031)
 7031 FORMAT(//,30X,'R E P O R T   5',/,
     *          20X,
     *          'SUMMARIZE GO BUS BY MARKET SEGMENT',//,
     *       1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ','    WALK  ','   DRIVE  ',
     *          '  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------')
      TTOTAL=0.0
      TTPNR=0.0
      DO C=1,NCATS
      TTPNR=TESUM(7,C)+TESUM(8,C)
      TTOTAL=TTOTAL+TTPNR
      WRITE(26,7009) C,(TESUM(K,C),K=7,8),TTPNR
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=7,8),TTOTAL
C
C  SUMMARIZE STATION MODE OF ACCESS DATA
C
      WRITE(26,7020)
 7020 FORMAT(//,30X,'R E P O R T   5A',/,
     *          20X,
     *          'SUMMARIZE GO RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------',
     *          '-------  ')
        TTWLK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0  
	      TTOTAL=0.0
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,5)=STASUM(K,1)+STASUM(K,2)+STASUM(K,3)+STASUM(K,4)
      TTWLK=TTWLK+STASUM(K,1)
	    TTBUS=TTBUS+STASUM(K,2)
	    TTPNR=TTPNR+STASUM(K,3)
	    TTKNR=TTKNR+STASUM(K,4)
	    TTOTAL=TTOTAL+STASUM(K,5)
      IF(STASUM(K,5).GT.0.1) THEN
	    WRITE(26,7021) IEQUIV(KS),STANAME(K),(STASUM(K,L),L=1,5)
 7021    FORMAT(2X,I4,3X,A29,1X,6F8.0)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
 7022 FORMAT(/3X,'TOTAL',31X,6F8.0)
      TTWLK=0.0
      TTBUS=0.0
      TTPNR=0.0
      TTOTAL=0.0
      WRITE(26,7023)
 7023 FORMAT(//,30X,'R E P O R T   5B',/,
     *          20X,
     *          'SUMMARIZE GO RAIL STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ',' DRIVE  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','------  ')
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM2(K,4)=STASUM2(K,1)+STASUM2(K,2)+STASUM2(K,3)
      TTWLK=TTWLK+STASUM2(K,1)
      TTBUS=TTBUS+STASUM2(K,2)
      TTPNR=TTPNR+STASUM2(K,3)
      TTOTAL=TTOTAL+STASUM2(K,4)
      IF(STASUM2(K,4).GT.0.1) THEN
	    WRITE(26,7024) IEQUIV(KS),STANAME(K),
     *                  (STASUM2(K,L),L=1,4)
 7024 FORMAT(2X,I4,3X,A29,1X,4F8.0)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTPNR,TTOTAL
      WRITE(26,7028)
 7028 FORMAT(//,30X,'R E P O R T   6A',/,
     *          20X,
     *          'SUMMARIZE TTC SUBWAY STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------',
     *          '-------  ')
        TTWLK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0  
	      TTOTAL=0.0
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,10)=STASUM(K,6)+STASUM(K,7)+STASUM(K,8)+STASUM(K,9)
      TTWLK=TTWLK+STASUM(K,6)
	    TTBUS=TTBUS+STASUM(K,7)
	    TTPNR=TTPNR+STASUM(K,8)
	    TTKNR=TTKNR+STASUM(K,9)
	    TTOTAL=TTOTAL+STASUM(K,10)
      IF(STASUM(K,10).GT.0.1) THEN
	    WRITE(26,7021) IEQUIV(KS),STANAME(K),(STASUM(K,L),L=6,10)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTOTAL=0.0
      WRITE(26,7025)
 7025 FORMAT(//,30X,'R E P O R T   6B',/,
     *          20X,
     *          'SUMMARIZE TTC SUBWAY STATION EGRESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' EGRESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','------  ')
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM2(K,7)=STASUM2(K,5)+STASUM2(K,6)
      TTWLK=TTWLK+STASUM2(K,5)
      TTBUS=TTBUS+STASUM2(K,6)
      TTOTAL=TTOTAL+STASUM2(K,7)
      IF(STASUM2(K,7).GT.0.1) THEN
	    WRITE(26,7026) IEQUIV(KS),STANAME(K),
     *                  (STASUM2(K,L),L=5,7)
 7026 FORMAT(2X,I4,3X,A29,1X,3F8.0)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTOTAL
C
C     GO BUS DRIVE ACCESS SUMMARY
C
      WRITE(26,7027)
 7027 FORMAT(//,30X,'R E P O R T  7',/,
     *          20X,
     *         'SUMMARIZE GO BUS STATION DRIVE ACCESS VOLUMES',//,
     *      1X,'STATION','                                ',
     *         '   DRIVE    '/,
     *      1X,'------------------------------------------------',
     *         '------------')
      TTPNR=0.0
	    DO K=1,MAX_STATIONS
	    KS=K+MAX_IZONES
	    IF(STASUM(K,11).GT.0.1) THEN
	    TTPNR=TTPNR+STASUM(K,11)
	    WRITE(26,7029) IEQUIV(KS),STANAME(K),STASUM(K,11)
 7029 FORMAT(2X,I4,3X,A29,1X,F8.0)
      ENDIF
      END DO
      WRITE(26,7022) TTPNR
      TOTGOR=TESUM(3,CI)
      PNRGOR=TESUM(11,CI)
C
C     SUMMARIZE TRANSFER INCIDENCE
C
       IF(CALIB) THEN
       DO K=1,2
       DO K1=1,5
       TXFERS(K,6)=TXFERS(K,6)+TXFERS(K,K1)
       END DO 
       END DO
       WRITE(26,9516) (TXFERS(1,K1),K1=1,6),
     *                (TXFERS(2,K2),K2=1,6)
 9516  FORMAT(//'       BUS TRANSFER INCIDENCE SUMMARY'/
     *          ' -----------------------------------------'/
     *          ' WALK ACCESS     0          1         2   ',
     *          '     3         4+      TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------  --------'/
     *          ' GO BUS     ',6(2X,F8.0)//
     *          ' DRIVE ACCESS    0          1         2   ',
     *          '     3          4+     TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------  --------'/
     *          ' GO BUS     ',6(2X,F8.0)//)
      END IF
C
C     SELF CALIBRATION ANALYSIS
C
      if(calib) then
      iter=iter+1
      write(26,7032) iter
 7032 format(/' Calibration Iteration ',i2/
     *        ' -------------------------'/)
      write(*,7032) iter
      call scalib(iter,tesum)
      tesum=0.0
      stasum=0.0
      stasum2=0.0
      txfers=0.0
      if(iter.lt.niter) go to 1000
      end if
C     
C     STATION CAPACITY RESTRAINT
C
      if(capres) then
      iter=iter+1
      write(26,7033) iter
 7033 format(/' Station Capacity Restraint Iteration ',i2/
     *        ' -------------------------------------'/)
      write(*,7033) iter
      call stacap(iter,stasum,converg)
      tesum=0.0
      stasum=0.0
      stasum2=0.0
      txfers=0.0
      if(iter.lt.citer.and.(.not.converg)) go to 1000
      end if
C
C     OUTPUT STATION LEVEL TRIP MATRICES
C
      IF(TRIPSOUT) THEN
C.....GO RAIL & TTC SUBWAY STATION-TO-STATION
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES    
      FILENO=43
	    MFVAL=CRSS(IZ,JZ)
      RECNO = ((DIZ-1)*MXZONES) + DJZ
      WRITE(FILENO,REC=RECNO) MFVAL
      FILENO=44
	    MFVAL=URSS(IZ,JZ)
      RECNO = ((DIZ-1)*MXZONES) + DJZ
      WRITE(FILENO,REC=RECNO) MFVAL
      END DO
      END DO
C.....GO RAIL & TTC SUBWAY BUS TO STATION
      DO IZ=1,MAX_IZONES
      DO JZ=1,MAX_STATIONS
      DJZ=JZ+MAX_IZONES
      FILENO=45
	    MFVAL=BCR(IZ,JZ)
      RECNO = ((IZ-1)*MXZONES) + DJZ
      WRITE(FILENO,REC=RECNO) MFVAL
      FILENO=47
	    MFVAL=BUR(IZ,JZ)
      RECNO = ((IZ-1)*MXZONES) + DJZ
      WRITE(FILENO,REC=RECNO) MFVAL
      FILENO=50
	    MFVAL=DTRAN(IZ,JZ)
      RECNO = ((IZ-1)*MXZONES) + DJZ
      WRITE(FILENO,REC=RECNO) MFVAL
      END DO
      END DO
C.....GO RAIL & TTC SUBWAY STATION-TO-ZONE
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_IZONES
      DIZ=IZ+MAX_IZONES
      FILENO=46
	    MFVAL=CRSTAZ(IZ,JZ)
      RECNO = ((DIZ-1)*MXZONES) + JZ
      WRITE(FILENO,REC=RECNO) MFVAL
      FILENO=48
	    MFVAL=URSTAZ(IZ,JZ)
      RECNO = ((DIZ-1)*MXZONES) + JZ
      WRITE(FILENO,REC=RECNO) MFVAL
      FILENO=49
	    MFVAL=GBSTAZ(IZ,JZ)
      RECNO = ((DIZ-1)*MXZONES) + JZ
      WRITE(FILENO,REC=RECNO) MFVAL
      END DO
      END DO
      END IF
C.....SUPER DISTRICT LEVEL SUMMARIES
      IF(CALIB) THEN
      DO K=1,3
      WRITE(87,8010) NAME(K),SDNAME
 8010 FORMAT(A13,31(',',A35))
      DO IZ=1,30
       DO JZ=1,30
       DTRIPS(IZ,31,K)=DTRIPS(IZ,31,K)+DTRIPS(IZ,JZ,K)
       DTRIPS(31,JZ,K)=DTRIPS(31,JZ,K)+DTRIPS(IZ,JZ,K)
       DTRIPS(31,31,K)=DTRIPS(31,31,K)+DTRIPS(IZ,JZ,K)
       END DO
      WRITE(87,8011) SDNAME(IZ),(DTRIPS(IZ,JZ,K),JZ=1,31)
 8011 FORMAT(A35,31(',',F10.1))
      END DO
      WRITE(87,8011) SDNAME(31),(DTRIPS(31,JZ,K),JZ=1,31)
      END DO
c
c     write out trip length frequency distribution
c
      write(88,302) 
  302 format('GO Rail Trip Length Frequency Distribution'/
     *       'distance,trips,percent,cumulative')
      cumval=0.0
      do k=1,151
      mfval=gortlf(k)/TOTGOR
      cumval=cumval+mfval
      write(88,301) (k-1),gortlf(k),mfval,cumval
  301 format(i3,',',f10.2,2(',',f10.4))
      end do
c
c     write out GO Rail Drive Access Ratio
c
      cumval=0.0
      write(88,304)
  304 format('GO Rail Drive Access Ratio Plot'/
     *       'dar,trips,percent,cumulative')
      do k=1,51
      tfac=(k-1)/10.0
      mfval=gordar(k)/pnrgor
      cumval=cumval+mfval
      if(gordar(k).gt.0) write(88,303) tfac,gordar(k),mfval,cumval
  303 format(f6.2,',',f10.2,2(',',f10.4))
      end do
      END IF
c
c     write out GO Rail Segment to Segment File
c
c      if(calib) then
      open(89,file=fstasum,status='unknown',
     *        form='formatted')
      write(89,306) gorname
  306 format('Segment',10(',',a25),',','Total')
      do k=1,maxgln
      write(89,305) gorname(k),(gorail(k,k1),k1=1,(maxgln+1))
  305 format(a25,21(',',f8.2))
      end do
      write(89,307) (gorail((maxgln+1),k1),k1=1,(maxgln+1))
  307 format('Total',21(',',f9.2))
      write(89,308)
  308 format('GO Rail Station Access Summary'/
     *       'station,name,walk,bus,p&r,k&r,total')
      do k=1,max_stations
      if(stanum(k).eq.1) then
      ista=iequiv(k+max_izones)
      write(89,309) ista,staname(k),(stasum(k,k1),k1=1,5)
  309 format(i5,',',a37,5(',',f8.0))
      end if
      end do
      write(89,310)
  310 format('GO Rail Station Egress Summary'/
     *       'station,name,walk,bus,total')
      do k=1,max_stations
      if(stanum(k).eq.1) then
      ista=iequiv(k+max_izones)
      cumval=stasum2(k,1)+stasum2(k,2)
      write(89,309) ista,staname(k),(stasum2(k,k1),k1=1,2),
     *              cumval
      end if
      end do
c      end if
C
C     FILE CLEANUP
C
      IF(DEBUG) THEN
      CLOSE(91,STATUS='DELETE')
      CLOSE(93,STATUS='DELETE')
	    END IF
      CALL GETTIM(IHR,IMIN,ISEC,I100)
      WRITE(26,8004) IHR,IMIN,ISEC
      WRITE(*,8004) IHR,IMIN,ISEC
 8004 FORMAT(/' Program Completed: ',I2,':',I2,':',I2)
      end
