      PROGRAM MLOGIT
C      
C      
C---------------------------------------------------------------------
C            MODE CHOICE MODEL APPLICATION PROGRAM  
C            NESTED LOGIT MODE CHOICE                                                        
C
C            PARSONS BRINCKERHOFF,INC.
C            TORONTO, ON CANADA
C
C
C---------------------------------------------------------------------
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
      integer*2         time(4),iz,jz,ista,ix,itemp,itemp2
      integer*2         unit,orgzn,destzn,type,imode,iter
      integer*2         staznei(max_stations,max_izones,3,5)
      integer*2         zonesta(max_izones),units(15),msta(2)
      integer*2         bsta(2,2),btxfer(2,2),busmode(2,5,2)
      integer*2         wsta(2,5),wdsta(2,5),bdsta(2,2),crdsta(2)
      integer*2         zindur(max_stations),zindcr(max_stations)
      integer*2         osta(2,12),asta(2,12),tsta,tdsta,dsta(2,10)
      integer*2         psta(2,10),pdsta(2,10),ksta(2,10),kdsta(2,10)
      integer*2         cstat,asta2(2,12),prosta(2,12),prasta(2,12)
      integer*2         asta3(2,12),osta3(2,12),egrind(2,12),mseg
      integer*2         pdsta1(2,10),pdsta2(2,10),znesta(50,3)
      integer*2         kdsta1(2,10),kdsta2(2,10),pind(2,10),kind(2,10)
      integer*2         sc,sc2,c,t1,t2,jst,maxiz,maxjz,ajz,pjz,cjz
      integer*2         morg,mdst,matr,mind,parkind,rntlind,lbusind
      integer*2         airstap(50,50,2),airstaa(50,50,2)
      integer*2         airrntp(10,50,2),airrnta(10,50,2)
      integer*4         recno,nfilerr,recl,fileno,ich,ist
      integer*4         index,zone,diz,djz,ci,iiz,ik,jjz
      integer*4         orista,dessta,date(3),ijpairs(7)
      integer*4         prosta3(2,12),prasta3(2,12),pindex
      integer*4         prcstat,prgrsta1,prgrsta2
	    real*4            frow(4000,4000)
	    real*4            srow(1000,1000)
	    real*4            prow(1000)
	    real*4            arow(1000,4000)
      real*4            stasta(5,max_stations,max_stations)
      real*4            stazne(7,max_stations,max_izones)
      real*4            wdist(2,5),bdist(2,2),twalk(2,2),skim(2,18)
      real*4            ewalk(2,2),mwalk(7),fwalk(2),nowalk,fshar(2)
      real*4            znestau(50)
      real*4            hsdist(1000),hstime(1000)
      real*4            wait1(4000),wait2(4000)
      real*4            invehl(4000),invehg(4000)
      real*4            invehs(4000),inveh(4000)
      real*4            transf(4000),wlkacc(4000)
      real*4            wlkegr(4000),invehp(4000)
      real*4            invehr(4000)
      real*4            walktfr(4000),fare(4000)
      real*4            bstrwait1(4000),bstrwait2(4000)
      real*4            bstrinvehl(4000),bstrinvehs(4000)
      real*4            bstrtransf(4000),bstrwlkacc(4000)
      real*4            bstrwlkegr(4000),bstrinveh(4000)
      real*4            bstrwalktfr(4000),bstrfare(4000)
      real*4            rapdwait1(4000),rapdwait2(4000)
      real*4            rapdinvehl(4000),rapdinvehs(4000)
      real*4            rapdtransf(4000),rapdwlkacc(4000)
      real*4            rapdwlkegr(4000),rapdinvehr(4000)
      real*4            rapdwalktfr(4000),rapdfare(4000)
      real*4            rapdinveh(4000)
      real*4            gbcapac(4000),gbeawt(4000)
      real*4            gbcrowd(4000),gblunrel(4000)
      real*4            rbcapac(4000),rbeawt(4000)
      real*4            rbcrowd(4000),rblunrel(4000)
      real*4            bstrcapac(4000),bstreawt(4000)
      real*4            bstrcrowd(4000),bstrlunrel(4000)
      real*4            hdist(4000),htime(4000)
      real*4            tdist(4000),ttime(4000)
      real*4            toldist(4000),hov_toldist(4000)
      real*4            hovdist(4000),tollcost(4000)
      real*4            ytolds0,ytolds2,tollcost2p(4000)
      real*4            toldist2p(4000)
      real*4            hdist2p(4000),htime2p(4000)
      real*4            tdist2p(4000),ttime2p(4000)
      real*4            pertrp1(4000),pertrp2(4000)
      real*4            pertrp3(4000),tperin(6),tperson
      real*4            pertrp4(4000),pertrp5(4000)
      real*4            pertrp6(4000),pertrp,tmotor
      real*4            walktime(4000),biketime(4000)
      real*4            upxivt(1000,1000)   
      real*4            gorzfare(4000),ttczfare(4000)
      real*4            wutil(2,5),tutil,twdist,butil(2,2)
      real*4            pnrrat,pnrrat2,putil(2,10),hwydst(4000)
      real*4            pdrvutil,kdrvutil,lsum2rl
      real*4            kutil(2,10),wait1a,wait1b
      real*4            wlkbacc,wlkbegr,wgutl,dbutl,walkdt,trpd
      real*4            wlksacc,wlksegr,wsutl,wlkracc,wlkregr,wrutl
      real*4            tcrw,tcrb,tcrp,tcrk,tcrw1,tcrw2,tcrb1,tcrb2
      real*4            tcrp1,tcrp2,tcrp3,tcrp4,tcrk1,tcrk2,tcrk3,tcrk4
      real*4            turw,turb,turp,turk,turw1,turw2,turb1,turb2
      real*4            turp1,turp2,turp3,turp4,turk1,turk2,turk3,turk4
      real*4            tgb,tgbw,tgbd,wlkm2,util(100),eutil(100),tstr
      real*4            tdrv0n,tdrv0t,tdrv2nh,tdrv2nn,tdrv2th,tdrv2tn
      real*4            tdrv3nh,tdrv3nn,tdrv3th,tdrv3tn,tauto,tschbus
      real*4            util0nt,util0t,util2nt,util2nth,util2t    
      real*4            utul2th,util3nt,util3nth,util3t,util3th
      real*4            cwprob(3),cbprob(3),cpprob(5),ckprob(5),cuprob
      real*4            lscwlk,lscbus,lscpr,lsckr,lscr
      real*4            uwprob(3),ubprob(3),upprob(5),ukprob(5)
      real*4            lsuwlk,lsubus,lsupr,lsukr,lsurb,lsstr,lsrpd
      real*4            strprob(2),rpdprob(2),tstrw,tstrd,trpdw,trpdd
      real*4            gobprob(3),lsgob,trnprob(5),lstrn(6),tshare(3)
      real*4            lsbas(6),peduh(3),pedut(3),tprob(3)
      real*4            tfac,dut,edut,mfval,cfare,vskmtrp(4000,25)
      real*4            auttrp(4000,12),outtrp(4000,17)
      real*4            trntrp(4000,4000),airtrp(4000,6)
      real*4            crss(max_stations,max_stations)
      real*4            crurss(max_stations,max_stations)
      real*4            urcrss(max_stations,max_stations)
      real*4            urss(max_stations,max_stations)
      real*4            bcr(max_izones,max_stations)
      real*4            bur(max_izones,max_stations)
      real*4            crstaz(max_stations,max_izones)
      real*4            urstaz(max_stations,max_izones)
      real*4            gbstaz(max_stations,max_izones)
      real*4            dtran(max_izones,max_stations)
      real*4            wlkcr(max_izones,max_stations)
      real*4            wlkur(max_izones,max_stations)
      real*4            crwlk(max_stations,max_izones)
      real*4            crurwlk(max_stations,max_izones)
      real*4            urwlk(max_stations,max_izones)
      real*4            crurstaz(max_stations,max_izones)
      real*4            kkcbd(5),mutil,walk1,cutil,mutilp,mutilk
      real*4            bwalk1,bwalk2,bwalk,dbwlk,swalk1,swalk2,stwalk
      real*4            rwalk1,rwalk2,rwalk,daprob(2),lsda,srprob(2)
      real*4            p2prob(4),p3prob(4),ls2per,ls3per,lsshr
      real*4            lsauto,atprob(2),autexp,lsmot,motor(3),rativt
      real*4            ulogsum(6),ntxfer,invrat,zinvehg,zinvehgt
      real*4            ntxferb,ntxferr,kkdist
      real*4            zinvehl,zinvehs,zinvehp,zinvehn,cvalue
      real*4            walkt,walk2,utilwk,biket,bike1,bike2,utilbk
      real*4            nmprob(2),lsnmot,tnmot,tnmwk,tnmbk
      real*4            znelot(max_izones,50,2),znernt(max_izones,10,2)
      real*4            ctotal,nmadj
      real*4            lprkcst,pspaces,shcost,shfactr,shdprice
      real*4            k_pubprk,xdist,denome,lslote,lslot,ulottrn(50)
      real*4            utilot(50,2),ulotwlk(50),ulotshl(50)
      real*4            eutilot(50,4),eproblot(50,4),problot(50),lspub
      real*4            urntshl(10),urnttrn(10),eutlrnt(10,2)
      real*4            probrnt(10,2),utlatxi,utlarnt,utlalmo,utladrp
      real*4            utlaonc,utlaupe,eutlair(10),probair(10),lsprv
      real*4            tlimo,trntl,ttaxi,toncl,aesum(12),utlauber
      real*4            accuber,costuber,kwait,tuber,tupx,utiluber
      real*4            expuber,tuberacc
      real*4            vcratio,lottrn(50,50,7),vehtrp,lotrips(50,4)
      real*4            wcr(50,max_stations),lcr
      real*4            wur(50,max_stations),lur
      real*4            lcrss(max_stations,max_stations)
      real*4            lurss(max_stations,max_stations)
      real*4            lcrstaz(max_stations,50)
      real*4            lurstaz(max_stations,50)
      real*4            splogsum(max_zones,50)
      real*4            spperson(max_zones,50),sptotal(50)
      real*4            utilvs0,utilvs1
      real*4            upxsta(max_stations,max_stations)
      real*8            denom,tesum(39,7),stasum(1000,12)
      real*8            stasum2(max_stations,7),maxvalue
      real*8            stasum3(max_stations,3)
      real*8            stasum4(max_stations,3),stasum5(max_stations,2)
      real*8            stasum6(max_stations,2)
      real*8            tdrv0,tdrv2,tdrv3,ttrip(25,6),ptrip(25,6)
      real*8            gbusivt(101,2),gbusrat(101,2)
      real*8            gbustivt(101,2),rapdrat(101,2)
      real*8            txfers(6,6),tottrn(41,41,6),gorail(21,21)
      real*8            totaut(41,41,4)
      real*8            ttcsubway(21,21),gorailp1(21,21)
      real*8            gorailp2(21,21),gorailp3(21,21)
      real*8            avlttc(41,41,2),gorailp4(21,21)
      real*8            airtprob(50,50,7),airrprob(10,50,7),pnrtrp(12)
      real*8            nonmot(41,41,3),spdist(41,41,50),spsum(51,10)
      logical           csort,eventsp,gorchk,ttcchk
      character*13      name(2),tname(6),nname(3),autoname(4)
      character*25      ttcname(4),gorname(10)
      character*80      header
      data              coeff/-0.16650,-0.41292,-0.35298,-0.35798,
     *                    -1.44400,
     *                    -0.00883,-0.35964,0.001023,-0.41292,-0.14153,
     *                    -0.02500,-0.06250,-0.02500,-0.00127,-0.06250,
     *                    -0.06200,-0.06250,-0.09375,82*0.0/
      data              staname/1000*'No Station'/
      data              name/'GO Rail      ',
     *                       'TTC Subway   '/
      data             tname/'Total_Transit',
     *                       'GO Rail      ',
     *                       'TTC Subway   ',
     *                       'GO Bus       ',
     *                       'Bus_Streetcar',
     *                       'Rapid_Bus    '/
      data          autoname/'Total_Auto   ',
     *                       'Drive_Alone  ',
     *                       '2_Person     ',
     *                       '3+_Person    '/
      data             nname/'Walk         ',
     *                       'Bicycle      ',
     *                       'Total        '/
      data              ttcname/'Yonge-University-Spadina',
     *                          'Bloor-Danforth',
     *                          'Scarborough-RT','Sheppard'/
      data              gorname/'Lakeshore West','Milton','Kitchener',
     *                          'Not Used','Barrie','Richmond Hill',
     *                          'Stouffville','Not Used',
     *                          'Lakeshore East',
     *                          'Union Station'/
      maxiz=0
      maxjz=0
      maxvalue=0.0
      iter=0
      units=0
      pertrp=0.0
      pertrp1=0.0
      pertrp2=0.0
      pertrp3=0.0
      pertrp4=0.0
      pertrp5=0.0
      pertrp6=0.0
      gbusivt=0.0
      gbustivt=0.0
      gbusrat=0.0
      rapdrat=0.0
      crurss=0.0
      urcrss=0.0
      crss=0.0
      urss=0.0
      tottrn=0.0
      totaut=0.0
      nonmot=0.0
      avlttc=0.0
      invrat=0.0
      rativt=0.0
      stazne=0.0
      ptrip=0.0
      ptest=.false.
      airtrn=.false.
      eventsp=.false.
      ctotal=0.0
      aesum=0.0
      pnrtrp=0.0
      stasum=0.0
      stasum2=0.0
      stasum3=0.0
      stasum4=0.0
      stasum5=0.0
      tuberacc=0.0
      upxind=0
      upxsta=0.0
      stasum6=0.0
      bstrfare=0.0
      rapdfare=0.0
c
c     initiate program and obtain input file information
c
      call rctl
      if(airpass) call freqdist
C
      if(air) airtrn=.true.
      IF(air) cnvrge=.true.
c
      if(ptest) call bucket
c
c     read station file
c
      call stafile
c
c     special event generation
c
      if(spevent) call sptgen
c -------------------------------------------------------------
c     create station-to-station utility values
c -------------------------------------------------------------
      imode=1
      call station(stasta,imode,upxivt,gorchk)
      imode=2
      call station(stasta,imode,upxivt,ttcchk)
      if(gorchk.or.ttcchk) then
      write(26,8010) gorchk,ttcchk
      write(*,8010) gorchk,ttcchk
 8010 format(//' MTAMC 8010 (F) NO STATION-TO-STATION PATHS FOR',
     *       ' GO RAIL=',L1,' AND/OR TTC SUBWAY=',L1//)
      STOP 8010
      END IF
c ---------------------------------------------------------------
c     create station-to-zone utility values
c ---------------------------------------------------------------
      do imode=1,3
      call egress(stazne,imode,zonesta,staznei)
      end do
      open(172,file='mlogit.lock',status='unknown',
     *          form='formatted')
      write(172,8005)
 8005 format(' End of Station-to-Zone Utility Computations')
      close(172,status='keep')
C--------------------------------------------------------------
C     read & store zone to pearson parking lot time
C--------------------------------------------------------------
      if(air.and.airtrn) then
      do iz=1,max_izones
      type=1
      orgzn=iz
      unit=84
      call mfread(unit,type,orgzn,destzn,htime,srow,prow,arow)
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
      do ni=1,50
      if(pequiv(ni).le.0) cycle   
      k=pequiv(ni)
      znelot(iz,ni,1)=htime(k)
      znelot(iz,ni,2)=hdist(k)
      end do
      do ni=1,10
      k=idint(rntldata(ni,1))
      if(k.le.0) cycle
      k=equiv(k)
      znernt(iz,ni,1)=htime(k)
      znernt(iz,ni,2)=hdist(k)
      end do
      end do
      close(84,status='keep')
      close(85,status='keep')
      open(84,file=fhwy0(1),
     *       status='old',form='binary')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      end if
c ---------------------------------------------------------------
c     invoke person trip bucket rounding if cvalue >0
c ---------------------------------------------------------------
      cvalue=tvalue(1)+tvalue(2)+tvalue(3)+tvalue(4)+tvalue(5)+
     *       tvalue(6)
      if(air) cvalue=0.0
      if(spevent.or.eventsp.or.visitor) cvalue=0.0
      if(cvalue.gt.0.and.(.not.air).and.(.not.airtrn)) call bucket
c
      if(airtrn) then
      write(*,8334)
 8334 FORMAT(/,1X,'Preliminary Transit Computations'/
     *         1X,'--------------------------------'/)
      else
       if(spevent.or.visitor) then
       write(*,8335)
 8335 format(//' Preliminary LogSum Computations'/
     *         ' -------------------------------'/)
       else
      write(*,8002)
 8002 format(//' Mode Choice Model Computations'/
     *         ' ------------------------------'/)
      end if
      end if
c ===============================================================
c     origin zone loop
c ===============================================================
 1000 continue
      ijpairs=0
      crss=0.0
      urss=0.0
      crurss=0.0
      urcrss=0.0
      bcr=0.0
      bur=0.0
      dtran=0.0
      wlkcr=0.0
      wlkur=0.0
      crwlk=0.0
      urwlk=0.0
      crurwlk=0.0
      crstaz=0.0
      crurstaz=0.0
      urstaz=0.0
      gbstaz=0.0
      gorail=0.0
      gorailp1=0.0
      gorailp2=0.0
      gorailp3=0.0
      gorailp4=0.0
      ttcsubway=0.0
      if(air.and.(.not.airtrn)) write(*,8002)
      do iz=1,max_izones
	    auttrp=0.0
	    outtrp=0.0
	    airtrp=0.0
	    vskmtrp=0.0
      bstrfare=0.0
      rapdfare=0.0
      nk=IZ
      nk=mod(nk,100)
      if(nk.EQ.0) WRITE(*,8001) iz,iequiv(iz)
 8001 FORMAT(' Processing Origin Zone=',I5,' (',i5,')')
      if(.not.ioi(iz).and.(.not.airtrn)) cycle
C
C     Parking Lot or Rental Facility Zone?
C
      if(air.and.airtrn.and.(iz.le.max_izones)) then
      parkind=0
      rntlind=0
      do k=1,50
      if(pequiv(k).eq.iz) parkind=k
      if(k.le.10) then
      k1=idint(rntldata(k,1))
      if(k1.eq.iequiv(iz)) rntlind=k
      end if
      end do  
      if(parkind.eq.0.and.rntlind.eq.0) cycle
      end if
C---------------------------------------------------------------
C    INPUT PERSON TRIP BY MARKET SEGMENT
C---------------------------------------------------------------
      if((.not.spevent).and.(.not.eventsp).and.
     *   (.not.visitor).and.(.not.lsbase)) then
      if(cvalue.gt.0.0.and.(.not.air)) then
      read(140) iiz,ik,(pertrp1(jjz),jjz=1,max_izones)  
       if(iiz.ne.iz) then
       write(26,11120) iz,iiz
11120  format(' iz=',i4,' iiz=',i4)
       stop 11120
       end if
      if(ncats.gt.1) then
      read(140) iiz,ik,(pertrp2(jjz),jjz=1,max_izones)
      read(140) iiz,ik,(pertrp3(jjz),jjz=1,max_izones) 
      read(140) iiz,ik,(pertrp4(jjz),jjz=1,max_izones)  
      read(140) iiz,ik,(pertrp5(jjz),jjz=1,max_izones)
      read(140) iiz,ik,(pertrp6(jjz),jjz=1,max_izones)   
      end if
      else
      type=1
      orgzn=iz
      unit=34
      call mfread(unit,type,orgzn,destzn,pertrp1,srow,prow,arow)
      if(ncats.gt.1) then
      unit=35
      call mfread(unit,type,orgzn,destzn,pertrp2,srow,prow,arow)
      unit=36
      call mfread(unit,type,orgzn,destzn,pertrp3,srow,prow,arow)
      unit=37
      call mfread(unit,type,orgzn,destzn,pertrp4,srow,prow,arow)
      unit=38
      call mfread(unit,type,orgzn,destzn,pertrp5,srow,prow,arow)
      unit=39
      call mfread(unit,type,orgzn,destzn,pertrp6,srow,prow,arow)
      end if
      end if
      if(air.and.(.not.airtrn)) then
      ctotal=0.0
      do jz=1,max_izones
      if(joi(jz)) ctotal=ctotal+pertrp1(jz)
      end do
      if(ctotal.le.0) cycle
      end if
      end if
c ----------------------------------------------------
c     walk and bus access to GO Rail & TTC Subway
c ----------------------------------------------------     
      do imode=1,2
      call stabus(iz,bsta,bdist,twalk,btxfer,imode,skim,busmode,
     *                   wdist,wsta,ewalk)
      end do
c -------------------------------------------------------------------------
c     TTC Subway to GO Rail Station Utility
c -------------------------------------------------------------------------
      imode=1
      call access(iz,imode,stasta,wsta,wdist,bsta,bdist,znestau,znesta)
c -----------------------------------------------------------
c     top ten drive access stations for GO Rail & TTC Subway
c -----------------------------------------------------------
      type=3
      orgzn=iz
      destzn=max_zones
      unit=85
      call mfread(unit,type,orgzn,destzn,frow,srow,hsdist,arow)
      unit=84
      call mfread(unit,type,orgzn,destzn,frow,srow,hstime,arow)
      close(84,status='keep')
      close(85,status='keep')
      open(84,file=fhwy0(1),
     *       status='old',form='binary')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      imode=1
      call stadrv(iz,zindcr,imode,hsdist)
      imode=2
      call stadrv(iz,zindur,imode,hsdist)
c -----------------------------------------------------------
c     obtain walk access GO Bus/Premium skim matrices
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
C...PREMIUM IN-VEHICLE TIME
      unit=40
      call mfread(unit,type,orgzn,destzn,invehp,srow,prow,arow)
C...RAPID IN-VEHICLE TIME
      unit=128
      call mfread(unit,type,orgzn,destzn,invehr,srow,prow,arow)
c
      if(ccr) then
C....NODE CAPACITY PENALTY
      unit=112
      call mfread(unit,type,orgzn,destzn,gbcapac,srow,prow,arow)
C....EXTRA ADDED WAIT TIME 
      unit=113
      call mfread(unit,type,orgzn,destzn,gbeawt,srow,prow,arow)
C....CROWDING TIME
      unit=114
      call mfread(unit,type,orgzn,destzn,gbcrowd,srow,prow,arow)
C....LINK UNRELIABILITY
      unit=115
      call mfread(unit,type,orgzn,destzn,gblunrel,srow,prow,arow)
      end if
c -----------------------------------------------------------
c     obtain walk access Bus/Streetcar skim matrices
c -----------------------------------------------------------
C.....1ST WAIT TIME
      unit=70
      call mfread(unit,type,orgzn,destzn,bstrwait1,srow,prow,arow)
C.....TOTAL WAIT TIME
      unit=71
      call mfread(unit,type,orgzn,destzn,bstrwait2,srow,prow,arow)
C.....NUMBER OF BOARDINGS
      unit=72
      call mfread(unit,type,orgzn,destzn,bstrtransf,srow,prow,arow)
C.....FARE
      if(mfwkbustr(13).gt.0) then
      unit=330
      call mfread(unit,type,orgzn,destzn,bstrfare,srow,prow,arow)
      end if
C...LOCAL BUS IN-VEHICLE TIME
      unit=67
      call mfread(unit,type,orgzn,destzn,bstrinvehl,srow,prow,arow)
C...STREETCAR IN-VEHICLE TIME
      unit=68
      call mfread(unit,type,orgzn,destzn,bstrinvehs,srow,prow,arow)
C....CENTROID WALK ACCESS TIME 
      unit=74
      call mfread(unit,type,orgzn,destzn,bstrwlkacc,srow,prow,arow)
C....CENTROID WALK EGRESS TIME 
      unit=73
      call mfread(unit,type,orgzn,destzn,bstrwlkegr,srow,prow,arow)
C...WALK TIME TRANSFER
      unit=69
      call mfread(unit,type,orgzn,destzn,bstrwalktfr,srow,prow,arow)
c
      if(ccr) then
C....NODE CAPACITY PENALTY
      unit=120
      call mfread(unit,type,orgzn,destzn,bstrcapac,srow,prow,arow)
C....EXTRA ADDED WAIT TIME 
      unit=121
      call mfread(unit,type,orgzn,destzn,bstreawt,srow,prow,arow)
C....CROWDING TIME
      unit=122
      call mfread(unit,type,orgzn,destzn,bstrcrowd,srow,prow,arow)
C....LINK UNRELIABILITY
      unit=123
      call mfread(unit,type,orgzn,destzn,bstrlunrel,srow,prow,arow)
      end if
c -----------------------------------------------------------
c     obtain walk access Rapid skim matrices
c -----------------------------------------------------------
C.....1ST WAIT TIME
      unit=79
      call mfread(unit,type,orgzn,destzn,rapdwait1,srow,prow,arow)
C.....TOTAL WAIT TIME
      unit=80
      call mfread(unit,type,orgzn,destzn,rapdwait2,srow,prow,arow)
C.....NUMBER OF BOARDINGS
      unit=81
      call mfread(unit,type,orgzn,destzn,rapdtransf,srow,prow,arow)
C.....FARE
      if(mfwkrap(14).gt.0) then
      unit=331
      call mfread(unit,type,orgzn,destzn,rapdfare,srow,prow,arow)
      end if
C...LOCAL BUS IN-VEHICLE TIME
      unit=75
      call mfread(unit,type,orgzn,destzn,rapdinvehl,srow,prow,arow)
C...STREETCAR IN-VEHICLE TIME
      unit=76
      call mfread(unit,type,orgzn,destzn,rapdinvehs,srow,prow,arow)
C....CENTROID WALK ACCESS TIME 
      unit=83
      call mfread(unit,type,orgzn,destzn,rapdwlkacc,srow,prow,arow)
C....CENTROID WALK EGRESS TIME 
      unit=82
      call mfread(unit,type,orgzn,destzn,rapdwlkegr,srow,prow,arow)
C...WALK TIME TRANSFER
      unit=78
      call mfread(unit,type,orgzn,destzn,rapdwalktfr,srow,prow,arow)
C...RAPID BUS IN-VEHICLE TIME
      unit=77
      call mfread(unit,type,orgzn,destzn,rapdinvehr,srow,prow,arow)
c
      if(ccr) then
C....NODE CAPACITY PENALTY
      unit=116
      call mfread(unit,type,orgzn,destzn,rbcapac,srow,prow,arow)
C....EXTRA ADDED WAIT TIME 
      unit=117
      call mfread(unit,type,orgzn,destzn,rbeawt,srow,prow,arow)
C....CROWDING TIME
      unit=118
      call mfread(unit,type,orgzn,destzn,rbcrowd,srow,prow,arow)
C....LINK UNRELIABILITY
      unit=119
      call mfread(unit,type,orgzn,destzn,rblunrel,srow,prow,arow)
      end if
c -----------------------------------------------------------
c     obtain drive alone skim matrices
c -----------------------------------------------------------
C.....NON-TOLL TIME
      unit=84
      call mfread(unit,type,orgzn,destzn,htime,srow,prow,arow)
C.....NON-TOLL DISTANCE
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
C.....TOLL TIME
      unit=86
      call mfread(unit,type,orgzn,destzn,ttime,srow,prow,arow)
C.....TOLL DISTANCE
      unit=87
      call mfread(unit,type,orgzn,destzn,tdist,srow,prow,arow)
C.....TOLL LANE DISTANCE
      unit=88
      call mfread(unit,type,orgzn,destzn,toldist,srow,prow,arow)
C.....TOLL COST
      unit=89
      call mfread(unit,type,orgzn,destzn,tollcost,srow,prow,arow)
c -----------------------------------------------------------
c     obtain 2-person skim matrices
c -----------------------------------------------------------
C.....HOV NON-TOLL TIME
      unit=90
      call mfread(unit,type,orgzn,destzn,htime2p,srow,prow,arow)
C.....HOV NON-TOLL DISTANCE
      unit=91
      call mfread(unit,type,orgzn,destzn,hdist2p,srow,prow,arow)
C.....HOV DISTANCE
      unit=92
      call mfread(unit,type,orgzn,destzn,hovdist,srow,prow,arow)
C.....HOV TOLL TIME
      unit=93
      call mfread(unit,type,orgzn,destzn,ttime2p,srow,prow,arow)
C.....HOV TOLL DISTANCE
      unit=94
      call mfread(unit,type,orgzn,destzn,tdist2p,srow,prow,arow)
C.....HOV TOLL LANE DISTANCE
      unit=95
      call mfread(unit,type,orgzn,destzn,toldist2p,srow,prow,arow)
C.....HOV TOLL COST
      unit=96
      call mfread(unit,type,orgzn,destzn,tollcost2p,srow,prow,arow)
C.....HOV TOLL LANE DISTANCE
      unit=97
      call mfread(unit,type,orgzn,destzn,hov_toldist,srow,prow,arow)
C---------------------------------------------------------------
C    Non-Motoried skim matrices
C---------------------------------------------------------------
      IF(NMOT) THEN
C.....WALK TIME
      unit=142
      call mfread(unit,type,orgzn,destzn,walktime,srow,prow,arow)
C.....BICYCLE TIME
      unit=144
      call mfread(unit,type,orgzn,destzn,biketime,srow,prow,arow)
      END IF
C---------------------------------------------------------------
C    GO Rail & TTC Subway Zone-to-Zone Fare Matrices
C---------------------------------------------------------------
C.....GO Rail
      if(mfcrss(14).gt.0) then
      unit=328
      call mfread(unit,type,orgzn,destzn,gorzfare,srow,prow,arow)
      end if
C.....TTC Subway
      if(mfurss(11).gt.0) then
      unit=329
      call mfread(unit,type,orgzn,destzn,ttczfare,srow,prow,arow)
      end if
c ===============================================================
c     destination zone loop
c ===============================================================
      do jz=1,max_izones
	    if(.not.joi(jz)) cycle
	    osta3=0
	    asta3=0
	    ulogsum=0.0
	    tperson=0.0
C
C   Is Attraction Zone also a Pearson Parking Lot ? (pjz)
C   Is Attraction Zone a CTA Zone? (cjz)
C   is Attraction Zone a Parking Location (ajz)  
C
      pjz=0
      cjz=0
      ajz=0
      do ni=1,50
      if(pequiv(ni).eq.jz) pjz=ni
      if(ni.le.2) then
      if(ctazne(ni).eq.jz) cjz=ni
      end if
      if(aequiv(ni).eq.jz) ajz=ni
      end do
C
C     Special Event Person Trips
C
      if(eventsp) then
      pertrp1(jz)=0.0
      do k=1,maxsp
      if(jz.eq.equiv(spzone(k))) pertrp1(jz)=pertrp1(jz)+spperson(iz,k)
      end do
      if(pertrp1(jz).le.0.0) cycle
      end if
C
C    STORE INPUT PERSON TRIPS
C
      tperin(1)=pertrp1(JZ)
      tperin(2)=pertrp2(JZ)
      tperin(3)=pertrp3(JZ)
      tperin(4)=pertrp4(JZ)
      tperin(5)=pertrp5(JZ)
      tperin(6)=pertrp6(JZ)
      tperson=tperin(1)+tperin(2)+tperin(3)+tperin(4)+
     *        tperin(5)+tperin(6)
C....................................................................
      IF(tperson.le.0.0.and.(debug)) THEN
      tperin(1)=100.0
      tperin(2)=100.0
      tperin(3)=100.0
      tperin(4)=100.0
      tperin(5)=100.0
      tperin(6)=100.0      
      tperson=tperin(1)+tperin(2)+tperin(3)+tperin(4)+
     *        tperin(5)+tperin(6)
      end if
      if(debug) then
      WRITE(26,9021) tperson,tperin(1),tperin(2),tperin(3),
     *               tperin(4),tperin(5),tperin(6)
 9021 FORMAT(/1X,'MARKET SEGMENTATION COMPUTATIONS'/
     *       1X,'----------------------------------'/
     *       1X,'PERSON TRIPS    TOTAL=',F10.2/
     *       1X,'PERSON TRIPS MARKET 1=',F10.2/
     *       1X,'PERSON TRIPS MARKET 2=',F10.2/
     *       1X,'PERSON TRIPS MARKET 3=',F10.2/
     *       1X,'PERSON TRIPS MARKET 4=',F10.2/
     *       1X,'PERSON TRIPS MARKET 5=',F10.2/
     *       1X,'PERSON TRIPS MARKET 6=',F10.2/)
      END IF
C....................................................................
      if(tperson.le.0.0.and.(.not.lsbase).and.(.not.airtrn).and.
     *  (.not.spevent).and.(.not.visitor)) cycle
      ijpairs(7)=ijpairs(7)+1
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
      twdist=WDIST(IMODE,ISTA)
      WSTA(IMODE,ISTA)=WSTA(IMODE,(ISTA-1))
      WDSTA(IMODE,ISTA)=WDSTA(IMODE,(ISTA-1))
      WUTIL(IMODE,ISTA)=WUTIL(IMODE,(ISTA-1))
      WDIST(IMODE,ISTA)=WDIST(IMODE,(ISTA-1))
      WSTA(IMODE,(ISTA-1))=TSTA
      WDSTA(IMODE,(ISTA-1))=TDSTA
      WUTIL(IMODE,(ISTA-1))=TUTIL
      WDIST(IMODE,(ISTA-1))=twdist
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
c
c     station level utilities for 4-part path analysis
c
      if(wsta(imode,ista).eq.0) cycle
      if(wutil(imode,ista).eq.0.0) cycle
      call part4(jz,wsta(imode,ista),stasta,stazne,imode,
     *           matr,morg,mdst,mind,mutil)
      if(matr.le.0) cycle
      walk1=stazne(3,(wdsta(imode,ista)-max_izones),jz)*coeff(7)
      cutil=wutil(imode,ista)+walk1
c............................................................
      if(debug) write(26,9022) wutil(imode,ista),walk1,cutil,mutil
 9022 format(/' 3 AND 4 PART PATH COMPARISON'/
     *        ' ----------------------------'/
     *        ' 3 PART       UTILITY=',F10.5/
     *        ' WALK EGRESS  UTILITY=',F10.5/
     *        ' TOTAL 3 PART UTILITY=',F10.5/
     *        ' 4 PART       UTILITY=',F10.5/)
c...........................................................
      if(mutil.gt.cutil) then
      wutil(imode,ista)=mutil
      asta(imode,ista)=matr+max_izones
      osta3(imode,ista)=morg+max_izones
      asta3(imode,ista)=mdst+max_izones
      egrind(imode,ista)=mind
c..........................................................
      if(debug) write(26,9023) mutil,iequiv(asta(imode,ista)),
     *          iequiv(osta3(imode,ista)),iequiv(asta3(imode,ista)),
     *          egrind(imode,ista) 
 9023 format(' URBAN RAIL PATH SELECTED'/
     *       ' ------------------------'/
     *       ' REVISED UTILITY               =',F10.5/
     *       ' GO RAIL         EGRESS STATION=',i5/
     *       ' TTC SUBWAY RAIL ACCESS STATION=',i5/
     *       ' TTC SUBWAY RAIL EGRESS STATION=',i5/
     *       ' FINAL           EGRESS MODE   =',I1/)
c...........................................................
      end if
	    end do
C-------------------------------------------------
C....BUS ACCESS STATION UTILITY COMPUTATION
c-------------------------------------------------
       do ista=1,2
       IMODE=1
       msta(ista)=0
       IF(DEBUG) WRITE(26,41) ISTA,NAME(IMODE)
   41  FORMAT(/' BUS ACCESS #',I1,' ---> ',a13/
     *         ' =================================')
	    CALL EGRSTA(JZ,BSTA(imode,ista),STASTA,STAZNE,
     *   BDSTA(imode,ista),IMODE,ZONESTA)
      CALL BUTL(ista,IZ,JZ,BDIST(imode,ista),BSTA(imode,ista),
     *  BDSTA(imode,ista),BUTIL(imode,ista),STASTA,STAZNE,IMODE)
       OSTA(IMODE,(ISTA+2))=BSTA(IMODE,ISTA)
       ASTA(IMODE,(ISTA+2))=BDSTA(IMODE,ISTA)
c
c     station level utilities for 4-part path analysis
c
      if(bsta(imode,ista).eq.0) go to 50
      if(butil(imode,ista).eq.0.0) go to 50
      call part4(jz,bsta(imode,ista),stasta,stazne,imode,
     *           matr,morg,mdst,mind,mutil)
      if(matr.le.0) go to 50
      walk1=stazne(3,(bdsta(imode,ista)-max_izones),jz)*coeff(7)
      cutil=butil(imode,ista)+walk1
      mutil=mutil+bdist(imode,ista)
c............................................................
      if(debug) write(26,9022) butil(imode,ista),walk1,cutil,mutil
c...........................................................
      if(mutil.gt.cutil) then
      butil(imode,ista)=mutil
      asta(imode,(ista+2))=matr+max_izones
      osta3(imode,(ista+2))=morg+max_izones
      asta3(imode,(ista+2))=mdst+max_izones
      egrind(imode,(ista+2))=mind
c..........................................................
      if(debug) write(26,9023) mutil,iequiv(asta(imode,(ista+2))),
     *          iequiv(osta3(imode,(ista+2))),
     *          iequiv(asta3(imode,(ista+2))),
     *          egrind(imode,(ista+2)) 
c...........................................................
      else
      if(debug) write(26,9123)
 9123 format(' URBAN RAIL EGRESS PATH NOT SELECTED')
c
c     Evaluate TTC Subway Path for transit access
c
      if(ttcacc.and.ista.eq.1) then
      call teval(jz,ista,znesta,znestau,bsta,bdsta,butil,
     *           stasta,stazne,msta(ista),crdsta(ista),mutil)
      end if
      if(msta(ista).gt.0) then
      if(debug) write(26,9056) msta(ista),
     *      iequiv(zneref(msta(ista),1)),
     *        staname(zneref(msta(ista),1)-max_izones),
     *      iequiv(crdsta(ista)),staname(crdsta(ista)-max_izones),
     *      iequiv(znesta(msta(ista),1)),
     *        staname(znesta(msta(ista),1)-max_izones),
     *      iequiv(znesta(msta(ista),2)),
     *        staname(znesta(msta(ista),2)-max_izones),
     *      butil(1,ista),mutil
 9056 format(/' TTC SUBWAY/GO RAIL EQUIVALENCE #',i2,' SELECTED'/
     *        ' -----------------------------------------------'/
     *        ' GO RAIL ACCESS STATION=',I4,1X,A20/
     *        ' GO RAIL EGRESS STATION=',I4,1X,A20/
     *        ' TTC     ACCESS STATION=',I4,1X,A20/
     *        ' TTC     EGRESS STATION=',I4,1X,A20/
     *        ' GO RAIL BUS    UTILITY=',F10.5/
     *        ' TTC/GO RAIL    UTILITY=',F10.5/)
      butil(1,ista)=mutil
      if(iequiv(znesta(msta(ista),2)).eq.TTCUNION) 
     * butil(1,ista)=butil(1,ista)+
     * (acnst(12)/(LSUM2CR*LSUM1TRN*LSUM3CW)) 
      if(iequiv(znesta(msta(ista),2)).ne.TTCUNION) then
      if(iequiv(znesta(msta(ista),2)).eq.BDMAIN) then
      butil(1,ista)=butil(1,ista)-(5.0/(LSUM2CR*LSUM1TRN*LSUM3CW))
      else
      butil(1,ista)=butil(1,ista)-(2.5/(LSUM2CR*LSUM1TRN*LSUM3CW))      
      end if
      end if
      osta(imode,(ista+2))=zneref(msta(ista),1)
      asta(imode,(ista+2))=crdsta(ista)
      end if
      end if
   50 IMODE=2
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
      close(85,status='keep')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      unit=85
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
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,hstime,hsdist,
     *  IMODE,PNRRAT,PNRRAT2,hwydst,pdrvutil)
c     CRPNR(IX)=PNRRAT
c     CRPNR2(IX)=PNRRAT2
      CALL KUTL(IX,IZ,JZ,ZINDCR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,hstime,hsdist,
     *  IMODE,kdrvutil)
c
c     station level utilities for 4-part path analysis
c
      if(zindcr(ix).eq.0) go to 342
      call part4(jz,(zindcr(ix)+max_izones),stasta,stazne,imode,
     *           matr,morg,mdst,mind,mutil)
      if(matr.le.0) go to 342
      walk1=stazne(3,(dsta(imode,ix)-max_izones),jz)*coeff(7)
c
c     park-and-ride evaluation
c
      if(putil(imode,ix).ne.0.0) then
      cutil=putil(imode,ix)+walk1
      mutilp=mutil+pdrvutil
c............................................................
      if(debug) write(26,9025)
 9025 format(/' PARK-N-RIDE')
      if(debug) write(26,9024) putil(imode,ix),walk1,cutil,mutil,
     *          pdrvutil,mutilp
 9024 format( ' 3 AND 4 PART PATH COMPARISON'/
     *        ' ----------------------------'/
     *        ' 3 PART       UTILITY=',F10.5/
     *        ' WALK EGRESS  UTILITY=',F10.5/
     *        ' TOTAL 3 PART UTILITY=',F10.5/
     *        ' 4 PART       UTILITY=',F10.5/
     *        ' DRIVE ACCESS UTILITY=',F10.5/
     *        ' TOTAL 4 PART UTILITY=',F10.5/)
c...........................................................
      pdsta1(imode,ix)=0
      pdsta2(imode,ix)=0
      pind(imode,ix)=0
      if(mutilp.gt.cutil) then
      putil(imode,ix)=mutilp
      dsta(imode,ix)=matr+max_izones
      pdsta1(imode,ix)=morg+max_izones
      pdsta2(imode,ix)=mdst+max_izones
      pind(imode,ix)=mind
c..........................................................
      if(debug) write(26,9023) mutilp,iequiv(dsta(imode,ix)),
     *          iequiv(pdsta1(imode,ix)),iequiv(pdsta2(imode,ix)),
     *          pind(imode,ix) 
c...........................................................
      end if
      end if
c
c     kiss-and-ride evaluation
c
      if(kutil(imode,ix).ne.0.0) then
      mutilk=mutil+kdrvutil
      cutil=kutil(imode,ix)+walk1
c..................................................................
      if(debug) write(26,9026)
 9026 format(/' KISS-N-RIDE')
      if(debug) write(26,9024) kutil(imode,ix),walk1,cutil,mutil,
     *          kdrvutil,mutilk
c.................................................................
      kdsta1(imode,ix)=0
      kdsta2(imode,ix)=0
      kind(imode,ix)=0
      if(mutilp.gt.cutil) then
      kutil(imode,ix)=mutilk
      kdsta1(imode,ix)=morg+max_izones
      kdsta2(imode,ix)=mdst+max_izones
      kind(imode,ix)=mind
c..........................................................
      if(debug) write(26,9023) mutilk,iequiv(dsta(imode,ix)),
     *          iequiv(kdsta1(imode,ix)),iequiv(kdsta2(imode,ix)),
     *          kind(imode,ix) 
c...........................................................
      end if
      end if
  342 continue
C...URBAN RAIL
      IMODE=2
      IF(DEBUG) WRITE(26,341) IX,NAME(IMODE)
      ITEMP=ZINDUR(IX)+MAX_IZONES
      CALL EGRSTA(JZ,ITEMP,STASTA,STAZNE,ITEMP2,
     *          IMODE,ZONESTA)
      DSTA(IMODE,IX)=ITEMP2
      CALL PUTL(IX,IZ,JZ,ZINDUR(IX),
     *  DSTA(imode,IX),PUTIL(imode,IX),STASTA,STAZNE,hstime,hsdist,
     *  IMODE,PNRRAT,PNRRAT2,hwydst,pdrvutil)
      CALL KUTL(IX,IZ,JZ,ZINDUR(IX),
     *  DSTA(imode,IX),KUTIL(imode,IX),STASTA,STAZNE,hstime,hsdist,
     *  IMODE,kdrvutil)
      end do
C.....SORT DRIVE ACCESS STATIONS - GO RAIL
      IMODE=1
      CALL USORT(1,PSTA,ZINDCR,DSTA,PDSTA,PUTIL,IMODE,
     *           PDSTA1,PDSTA2,PIND)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      OSTA3(imode,5)=PDSTA1(imode,1)
      OSTA3(imode,6)=PDSTA1(imode,2)
      OSTA3(imode,7)=PDSTA1(imode,3)
      OSTA3(imode,8)=PDSTA1(imode,4)
      ASTA3(imode,5)=PDSTA2(imode,1)
      ASTA3(imode,6)=PDSTA2(imode,2)
      ASTA3(imode,7)=PDSTA2(imode,3)
      ASTA3(imode,8)=PDSTA2(imode,4)
      EGRIND(imode,5)=PIND(imode,1)
      EGRIND(imode,6)=PIND(imode,2)
      EGRIND(imode,7)=PIND(imode,3)
      EGRIND(imode,8)=PIND(imode,4)
      CALL USORT(2,KSTA,ZINDCR,DSTA,KDSTA,KUTIL,IMODE,
     *           KDSTA1,KDSTA2,KIND)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
      OSTA3(imode,9)=KDSTA1(imode,1)
      OSTA3(imode,10)=KDSTA1(imode,2)
      OSTA3(imode,11)=KDSTA1(imode,3)
      OSTA3(imode,12)=KDSTA1(imode,4)
      ASTA3(imode,9)=KDSTA2(imode,1)
      ASTA3(imode,10)=KDSTA2(imode,2)
      ASTA3(imode,11)=KDSTA2(imode,3)
      ASTA3(imode,12)=KDSTA2(imode,4)
      EGRIND(imode,9)=KIND(imode,1)
      EGRIND(imode,10)=KIND(imode,2)
      EGRIND(imode,11)=KIND(imode,3)
      EGRIND(imode,12)=KIND(imode,4)
C.....SORT DRIVE ACCESS STATIONS - TTC SUBWAY
      IMODE=2
      CALL USORT(1,PSTA,ZINDUR,DSTA,PDSTA,PUTIL,IMODE,
     *           PDSTA1,PDSTA2,PIND)
      OSTA(imode,5)=PSTA(imode,1)
      OSTA(imode,6)=PSTA(imode,2)
      OSTA(imode,7)=PSTA(imode,3)
      OSTA(imode,8)=PSTA(imode,4)
      ASTA(imode,5)=PDSTA(imode,1)
      ASTA(imode,6)=PDSTA(imode,2)
      ASTA(imode,7)=PDSTA(imode,3)
      ASTA(imode,8)=PDSTA(imode,4)
      CALL USORT(2,KSTA,ZINDUR,DSTA,KDSTA,KUTIL,IMODE,
     *           KDSTA1,KDSTA2,KIND)
      OSTA(imode,9)=KSTA(imode,1)
      OSTA(imode,10)=KSTA(imode,2)
      OSTA(imode,11)=KSTA(imode,3)
      OSTA(imode,12)=KSTA(imode,4)
      ASTA(imode,9)=KDSTA(imode,1)
      ASTA(imode,10)=KDSTA(imode,2)
      ASTA(imode,11)=KDSTA(imode,3)
      ASTA(imode,12)=KDSTA(imode,4)
C-------------------------------------------------------------------
C            WALK --> BUS/STREETCAR UTILITY COMPUTATION                   |
C-------------------------------------------------------------------
      WAIT1A=0.0
      WAIT1B=0.0
      BSTRINVEH(JZ)=BSTRINVEHL(JZ)+BSTRINVEHS(JZ)
      WLKSACC=BSTRWLKACC(JZ)
      WLKSEGR=BSTRWLKEGR(JZ)
      BSTRFARE(JZ)=BFARETTC*100.0 + BSTRFARE(JZ)*100.0
      IF(BSTRINVEH(JZ).GT.0.0) THEN
      WAIT1A=AMIN1(BSTRWAIT1(JZ),WAITLT)
      WAIT1B=DIM(BSTRWAIT1(JZ),WAITLT)
      NTXFERB=BSTRTRANSF(JZ)-1.0
      IF(NTXFERB.GT.0.01) THEN
      NTXFERB=NTXFERB+COEFF(26)
      ELSE
      NTXFERB=0.0
      END IF
      WSUTL=COEFF(11)*BSTRINVEH(JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + 
     *        COEFF(15)*(BSTRWAIT2(JZ)-BSTRWAIT1(JZ)) +
     *        COEFF(17)*BSTRWALKTFR(JZ) +
     *        COEFF(34)*(NTXFERB**COEFF(25)) +
     *        COEFF(20)*BSTRCAPAC(jz) + 
     *        COEFF(21)*BSTREAWT(jz) +
     *        COEFF(22)*BSTRCROWD(jz) + 
     *        COEFF(23)*BSTRLUNREL(jz)
      WSUTL=WSUTL/(LSUM1TRN*LSUM2SB)
      ELSE
      WSUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9044) BSTRINVEH(JZ),BSTRINVEHL(JZ),BSTRINVEHS(JZ),
     *               BSTRFARE(JZ),BSTRTRANSF(JZ),BSTRWAIT1(JZ),
     *               WAIT1A,WAIT1B,BSTRWAIT2(JZ),BSTRWALKTFR(JZ),
     *               WLKSACC,WLKSEGR,
     *               BSTRCAPAC(JZ),BSTREAWT(JZ),BSTRCROWD(JZ),
     *               BSTRLUNREL(JZ),
     *               WSUTL
 9044 FORMAT(//1X,'Walk --> Bus/Steetcar Level-Of-Service Data'/
     *       1X,  '-------------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
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
     *       1X,'BUS   EGRESS   WALK  =',F8.2//
     *       1X,'NODE CAPACITY PENALTY=',F8.2/
     *       1X,'EXTRA ADDED WAIT TIME=',F8.2/
     *       1X,'CROWDING TIME        =',F8.2/
     *       1X,'LINK UNREL TIME      =',F8.2//
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C-------------------------------------------------------------------
C            WALK --> RAPID UTILITY COMPUTATION                   |
C-------------------------------------------------------------------
      WAIT1A=0.0
      WAIT1B=0.0
      RAPDINVEH(JZ)=RAPDINVEHL(JZ)+RAPDINVEHS(JZ)+RAPDINVEHR(JZ)
      WLKRACC=RAPDWLKACC(JZ)
      WLKREGR=RAPDWLKEGR(JZ)
      RAPDFARE(JZ)=BFARETTC*100.0 + RAPDFARE(JZ)*100.0
      IF(RAPDINVEHR(JZ).GT.0.0) THEN
      WAIT1A=AMIN1(RAPDWAIT1(JZ),WAITLT)
      WAIT1B=DIM(RAPDWAIT1(JZ),WAITLT)
      NTXFERR=RAPDTRANSF(JZ)-1.0
      IF(NTXFERR.GT.0.01) THEN
      NTXFERR=NTXFERR+COEFF(26)
      ELSE
      NTXFERR=0.0
      END IF
      WRUTL=COEFF(11)*RAPDINVEH(JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + 
     *        COEFF(15)*(RAPDWAIT2(JZ)-RAPDWAIT1(JZ)) +
     *        COEFF(17)*RAPDWALKTFR(JZ) +
     *        COEFF(35)*(NTXFERR**COEFF(25)) +
     *        COEFF(20)*RBCAPAC(jz) + 
     *        COEFF(21)*RBEAWT(jz) +
     *        COEFF(22)*RBCROWD(jz) + 
     *        COEFF(23)*RBLUNREL(jz)
      WRUTL=WRUTL/(LSUM1TRN*LSUM2RB)
      ELSE
      WRUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9045) RAPDINVEH(JZ),RAPDINVEHL(JZ),RAPDINVEHS(JZ),
     *               RAPDINVEHR(JZ),
     *               RAPDFARE(JZ),RAPDTRANSF(JZ),RAPDWAIT1(JZ),
     *               WAIT1A,WAIT1B,RAPDWAIT2(JZ),RAPDWALKTFR(JZ),
     *               WLKSACC,WLKSEGR,
     *               RBCAPAC(JZ),RBEAWT(JZ),RBCROWD(JZ),
     *               RBLUNREL(JZ),
     *               WRUTL
 9045 FORMAT(//1X,'Walk --> Rapid Level-Of-Service Data'/
     *       1X,  '-------------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'LOCAL BUS IN-VEHICLE =',F8.2/
     *       1X,'STREETCAR IN-VEHICLE =',F8.2/
     *       1X,'RAPID     IN-VEHICLE =',F8.2/
     *       1X,'BUS   FARE           =',F8.2/
     *       1X,'BUS   BOARDINGS      =',F8.2/
     *       1X,'BUS   1ST WAIT       =',F8.2/
     *       1X,'BUS   1ST WAIT (<5)  =',F8.2/
     *       1X,'BUS   1ST WAIT (>5)  =',F8.2/
     *       1X,'BUS   TOTAL WAIT     =',F8.2/
     *       1X,'BUS   TRANSFER WALK  =',F8.2/
     *       1X,'BUS   ACCESS   WALK  =',F8.2/
     *       1X,'BUS   EGRESS   WALK  =',F8.2//
     *       1X,'NODE CAPACITY PENALTY=',F8.2/
     *       1X,'EXTRA ADDED WAIT TIME=',F8.2/
     *       1X,'CROWDING TIME        =',F8.2/
     *       1X,'LINK UNREL TIME      =',F8.2//
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C-------------------------------------------------------------------
C            WALK --> GO BUS UTILITY COMPUTATION                   |
C-------------------------------------------------------------------
      WAIT1A=0.0
      WAIT1B=0.0
      WGUTL=0.0
      INVEH(JZ)=INVEHL(JZ)+INVEHG(JZ)+INVEHS(JZ)+INVEHP(JZ)+INVEHR(JZ)
      WLKBACC=WLKACC(JZ)
      WLKBEGR=WLKEGR(JZ)
      FARE(JZ)=FARE(JZ)*100.0
      IF(INVEHG(JZ).GT.0.0.OR.INVEHP(JZ).GT.0.0) THEN
      WAIT1A=AMIN1(WAIT1(JZ),WAITLT)
      WAIT1B=DIM(WAIT1(JZ),WAITLT)
      NTXFER=TRANSF(JZ)-1.0
      IF(NTXFER.GT.0.01) THEN
      NTXFER=NTXFER+COEFF(26)
      ELSE
      NTXFER=0.0
      END IF
      INVRAT=1.0-((INVEHG(JZ)+INVEHP(JZ))/INVEH(JZ))
      RATIVT=LOG((INVEHG(JZ)+INVEHP(JZ))/INVEH(JZ))
      WGUTL=COEFF(11)*INVEH(JZ) + COEFF(12)*WAIT1A +
     *        COEFF(13)*WAIT1B + COEFF(15)*(WAIT2(JZ)-WAIT1(JZ)) +
     *        COEFF(17)*WALKTFR(JZ) + KGOBUSW +
     *        COEFF(19)*(NTXFER**COEFF(25)) +
     *        COEFF(36)*INVRAT +
     *        COEFF(40)*RATIVT +
     *        COEFF(20)*GBCAPAC(jz) + 
     *        COEFF(21)*GBEAWT(jz) +
     *        COEFF(22)*GBCROWD(jz) + 
     *        COEFF(23)*GBLUNREL(jz)
      WGUTL=WGUTL/(LSUM1TRN*LSUM2GB)
      ELSE
      WGUTL=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9043) INVEH(JZ),INVEHG(JZ),INVEHL(JZ),INVEHS(JZ),
     *               INVEHP(JZ),INVEHR(JZ),
     *               FARE(JZ),TRANSF(JZ),WAIT1(JZ),
     *               WAIT1A,WAIT1B,WAIT2(JZ),WALKTFR(JZ),
     *               WLKBACC,WLKBEGR,
     *               GBCAPAC(JZ),GBEAWT(JZ),GBCROWD(JZ),
     *               GBLUNREL(JZ),INVRAT,RATIVT,
     *               WGUTL
 9043 FORMAT(//1X,'Walk --> GO Bus Level-Of-Service Data'/
     *       1X,  '-------------------------------------'//
     *       1X,'TOTAL IVTT           =',F8.2/
     *       1X,'GO BUS IN-VEHICLE    =',F8.2/
     *       1X,'LOCAL BUS IN-VEHICLE =',F8.2/
     *       1X,'STREETCAR IN-VEHICLE =',F8.2/
     *       1X,'PREMIUM   IN-VEHICLE =',F8.2/
     *       1X,'RAPID     IN-VEHICLE =',F8.2/
     *       1X,'BUS   FARE           =',F8.2/
     *       1X,'BUS   BOARDINGS      =',F8.2/
     *       1X,'BUS   1ST WAIT       =',F8.2/
     *       1X,'BUS   1ST WAIT (<5)  =',F8.2/
     *       1X,'BUS   1ST WAIT (>5)  =',F8.2/
     *       1X,'BUS   TOTAL WAIT     =',F8.2/
     *       1X,'BUS   TRANSFER WALK  =',F8.2/
     *       1X,'BUS   ACCESS   WALK  =',F8.2/
     *       1X,'BUS   EGRESS   WALK  =',F8.2//
     *       1X,'NODE CAPACITY PENALTY=',F8.2/
     *       1X,'EXTRA ADDED WAIT TIME=',F8.2/
     *       1X,'CROWDING TIME        =',F8.2/
     *       1X,'LINK UNREL TIME      =',F8.2//
     *       1X,'IVTT 1-RATIO         =',F8.4/
     *       1X,'IVTT LOG RATIO       =',F8.4/
     *       1X,'MODE CHOICE MODEL UTILITY=',F10.5/)
      END IF
C.................................................................
C---------------------------------------------------------------------
C            DRIVE --> GO BUS UTILITY COMPUTATION                    |
C---------------------------------------------------------------------
      CSTAT=MAX_ZONES
      IMODE=3
	    CALL DRVNEW(JZ,DBUTL,hsdist,hstime,CSTAT,IMODE,
     *            STAZNE,HWYDST)
	    IT=CSTAT-MAX_IZONES
	    WALKDT=STAZNE(3,IT,JZ)
      NTXFER=STAZNE(1,IT,JZ)-1.0
      IF(NTXFER.GT.0.01) THEN
      NTXFER=NTXFER+COEFF(26)
      ELSE
      NTXFER=0.0
      END IF
	    IF(DBUTL.NE.0.0) THEN
      DBUTL=DBUTL+COEFF(24)*(NTXFER**COEFF(25))
	    DBUTL=(DBUTL+KGOBUSD)/(LSUM1TRN*LSUM2GB)
	    END IF
C ----------------------------------------------------------------------------
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR WALK
C
      IF(NMOT) THEN
          WALKT=WALKTIME(JZ)
          WALK1=AMIN1(WALKT,MWALKT)
          WALK2=DIM(WALKT,MWALKT)
          UTILWK= MWALK1*WALK1+ MWALK2*WALK2
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR BICYCLE
C
          BIKET=BIKETIME(JZ)
          BIKE1=AMIN1(BIKET,MBIKET)
          BIKE2=DIM(BIKET,MBIKET)
          UTILBK=MBIKE1*BIKE1+MBIKE2*BIKE2
      ELSE
      UTILWK=0.0
      UTILBK=0.0
      END IF
C -------------------------------------------------------------------------------
C
C.................................................................
      IF(DEBUG.AND.NMOT) THEN
      WRITE(26,9028) IEQUIV(IZ),IEQUIV(JZ),
     *               WALKT,WALK1,WALK2,BIKET,BIKE1,BIKE2,
     *               UTILWK,UTILBK
 9028 FORMAT(/1X,'NON-MOTORIZED MODE UTILITY COMPUTATIONS'/
     *       1X,'----------------------------------------'/
     *       1X,'ORIGIN ZONE                =',I10/
     *       1X,'DESTINATION ZONE           =',I10/
     *       1X,'TOTAL  WALK MODE TIME      =',F10.2/
     *       1X,'FIRST  WALK MODE TIME      =',F10.2/
     *       1X,'SECOND WALK MODE TIME      =',F10.2/
     *       1X,'TOTAL  BIKE MODE TIME      =',F10.2/
     *       1X,'FIRST  BIKE MODE TIME      =',F10.2/
     *       1X,'SECOND BIKE MODE TIME      =',F10.2/
     *       1X,'WALK   MODE UTILITY        =',F10.5/
     *       1X,'BIKE   MODE UTILITY        =',F10.5//)
      END IF
C..................................................................... 	    
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
      PROSTA3=0
      PRASTA3=0
      PRCSTAT=0
      DO 18 K=1,12
      DO 18 L=1,2
      IF(OSTA(L,K).LE.0) OSTA(L,K)=MAX_ZONES
      IF(ASTA(L,K).LE.0) ASTA(L,K)=MAX_ZONES
      IF(OSTA3(L,K).LE.0) OSTA3(L,K)=MAX_ZONES
      IF(ASTA3(L,K).LE.0) ASTA3(L,K)=MAX_ZONES
      DC=ASTA(L,K)-MAX_IZONES
      IC=STAIND(DC,JZ)
      IF(IC.LT.1) IC=1
      ASTA2(L,K)=IC+(L-1)*4
      PROSTA(L,K)=IEQUIV(OSTA(L,K))
      PRASTA(L,K)=IEQUIV(ASTA(L,K))
      IF(OSTA3(L,K).GT.0) PROSTA3(L,K)=IEQUIV(OSTA3(L,K))
      IF(ASTA3(L,K).GT.0) PRASTA3(L,K)=IEQUIV(ASTA3(L,K))
   18 CONTINUE
      IF(CSTAT.GT.0) PRCSTAT=IEQUIV(CSTAT)
C
C   CHECK PNR & KNR UTILITIES
C
      DO L=1,2
      DO K=1,4
      IF(PUTIL(L,K).LT.-999.0) PUTIL(L,K)=0.0
      IF(KUTIL(L,K).LT.-999.0) KUTIL(L,K)=0.0
      END DO
      END DO
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9000) IEQUIV(IZ),IEQUIV(JZ),WUTIL(1,1),WUTIL(1,2),
     *               BUTIL(1,1),BUTIL(1,2),(PUTIL(1,K1),K1=1,4),
     *               (KUTIL(1,K2),K2=1,4),WUTIL(2,1),WUTIL(2,2),
     *               BUTIL(2,1),BUTIL(2,2),(PUTIL(2,K3),K3=1,4),
     *               (KUTIL(2,K4),K4=1,4),
     *               WGUTL,DBUTL,WSUTL,WRUTL,UTILWK,UTILBK
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
     *       1X,'DRIVE    -> GO BUS           UTILITY=',F10.4/
     *       1X,'WALK     -> BUS/STREETCAR    UTILITY=',F10.4/
     *       1X,'WALK     -> RAPID BUS        UTLLITY=',F10.4/
     *       1X,'WALK MODE                    UTILITY=',F10.5/
     *       1X,'BIKE MODE                    UTILITY=',F10.5/)
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
      WRITE(26,9004) (PROSTA(2,K),PRASTA(2,K),K=1,12),PRCSTAT
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
     *       1X,'TTC   RAIL     K&R -->STA #4 ',I4,'-->',I4//
     *       1X,'GO    BUS    DRIVE -->STA    ',I4/)
      WRITE(26,9064) (PROSTA3(1,K),PRASTA3(1,K),K=1,12)
 9064 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY - ',
     *             'GO RAIL EGRESS'/
     *       1X,'-------------------------------------------',
     *             '------------'/
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
      IF(MSTA(1).GT.0) THEN
      WRITE(26,9065) ZNESTA(MSTA(1),1),ZNESTA(MSTA(1),2)
 9065 FORMAT(//1X,'ORIGIN --> DESTINATION STATION SUMMARY - ',
     *             'GO RAIL ACCESS'/
     *       1X,'-------------------------------------------',
     *             '------------'/
     *       1X,'TTC   RAIL     BUS -->STA #1 ',I4,'-->',I4/)
      ELSE
      WRITE(26,9065) MSTA(1),MSTA(1)      
      END IF
      END IF
C.....................................................................
C
C  OUTPUT VIRTUAL PATH BUILDING STATION VALUES
C
      IF(VSKIM) THEN
      IF(MSTA(1).GT.0) THEN
      PRGRSTA1=IEQUIV(ZNESTA(MSTA(1),1))
      PRGRSTA2=IEQUIV(ZNESTA(MSTA(1),2))
      ELSE
      PRGRSTA1=0
      PRGRSTA2=0
      END IF
      WRITE(179) IEQUIV(IZ),IEQUIV(JZ),
     *  (PROSTA(1,K),K=1,12),
     *  (PRASTA(1,K1),K1=1,12),
     *  (PROSTA(2,K2),K2=1,12),
     *  (PRASTA(2,K3),K3=1,12),PRCSTAT,
     *  (PROSTA3(1,K4),K4=1,12),
     *  (PRASTA3(1,K5),K5=1,12),
     *  PRGRSTA1,PRGRSTA2
C     WRITE(26,9066) IEQUIV(IZ),IEQUIV(JZ)
C9066 FORMAT(I4,'-',I4)
C9066 FORMAT(I4,',',I4,75(',',I4))
C     CYCLE
      END IF
C
C  WALK ACCESS MARKET SEGMENTATION COMPUTATIONS
C
      MWALK(1)=PWALK(IZ,1)*PWALK(JZ,1)
      MWALK(2)=PWALK(IZ,1)*PWALK(JZ,2)
      MWALK(3)=PWALK(IZ,2)*PWALK(JZ,1)
      MWALK(4)=PWALK(IZ,2)*PWALK(JZ,2)
      NOWALK=1.0-PWALK(IZ,2)-PWALK(IZ,1)
      IF(NOWALK.LT.0.0) NOWALK=0.0
      MWALK(5)=NOWALK*PWALK(JZ,1)
      MWALK(6)=NOWALK*PWALK(JZ,2)
      MWALK(7)=1.0-MWALK(1)-MWALK(2)-MWALK(3)-MWALK(4)-MWALK(5)
     *          -MWALK(6)
C
C  COMPUTE FTA-RELATED ACCESS PROPORTIONS
C
      FWALK(1)=MWALK(1)+MWALK(2)+MWALK(3)+MWALK(4)
      FWALK(2)=MWALK(5)+MWALK(6)
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9027) PWALK(IZ,1),PWALK(IZ,2),
     *               PWALK(JZ,1),PWALK(JZ,2),MWALK(1),
     *               MWALK(2),MWALK(3),MWALK(4),
     *               MWALK(5),MWALK(6),MWALK(7),
     *               FWALK(1),FWALK(2)
 9027 FORMAT(/1X,'ACCESS SEGMENTATION COMPUTATIONS'/
     *       1X,'----------------------------------'/
     *       1X,'SHORT WALK      ORIGIN =',F10.4/
     *       1X,'LONG  WALK      ORIGIN =',F10.4/
     *       1X,'SHORT WALK      DESTIN =',F10.4/
     *       1X,'LONG  WALK      DESTIN =',F10.4//
     *       1X,'WALK  SEGMENT    1     =',F10.4/
     *       1X,'WALK  SEGMENT    2     =',F10.4/
     *       1X,'WALK  SEGMENT    3     =',F10.4/
     *       1X,'WALK  SEGMENT    4     =',F10.4/
     *       1X,'DRIVE SEGMENT    5     =',F10.4/
     *       1X,'DRIVE SEGMENT    6     =',F10.4/
     *       1X,'NO    TRANSIT    7     =',F10.4//
     *       1X,'WALK  SEGMENT   FTA    =',F10.4/
     *       1X,'DRIVE SEGMENT   FTA    =',F10.4//)
      END IF
C.............................................................
      ulogsum=0.0
C
C  MARKET SEGMENT LOOP
C
      DO 100 C=1,NCATS
      fshar=0.0
      if(tperin(c).le.0.0.and.(.not.lsbase).and.(.not.airtrn).and.
     *  (.not.spevent).and.(.not.visitor)) cycle
      ijpairs(c)=ijpairs(c)+1
C
C ====================  AIR PASSENGER MODEL SECTION ==================
C
C       PEARSON PARKING LOT CHOICE
C
      IF(AIR.AND.(.NOT.AIRTRN)) THEN
      IF(LDEBUG) WRITE(26,36) C
   36 FORMAT(/' PEARSON PARKING LOT CHOICE DRIVE ALONE UTILITY',
     *       ' COMPUTATIONS - MARKET SEGMENT ',I1/
     *       ' ------------------------------------------',
     *       '-------------'/
     *       ' PROD  ATTR            HIGHWAY     ZNELOT ',
     *       '    WALK     SHUTTLE   TRANSIT   TRANSIT'/
     *       ' ZONE  ZONE   LOT    TIME   DIST   UTILITY',
     *       '   UTILITY   UTILITY   UTILITY   LOGSUM      KLOTTRN'/
     *       ' ----  ----  -----  -----  -----  --------',
     *       '  --------  --------  --------  ----------  ----------')
      DO NI=1,50
      IF(PEQUIV(NI).LE.0) CYCLE
      IF(PRKDATA(NI,1).EQ.0) CYCLE
      KJZ=PEQUIV(NI)
C....EMPLOYEE LOT ?
        IF(PRKDATA(NI,4).GT.0.AND.(.NOT.AIRPASS)) THEN
        WAIT1A=DMIN1(PRKDATA(NI,8),WAITLT) 
        WAIT1B=DIM(PRKDATA(NI,8),WAITLT)
        LPRKCST=PRKDATA(NI,6)
        PSPACES=PRKDATA(NI,4)
        SHCOST=PRKDATA(NI,11)
        SFACTR=PRKDATA(NI,13)
        K_PUBPRK=0.0
        SHDPRICE=PRKDATA(NI,19)
C....PUBLIC LOT
        ELSE
        WAIT1A=DMIN1(PRKDATA(NI,7),WAITLT) 
        WAIT1B=DIM(PRKDATA(NI,7),WAITLT)
        LPRKCST=PRKDATA(NI,5)
        PSPACES=PRKDATA(NI,3)
        IF(EMPPRK) PSPACES=0
        SHCOST=PRKDATA(NI,10)
        SFACTR=PRKDATA(NI,12)
        K_PUBPRK=KPUBPRK
        SHDPRICE=PRKDATA(NI,19)
        END IF        
C...ZONE TO LOT
      UTILOT(NI,1)=0.0
      IF(PSPACES.GT.0) THEN
      UTILOT(NI,1)=COEFF(46)*ZNELOT(IZ,NI,1) +
     *             COEFF(50+C)*OPCOST*ZNELOT(IZ,NI,2) +
     *             COEFF(50+C)*LPRKCST+
     *             COEFF(42)*LOG(PSPACES)+
     *             K_PUBPRK/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)+
     *             SHDPRICE/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C...LOT TO ATTRACTION ZONE - WALK ACCESS
      ULOTWLK(NI)=0.0
      IF(PRKDATA(NI,2).EQ.1.0) THEN
       IF(KJZ.EQ.JZ) THEN
       ULOTWLK(NI)=(COEFF(43)*PRKDATA(NI,14))/
     *   (LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
       ELSE
        IF(PJZ.GT.0) THEN
        IF(PRKDATA(PJZ,2).EQ.1.0) THEN
        XDIST=SQRT(((SXCOORD(KJZ)-SXCOORD(JZ))**2.0)+
     *  ((SYCOORD(KJZ)-SYCOORD(JZ))**2.0))/1000.0
        ULOTWLK(NI)=(COEFF(43)*XDIST*12.0)/
     *     (LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
        END IF
        END IF
        END IF
       END IF
C...LOT TO ATTRACTION ZONE - SHUTTLE 
       ULOTSHL(NI)=0.0
       IF(PRKDATA(NI,2).EQ.2.AND.CJZ.GT.0) THEN
       ULOTSHL(NI)=
     *     COEFF(46)* ZNELOT(JZ,NI,1)*SFACTR+
     *     COEFF(44)*WAIT1A+COEFF(45)*WAIT1B+
     *     COEFF(50+C)*SHCOST+
     *     COEFF(43)*PRKDATA(NI,9)
       ULOTSHL(NI)=ULOTSHL(NI)/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
       END IF
C....LOT TO ATTRACTION ZONE - TRANSIT
       ULOTTRN(NI)=0.0
       IF(AIRTPROB(NI,AJZ,7).NE.0.0) THEN
       ULOTTRN(NI)=(AIRTPROB(NI,AJZ,7)+KLOTTRN)/
     *       (LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
       END IF
       IF(TRNLOT.AND.PRKDATA(NI,2).EQ.1.0) ULOTTRN(NI)=0.0
C....CHECK FOR PARKING AND DESTINATION ZONE BEING EQUAL
       IF(JZ.EQ.KJZ) THEN
       ULOTWLK(NI)=(COEFF(43)*3.0)/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
       ULOTSHL(NI)=0.0
       ULOTTRN(NI)=0.0
       END IF
C ----------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,334) IEQUIV(IZ),IEQUIV(JZ),IEQUIV(KJZ),
     *             ZNELOT(IZ,NI,1),ZNELOT(IZ,NI,2),
     *             UTILOT(NI,1),ULOTWLK(NI),ULOTSHL(NI),
     *             ULOTTRN(NI),AIRTPROB(NI,AJZ,7),KLOTTRN
  334 FORMAT(1X,I4,2X,I4,2X,I5,1X,F6.2,1X,F6.2,4(2X,F8.3),2(2X,F10.5))
      END IF
C ----------------------------------------------------------
      END DO
      IF(LDEBUG) WRITE(26,4336) C
 4336 FORMAT(/' PEARSON PARKING LOT CHOICE DRIVE ALONE PROBABILITY',
     *       ' COMPUTATIONS - MARKET SEGMENT ',I1/
     *       ' ------------------------------------------',
     *       '----------------'/
     *  ' PROD  ATTR            LOT        LOT      WALK    SHUTTLE',
     *  '   TRANSIT    WALK     SHUTTLE   TRANSIT'/
     *  ' ZONE  ZONE   LOT    UTILITY     SHARE     SHARE     SHARE ',
     *  '    SHARE   UTILITY   UTILITY   UTILITY'/
     *  ' ----  ----  -----   --------  -------   -------   -------',
     *  '   -------  --------  --------  --------')
     
C....COMPUTE LOT PROBABILITIES
      DENOM=0.0
      DO NI=1,50
      IF(PEQUIV(NI).LE.0) CYCLE
      IF(PRKDATA(NI,1).EQ.0) CYCLE
C
      EUTILOT(NI,1)=0.0
      EUTILOT(NI,2)=0.0
      EUTILOT(NI,3)=0.0
      EUTILOT(NI,4)=0.0
      UTILOT(NI,2)=0.0
      LSLOTE=0.0
      IF(ULOTWLK(NI).NE.0) EUTILOT(NI,2)=EXP(ULOTWLK(NI))
      IF(ULOTSHL(NI).NE.0) EUTILOT(NI,3)=EXP(ULOTSHL(NI))
      IF(ULOTTRN(NI).NE.0) EUTILOT(NI,4)=EXP(ULOTTRN(NI))
      DENOME=EUTILOT(NI,2)+EUTILOT(NI,3)+EUTILOT(NI,4)
      IF(DENOME.NE.0.0) LSLOTE=LOG(DENOME)
      IF(UTILOT(NI,1).NE.0.0) THEN
      UTILOT(NI,2)=UTILOT(NI,1)+LSUM1AUTO*LSUM2AUTO*LSLOTE
      EUTILOT(NI,1)=EXP(UTILOT(NI,2))
      END IF    
      IF(DENOME.GT.0) THEN
      DENOM=DENOM+EUTILOT(NI,1)
      ELSE
C....NO EGRESS AVAIALABLE
      EUTILOT(NI,1)=0.0
      END IF
      END DO
      IF(DENOM.EQ.0) GO TO 39
      DO NI=1,50
      IF(PEQUIV(NI).LE.0) CYCLE
      IF(PRKDATA(NI,1).EQ.0) CYCLE
      KJZ=PEQUIV(NI)
      PROBLOT(NI)=EUTILOT(NI,1)/DENOM
      DENOME=EUTILOT(NI,2)+EUTILOT(NI,3)+EUTILOT(NI,4)
      IF(DENOME.GT.0) THEN
      EPROBLOT(NI,1)=EUTILOT(NI,2)/DENOME
      EPROBLOT(NI,2)=EUTILOT(NI,3)/DENOME
      EPROBLOT(NI,3)=EUTILOT(NI,4)/DENOME
      ELSE
      EPROBLOT(NI,1)=0.0
      EPROBLOT(NI,2)=0.0
      EPROBLOT(NI,3)=0.0
      END IF
C --------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,42) IEQUIV(IZ),IEQUIV(JZ),IEQUIV(KJZ),
     *             UTILOT(NI,2),PROBLOT(NI),
     *             (EPROBLOT(NI,K),K=1,3),
     *             ULOTWLK(NI),ULOTSHL(NI),ULOTTRN(NI)
   42 FORMAT(1X,I4,2X,I4,2X,I5,2X,F8.3,4(2X,F8.5),3(2X,F8.4))
 3888 FORMAT(I4,',',I4,3(',',F8.4))
      END IF
C ---------------------------------------------------------------
      END DO
   39 CONTINUE
      LSLOT=0.0
      IF(DENOM.NE.0.0) LSLOT=LSUM1AUTO*DLOG(DENOM)  
      END IF
C
C  RENTAL CAR FACILITY SELECTION
C
      IF(AIRPASS.AND.(.NOT.AIRTRN)) THEN
C      IF(AIR.AND.(.NOT.AIRTRN)) THEN
      IF(LDEBUG) WRITE(26,436) C
  436 FORMAT(/' RENTAL CAR FACILITY UTILITY COMPUTATIONS',
     *       ' - MARKET SEGMENT ',I1/
     *       ' -----------------------------------------',
     *       '------------------'/
     *       ' PROD  ATTR         SHUTTLE   TRANSIT'/
     *       ' ZONE  ZONE   TAZ   UTILITY   UTILITY'/
     *       ' ----  ----  -----  --------  --------')
      DO NI=1,10
      KJZ=IDINT(RNTLDATA(NI,1))
      IF(KJZ.EQ.0) CYCLE
      KJZ=EQUIV(KJZ)
C...LOT TO ATTRACTION ZONE - SHUTTLE
       URNTSHL(NI)=
     *     COEFF(46)*ZNERNT(JZ,NI,1)*RNTLDATA(NI,6)+
     *     COEFF(44)*(RNTLDATA(NI,5)+RNTLDATA(NI,3))+
     *     COEFF(50+C)*RNTLDATA(NI,4)
       URNTSHL(NI)=URNTSHL(NI)/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
C....LOT TO ATTRACTION ZONE - TRANSIT
       URNTTRN(NI)=0.0
       IF(AIRRPROB(NI,AJZ,7).NE.0.0.AND.AJZ.GT.0) THEN
C      URNTTRN(NI)=AIRRPROB(NI,AJZ,7)/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
       URNTTRN(NI)=AIRRPROB(NI,AJZ,7)
       END IF
C ----------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,434) IEQUIV(IZ),IEQUIV(JZ),IEQUIV(KJZ),
     *              URNTSHL(NI),URNTTRN(NI)
  434 FORMAT(1X,I4,2X,I4,2X,I5,2(2X,F8.3))
      END IF
C ----------------------------------------------------------
      END DO
      IF(LDEBUG) WRITE(26,4346) C
 4346 FORMAT(/' RENTAL FACILITY PROBABILITY COMPUTATIONS',
     *        ' - MARKET SEGMENT ',I1/
     *        ' -----------------------------------------',
     *        '------------------'/
     *  ' PROD  ATTR            LOT   SHUTTLE  TRANSIT'/
     *  ' ZONE  ZONE   TAZ     SHARE    SHARE    SHARE'/
     *  ' ----  ----  -----  -------  -------  -------')
C....COMPUTE PROBABILITIES
      DENOM=0.0
      DO NI=1,10
      KJZ=IDINT(RNTLDATA(NI,1))
      IF(KJZ.EQ.0) CYCLE
      EUTLRNT(NI,1)=0.0
      EUTLRNT(NI,2)=0.0
      IF(URNTSHL(NI).NE.0) EUTLRNT(NI,1)=EXP(URNTSHL(NI))
      IF(URNTTRN(NI).NE.0) EUTLRNT(NI,2)=EXP(URNTTRN(NI))
      DENOM=EUTLRNT(NI,1)+EUTLRNT(NI,2)
      IF(DENOM.NE.0.0) THEN
      PROBRNT(NI,1)=EUTLRNT(NI,1)/DENOM
      PROBRNT(NI,2)=EUTLRNT(NI,2)/DENOM
      END IF
      IF(CONRAC) THEN
      PROBRNT(NI,1)=0.0
      PROBRNT(NI,2)=1.0
      ELSE
      PROBRNT(NI,1)=1.0
      PROBRNT(NI,2)=0.0
      END IF
C --------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,444) IEQUIV(IZ),IEQUIV(JZ),KJZ,RNTLDATA(NI,2),
     *             PROBRNT(NI,1),PROBRNT(NI,2)
  444 FORMAT(1X,I4,2X,I4,2X,I5,3(1X,F8.5))
      END IF
C ---------------------------------------------------------------
      END DO
C
C  AIR PASSENGER MODE CHOICE COMPUTATIONS
C
      UTLATXI=ACOEF(1)*HTIME2P(JZ)+ACOEF(2)*0.25+
     *        ACOEF(3)*(265+245*HDIST2P(JZ))*LCPI+
     *        ACNST(1)
      CALL UBERCOMP(IZ,HTIME2P(JZ),HDIST2P(JZ),ACOEF(2),ACOEF(1),
     *              COSTUBER,ACCUBER,KWAIT)
      UTLAUBER=ACCUBER + ACOEF(3)*(COSTUBER*100.0)*LCPI +
     *        ACNST(10)
      UTLARNT=ACOEF(1)*HTIME(JZ)+ACOEF(2)*RNTWAIT+
     *        ACOEF(3)*ACOEF(4)*HDIST(JZ)*LCPI+
     *        ACNST(2)
      UTLALMO=ACOEF(1)*HTIME2P(JZ)+ACOEF(2)*0.25+
     *        ACOEF(3)*(5682.18+138.99*HDIST2P(JZ))*LCPI+
     *        ACNST(3)
      UTLADRP=ACOEF(1)*HTIME2P(JZ)+ACOEF(2)*0.25+
     *        ACOEF(3)*OPCOST*HDIST2P(JZ)*LCPI+
     *        ACNST(4)
      UTLAONC=ACOEF(1)*HTIME2P(JZ)+ACOEF(2)*1.0+
     *        ACOEF(3)*(599.15+89.84*HDIST2P(JZ))*LCPI+
     *        ACNST(6)
      UTLAUPE=0.0
C
      UTLATXI=UTLATXI/LSUM1AUTO
      UTLAUBER=UTLAUBER/LSUM1AUTO
      UTLALMO=UTLALMO/LSUM1AUTO
      UTLADRP=UTLADRP/LSUM1AUTO
      UTLAONC=UTLAONC/LSUM1AUTO
      UTLARNT=UTLARNT/LSUM1AUTO
      UTLAUPE=UTLAUPE/LSUM1AUTO
C ------------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,450) C,UTLATXI,UTLALMO,UTLADRP,LSLOT,UTLAONC,UTLAUBER,
     *                UTLARNT
  450 FORMAT(/
     *       1X,' AIR PASSENGER MODEL UTILITIES -- MARKET SEGMENT ',I1/
     *       1X,' -----------------------------'/
     *       1X,'                                   PARKING'/
     *       1X,'   TAXI       LIMO      DROPOFF      LOT  ',
     *          '    ON-CALL     TNC       RENTAL'/
     *       1X,'  UTILITY    UTILITY    UTILITY    UTILITY',
     *          '    UTILITY    UTILITY    UTILITY'/
     *       1X,' ---------  ---------  ---------  ---------',
     *          '  ---------  ---------  ---------'/
     *       7(2X,F9.5))
      END IF
C -------------------------------------------------------------------------
      END IF
C================== END OF AIR PASSENGER SECTION ============================
C---------------------------------------------------------------------
C       HIGHWAY MODE UTILITY VALUE COMPUTATION                       |
C---------------------------------------------------------------------
C
C  INITIALIZE UTILITIES
C
      UTIL0NT=0.0
      UTIL0T=0.0
      UTIL2NT=0.0
      UTIL2NTH=0.0
      UTIL2T=0.0
      UTIL2TH=0.0
      UTIL3NT=0.0
      UTIL3NTH=0.0
      UTIL3T=0.0
      UTIL3TH=0.0
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR DRIVE-ALONE AUTO
C
C....NON-TOLL 
      IF(HTIME(JZ).GT.0) THEN
      UTIL0NT=COEFF(11)* HTIME(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/2.0
     *        +COEFF(50+C)* OPCOST*HDIST(JZ)
      UTIL0NT=UTIL0NT/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C....TOLL 
      IF((TTIME(JZ).GT.0).AND.(TOLDIST(JZ).GT.0.01)) THEN
      YTOLDS0=YINTER+YSLOPE*TDIST(JZ)
	    YTOLDS0=MIN(YTOLDS0,0.0)
      UTIL0T = COEFF(11)* TTIME(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/2.0
     *        +COEFF(50+C)* OPCOST* TDIST(JZ)
     *        +COEFF(70+C)* TOLLCOST(JZ)
     *        + YTOLDS0  +KTOLL
      UTIL0T=UTIL0T/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 2-PERSON AUTO
C
C....NON-TOLL/NON-HOV (BASED UPON DRIVE ALONE)
      IF(HTIME(JZ).GT.0) THEN
      UTIL2NT =COEFF(11)* HTIME(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV2P*2.0)
     *        +COEFF(50+C)* OPCOST*(HDIST(JZ)/HOV2P)
      UTIL2NT=UTIL2NT/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C....HOV UTILITY FOR NON-TOLL CHOICE
	    IF(HOVDIST(JZ).GT.0.01) THEN
      UTIL2NTH=COEFF(11)* HTIME2P(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV2P*2.0)
     *        +COEFF(50+C)* OPCOST* HDIST2P(JZ)/HOV2P
     *        +KHOV2 
      UTIL2NTH=UTIL2NTH/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C....TOLL/NON-HOV (BASED UPON DRIVE ALONE)
      IF((TTIME(JZ).GT.0).AND.(TOLDIST(JZ).GT.0.01)) THEN
      UTIL2T = COEFF(11)* TTIME(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV2P*2.0)
     *        +COEFF(50+C)* OPCOST* (TDIST(JZ)/HOV2P)
     *        +COEFF(70+C)* TOLLCOST(JZ)/HOV2P
     *        + YTOLDS0  +KTOLL2
      UTIL2T=UTIL2T/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF(HOV_TOLDIST(JZ).GT.0.01) THEN
      YTOLDS2=YINTER+YSLOPE*TDIST2P(JZ)
	    YTOLDS2=MIN(YTOLDS2,0.0)
      UTIL2TH= COEFF(11)* TTIME2P(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV2P*2.0)
     *        +COEFF(50+C)* OPCOST* TDIST2P(JZ)/HOV2P
     *        +COEFF(70+C)* (DSCT2P*TOLLCOST2P(JZ))/HOV2P
     *        +YTOLDS2 +KTOLL2 +KHOV2
      UTIL2TH=UTIL2TH/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO) 
      END IF
C
C       COMPUTE THE COMMON EXPONENTIATED UTILITY FOR 3-PERSON AUTO
C
C....NON-TOLL/NON-HOV
      IF(HTIME(JZ).GT.0) THEN
      UTIL3NT =COEFF(11)* HTIME(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV3P*2.0)
     *        +COEFF(50+C)* OPCOST*(HDIST(JZ)/HOV3P)
      UTIL3NT=UTIL3NT/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C....HOV UTILITY FOR NON-TOLL CHOICE
	    IF(HOVDIST(JZ).GT.0.01) THEN
      UTIL3NTH =COEFF(11)* HDIST2P(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV3P*2.0)
     *        +COEFF(50+C)* OPCOST* HDIST2P(JZ)/HOV3P
     *        +KHOV3 
      UTIL3NTH=UTIL3NTH/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C....TOLL/NON-HOV
      IF((TTIME(JZ).GT.0).AND.(TOLDIST(JZ).GT.0.01)) THEN
      UTIL3T = COEFF(11)* TTIME(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV3P*2.0)
     *        +COEFF(50+C)* OPCOST* (TDIST(JZ)/HOV3P)
     *        +COEFF(70+C)* TOLLCOST(JZ)/HOV3P
     *        + YTOLDS2  +KTOLL3
      UTIL3T=UTIL3T/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO)
      END IF
C....HOV UTILITY FOR TOLL CHOICE
	    IF(HOV_TOLDIST(JZ).GT.0.01) THEN
      YTOLDS2=YINTER+YSLOPE*TDIST2P(JZ)
	    YTOLDS2=MIN(YTOLDS2,0.0)
      UTIL3TH= COEFF(11)* TTIME2P(JZ)
     *        +COEFF(50+C)* PRKCST(JZ)/(HOV3P*2.0)
     *        +COEFF(50+C)* OPCOST* TDIST2P(JZ)/HOV3P
     *        +COEFF(70+C)* (DSCT3P*TOLLCOST2P(JZ))/HOV3P
     *        +YTOLDS2 +KTOLL3 +KHOV3
      UTIL3TH=UTIL3TH/(LSUM1AUTO*LSUM2AUTO*LSUM3AUTO) 
      END IF
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,9035) IEQUIV(IZ),IEQUIV(JZ),HTIME(JZ),HDIST(JZ),
     *               TTIME(JZ),TDIST(JZ),TOLLCOST(JZ),TOLDIST(JZ),
     *               YTOLDS0,
     *               HTIME2P(JZ),HDIST2P(JZ),HOVDIST(JZ),
     *               TTIME2P(JZ),TDIST2P(JZ),
     *               TOLDIST2P(JZ),HOV_TOLDIST(JZ),
     *               TOLLCOST2P(JZ),DSCT2P,YTOLDS2,DSCT3P,
     *               PRKCST(JZ),
     *               UTIL0NT,UTIL0T,UTIL2NT,UTIL2NTH,UTIL2T,UTIL2TH,
     *               UTIL3NT,UTIL3NTH,UTIL3T,UTIL3TH
 9035 FORMAT(/1X,'HIGHWAY MODE UTILITY COMPUTATIONS'/
     *       1X,'---------------------------------------'/
     *       1X,'ORIGIN ZONE                =',I10/
     *       1X,'DESTINATION ZONE           =',I10//
     *       1X,'DA AUTO TIME-    NON TOLL  =',F10.2/
     *       1X,'DA AUTO DISTANCE-NON TOLL  =',F10.2//
     *       1X,'DA AUTO TIME-        TOLL  =',F10.2/
     *       1X,'DA AUTO DISTANCE-    TOLL  =',F10.2/
     *       1X,'DA AUTO COST-        TOLL  =',F10.2/
     *       1X,'DA TOLL DISTANCE -   TOLL  =',F10.2/
     *       1X,'DA TOLL DISTANCE CONSTANT  =',F10.2//
     *       1X,'2P AUTO TIME-        HOV   =',F10.2/
     *       1X,'2P AUTO DISTANCE-    HOV   =',F10.2/
     *       1X,'2P HOV  LANE DISTANCE      =',F10.2//
     *       1X,'2P AUTO TIME-    TOLL/HOV  =',F10.2/
     *       1X,'2P AUTO DISTANCE-TOLL/HOV  =',F10.2/
     *       1X,'2P TOLL DISTANCE-TOLL/HOV  =',F10.2/
     *       1X,'2P HOV  DISTANCE-TOLL/HOV  =',F10.2/
     *       1X,'2P TOLL COST               =',F10.2/
     *       1X,'2P TOLL DISCOUNT   FACTOR  =',F10.2/
     *       1X,'2P TOLL DISTANCE CONSTANT  =',F10.2//
     *       1X,'3P TOLL DISCOUT    FACTOR  =',F10.2/
     *       1X,'PARKING COST               =',F10.2//
     *       1X,'DRIVE ALONE NON-TOLL UTIL  =',F10.5/
     *       1X,'DRIVE ALONE TOLL UTILITY   =',F10.5//
     *       1X,'2 PERSON NON-TOLL UTILITY  =',F10.5/
     *       1X,'2 PERSON NON-TOLL/HOV UTIL =',F10.5/
     *       1X,'2 PERSON TOLL UTILITY      =',F10.5/
     *       1X,'2 PERSON TOLL/HOV UTILITY  =',F10.5//
     *       1X,'3 PERSON NON-TOLL UTILITY  =',F10.5/
     *       1X,'3 PERSON NON-TOLL/HOV UTIL =',F10.5/
     *       1X,'3 PERSON TOLL UTILITY      =',F10.5/
     *       1X,'3 PERSON TOLL/HOV UTILITY  =',F10.5//)
      END IF
C........................................................
C
C  WALK ACCESS MARKET SEGMENTATION LOOP INDEX M=WALK SEGMENTATION
C
      DO 2000 M=1,7
C
C  INITIALIZE TRIP VALUES
C
      TCR=0.0
      TUR=0.0
      TGB=0.0
      TSTR=0.0
      TRPD=0.0
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
      UTIL=0.0
      EUTIL=0.0
      TDRV0N=0.0
      TDRV0T=0.0
      TDRV2NH=0.0
      TDRV2NN=0.0
      TDRV2TH=0.0
      TDRV2TN=0.0
      TDRV3NH=0.0
      TDRV3NN=0.0
      TDRV3TH=0.0
      TDRV3TN=0.0
      TNMOT=0.0
      TNMWK=0.0
      TNMBK=0.0
      TSCHBUS=0.0
C
C  PERSON TRIPS BY WALK ACCESS SEGMENT
C
      if(lsbase) tperin(c)=1.0
      PERTRP=TPERIN(C)*MWALK(M)
      if(pertrp.lt.0.001.AND.(.NOT.DEBUG).and.(.not.airtrn).and.
     *  (.not.spevent).and.(.not.visitor).and.(.not.lsbase)) GOTO 2000
      if(airtrn.and.m.gt.1) go to 2000
      if(waterloo.and.(m.gt.1)) go to 2000
      IF(M.EQ.7) GO TO 225
      BWALK1=0.0
      BWALK2=0.0
      BWALK=0.0
      DBWLK=0.0
      SWALK1=0.0
      SWALK2=0.0
      STWALK=0.0
      RWALK1=0.0
      RWALK2=0.0
      RWALK=0.0
C
C CONSTRAIN PRODUCTION & ATTRACTION WALK TIME
C
C            1    2   3   4   5   6
      GO TO (211,212,213,214,215,216),M
 211  BWALK1=AMIN1(WLKBACC,SWALK)
      BWALK2=AMIN1(WLKBEGR,SWALK)
      BWALK=BWALK1+BWALK2
      DBWLK=AMIN1(WALKDT,SWALK)
      SWALK1=AMIN1(WLKSACC,SWALK)
      SWALK2=AMIN1(WLKSEGR,SWALK)
      STWALK=SWALK1+SWALK2
      RWALK1=AMIN1(WLKRACC,SWALK)
      RWALK2=AMIN1(WLKREGR,SWALK)
      RWALK=RWALK1+RWALK2
      GO TO 210
  212 BWALK1=AMIN1(WLKBACC,SWALK)
      BWALK2=AMIN1(WLKBEGR,LWALK)
      BWALK=BWALK1+BWALK2
      DBWLK=AMIN1(WALKDT,LWALK)
      SWALK1=AMIN1(WLKSACC,SWALK)
      SWALK2=AMIN1(WLKSEGR,LWALK)
      STWALK=SWALK1+SWALK2
      RWALK1=AMIN1(WLKRACC,SWALK)
      RWALK2=AMIN1(WLKREGR,LWALK)
      RWALK=RWALK1+RWALK2
      GO TO 210
  213 BWALK1=AMIN1(WLKBACC,LWALK)
      BWALK2=AMIN1(WLKBEGR,SWALK)
      BWALK=BWALK1+BWALK2
      DBWLK=AMIN1(WALKDT,SWALK)
      SWALK1=AMIN1(WLKSACC,LWALK)
      SWALK2=AMIN1(WLKSEGR,SWALK)
      STWALK=SWALK1+SWALK2
      RWALK1=AMIN1(WLKRACC,LWALK)
      RWALK2=AMIN1(WLKREGR,SWALK)
      RWALK=RWALK1+RWALK2
      GO TO 210
  214 BWALK1=AMIN1(WLKBACC,LWALK)
      BWALK2=AMIN1(WLKBEGR,LWALK)
      BWALK=BWALK1+BWALK2
      DBWLK=AMIN1(WALKDT,LWALK)
      SWALK1=AMIN1(WLKSACC,LWALK)
      SWALK2=AMIN1(WLKSEGR,LWALK)
      STWALK=SWALK1+SWALK2
      RWALK1=AMIN1(WLKRACC,LWALK)
      RWALK2=AMIN1(WLKREGR,LWALK)
      RWALK=RWALK1+RWALK2
      GO TO 210
  215 BWALK=AMIN1(WLKBEGR,SWALK)
      DBWLK=AMIN1(WALKDT,SWALK)
      STWALK=AMIN1(WLKSEGR,SWALK)
      RWALK=AMIN1(WLKREGR,SWALK)
      GOTO 210
  216 BWALK=AMIN1(WLKBEGR,LWALK)
      DBWLK=AMIN1(WALKDT,LWALK)   
      STWALK=AMIN1(WLKSEGR,LWALK)
      RWALK=AMIN1(WLKREGR,LWALK)
  210 CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9006) C,M,PERTRP,BWALK,DBWLK,STWALK,RWALK
 9006 FORMAT(1X,'MARKET SEGMENT=',I2,' ACCESS SEGMENT=',I2,/
     *       1X,'---------------------------------------------'//
     *       1X,' PERTRP=',F12.5//
     *       1X,'SUMMARY OF CONSTRAINED WALK TIMES'/
     *       1X,'-----------------------------------'/
     *       1X,'WALK   --> GO    BUS =',F8.2/
     *       1X,'DRIVE  --> GO    BUS =',F8.2/
     *       1X,'WALK   --> BUS/STRCAR=',F8.2/
     *       1X,'WALK   --> RAPID BUS =',F8.2/)
      END IF
C....................................................................
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
      IF(EGRIND(IMODE,IST).GT.0) CALL TXFARE(CFARE)
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
      IF(EGRIND(IMODE,IST).GT.0) CALL TXFARE(CFARE)
      IF(MSTA(IST).GT.0) CALL TXFARE(CFARE)
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
      IF(EGRIND(IMODE,IST).GT.0) CALL TXFARE(CFARE)
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
      IF(EGRIND(IMODE,IST).GT.0) CALL TXFARE(CFARE)
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
      UTIL(25)=WGUTL +  (COEFF(17)*BWALK +
     *         COEFF(50+C)*FARE(JZ))/(LSUM2GB*LSUM1TRN)
      EUTIL(25)=EXP(UTIL(25))
      END IF
C..DRIVE ACCESS TO GO BUS
      IF(DBUTL.NE.0.0.AND.(.NOT.NDRVGOB)) THEN
      IT=CSTAT-MAX_IZONES
      UTIL(26)=DBUTL + (COEFF(17)*DBWLK +
     *          COEFF(50+C)*STAZNE(4,IT,JZ))/(LSUM2GB*LSUM1TRN) +
     *          KDTRN(C)/(LSUM1TRN*LSUM2GB)   
      EUTIL(26)=EXP(UTIL(26))
      END IF

C..WALK ACCESS TO BUS/STREETCAR
      IF(WSUTL.NE.0.0) THEN
      UTIL(38)=WSUTL +  (COEFF(17)*STWALK +
     *         COEFF(50+C)*BSTRFARE(JZ))/(LSUM2SB*LSUM1TRN) +
     *         KBUSTRW/(LSUM2SB*LSUM1TRN)
      EUTIL(38)=EXP(UTIL(38))
      END IF
C..DRIVE ACCESS TO BUS/STREETCAR
      IF(WSUTL.NE.0.0.AND.(.NOT.NDRVBUS)) THEN
      UTIL(52)=WSUTL +  (COEFF(17)*SWALK2 + COEFF(16)*(SWALK1/8.0) +
     *         COEFF(38)*(NTXFERB**COEFF(25)) +
     *         COEFF(50+C)*BSTRFARE(JZ))/(LSUM2SB*LSUM1TRN) +
     *         KINFLSTR/(LSUM2SB*LSUM1TRN)
      EUTIL(52)=EXP(UTIL(52))
      END IF
C..WALK ACCESS TO RAPID BUS
      IF(WRUTL.NE.0.0) THEN
      UTIL(49)=WRUTL +  (COEFF(17)*RWALK +
     *         COEFF(50+C)*RAPDFARE(JZ))/(LSUM2RB*LSUM1TRN)
      EUTIL(49)=EXP(UTIL(49))
      END IF
C..DRIVE ACCESS TO RAPID BUS
      IF(WRUTL.NE.0.0.AND.(.NOT.NDRVRAP)) THEN
      UTIL(53)=WRUTL +  (COEFF(17)*RWALK2 + COEFF(16)*(RWALK1/8.0) +
     *         COEFF(50+C)*RAPDFARE(JZ))/(LSUM2RB*LSUM1TRN) +
     *         COEFF(39)*(NTXFERR**COEFF(25)) +
     *         KINFLRPD/(LSUM2RB*LSUM1TRN)
      EUTIL(53)=EXP(UTIL(53))
      END IF
      IF(ZEROCAR.AND.C.LE.2.AND.NCATS.GT.1) THEN
      UTIL(26)=0.0
      EUTIL(26)=0.0
      UTIL(52)=0.0
      EUTIL(52)=0.0
      UTIL(53)=0.0
      EUTIL(53)=0.0
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9306) UTIL(25),EUTIL(25),UTIL(26),EUTIL(26),
     *               UTIL(38),EUTIL(38),UTIL(52),EUTIL(52),
     *               UTIL(49),EUTIL(49),UTIL(53),EUTIL(53)
 9306 FORMAT(/1X,'SUMMARY OF TRANSIT MODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',12X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',10X,'----------',5X,
     *          '----------'/
     *       1X,'WALK  ACCESS TO GO BUS        =',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO GO BUS        =',F10.5,3X,E12.5/
     *       1X,'WALK  ACCESS TO BUS/STREETCAR =',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO BUS/STREETCAR =',F10.5,3X,E12.5/
     *       1X,'WALK  ACCESS TO RAPID BUS     =',F10.5,3X,E12.5/
     *       1X,'DRIVE ACCESS TO RAPID BUS     =',F10.5,3X,E12.5/)
      END IF
C.....................................................................
C
C  GENERAL MODE UTILITIES & PROBABILITIES
C
C
C..DRIVE ALONE UTILITY-NON-TOLL
C
 225  IF(UTIL0NT.NE.0.0) THEN
      UTIL(39)=UTIL0NT
      EUTIL(39)=EXP(UTIL(39))
	    ENDIF
C
C..DRIVE ALONE UTILITY-TOLL
C
      IF(UTIL0T.NE.0.0) THEN
	    UTIL(40)=UTIL0T
      EUTIL(40)=EXP(UTIL(40))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - NON-TOLL/NON-HOV
C
      IF(UTIL2NT.NE.0.0) THEN
	    UTIL(41)=UTIL2NT
      EUTIL(41)=EXP(UTIL(41))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - NON-TOLL/HOV
C
      IF(UTIL2NTH.NE.0.0) THEN
	    UTIL(42)=UTIL2NTH
      EUTIL(42)=EXP(UTIL(42))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - TOLL/NON-HOV
C
      IF(UTIL2T.NE.0.0) THEN
	    UTIL(43)=UTIL2T
      EUTIL(43)=EXP(UTIL(43))
	    ENDIF
C
C..2 PERSON AUTO UTILITY - TOLL/HOV
C
      IF(UTIL2TH.NE.0.0) THEN
	    UTIL(44)=UTIL2TH
      EUTIL(44)=EXP(UTIL(44))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - NON-TOLL/NON-HOV
      IF(UTIL3NT.NE.0.0) THEN
	    UTIL(45)=UTIL3NT
      EUTIL(45)=EXP(UTIL(45))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - NON-TOLL/HOV
      IF(UTIL3NTH.NE.0.0) THEN
	    UTIL(46)=UTIL3NTH
      EUTIL(46)=EXP(UTIL(46))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - TOLL
      IF(UTIL3T.NE.0.0) THEN
	    UTIL(47)=UTIL3T
      EUTIL(47)=EXP(UTIL(47))
	    ENDIF
C
C..3+ PERSON AUTO UTILITY - TOLL/HOV
      IF(UTIL3TH.NE.0.0) THEN
	    UTIL(48)=UTIL3TH
      EUTIL(48)=EXP(UTIL(48))
	    ENDIF
C
C..DRIVE-ALONE PROBABILITIES
      DAPROB(1)=1.0
      DAPROB(2)=0.0
      LSDA=0.0
      DENOM=EUTIL(39) + EUTIL(40)
      IF(DENOM.GT.0.0) THEN
      LSDA=DLOG(DENOM)
      DAPROB(1)=EUTIL(39)/DENOM
      DAPROB(2)=1.0-DAPROB(1)
      ENDIF
      IF(ZEROCAR.AND.C.LE.2) LSDA=0.0
C..2-PERSON DRIVE
      P2PROB(1)=1.0
      P2PROB(2)=0.0
      P2PROB(3)=0.0
      P2PROB(4)=0.0
      LS2PER=0.0
      DENOM=EUTIL(41)+EUTIL(42)+EUTIL(43)+EUTIL(44)
      IF(DENOM.GT.0.0) THEN
      LS2PER=DLOG(DENOM)
      P2PROB(2)=EUTIL(42)/DENOM
      P2PROB(3)=EUTIL(43)/DENOM
      P2PROB(4)=EUTIL(44)/DENOM
      P2PROB(1)=1.0 - P2PROB(2) - P2PROB(3) - P2PROB(4)
      ENDIF
C..3+PERSON DRIVE
      P3PROB(1)=1.0
      P3PROB(2)=0.0
      P3PROB(3)=0.0
      P3PROB(4)=0.0
      LS3PER=0.0
      DENOM=EUTIL(45) + EUTIL(46)+EUTIL(47)+EUTIL(48)
      IF(DENOM.GT.0.0) THEN
      LS3PER=DLOG(DENOM)
      P3PROB(2)=EUTIL(46)/DENOM
      P3PROB(3)=EUTIL(47)/DENOM
      P3PROB(4)=EUTIL(48)/DENOM
      P3PROB(1)=1.0-P3PROB(2)-P3PROB(3)-P3PROB(4)
      ENDIF
C..NON-MOTORIZED
      NMPROB(1)=1.0
      NMPROB(2)=0.0
      LSNMOT=0.0
      IF(NMOT) THEN
      UTIL(64)=UTILBK+KBIKE(C)/LSUM1NM
      EUTIL(64)=EXP(UTIL(64))
      UTIL(63)=UTILWK
      EUTIL(63)=EXP(UTIL(63))
      DENOM=EUTIL(63)+EUTIL(64)
      IF(DENOM.GT.0.0) THEN
      LSNMOT=DLOG(DENOM)
      NMPROB(1)=EUTIL(63)/DENOM
      NMPROB(2)=1.0-NMPROB(1)
      END IF
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9116) C,M,
     *               UTIL(39),EUTIL(39),DAPROB(1),
     *               UTIL(40),EUTIL(40),DAPROB(2),
     *               UTIL(41),EUTIL(41),P2PROB(1),
     *               UTIL(42),EUTIL(42),P2PROB(2),
     *               UTIL(43),EUTIL(43),P2PROB(3),
     *               UTIL(44),EUTIL(44),P2PROB(4),
     *               UTIL(45),EUTIL(45),P3PROB(1),
     *               UTIL(46),EUTIL(46),P3PROB(2),
     *               UTIL(47),EUTIL(47),P3PROB(3),
     *               UTIL(48),EUTIL(48),P3PROB(4),
     *               UTIL(63),EUTIL(63),NMPROB(1),
     *               UTIL(64),EUTIL(64),NMPROB(2)
 9116 FORMAT(/1X,'MARKET SEGMENT=',I2,' ACCESS SEGMENT=',I2//
     *       1X,'SUMMARY OF GENERAL MODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',7X,'UTIL',10X,'EUTIL',3X,
     *       3X,' PROB'/
     *       1X,'                     ',4X,'----------',5X,
     *          '----------',3X,'------'/
     *       1X,'DRIVE ALONE NON-TOLL     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'DRIVE ALONE TOLL         ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   NON-TOLL     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   NON-TOLL/HOV ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   TOLL         ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'2  PERSON   TOLL/HOV     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   NON-TOLL     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   NON-TOLL/HOV ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   TOLL         ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'3  PERSON   TOLL/HOV     ',F10.5,3X,E12.5,3X,F6.4//
     *       1X,'WALK                     ',F10.5,3X,E12.5,3X,F6.4/
     *       1X,'BICYCLE                  ',F10.5,3X,E12.5,3X,F6.4/)
      END IF
C
C....................................................................
C
C   ELIMINATE UNAVAILABLE TRANSIT ACCESS MODES FOR PEARSON LOT CHOICE
C
      IF(AIR.AND.AIRTRN) THEN
      DO NI=2,12
      EUTIL(NI)=0.0
      END DO
      DO NI=14,24
      EUTIL(NI)=0.0
      END DO
      EUTIL(52)=0.0          !Drive to Local Bus/Streetcar
      EUTIL(53)=0.0          !Drive to Rapid Bus
      EUTIL(26)=0.0          !Drive to GO Bus
C....CHECK FOR FEEDER BUS USE IN URBAN RAIL PATH
      LBUSIND=0
      SC2=ASTA(2,1)-MAX_IZONES
      IF(SC2.GT.0)  LBUSIND=STAZNEI(SC2,JZ,2,1)+STAZNEI(SC2,JZ,2,2)+
     *  STAZNEI(SC2,JZ,2,3)+STAZNEI(SC2,JZ,2,4)+STAZNEI(SC2,JZ,2,5)
      IF(LBUSIND.GT.0) EUTIL(13)=0.0
      END IF
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
      IF(C.LE.2.AND.ZEROCAR) DENOM=0.0
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
C...UBER ACCESS FOR AIR PASSENGERS
      UTILUBER=0.0
      EXPUBER=0.0
      IF(IEQUIV(OSTA(1,3)).EQ.GOUNION.OR.
     *   IEQUIV(OSTA(1,4)).EQ.GOUNION.AND.AIRPASS) THEN
      DESSTA=EQUIV(GOUNION)
      CALL UBERCOMP(IZ,HTIME2P(DESSTA),HDIST2P(DESSTA),ACOEF(2),
     *              ACOEF(1),COSTUBER,ACCUBER,KWAIT)
      UTILUBER=ACCUBER + ACOEF(3)*(COSTUBER*100.0)*LCPI +
     *         ACNST(11)
      EXPUBER=EXP(UTILUBER)
      END IF
C...PROBABILITIES
      CWPROB(3)=0.0
      CBPROB(3)=0.0
      CPPROB(5)=0.0
      CKPROB(5)=0.0
      CUPROB=0.0
      LSCR=0.0
      DENOM=EUTIL(27)+EUTIL(28)+EUTIL(29)+EUTIL(30)
      IF(DENOM.GT.0.0) THEN
      IF(AIRPASS) DENOM=DENOM+EXPUBER
      CWPROB(3)=EUTIL(27)/DENOM
      CBPROB(3)=EUTIL(28)/DENOM
      CPPROB(5)=EUTIL(29)/DENOM
      CUPROB=EXPUBER/DENOM
      CKPROB(5)=1.0-CWPROB(3)-CBPROB(3)-CPPROB(5)-CUPROB
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
      IF(LDEBUG) THEN
      WRITE(26,9209) (UTIL(K),EUTIL(K),K=27,30),UTILUBER,EXPUBER
 9209 FORMAT(/1X,'SUMMARY OF GO RAIL ACCESS UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'WALK ACCESS TO GO RAIL ',F10.5,3X,E12.5/
     *       1X,'BUS  ACCESS TO GO RAIL ',F10.5,3X,E12.5//
     *       1X,'P&R  ACCESS TO GO RAIL ',F10.5,3X,E12.5/
     *       1X,'K&R  ACCESS TO GO RAIL ',F10.5,3X,E12.5//
     *       1X,'UBER ACCESS TO GO RAIL ',F10.5,3X,E12.5)
      WRITE(26,9210) CWPROB(3),CBPROB(3),CPPROB(5),CKPROB(5),
     *               CUPROB,LSCR
 9210 FORMAT(/1X,'SUMMARY OF GO RAIL ACCESS CHOICE PROB:'/
     *       1X,'---------------------------------------------'/
     *       1X,'WALK  ACCESS=',F8.5/ 
     *       1X,'BUS   ACCESS=',F8.5/
     *       1X,'P&R   ACCESS=',F8.5/
     *       1X,'K&R   ACCESS=',F8.5/
     *       1X,'UBER  ACCESS=',F8.5/
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
      IF(C.LE.2.AND.ZEROCAR) DENOM=0.0
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
C..BUS/STREETCAR
      STRPROB(1)=0.0
      STRPROB(2)=0.0
      LSSTR=0.0
      DENOM=EUTIL(38)+EUTIL(52)
      IF(DENOM.GT.0.0) THEN
      LSSTR=DLOG(DENOM)
      STRPROB(1)=EUTIL(38)/DENOM
      STRPROB(2)=1.0-STRPROB(1)
      END IF
C..RAPID BUS
      RPDPROB(1)=0.0
      RPDPROB(2)=0.0
      LSRPD=0.0
      DENOM=EUTIL(49)+EUTIL(53)
      IF(DENOM.GT.0.0) THEN
      LSRPD=DLOG(DENOM)
      RPDPROB(1)=EUTIL(49)/DENOM
      RPDPROB(2)=1.0-RPDPROB(1)
      END IF
C........................................................................
      IF(DEBUG) THEN
      WRITE(26,9011) GOBPROB(1),GOBPROB(2),LSGOB,
     *               STRPROB(1),STRPROB(2),LSSTR,
     *               RPDPROB(1),RPDPROB(2),LSRPD
 9011 FORMAT(/1X,'SUMMARY OF BASIC TRANSIT CHOICE PROBABILITIES'/
     *       1X,'----------------------------------------------'/
     *       1X,'WALK  ACCESS --> GO BUS      =',F8.5/
     *       1X,'DRIVE ACCESS --> GO BUS      =',F8.5/
     *       1X,'GO BUS LOGSUM                =',F8.2//
     *       1X,'WALK  ACCESS --> BUS/STR.CAR =',F8.5/
     *       1X,'DRIVE ACCESS --> BUS/STR.CAR =',F8.5/
     *       1X,'BUS/STREETCAR LOGSUM         =',F8.2//
     *       1X,'WALK  ACCESS --> RAPID BUS   =',F8.5/
     *       1X,'DRIVE ACCESS --> RAPID BUS   =',F8.5/
     *       1X,'RAPID BUS LOGSUM             =',F8.2/)     
      END IF
C.......................................................................
C
C  SHARED RIDE UTILITIES AND PROBABILITIES
C 
      IF(LS2PER.NE.0.0) THEN
	    UTIL(65)=LSUM3P2*LS2PER + K2P(C)/(LSUM1AUTO*LSUM2SR)
      EUTIL(65)=EXP(UTIL(65))
	    ENDIF
C
      IF(LS3PER.NE.0.0) THEN
	    UTIL(66)=LSUM3P3*LS3PER + K3P(C)/(LSUM1AUTO*LSUM2SR)
      EUTIL(66)=EXP(UTIL(66))
	    ENDIF
C
      SRPROB(1)=0.0
      SRPROB(2)=0.0
      LSSHR=0.0
C
      DENOM=EUTIL(65)+EUTIL(66)
C
      IF(DENOM.GT.0.0) THEN
      LSSHR=DLOG(DENOM)
      SRPROB(1)=EUTIL(65)/DENOM
      SRPROB(2)=1.0-SRPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9012) C,LS2PER,LS3PER,
     *               K2P(C),K3P(C),
     *               UTIL(65),UTIL(66),
     *               SRPROB(1),SRPROB(2),LSSHR
 9012 FORMAT(/1X,'SUMMARY OF SHARED RIDE CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'MARKET SEGMENT   =',I9/
     *       1X,'2  PERSON LOGSUM =',F9.3/
     *       1X,'3+ PERSON LOGSUM =',F9.3/
     *       1X,'K2P      CONSTANT=',F9.4/
     *       1X,'K3P      CONSTANT=',F9.4/
     *       1X,'2  PERSON UTILITY=',F9.3/
     *       1X,'3+ PERSON UTILITY=',F9.3/
     *       1X,'2  PERSON PROB   =',F9.4/ 
     *       1X,'3+ PERSON PROB   =',F9.4//
     *       1X,'SHARED RIDE LOGSUM=',F10.5)
	  END IF
C....................................................................
C
C  CALCULATE PRIMARY TRANSIT MODE PROBABILITIES
C
      DIZ=IEQUIV(IZ)
      DJZ=IEQUIV(JZ)
      DIZ=DEQUIV(DIZ)
      DJZ=DEQUIV(DJZ)
      KKCBD=0.0
      IF(DJZ.EQ.1) THEN
      KKCBD(1)=KCBD(1)          !GO Rail
      KKCBD(2)=KCBD(2)          !TTC Subway
      KKCBD(3)=KCBD(3)          !GO Bus
      KKCBD(4)=KCBD(4)          !Bus/Streetcar
      KKCBD(5)=KCBD(5)          !Rapid Bus
      IF(DIZ.LE.4) KKCBD(1)=0.0
      IF(DIZ.GT.2) KKCBD(4)=0.0
      END IF
      KKDIST=KDIST(DJZ)
C..GO RAIL
      IF(LSCR.NE.0.0) THEN
      UTIL(35)=LSUM2CR*LSCR + KCR(C)/(LSUM1TRN) + KKCBD(1)/LSUM1TRN
     * +KKDIST/LSUM1TRN + (COEFF(50+C)*(GORZFARE(JZ)*100.0))/LSUM1TRN
      EUTIL(35)=EXP(UTIL(35))
      END IF
      IF(AIRTRN) EUTIL(35)=0.0
C..TTC SUBWAY
      IF(LSURB.NE.0.0) THEN
      UTIL(36)=LSUM2UR*LSURB + KUR(C)/(LSUM1TRN)+ KKCBD(2)/LSUM1TRN
     * +KKDIST/LSUM1TRN + (COEFF(50+C)*(TTCZFARE(JZ)*100.0))/LSUM1TRN
      EUTIL(36)=EXP(UTIL(36))
      END IF
C..GO BUS
      IF(LSGOB.NE.0.0) THEN
      UTIL(37)=LSUM2GB*LSGOB + KGBUS(C)/(LSUM1TRN)+ KKCBD(3)/LSUM1TRN
     *        +KKDIST/LSUM1TRN
      EUTIL(37)=EXP(UTIL(37))
      END IF
      IF(AIRTRN) EUTIL(37)=0.0
C..BUS/STREETCAR
      IF(LSSTR.NE.0.0) THEN
      UTIL(50)=LSUM2SB*LSSTR + KLBUS(C)/LSUM1TRN + KKCBD(4)/LSUM1TRN
     *        +KKDIST/LSUM1TRN
      EUTIL(50)=EXP(UTIL(50))
      END IF
C..RAPID BUS
      IF(LSRPD.NE.0.0) THEN
      UTIL(51)=LSUM2RB*LSRPD + KRBUS(C)/LSUM1TRN + KKCBD(5)/LSUM1TRN
     *        +KKDIST/LSUM1TRN
      EUTIL(51)=EXP(UTIL(51))
      END IF
C..TRANSIT PROBABILITIES
      TRNPROB=0.0
      LSTRN(C)=0.0
      DENOM=EUTIL(35)+EUTIL(36)+EUTIL(37)+EUTIL(50)+EUTIL(51)
      IF(DENOM.GT.0.0) THEN
      LSTRN(C)=DLOG(DENOM)
      TRNPROB(1)=EUTIL(35)/DENOM
      TRNPROB(2)=EUTIL(36)/DENOM
      TRNPROB(3)=EUTIL(37)/DENOM
      TRNPROB(4)=EUTIL(50)/DENOM
      TRNPROB(5)=1.0-TRNPROB(1)-TRNPROB(2)-TRNPROB(3)-TRNPROB(4)
      END IF
      IF(IZ.EQ.JZ) LSTRN(C)=0.0
C....................................................................
      IF(DEBUG.OR.(LDEBUG.AND.(.NOT.AIRTRN))) THEN
      WRITE(26,9013) C,M,(UTIL(K),EUTIL(K),K=35,37),UTIL(50),
     *               EUTIL(50),UTIL(51),EUTIL(51),KKDIST
 9013 FORMAT(/1X,'SUMMARY OF PRIMARY TRANSIT UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,' MARKET SEGMENT=',I1,' ACCESS SEGMENT=',I1/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'GO RAIL                 ',F10.5,3X,E12.5/
     *       1X,'TTC SUBWAY              ',F10.5,3X,E12.5/
     *       1X,'GO BUS                  ',F10.5,3X,E12.5/
     *       1X,'BUS/STREETCAR           ',F10.5,3X,E12.5/
     *       1X,'RAPID BUS               ',F10.5,3X,E12.5//
     *       1X,'DISTRICT LEVEL CONSTANT ',F10.5/)
      WRITE(26,9014) TRNPROB(1),TRNPROB(2),TRNPROB(3),
     *               TRNPROB(4),TRNPROB(5),LSTRN(C)
 9014 FORMAT(/1X,'SUMMARY OF TRANSIT CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'GO RAIL        =',F8.5/
     *       1X,'TTC SUBWAY     =',F8.5/
     *       1X,'GO BUS         =',F8.5/
     *       1X,'BUS/STREETCAR  =',F8.5/
     *       1X,'RAPID BUS      =',F8.5/
     *       1X,'TRANSIT LOGSUM =',F10.5/)
      END IF
C.....................................................................
C
C SAVE TRANSIT PROBABILITIES & STATION VALUES 
C FOR PARKING LOT CHOICE MODEL
C
      IF(AIR.AND.AIRTRN.AND.(PARKIND.GT.0)) THEN
      JZIND=0
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      IF(JZIND.LE.0) THEN
      WRITE(26,8043) IEQUIV(JZ)
      WRITE(*,8043)  IEQUIV(JZ)
 8043 FORMAT(//1X,'MLOGIT 8043 (F) NO PEARSON INDICATOR MATCH FOR',
     *          ' ATTRACTION ZONE ',I4//)
      STOP
      END IF 
      AIRTPROB(PARKIND,JZIND,1)=TRNPROB(1)   !GO Rail
      AIRTPROB(PARKIND,JZIND,2)=TRNPROB(2)   !TTC Subway
      AIRTPROB(PARKIND,JZIND,3)=TRNPROB(3)   !GO Bus
      AIRTPROB(PARKIND,JZIND,4)=TRNPROB(4)   !Bus/StreetCar
      AIRTPROB(PARKIND,JZIND,5)=TRNPROB(5)   !Rapid Bus
C     AIRTPROB(PARKIND,JZIND,6)=TRNPROB(6)   !High Level BRT
      AIRTPROB(PARKIND,JZIND,7)=LSTRN(1)
      AIRSTAP(PARKIND,JZIND,1)=OSTA(1,1)
      AIRSTAP(PARKIND,JZIND,2)=OSTA(2,1)
      AIRSTAA(PARKIND,JZIND,1)=ASTA(1,1)
      AIRSTAA(PARKIND,JZIND,2)=ASTA(2,1)
C ----------------------------------------------------
      IF(LDEBUG) THEN                             
      WRITE(26,8047) IEQUIV(IZ),PARKIND,IEQUIV(JZ),JZIND
 8047 FORMAT(/' IZ=',I4,' PARKIND=',I2,' JZ=',I4,
     *       ' JZIND=',I2)
      DO K=1,2
      WRITE(26,8148) K,
     *        AIRSTAP(PARKIND,JZIND,K),AIRSTAA(PARKIND,JZIND,K)
 8148 FORMAT(1X,I2,' OSTA=',I4,' ASTA=',I4)
      END DO
      DO K=1,6
      WRITE(26,8152) K,AIRTPROB(PARKIND,JZIND,K)
 8152 FORMAT(1X,I2,'  TRNPROB=',F8.3)
      END DO
      WRITE(26,8153) AIRTPROB(PARKIND,JZIND,7)
 8153 FORMAT(1X,' 7','    LSTRN=',F8.3//)
      END IF
C -------------------------------------------------------------------
      END IF
C
C SAVE TRANSIT PROBABILITIES & STATION VALUES 
C FOR RENTAL LOT CHOICE MODEL
C
      IF(AIR.AND.AIRTRN.AND.(RNTLIND.GT.0)) THEN
      JZIND=0
      DO K=1,50
      IF(AEQUIV(K).EQ.JZ) JZIND=K
      END DO
      IF(JZIND.LE.0) THEN
      WRITE(26,8043) IEQUIV(JZ)
      WRITE(*,8043)  IEQUIV(JZ)
 8044 FORMAT(//1X,'MLOGIT 8044 (F) NO RENTAL CAR INDICATOR MATCH FOR',
     *          ' ATTRACTION ZONE ',I4//)
      STOP
      END IF 
      AIRRPROB(RNTLIND,JZIND,1)=TRNPROB(1)   !GO Rail
      AIRRPROB(RNTLIND,JZIND,2)=TRNPROB(2)   !TTC Subway
      AIRRPROB(RNTLIND,JZIND,3)=TRNPROB(3)   !GO Bus
      AIRRPROB(RNTLIND,JZIND,4)=TRNPROB(4)   !Bus/StreetCar
      AIRRPROB(RNTLIND,JZIND,5)=TRNPROB(5)   !Rapid Bus
C     AIRRPROB(RNTLIND,JZIND,6)=TRNPROB(6)   !High Level BRT
      AIRRPROB(RNTLIND,JZIND,7)=LSTRN(1)
      AIRRNTP(RNTLIND,JZIND,1)=OSTA(1,1)
      AIRRNTP(RNTLIND,JZIND,2)=OSTA(2,1)
      AIRRNTA(RNTLIND,JZIND,1)=ASTA(1,1)
      AIRRNTA(RNTLIND,JZIND,2)=ASTA(2,1)
C ----------------------------------------------------
      IF(LDEBUG) THEN                             
      WRITE(26,8045) IEQUIV(IZ),RNTLIND,IEQUIV(JZ),JZIND
 8045 FORMAT(/' IZ=',I4,' RNTLIND=',I2,' JZ=',I4,
     *       ' JZIND=',I2)
      DO K=1,2
      WRITE(26,8148) K,
     *        AIRRNTP(RNTLIND,JZIND,K),AIRRNTA(RNTLIND,JZIND,K)
      END DO
      DO K=1,6
      WRITE(26,8152) K,AIRRPROB(RNTLIND,JZIND,K)
      END DO
      WRITE(26,8153) AIRRPROB(RNTLIND,JZIND,7)
      END IF
C -------------------------------------------------------------------
      END IF
C
C  DRIVE ALONE & SHARED RIDE PROBABILITIES
C
C ..DRIVE-ALONE UTILITY
      IF(LSDA.NE.0.0) THEN
	    UTIL(67)=LSUM2DA*LSUM3DA*LSDA + KDA(C)/LSUM1AUTO
      EUTIL(67)=EXP(UTIL(67))
	    ENDIF
C..SHARED RIDE UTILITY
      IF(LSSHR.NE.0.0) THEN
	    UTIL(68)=LSUM2SR*LSSHR + KSR(C)/LSUM1AUTO
      EUTIL(68)=EXP(UTIL(68))
	    ENDIF
C..AUTO SUBMODE PROBABILITIES
      ATPROB(1)=0.0
      ATPROB(2)=0.0
      LSAUTO=0.0
      DENOM=EUTIL(67)+EUTIL(68)
      IF(DENOM.GT.0.0) THEN
      LSAUTO=DLOG(DENOM)
      ATPROB(1)=EUTIL(67)/DENOM
      ATPROB(2)=1.0-ATPROB(1)
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9115) UTIL(67),EUTIL(67),UTIL(68),EUTIL(68)
 9115 FORMAT(/1X,'SUMMARY OF AUTO SUBMODE UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'DRIVE ALONE             ',F10.5,3X,F12.5,/
     *       1X,'SHARED RIDE             ',F10.5,3X,F12.5) 
      WRITE(26,9416) ATPROB(1),ATPROB(2),LSAUTO
 9416 FORMAT(/1X,'SUMMARY OF AUTO SUBMODE CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'DRIVE ALONE   =',F8.5/ 
     *       1X,'SHARED RIDE   =',F8.5//
     *       1X,'AUTO LOGSUM   =',F10.5)
      END IF
C....................................................................
C
C  CALCULATE MOTORIZED UTILITIES AND PROBABILITIES
C
C..AUTO
      IF(LSAUTO.NE.0.0) THEN
      UTIL(69)=LSUM1AUTO*LSAUTO + KAUT(C)
      EUTIL(69)=EXP(UTIL(69))
      ELSE
      AUTEXP=0.0
      ENDIF
      IF(AIR.AND.(.NOT.AIRTRN)) THEN
      IF(LSLOT.NE.0.0) THEN
      UTIL(69)=LSLOT
      EUTIL(69)=EXP(UTIL(69))
      ELSE
      AUTEXP=0.0
      END IF
      END IF
C..TRANSIT
      PINDEX=IFIX(HDIST(JZ)/5.0)+1
      IF(PINDEX.GT.25) PINDEX=25
      IF(LSTRN(C).NE.0.0) THEN
      UTIL(70)=LSUM1TRN*LSTRN(C) + KTRN(C) + KTRNT(PINDEX,C)
      EUTIL(70)=EXP(UTIL(70))
      END IF
      IF(NOTRANSIT) EUTIL(70)=0.0
C..NON-MOTORIZED
      IF(LSNMOT.NE.0.0.AND.NMOT) THEN
      NMADJ=0.0
      IF((DIZ.LE.2).AND.(DJZ.LE.2).AND.(DIZ.GT.0).AND.(DJZ.GT.0)) 
     *    NMADJ=KNMCBD(DIZ,DJZ)
      UTIL(71)=LSUM1NM*LSNMOT+KNMOT(C)+NMADJ
      EUTIL(71)=EXP(UTIL(71))
      END IF
C..NON-TRANSIT EXPOENTIATED UTILITY FOR FTA USERBEN FILE
      AUTEXP=EUTIL(69)+EUTIL(71)
C..PROBABILITIES
      MOTOR=0.0
      LSMOT=0.0
      DENOM=EUTIL(69)+EUTIL(70)+EUTIL(71)
      IF(DENOM.GT.0.0) THEN
      LSMOT=DLOG(DENOM)
      MOTOR(2)=EUTIL(70)/DENOM
      MOTOR(3)=EUTIL(71)/DENOM
      MOTOR(1)=1.0-MOTOR(2)-MOTOR(3)
      MOTOR(1)=AMAX1(MOTOR(1),0.0)
      IF(MWALK(M).GT.0) ULOGSUM(C)=ULOGSUM(C)+EXP(LOG(MWALK(M))+LSMOT)
      END IF
C========================================================================
C
C  AIR PASSENGER PROBABILITY COMPUTATIONS
C
      IF(AIRPASS.AND.(.NOT.AIRTRN).AND.(AIR)) THEN
C      IF((.NOT.AIRTRN).AND.(AIR)) THEN
      DO NI=1,10
      EUTLAIR(NI)=0.0
      PROBAIR(NI)=0.0
      END DO
      EUTLAIR(1)=EXP(UTLATXI)
      IF(UTLARNT.NE.0.0) EUTLAIR(2)=EXP(UTLARNT)
      EUTLAIR(3)=EXP(UTLALMO)
      EUTLAIR(4)=EXP(UTLADRP)
      IF(LSLOT.NE.0.0) EUTLAIR(5)=EXP(LSLOT+ACNST(5))
      EUTLAIR(6)=EXP(UTLAONC)
      IF(LSTRN(C).NE.0.0) EUTLAIR(7)=EXP(LSTRN(C)+ACNST(7))
      EUTLAIR(8)=EXP(UTLAUBER)
      IF(UBERIN) EUTLAIR(8)=0.5*EUTLAIR(8)
C...COMPUTE PRIVATE SHARES & LOGSUM
      DENOM=EUTLAIR(1)+EUTLAIR(2)+EUTLAIR(3)+EUTLAIR(4)+EUTLAIR(5)+
     *      EUTLAIR(8)
      IF(DENOM.GT.0.0) LSPRV=LSUM1AUTO*DLOG(DENOM)
      DO NI=1,5
      PROBAIR(NI)=EUTLAIR(NI)/DENOM
      END DO
      PROBAIR(8)=EUTLAIR(8)/DENOM
C...COMPUTE PUBLIC SHARES & LOGSUM
      DENOM=EUTLAIR(6)+EUTLAIR(7)
      IF(DENOM.GT.0.0) LSPUB=LSUM1AUTO*DLOG(DENOM)
      PROBAIR(6)=EUTLAIR(6)/DENOM
      PROBAIR(7)=(EUTLAIR(7)/DENOM)
C...COMPUTE FINAL SHARES
      DENOM=EXP(LSPUB+ACNST(9))+EXP(LSPRV)
      PROBAIR(10)=EXP(LSPRV)/DENOM
      PROBAIR(9)=1.0-PROBAIR(10)
C...COMPUTE THE NON-TRANSIT EXPONENTIATED UTILITY
      AUTEXP=EUTLAIR(1)+EUTLAIR(2)+EUTLAIR(3)+EUTLAIR(4)+EUTLAIR(5)+
     *       EUTLAIR(6)+EUTLAIR(8)
      AUTEXP=EXP(LSUM1AUTO*LOG(AUTEXP))
C     WRITE(26,453) (EUTLAIR(K),K=1,8),LSPRV,LSPUB
  453 FORMAT(' EUTLAIR=',8(1X,F8.5),' LSPRV=',F10.5,' LSPUB=',F10.5)
C ------------------------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,451) (PROBAIR(K),K=1,5),PROBAIR(8),(PROBAIR(K2),K2=6,7),
     *              (PROBAIR(K1),K1=9,10)
  451 FORMAT(/
     *    1X,' AIR PASSENGER MODEL MODE SHARE VALUES'/
     *    1X ' -------------------------------------'/
     *    1X,'                                            PARKING'/
     *    1X,'   TAXI       RENTAL    LIMO      DROPOFF      LOT  ',
     *       '      TNC      ON-CALL    TRANSIT    PUBLIC     PRIVATE'/
     *    1X,'  SHARE       SHARE     SHARE      SHARE      SHARE ',
     *    '      SHARE      SHARE      SHARE      SHARE     SHARE'/
     *    1X,' ---------   --------  ---------  ---------  ---------',
     *    '  ---------  ---------  ---------  ---------  ---------'/
     *       10(2X,F9.5))
      END IF
C -------------------------------------------------------------------------      
      END IF
C============================================================================
C
C  COMPUTE FTA RELATED USER BENEFIT INFORMATION
C
      IF(AIRPASS) THEN
      IF((M.LE.4).AND.(FWALK(1).GT.0.0))
     *    FSHAR(1)=FSHAR(1)+(MWALK(M)/FWALK(1))*(PROBAIR(7)*PROBAIR(9))
      IF((M.EQ.5.OR.M.EQ.6).AND.(FWALK(2).GT.0.0))
     *    FSHAR(2)=FSHAR(2)+(MWALK(M)/FWALK(2))*(PROBAIR(7)*PROBAIR(9))
      ELSE
      IF((M.LE.4).AND.(FWALK(1).GT.0.0)) THEN
          FSHAR(1)=FSHAR(1)+(MWALK(M)/FWALK(1))*MOTOR(2)
      END IF
      IF((M.EQ.5.OR.M.EQ.6).AND.(FWALK(2).GT.0.0)) THEN
          FSHAR(2)=FSHAR(2)+(MWALK(M)/FWALK(2))*MOTOR(2)
      END IF
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9017) UTIL(69),EUTIL(69),
     *               UTIL(70),EUTIL(70),
     *               UTIL(71),EUTIL(71)
 9017 FORMAT(/1X,'SUMMARY OF BASIC UTILITIES'/
     *       1X,'-----------------------------------'/
     *       1X,'                     ',6X,'UTIL',10X,'EUTIL'/
     *       1X,'                     ',3X,'----------',5X,
     *          '----------'/
     *       1X,'AUTO                    ',F10.5,3X,E13.6/
     *       1X,'TRANSIT                 ',F10.5,3X,E13.6/
     *       1X,'NON-MOTORIZED           ',F10.5,3X,E13.6)
      WRITE(26,9018) MOTOR(1),MOTOR(2),MOTOR(3),LSMOT,C,
     *               ULOGSUM(C),
     *               FSHAR(1),FSHAR(2)
 9018 FORMAT(/1X,'SUMMARY OF BASIC CHOICE PROBABILITIES'/
     *       1X,'---------------------------------------------'/
     *       1X,'AUTO          =',F8.5/ 
     *       1X,'TRANSIT       =',F8.5/
     *       1X,'NON-MOTORIZED =',F8.5/
     *       1X,'TOTAL LOGSUM  =',F10.5//,
     *       1X,'SUMMARY OF WEIGHTED LOGSUM VALUES'/
     *       1X,'---------------------------------'/
     *       1X,'MARKET SEGMENT=',I8/
     *       1X,'TOTAL   LOGSUM=',F10.5//
     *       1X,'SUMMARY OF TRANSIT MODE SHARES  '/
     *       1X,'--------------------------------'/
     *       1X,' WALK ACCESS  =',F8.5/
     *       1X,'DRIVE ACCESS  =',F8.5/) 
      WRITE(26,9019) FSHAR(1),FWALK(1),
     *               FSHAR(2),FWALK(2)
 9019 FORMAT(/1X,'SUMMARY OF SUMMIT COMPUTATIONS'/
     *        1X,'------------------------------'/
     *        1X,'WALK/DRIVE TRANSIT SHARE=',F8.5/
     *        1X,'           MARKET  SHARE=',F8.5/
     *        1X,'DRIVE ONLY TRANSIT SHARE=',F8.5/
     *        1X,'           MARKET  SHARE=',F8.5/)
      END IF
C....................................................................
C
C     COMPUTE TRIP VALUES
C
      TSCHBUS=PERTRP*SCHBUS
      TMOTOR=PERTRP-TSCHBUS
      TTRAN=TMOTOR*MOTOR(2)
      TNMOT=TMOTOR*MOTOR(3)
      TAUTO=TMOTOR-TTRAN-TNMOT
C
C   AIR PASSENGER TRIP MATRIX VALUES
C
      IF(AIRPASS.AND.(.NOT.AIRTRN)) THEN
C     IF((.NOT.AIRTRN)) THEN
      PERTRP=PERTRP*AIRFCTR
      TNMOT=0.0
      TSCHBUS=0.0
      TTRAN=PERTRP*PROBAIR(9)*PROBAIR(7)
      TDROP=PERTRP*PROBAIR(10)*PROBAIR(4)
      TLIMO=PERTRP*PROBAIR(10)*PROBAIR(3)
      TRNTL=PERTRP*PROBAIR(10)*PROBAIR(2)
      TTAXI=PERTRP*PROBAIR(10)*PROBAIR(1)
      TAUTO=PERTRP*PROBAIR(10)*PROBAIR(5)
      TUBER=PERTRP*PROBAIR(10)*PROBAIR(8)
      TONCL=PERTRP-TTRAN-TDROP-TLIMO-TRNTL-TTAXI-TAUTO-TUBER
C ---------------------------------------------------------
      IF(LDEBUG) THEN
      WRITE(26,452) IEQUIV(IZ),IEQUIV(JZ),
     *              PERTRP,TTRAN,TDROP,TLIMO,TRNTL,TTAXI,TAUTO,TONCL,
     *              TUBER
  452 FORMAT(//' AIR PASSENGER TRIP VALUES'/
     *       ' -------------------------'/
     *       ' ORIGIN  =',I8/
     *       ' DEST    =',I8/
     *       ' PERTRP  =',F8.2/
     *       ' TRANSIT =',F8.2/
     *       ' DROP-OFF=',F8.2/
     *       ' LIMO    =',F8.2/
     *       ' RENTAL  =',F8.2/
     *       ' TAXI    =',F8.2/
     *       ' PARKED  =',F8.2/
     *       ' ON-CALL =',F8.2/
     *       ' TNC     =',F8.2)
      END IF
C --------------------------------------------------------------
      END IF
C
      TDRV0=TAUTO*ATPROB(1)
      TDSHR=TAUTO-TDRV0
C
      TDRV2=TDSHR*SRPROB(1)
      TDRV3=TDSHR-TDRV2
C
      TDRV0N=TDRV0*DAPROB(1)
      TDRV0T=TDRV0-TDRV0N
C
      TDRV2NH=TDRV2*P2PROB(2)
      TDRV2NN=TDRV2*P2PROB(1)
      TDRV2TH=TDRV2*P2PROB(4)
      TDRV2TN=TDRV2*P2PROB(3)
C
      TDRV3NH=TDRV3*P3PROB(2)
      TDRV3NN=TDRV3*P3PROB(1)
      TDRV3TH=TDRV3*P3PROB(4)
      TDRV3TN=TDRV3*P3PROB(3)
C
      TCR=TTRAN*TRNPROB(1)
      TUR=TTRAN*TRNPROB(2)
      TGB=TTRAN*TRNPROB(3)
      TSTR=TTRAN*TRNPROB(4)
      TRPD=TTRAN-TCR-TUR-TGB-TSTR
      TRPD=AMAX1(TRPD,0.0)
C
      TGBW=TGB*GOBPROB(1)
      TGBD=TGB-TGBW
      TGBD=AMAX1(TGBD,0.0)
C
      TSTRW=TSTR*STRPROB(1)
      TSTRD=TSTR-TSTRW
      TSTRD=AMAX1(TSTRD,0.0)
C
      TRPDW=TRPD*RPDPROB(1)
      TRPDD=TRPD-TRPDW
      TRPDD=AMAX1(TRPDD,0.0)
C
      TCRW=TCR*CWPROB(3)
      TCRB=TCR*CBPROB(3)
      TCRP=TCR*CPPROB(5)
      TUBERACC=TCR*CUPROB
      TCRK=TCR-(TCRW+TCRB+TCRP+TUBERACC)
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
      TNMWK=TNMOT*NMPROB(1)
      TNMBK=TNMOT*NMPROB(2)
C
C  INTRAZONAL COMPUTATIONS
C
      IF(IZ.EQ.JZ) THEN
      TTRAN=0.0
      TAUTO=0.0
      TNMOT=0.0
      TDRV0=0.0
      TDSHR=0.0
      TCR=0.0
      TUR=0.0
      TGB=0.0
      TSTR=0.0
      TRPD=0.0
      TDRV2=0.0
      TDRV3=0.0
      TGBW=0.0
      TGBD=0.0
      TSTRW=0.0
      TSTRD=0.0
      TRPDW=0.0
      TRPDD=0.0
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
      TDRV0N=0.0
      TDRV0T=0.0
      TDRV2NH=0.0
      TDRV2NN=0.0
      TDRV2TH=0.0
      TDRV2TN=0.0
      TDRV3NH=0.0
      TDRV3NN=0.0
      TDRV3TH=0.0
      TDRV3TN=0.0
      TNMWK=0.0
      TNMBK=0.0
      TSCHBUS=0.0
      TSCHBUS=PERTRP*INTRASCHBUS(C)
      TDRV2NN=PERTRP*INTRA2P(C)
      TDRV3NN=PERTRP*INTRA3P(C)
      IF(NMOT) THEN
      TNMBK=PERTRP*INTRABIKE(C)
      TNMWK=PERTRP*INTRAWLK(C)
      TNMOT=TNMBK+TNMWK
      ELSE
      TNMBK=0.0
      TNMWK=0.0
      TNMOT=0.0
      END IF
      TDRV0N=PERTRP-TNMWK-TDRV2NN-TDRV3NN-TNMBK-TSCHBUS
      TDRV0N=AMAX1(TDRV0N,0.0)
      TDRV0=TDRV0N
      TDRV2=TDRV2NN
      TDRV3=TDRV3NN
      TAUTO=TDRV0+TDRV2+TDRV3
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9441) C,M,PERTRP,INTRA2P(C),
     *                INTRA3P(C),INTRABIKE(C),
     *                INTRASCHBUS(C),INTRAWLK(C)
 9441 FORMAT(1X,' INTRAZONAL PROPORTIONS'/
     *       1X,' ----------------------'/
     *       ' SEGMENT=',I1,' ACCESS=',I1,' PERTRP=',F10.5,
     *       ' INTRA2P=',F6.4,
     *       ' INTRA3P=',F6.4,' INTRABIKE=',F6.4,
     *       ' INTRASCHBUS=',F6.4,' INTRAWLK=',F6.4)
      END IF
C....................................................................
      END IF
C
      IF(DJZ.EQ.1) THEN
      ESTCBD(1)=ESTCBD(1)+TCR
      ESTCBD(2)=ESTCBD(2)+TUR
      ESTCBD(3)=ESTCBD(3)+TGB
      ESTCBD(4)=ESTCBD(4)+TSTR
      ESTCBD(5)=ESTCBD(5)+TRPD
      END IF
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9421) C,M,PERTRP,TTRAN,TAUTO,TNMOT,TCR,TUR,TGB,
     *               TSTR,TRPD,TGBW,TGBD,TSTRW,TSTRD,TRPDW,TRPDD,
     *               TCRW,TCRB,TCRP,TCRK,TCRW1,TCRW2,
     *               TCRB1,TCRB2,
     *               TCRP1,TCRP2,TCRP3,TCRP4,
     *               TCRK1,TCRK2,TCRK3,TCRK4,
     *               TURW,TURB,TURP,TURK,TURW1,TURW2,
     *               TURB1,TURB2,
     *               TURP1,TURP2,TURP3,TURP4,
     *               TURK1,TURK2,TURK3,TURK4,
     *               TDRV0,TDRV2,TDRV3,
     *               TDRV0N,TDRV0T,
     *               TDRV2NH,TDRV2NN,TDRV2TH,TDRV2TN,
     *               TDRV3NH,TDRV3NN,TDRV3TH,TDRV3TN,
     *               TNMWK,TNMBK
 9421 FORMAT(/1X,'SUMMARY OF MODAL TRIP VALUES'/
     *       1X,'-----------------------------------'/
     *       1X,'MARKET SEGMENT                 =',I2/
     *       1X,'ACCESS SEGMENT                 =',I2//
     *       1X,'PERSON TRIPS                   =',F10.5/
     *       1X,'TOTAL TRANSIT                  =',F10.5/
     *       1X,'TOTAL AUTO                     =',F10.5/
     *       1X,'TOTAL NON-MOTORIZED            =',F10.5//
     *       1X,'GO RAIL                        =',F10.5/
     *       1X,'TTC SUBWAY                     =',F10.5/
     *       1X,'GO BUS                         =',F10.5/
     *       1X,'BUS/STREETCAR                  =',F10.5/
     *       1X,'RAPID BUS                      =',F10.5//
     *       1X,'GO BUS WALK                    =',F10.5/
     *       1X,'GO BUS DRIVE                   =',F10.5/
     *       1X,'BUS STREETCAR WALK             =',F10.5/
     *       1X,'BUS STREETCAR DRIVE            =',F10.5/
     *       1X,'RAPID BUS WALK                 =',F10.5/
     *       1X,'RAPID BUS DRIVE                =',F10.5//
     *       1X,'WALK  --> GO RAIL              =',F10.5/
     *       1X,'BUS   --> GO RAIL              =',F10.5/
     *       1X,'P&R   --> GO RAIL              =',F10.5/
     *       1X,'K&R   --> GO RAIL              =',F10.5//
     *       1X,'GO RAIL       WALK  -STATION #1=',F10.5/
     *       1X,'GO RAIL       WALK  -STATION #2=',F10.5/
     *       1X,'GO RAIL       BUS   -STATION #1=',F10.5/
     *       1X,'GO RAIL       BUS   -STATION #2=',F10.5/
     *       1X,'GO RAIL       PNR   -STATION #1=',F10.5/
     *       1X,'GO RAIL       PNR   -STATION #2=',F10.5/
     *       1X,'GO RAIL       PNR   -STATION #3=',F10.5/
     *       1X,'GO RAIL       PNR   -STATION #4=',F10.5/
     *       1X,'GO RAIL       PNR   -STATION #1=',F10.5/
     *       1X,'GO RAIL       PNR   -STATION #2=',F10.5/
     *       1X,'GO RAIL       KNR   -STATION #3=',F10.5/
     *       1X,'GO RAIL       KNR   -STATION #4=',F10.5//
     *       1X,'WALK  --> TTC SUBWAY           =',F10.5/
     *       1X,'BUS   --> TTC SUBWAY           =',F10.5/
     *       1X,'P&R   --> TTC SUBWAY           =',F10.5/
     *       1X,'K&R   --> TTC SUBWAY           =',F10.5//
     *       1X,'TTC SUBWAY    WALK  -STATION #1=',F10.5/
     *       1X,'TTC SUBWAY    WALK  -STATION #2=',F10.5/
     *       1X,'TTC SUBWAY    BUS   -STATION #1=',F10.5/
     *       1X,'TTC SUBWAY    BUS   -STATION #2=',F10.5/
     *       1X,'TTC SUBWAY    PNR   -STATION #1=',F10.5/
     *       1X,'TTC SUBWAY    PNR   -STATION #2=',F10.5/
     *       1X,'TTC SUBWAY    PNR   -STATION #3=',F10.5/
     *       1X,'TTC SUBWAY    PNR   -STATION #4=',F10.5/
     *       1X,'TTC SUBWAY    KNR   -STATION #1=',F10.5/
     *       1X,'TTC SUBWAY    KNR   -STATION #2=',F10.5/
     *       1X,'TTC SUBWAY    KNR   -STATION #3=',F10.5/
     *       1X,'TTC SUBWAY    KNR   -STATION #4=',F10.5//
     *       1X,'DRIVE ALONE                   =',F10.5/
     *       1X,'2-PERSON AUTO                 =',F10.5/
     *       1X,'3+PERSON AUTO                 =',F10.5/
     *       1X,'DRIVE ALONE NON-TOLL          =',F10.5/
     *       1X,'DRIVE ALONE TOLL              =',F10.5/
     *       1X,'2 PERSON NON-TOLL/HOV         =',F10.5/
     *       1X,'2 PERSON NON-TOLL/NON-HOV     =',F10.5/
     *       1X,'2 PERSON TOLL/HOV             =',F10.5/
     *       1X,'2 PERSON TOLL/NON-HOV         =',F10.5/
     *       1X,'3+PERSON NON-TOLL/HOV         =',F10.5/
     *       1X,'3+PERSON NON-TOLL/NON-HOV     =',F10.5/
     *       1X,'3+PERSON TOLL/HOV             =',F10.5/
     *       1X,'3+PERSON TOLL/NON-HOV         =',F10.5//
     *       1X,'NON-MOTORIZED WALK            =',F10.5/
     *       1X,'NON-MOTORIZED BIKE            =',F10.5/)
      END IF
C....................................................................
C
C  STORE TRIP END SUMMARY INFORMATION
C
C..UPPER LEVEL
      TESUM(1,C)=TESUM(1,C) + PERTRP
      TESUM(2,C)=TESUM(2,C) + TTRAN
      TESUM(3,C)=TESUM(3,C) + TCR
      TESUM(4,C)=TESUM(4,C) + TUR
      TESUM(5,C)=TESUM(5,C) + TGB
      TESUM(6,C)=TESUM(6,C) + TSCHBUS
      TESUM(36,C)=TESUM(36,C) + TNMOT
C..TRANSIT PRIMARY/SUBMODE LEVEL
      TESUM(7,C)=TESUM(7,C) + TGBW
      TESUM(8,C)=TESUM(8,C) + TGBD
      TESUM(9,C)=TESUM(9,C) + TCRW
      TESUM(10,C)=TESUM(10,C) + TCRB
      TESUM(11,C)=TESUM(11,C) + TCRP
      TESUM(12,C)=TESUM(12,C) + TCRK
      TESUM(39,C)=TESUM(39,C) + TUBERACC
      TESUM(13,C)=TESUM(13,C) + TURW
      TESUM(14,C)=TESUM(14,C) + TURB
      TESUM(15,C)=TESUM(15,C) + TURP
      TESUM(16,C)=TESUM(16,C) + TURK
      TESUM(17,C)=TESUM(17,C) + TSTRW
      TESUM(18,C)=TESUM(18,C) + TRPDW
      TESUM(19,C)=TESUM(19,C) + TSTRD
      TESUM(20,C)=TESUM(20,C) + TRPDD
      TESUM(37,C)=TESUM(37,C) + TSTRW + TSTRD
      TESUM(38,C)=TESUM(38,C) + TRPDW + TRPDD
C...AUTO TRIPS
      TESUM(21,C)=TESUM(21,C) + TDRV0N
      TESUM(22,C)=TESUM(22,C) + TDRV0T
      TESUM(24,C)=TESUM(24,C) + TDRV2NH
      TESUM(23,C)=TESUM(23,C) + TDRV2NN
      TESUM(26,C)=TESUM(26,C) + TDRV2TH
      TESUM(25,C)=TESUM(25,C) + TDRV2TN
      TESUM(28,C)=TESUM(28,C) + TDRV3NH
      TESUM(27,C)=TESUM(27,C) + TDRV3NN
      TESUM(30,C)=TESUM(30,C) + TDRV3TH
      TESUM(29,C)=TESUM(29,C) + TDRV3TN
C...NON-MOTORIZED
      TESUM(34,C)=TESUM(34,C) + TNMWK
      TESUM(35,C)=TESUM(35,C) + TNMBK
C...TRANSIT BY TRIP LENGTH
      TTRIP(PINDEX,C)=TTRIP(PINDEX,C)+TTRAN
      IF(LSTRN(C).NE.0.0.AND.(IZ.NE.JZ)) 
     *   PTRIP(PINDEX,C)=PTRIP(PINDEX,C)+PERTRP
C...DISTRICT LEVEL SUMMARIES
      IF(DIZ.LE.0.OR.DIZ.GT.40.OR.DJZ.LE.0.OR.DJZ.GT.40) THEN
      WRITE(41,8030) IEQUIV(IZ),DIZ,IEQUIV(JZ),DJZ
 8030 FORMAT(' PTAZ=',I5,' DIZ=',I3,' ATAZ=',I5,' DJZ=',I3)
      ELSE
      TOTTRN(DIZ,DJZ,1)=TOTTRN(DIZ,DJZ,1)+TTRAN
      TOTTRN((MAXPD+1),DJZ,1)=TOTTRN((MAXPD+1),DJZ,1)+TTRAN
      TOTTRN(DIZ,(MAXPD+1),1)=TOTTRN(DIZ,(MAXPD+1),1)+TTRAN
      TOTTRN((MAXPD+1),(MAXPD+1),1)=TOTTRN((MAXPD+1),(MAXPD+1),1)+TTRAN
      TOTTRN(DIZ,DJZ,2)=TOTTRN(DIZ,DJZ,2)+TCR
      TOTTRN((MAXPD+1),DJZ,2)=TOTTRN((MAXPD+1),DJZ,2)+TCR
      TOTTRN(DIZ,(MAXPD+1),2)=TOTTRN(DIZ,(MAXPD+1),2)+TCR
      TOTTRN((MAXPD+1),(MAXPD+1),2)=TOTTRN((MAXPD+1),(MAXPD+1),2)+TCR
      TOTTRN(DIZ,DJZ,3)=TOTTRN(DIZ,DJZ,3)+TUR
      TOTTRN((MAXPD+1),DJZ,3)=TOTTRN((MAXPD+1),DJZ,3)+TUR
      TOTTRN(DIZ,(MAXPD+1),3)=TOTTRN(DIZ,(MAXPD+1),3)+TUR
      TOTTRN((MAXPD+1),(MAXPD+1),3)=TOTTRN((MAXPD+1),(MAXPD+1),3)+TUR
      TOTTRN(DIZ,DJZ,4)=TOTTRN(DIZ,DJZ,4)+TGB
      TOTTRN((MAXPD+1),DJZ,4)=TOTTRN((MAXPD+1),DJZ,4)+TGB
      TOTTRN(DIZ,(MAXPD+1),4)=TOTTRN(DIZ,(MAXPD+1),4)+TGB
      TOTTRN((MAXPD+1),(MAXPD+1),4)=TOTTRN((MAXPD+1),(MAXPD+1),4)+TGB
      TOTTRN(DIZ,DJZ,5)=TOTTRN(DIZ,DJZ,5)+TSTR
      TOTTRN((MAXPD+1),DJZ,5)=TOTTRN((MAXPD+1),DJZ,5)+TSTR
      TOTTRN(DIZ,(MAXPD+1),5)=TOTTRN(DIZ,(MAXPD+1),5)+TSTR
      TOTTRN((MAXPD+1),(MAXPD+1),5)=TOTTRN((MAXPD+1),(MAXPD+1),5)+TSTR
      TOTTRN(DIZ,DJZ,6)=TOTTRN(DIZ,DJZ,6)+TRPD
      TOTTRN((MAXPD+1),DJZ,6)=TOTTRN((MAXPD+1),DJZ,6)+TRPD
      TOTTRN(DIZ,(MAXPD+1),6)=TOTTRN(DIZ,(MAXPD+1),6)+TRPD
      TOTTRN((MAXPD+1),(MAXPD+1),6)=TOTTRN((MAXPD+1),(MAXPD+1),6)+TRPD
C
      TOTAUT(DIZ,DJZ,1)=TOTAUT(DIZ,DJZ,1)+TAUTO
      TOTAUT((MAXPD+1),DJZ,1)=TOTAUT((MAXPD+1),DJZ,1)+TAUTO
      TOTAUT(DIZ,(MAXPD+1),1)=TOTAUT(DIZ,(MAXPD+1),1)+TAUTO
      TOTAUT((MAXPD+1),(MAXPD+1),1)=TOTAUT((MAXPD+1),(MAXPD+1),1)+TAUTO
      TOTAUT(DIZ,DJZ,2)=TOTAUT(DIZ,DJZ,2)+TDRV0
      TOTAUT((MAXPD+1),DJZ,2)=TOTAUT((MAXPD+1),DJZ,2)+TDRV0
      TOTAUT(DIZ,(MAXPD+1),2)=TOTAUT(DIZ,(MAXPD+1),2)+TDRV0
      TOTAUT((MAXPD+1),(MAXPD+1),2)=TOTAUT((MAXPD+1),(MAXPD+1),2)+TDRV0
      TOTAUT(DIZ,DJZ,3)=TOTAUT(DIZ,DJZ,3)+TDRV2
      TOTAUT((MAXPD+1),DJZ,3)=TOTAUT((MAXPD+1),DJZ,3)+TDRV2
      TOTAUT(DIZ,(MAXPD+1),3)=TOTAUT(DIZ,(MAXPD+1),3)+TDRV2
      TOTAUT((MAXPD+1),(MAXPD+1),3)=TOTAUT((MAXPD+1),(MAXPD+1),3)+TDRV2
      TOTAUT(DIZ,DJZ,4)=TOTAUT(DIZ,DJZ,4)+TDRV3
      TOTAUT((MAXPD+1),DJZ,4)=TOTAUT((MAXPD+1),DJZ,4)+TDRV3
      TOTAUT(DIZ,(MAXPD+1),4)=TOTAUT(DIZ,(MAXPD+1),4)+TDRV3
      TOTAUT((MAXPD+1),(MAXPD+1),4)=TOTAUT((MAXPD+1),(MAXPD+1),4)+TDRV3
C
       IF(TUR.GT.0) THEN
       AVLTTC(DIZ,DJZ,1)=AVLTTC(DIZ,DJZ,1)+PERTRP
       AVLTTC((MAXPD+1),DJZ,1)=AVLTTC((MAXPD+1),DJZ,1)+PERTRP
       AVLTTC(DIZ,(MAXPD+1),1)=AVLTTC(DIZ,(MAXPD+1),1)+PERTRP
       AVLTTC((MAXPD+1),(MAXPD+1),1)=AVLTTC((MAXPD+1),(MAXPD+1),1)+
     *         PERTRP
       END IF
       IF(TSTR.GT.0) THEN
       AVLTTC(DIZ,DJZ,2)=AVLTTC(DIZ,DJZ,2)+PERTRP
       AVLTTC((MAXPD+1),DJZ,2)=AVLTTC((MAXPD+1),DJZ,2)+PERTRP
       AVLTTC(DIZ,(MAXPD+1),2)=AVLTTC(DIZ,(MAXPD+1),2)+PERTRP
       AVLTTC((MAXPD+1),(MAXPD+1),2)=AVLTTC((MAXPD+1),(MAXPD+1),2)+
     *        PERTRP
       END IF
C...NON MOTORIZED
      NONMOT(DIZ,DJZ,1)=NONMOT(DIZ,DJZ,1)+TNMWK
      NONMOT((MAXPD+1),DJZ,1)=NONMOT((MAXPD+1),DJZ,1)+TNMWK
      NONMOT(DIZ,(MAXPD+1),1)=NONMOT(DIZ,(MAXPD+1),1)+TNMWK
      NONMOT((MAXPD+1),(MAXPD+1),1)=NONMOT((MAXPD+1),(MAXPD+1),1)+TNMWK
      NONMOT(DIZ,DJZ,2)=NONMOT(DIZ,DJZ,2)+TNMBK
      NONMOT((MAXPD+1),DJZ,2)=NONMOT((MAXPD+1),DJZ,2)+TNMBK
      NONMOT(DIZ,(MAXPD+1),2)=NONMOT(DIZ,(MAXPD+1),2)+TNMBK
      NONMOT((MAXPD+1),(MAXPD+1),2)=NONMOT((MAXPD+1),(MAXPD+1),2)+TNMBK
      NONMOT(DIZ,DJZ,3)=NONMOT(DIZ,DJZ,3)+TNMBK+TNMWK
      NONMOT((MAXPD+1),DJZ,3)=NONMOT((MAXPD+1),DJZ,3)+TNMBK+TNMWK
      NONMOT(DIZ,(MAXPD+1),3)=NONMOT(DIZ,(MAXPD+1),3)+TNMBK+TNMWK
      NONMOT((MAXPD+1),(MAXPD+1),3)=NONMOT((MAXPD+1),(MAXPD+1),3)+
     *          TNMBK+TNMWK
      END IF
      ICH=IFIX(INVEHG(JZ)+INVEHP(JZ))+1
      ICH=MIN0(ICH,101)
      GBUSIVT(ICH,1)=GBUSIVT(ICH,1)+TGBW
      ICH=IFIX(INVEH(JZ))+1
      ICH=MIN0(ICH,101)
      GBUSTIVT(ICH,1)=GBUSTIVT(ICH,1)+TGBW
      IST=1
      IF(INVEH(JZ).GT.0.0) 
     *   IST=IFIX(((INVEHG(JZ)+INVEHP(JZ))/INVEH(JZ))*100.0)+1
      IST=MIN0(IST,101)
      GBUSRAT(IST,1)=GBUSRAT(IST,1)+TGBW
C...GO BUS WALK ACCESS TRANSFERS
      IF(TRANSF(JZ).GT.0) THEN
      ICH=IFIX(TRANSF(JZ)+0.01)
      ICH=MIN0(ICH,5)
      TXFERS(5,ICH)=TXFERS(5,ICH)+TGBW
      END IF
C...BUS/STREETCAR TRANSFERS
      IF(BSTRTRANSF(JZ).GT.0) THEN
      ICH=IFIX(BSTRTRANSF(JZ)+0.01)
      ICH=MIN0(ICH,5)
      TXFERS(1,ICH)=TXFERS(1,ICH)+TSTRW
      TXFERS(2,ICH)=TXFERS(2,ICH)+TSTRD
      END IF
C...RAPID BUS TRANSFERS
      IF(RAPDTRANSF(JZ).GT.0) THEN
      ICH=IFIX(RAPDTRANSF(JZ)+0.01)
      ICH=MIN0(ICH,5)
      TXFERS(3,ICH)=TXFERS(3,ICH)+TRPDW
      TXFERS(4,ICH)=TXFERS(4,ICH)+TRPDD
      END IF
C...RAPID BUS IN-VEHICLE RATIO
      IST=1
      IF(RAPDINVEH(JZ).GT.0.0) 
     *   IST=IFIX((RAPDINVEHR(JZ)/RAPDINVEH(JZ))*100.0)+1
      IST=MIN0(IST,101)
      RAPDRAT(IST,1)=RAPDRAT(IST,1)+TRPDW
      RAPDRAT(IST,2)=RAPDRAT(IST,2)+TRPDD
C
C  SAVE TRIPS FOR ZONE-TO-ZONE OUTPUT
C
      IF(VSKIM) THEN
      VSKMTRP(JZ,1)=VSKMTRP(JZ,1)+TCRW1
      VSKMTRP(JZ,2)=VSKMTRP(JZ,2)+TCRW2
      VSKMTRP(JZ,3)=VSKMTRP(JZ,3)+TCRB1
      VSKMTRP(JZ,4)=VSKMTRP(JZ,4)+TCRB2
      VSKMTRP(JZ,5)=VSKMTRP(JZ,5)+TCRP1
      VSKMTRP(JZ,6)=VSKMTRP(JZ,6)+TCRP2
      VSKMTRP(JZ,7)=VSKMTRP(JZ,7)+TCRP3
      VSKMTRP(JZ,8)=VSKMTRP(JZ,8)+TCRP4
      VSKMTRP(JZ,9)=VSKMTRP(JZ,9)+TCRK1
      VSKMTRP(JZ,10)=VSKMTRP(JZ,10)+TCRK2
      VSKMTRP(JZ,11)=VSKMTRP(JZ,11)+TCRK3
      VSKMTRP(JZ,12)=VSKMTRP(JZ,12)+TCRK4
      VSKMTRP(JZ,13)=VSKMTRP(JZ,13)+TURW1
      VSKMTRP(JZ,14)=VSKMTRP(JZ,14)+TURW2
      VSKMTRP(JZ,15)=VSKMTRP(JZ,15)+TURB1
      VSKMTRP(JZ,16)=VSKMTRP(JZ,16)+TURB2
      VSKMTRP(JZ,17)=VSKMTRP(JZ,17)+TURP1 
      VSKMTRP(JZ,18)=VSKMTRP(JZ,18)+TURP2
      VSKMTRP(JZ,19)=VSKMTRP(JZ,19)+TURP3
      VSKMTRP(JZ,20)=VSKMTRP(JZ,20)+TURP4
      VSKMTRP(JZ,21)=VSKMTRP(JZ,21)+TURK1
      VSKMTRP(JZ,22)=VSKMTRP(JZ,22)+TURK2
      VSKMTRP(JZ,23)=VSKMTRP(JZ,23)+TURK3
      VSKMTRP(JZ,24)=VSKMTRP(JZ,24)+TURK4
      VSKMTRP(JZ,25)=VSKMTRP(JZ,25)+TGBD     
      END IF
      OUTTRP(JZ,1)=OUTTRP(JZ,1)+TCRW
      OUTTRP(JZ,2)=OUTTRP(JZ,2)+TCRB
      OUTTRP(JZ,3)=OUTTRP(JZ,3)+TCRP
      OUTTRP(JZ,4)=OUTTRP(JZ,4)+TCRK
      OUTTRP(JZ,5)=OUTTRP(JZ,5)+TURW
      OUTTRP(JZ,6)=OUTTRP(JZ,6)+TURB
      OUTTRP(JZ,7)=OUTTRP(JZ,7)+TURP
      OUTTRP(JZ,8)=OUTTRP(JZ,8)+TURK
      OUTTRP(JZ,9)=OUTTRP(JZ,9)+TGBW
      OUTTRP(JZ,10)=OUTTRP(JZ,10)+TGBD
      OUTTRP(JZ,11)=OUTTRP(JZ,11)+TSTRW
      OUTTRP(JZ,12)=OUTTRP(JZ,12)+TSTRD
      OUTTRP(JZ,13)=OUTTRP(JZ,13)+TRPDW
      OUTTRP(JZ,14)=OUTTRP(JZ,14)+TRPDD
      OUTTRP(JZ,15)=OUTTRP(JZ,15)+TTRAN
      OUTTRP(JZ,16)=OUTTRP(JZ,16)+TNMWK
      OUTTRP(JZ,17)=OUTTRP(JZ,17)+TNMBK
      AUTTRP(JZ,1)=AUTTRP(JZ,1)+TDRV0N
      AUTTRP(JZ,2)=AUTTRP(JZ,2)+TDRV0T
      AUTTRP(JZ,3)=AUTTRP(JZ,3)+TDRV2NH
      AUTTRP(JZ,4)=AUTTRP(JZ,4)+TDRV2NN
      AUTTRP(JZ,5)=AUTTRP(JZ,5)+TDRV2TH
      AUTTRP(JZ,6)=AUTTRP(JZ,6)+TDRV2TN
      AUTTRP(JZ,7)=AUTTRP(JZ,7)+TDRV3NH
      AUTTRP(JZ,8)=AUTTRP(JZ,8)+TDRV3NN
      AUTTRP(JZ,9)=AUTTRP(JZ,9)+TDRV3TH
      AUTTRP(JZ,10)=AUTTRP(JZ,10)+TDRV3TN
      AUTTRP(JZ,11)=AUTTRP(JZ,11)+TNMWK
      AUTTRP(JZ,12)=AUTTRP(JZ,12)+TNMBK
C
C...AIR PASSENGER SUMMARY
C
      IF(AIRPASS) THEN
      AESUM(1)=AESUM(1)+TDRV0
      AESUM(2)=AESUM(2)+TDRV2
      AESUM(3)=AESUM(3)+TDRV3
      AESUM(5)=AESUM(5)+TDROP
      AESUM(6)=AESUM(6)+TLIMO
      AESUM(7)=AESUM(7)+TRNTL
      AESUM(8)=AESUM(8)+TTAXI
      AESUM(9)=AESUM(9)+TONCL
      AESUM(10)=AESUM(10)+TTRAN
      AESUM(11)=AESUM(11)+TUBER
      AESUM(12)=AESUM(12)+PERTRP
      AIRTRP(JZ,1)=AIRTRP(JZ,1)+TDROP
      AIRTRP(JZ,2)=AIRTRP(JZ,2)+TLIMO
      AIRTRP(JZ,3)=AIRTRP(JZ,3)+TRNTL
      AIRTRP(JZ,4)=AIRTRP(JZ,4)+TTAXI
      AIRTRP(JZ,5)=AIRTRP(JZ,5)+TONCL
      AIRTRP(JZ,6)=AIRTRP(JZ,6)+TUBER
      END IF
C
C  ALLOCATE TO LAX PARKING LOTS
C
      IF(AIR.AND.(.NOT.AIRTRN).AND.AJZ.GT.0) THEN
      TPRKSHL=0.0
      DO 43 NI=1,50
      IF(PEQUIV(NI).LE.0) GO TO 43
      IF(PRKDATA(NI,1).EQ.0) GO TO 43
      DENOM=PROBLOT(NI)*
     *           (TDRV0+TDRV2+TDRV3)
      VEHTRP=PROBLOT(NI)*
     *           (TDRV0+TDRV2/HOV2P+TDRV3/HOV3P)
      LOTRIPS(NI,1)=LOTRIPS(NI,1)+VEHTRP
      LOTRIPS(NI,2)=LOTRIPS(NI,2)+DENOM*EPROBLOT(NI,1)
      LOTRIPS(NI,3)=LOTRIPS(NI,3)+DENOM*EPROBLOT(NI,2)
      LOTRIPS(NI,4)=LOTRIPS(NI,4)+DENOM*EPROBLOT(NI,3)
C
C  STORE PEARSON PARKING LOT - TRANSIT TRIPS
C
      IF(EPROBLOT(NI,3).GT.0) THEN
      LOTTRN(NI,AJZ,1)=LOTTRN(NI,AJZ,1)+
     *  DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,1)
      LOTTRN(NI,AJZ,2)=LOTTRN(NI,AJZ,2)+
     *  DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,2)
      LOTTRN(NI,AJZ,3)=LOTTRN(NI,AJZ,3)+
     *  DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,3)
      LOTTRN(NI,AJZ,4)=LOTTRN(NI,AJZ,4)+
     *  DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,4)
      LOTTRN(NI,AJZ,5)=LOTTRN(NI,AJZ,5)+
     *  DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,5)
      LOTTRN(NI,AJZ,6)=LOTTRN(NI,AJZ,6)+
     *  DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,6)
      LCR=DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,1)
      LUR=DENOM*EPROBLOT(NI,3)*AIRTPROB(NI,AJZ,2)
      IF(LCR.GT.0.0) THEN
      ORISTA=AIRSTAP(NI,AJZ,1)-MAX_IZONES
      DESSTA=AIRSTAA(NI,AJZ,1)-MAX_IZONES
      LCRSS(ORISTA,DESSTA)=LCRSS(ORISTA,DESSTA) + LCR
      WCR(NI,ORISTA)=WCR(NI,ORISTA) + LCR
      LCRSTAZ(DESSTA,AJZ)=LCRSTAZ(DESSTA,AJZ) + LCR
      END IF
      IF(LUR.GT.0.0) THEN
      ORISTA=AIRSTAP(NI,AJZ,2)-MAX_IZONES
      DESSTA=AIRSTAA(NI,AJZ,2)-MAX_IZONES
      LURSS(ORISTA,DESSTA)=LURSS(ORISTA,DESSTA) + LUR
      WUR(NI,ORISTA)=WUR(NI,ORISTA) + LUR
      LURSTAZ(DESSTA,AJZ)=LURSTAZ(DESSTA,AJZ) + LUR
      END IF
      END IF
   43 CONTINUE
      END IF
C
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
      STASUM5((EQUIV(GOUNION)-MAX_IZONES),1)=
     *                    STASUM5((EQUIV(GOUNION)-MAX_IZONES),1)+
     *                    TUBERACC
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
      Stasum5((EQUIV(PEARSON)-MAX_IZONES),2)=
     *                    STASUM5((EQUIV(PEARSON)-MAX_IZONES),2)+
     *                    TUBERACC
C..URBAN RAIL STATION SUMMARY MATRIX FOR EGRESS FROM GO RAIL TRIPS
      STASUM3((OSTA3(1,1)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,1)-MAX_IZONES),1)+ TCRW1
      STASUM3((OSTA3(1,2)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,2)-MAX_IZONES),1)+ TCRW2
      STASUM3((OSTA3(1,3)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,3)-MAX_IZONES),1)+ TCRB1
      STASUM3((OSTA3(1,4)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,4)-MAX_IZONES),1)+ TCRB2
      STASUM3((OSTA3(1,5)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,5)-MAX_IZONES),1)+ TCRP1
      STASUM3((OSTA3(1,6)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,6)-MAX_IZONES),1)+ TCRP2
      STASUM3((OSTA3(1,7)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,7)-MAX_IZONES),1)+ TCRP3
      STASUM3((OSTA3(1,8)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,8)-MAX_IZONES),1)+ TCRP4
      STASUM3((OSTA3(1,9)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,9)-MAX_IZONES),1)+ TCRK1
      STASUM3((OSTA3(1,10)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,10)-MAX_IZONES),1)+TCRK2
      STASUM3((OSTA3(1,11)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,11)-MAX_IZONES),1)+TCRK3
      STASUM3((OSTA3(1,12)-MAX_IZONES),1)=
     *      STASUM3((OSTA3(1,12)-MAX_IZONES),1)+TCRK4
C..URBAN RAIL STATION EGRESS SUMMARY MATRIX FOR EGRESS FROM GO RAIL TRIPS
      STASUM3((ASTA3(1,1)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,1)-MAX_IZONES),2)+TCRW1
      STASUM3((ASTA3(1,2)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,2)-MAX_IZONES),2)+TCRW2
      STASUM3((ASTA3(1,3)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,3)-MAX_IZONES),2)+TCRB1
      STASUM3((ASTA3(1,4)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,4)-MAX_IZONES),2)+TCRB2
      STASUM3((ASTA3(1,5)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,5)-MAX_IZONES),2)+TCRP1
      STASUM3((ASTA3(1,6)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,6)-MAX_IZONES),2)+TCRP2
      STASUM3((ASTA3(1,7)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,7)-MAX_IZONES),2)+TCRP3
      STASUM3((ASTA3(1,8)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,8)-MAX_IZONES),2)+TCRP4
      STASUM3((ASTA3(1,9)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,9)-MAX_IZONES),2)+TCRK1
      STASUM3((ASTA3(1,10)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,10)-MAX_IZONES),2)+TCRK2
      STASUM3((ASTA3(1,11)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,11)-MAX_IZONES),2)+TCRK3
      STASUM3((ASTA3(1,12)-MAX_IZONES),2)=
     *   STASUM3((ASTA3(1,12)-MAX_IZONES),2)+TCRK4
C..TTC SUBWAY STATION ACCESS/EGRESS SUMMARY MATRIX FOR ACCESS TO GO RAIL TRIPS
      if(msta(1).gt.0) then
      orista=znesta(msta(1),1)-max_izones
      dessta=znesta(msta(1),2)-max_izones
      if(orista.gt.0.and.dessta.gt.0) then
      stasum4(orista,1)=stasum4(orista,1)+ tcrb1
      stasum4(dessta,2)=stasum4(dessta,2)+ tcrb1
      end if
      end if
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
      call upxsum(orista,dessta,tcrw1,upxsta,stasum6)
      orista=osta(1,2)-MAX_IZONES
      dessta=asta(1,2)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrw2)
      call statsum(maxgln,orista,dessta,tcrw2,gorail)
      call upxsum(orista,dessta,tcrw2,upxsta,stasum6)
      orista=osta(1,3)-MAX_IZONES
      dessta=asta(1,3)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrb1)
      call statsum(maxgln,orista,dessta,tcrb1,gorail)
      call upxsum(orista,dessta,tcrb1,upxsta,stasum6)
      orista=osta(1,4)-MAX_IZONES
      dessta=asta(1,4)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrb2)
      call statsum(maxgln,orista,dessta,tcrb2,gorail)
      call upxsum(orista,dessta,tcrb2,upxsta,stasum6)
      orista=osta(1,5)-MAX_IZONES
      dessta=asta(1,5)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp1)
      call statsum(maxgln,orista,dessta,tcrp1,gorail)
      call statsum(maxgln,orista,dessta,pertrp,gorailp1)
      call upxsum(orista,dessta,tcrp1,upxsta,stasum6)
      orista=osta(1,6)-MAX_IZONES
      dessta=asta(1,6)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp2)
      call statsum(maxgln,orista,dessta,tcrp2,gorail)
      call statsum(maxgln,orista,dessta,pertrp,gorailp2)
      call upxsum(orista,dessta,tcrp2,upxsta,stasum6)
      orista=osta(1,7)-MAX_IZONES
      dessta=asta(1,7)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp3)
      call statsum(maxgln,orista,dessta,tcrp3,gorail)
      call statsum(maxgln,orista,dessta,pertrp,gorailp3)
      call upxsum(orista,dessta,tcrp3,upxsta,stasum6)
      orista=osta(1,8)-MAX_IZONES
      dessta=asta(1,8)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrp4)
      call statsum(maxgln,orista,dessta,tcrp4,gorail)
      call statsum(maxgln,orista,dessta,pertrp,gorailp4)
      call upxsum(orista,dessta,tcrp4,upxsta,stasum6)
      orista=osta(1,9)-MAX_IZONES
      dessta=asta(1,9)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk1)
      call statsum(maxgln,orista,dessta,tcrk1,gorail)
      call upxsum(orista,dessta,tcrk1,upxsta,stasum6)
      orista=osta(1,10)-MAX_IZONES
      dessta=asta(1,10)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk2)
      call statsum(maxgln,orista,dessta,tcrk2,gorail)
      call upxsum(orista,dessta,tcrk2,upxsta,stasum6)
      orista=osta(1,11)-MAX_IZONES
      dessta=asta(1,11)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk3)
      call statsum(maxgln,orista,dessta,tcrk3,gorail)
      call upxsum(orista,dessta,tcrk3,upxsta,stasum6)
      orista=osta(1,12)-MAX_IZONES
      dessta=asta(1,12)-MAX_IZONES
      crss(orista,dessta)=crss(orista,dessta) + sngl(tcrk4)
      call statsum(maxgln,orista,dessta,tcrk4,gorail)
      call upxsum(orista,dessta,tcrk4,upxsta,stasum6)
C.....TTC SUBWAY STATION-TO-STATION EGRESS FOR GO RAIL TRIPS
      orista=osta3(1,1)-MAX_IZONES
      dessta=asta3(1,1)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrw1)
      orista=osta3(1,2)-MAX_IZONES
      dessta=asta3(1,2)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrw2)
      orista=osta3(1,3)-MAX_IZONES
      dessta=asta3(1,3)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrb1)
      orista=osta3(1,4)-MAX_IZONES
      dessta=asta3(1,4)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrb2)
      orista=osta3(1,5)-MAX_IZONES
      dessta=asta3(1,5)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrp1)
      orista=osta3(1,6)-MAX_IZONES
      dessta=asta3(1,6)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrp2)
      orista=osta3(1,7)-MAX_IZONES
      dessta=asta3(1,7)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrp3)
      orista=osta3(1,8)-MAX_IZONES
      dessta=asta3(1,8)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrp4)
      orista=osta3(1,9)-MAX_IZONES
      dessta=asta3(1,9)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrk1)
      orista=osta3(1,10)-MAX_IZONES
      dessta=asta3(1,10)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrk2)
      orista=osta3(1,11)-MAX_IZONES
      dessta=asta3(1,11)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrk3)
      orista=osta3(1,12)-MAX_IZONES
      dessta=asta3(1,12)-MAX_IZONES
      crurss(orista,dessta)=crurss(orista,dessta) + sngl(tcrk4)
C.... TTC SUBWAY STATION-TO-STATION ACCESS FOR GO RAIL TRIPS
      if(msta(1).gt.0) then
      orista=znesta(msta(1),1)-max_izones
      dessta=znesta(msta(1),2)-max_izones
      if(orista.gt.0.and.dessta.gt.0) then
      urcrss(orista,dessta)=urcrss(orista,dessta)+sngl(tcrb1)
      end if
      end if
c     if(msta(2).gt.0) then
c     orista=znesta(msta(2),1)-max_izones
c     dessta=znesta(msta(2),2)-max_izones
c     urcrss(orista,dessta)=urcrss(orista,dessta)+sngl(tcrb2)
c     end if
C.....TTC SUBWAY STATION-TO-STATION
      orista=osta(2,1)-MAX_IZONES
      dessta=asta(2,1)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turw1)
      call statsum(maxtln,orista,dessta,turw1,ttcsubway)
      orista=osta(2,2)-MAX_IZONES
      dessta=asta(2,2)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turw2)
      call statsum(maxtln,orista,dessta,turw2,ttcsubway)
      orista=osta(2,3)-MAX_IZONES
      dessta=asta(2,3)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turb1)
      call statsum(maxtln,orista,dessta,turb1,ttcsubway)
      orista=osta(2,4)-MAX_IZONES
      dessta=asta(2,4)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turb2)
      call statsum(maxtln,orista,dessta,turb2,ttcsubway)
      orista=osta(2,5)-MAX_IZONES
      dessta=asta(2,5)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp1)
      call statsum(maxtln,orista,dessta,turp1,ttcsubway)
      orista=osta(2,6)-MAX_IZONES
      dessta=asta(2,6)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp2)
      call statsum(maxtln,orista,dessta,turp2,ttcsubway)
      orista=osta(2,7)-MAX_IZONES
      dessta=asta(2,7)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp3)
      call statsum(maxtln,orista,dessta,turp3,ttcsubway)
      orista=osta(2,8)-MAX_IZONES
      dessta=asta(2,8)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turp4)
      call statsum(maxtln,orista,dessta,turp4,ttcsubway)
      orista=osta(2,9)-MAX_IZONES
      dessta=asta(2,9)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk1)
      call statsum(maxtln,orista,dessta,turk1,ttcsubway)
      orista=osta(2,10)-MAX_IZONES
      dessta=asta(2,10)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk2)
      call statsum(maxtln,orista,dessta,turk2,ttcsubway)
      orista=osta(2,11)-MAX_IZONES
      dessta=asta(2,11)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk3)
      call statsum(maxtln,orista,dessta,turk3,ttcsubway)
      orista=osta(2,12)-MAX_IZONES
      dessta=asta(2,12)-MAX_IZONES
      urss(orista,dessta)=urss(orista,dessta) + sngl(turk4)
      call statsum(maxtln,orista,dessta,turk4,ttcsubway)
C.....GO RAIL WALK EGRESS STATION TO ZONE
      dessta=asta(1,1)-MAX_IZONES
      if(egrind(1,1).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrw1)
      end if
      dessta=asta(1,2)-MAX_IZONES
      if(egrind(1,2).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrw2)
      end if
      dessta=asta(1,3)-MAX_IZONES
      if(egrind(1,3).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrb1)
      end if
      dessta=asta(1,4)-MAX_IZONES
      if(egrind(1,4).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrb2)
      end if
      dessta=asta(1,5)-MAX_IZONES
      if(egrind(1,5).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrp1)
      end if
      dessta=asta(1,6)-MAX_IZONES
      if(egrind(1,6).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrp2)
      end if
      dessta=asta(1,7)-MAX_IZONES
      if(egrind(1,7).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrp3)
      end if
      dessta=asta(1,8)-MAX_IZONES
      if(egrind(1,8).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrp4)
      end if
      dessta=asta(1,9)-MAX_IZONES
      if(egrind(1,9).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrk1)
      end if
      dessta=asta(1,10)-MAX_IZONES
      if(egrind(1,10).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrk2)
      end if
      dessta=asta(1,11)-MAX_IZONES
      if(egrind(1,11).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrk3)
      end if
      dessta=asta(1,12)-MAX_IZONES
      if(egrind(1,12).eq.0.and.staind(dessta,jz).eq.1) then
      crwlk(dessta,jz)=crwlk(dessta,jz) + sngl(tcrk4)
      end if
C.....GO RAIL STATION TO ZONE VIA BUS
      dessta=asta(1,1)-MAX_IZONES
      if(egrind(1,1).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrw1)
      end if
      dessta=asta(1,2)-MAX_IZONES
      if(egrind(1,2).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrw2)
      end if
      dessta=asta(1,3)-MAX_IZONES
      if(egrind(1,3).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrb1)
      end if
      dessta=asta(1,4)-MAX_IZONES
      if(egrind(1,4).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrb2)
      end if
      dessta=asta(1,5)-MAX_IZONES
      if(egrind(1,5).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp1)
      end if
      dessta=asta(1,6)-MAX_IZONES
      if(egrind(1,6).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp2)
      end if
      dessta=asta(1,7)-MAX_IZONES
      if(egrind(1,7).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp3)
      end if
      dessta=asta(1,8)-MAX_IZONES
      if(egrind(1,8).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrp4)
      end if
      dessta=asta(1,9)-MAX_IZONES
      if(egrind(1,9).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk1)
      end if
      dessta=asta(1,10)-MAX_IZONES
      if(egrind(1,10).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk2)
      end if
      dessta=asta(1,11)-MAX_IZONES
      if(egrind(1,11).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk3)
      end if
      dessta=asta(1,12)-MAX_IZONES
      if(egrind(1,12).eq.0.and.staind(dessta,jz).eq.2) then
      crstaz(dessta,jz)=crstaz(dessta,jz) + sngl(tcrk4)
      end if
C.....TTC SUBWAY RAIL WALK EGRESS STATION TO ZONE FOR GO RAIL TRIPS
      dessta=asta3(1,1)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrw1)
      end if
      dessta=asta3(1,2)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrw2)
      end if
      dessta=asta3(1,3)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrb1)
      end if
      dessta=asta3(1,4)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrb2)
      end if
      dessta=asta3(1,5)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrp1)
      end if
      dessta=asta3(1,6)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrp2)
      end if
      dessta=asta3(1,7)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrp3)
      end if
      dessta=asta3(1,8)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrp4)
      end if
      dessta=asta3(1,9)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrk1)
      end if
      dessta=asta3(1,10)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrk2)
      end if
      dessta=asta3(1,11)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrk3)
      end if
      dessta=asta3(1,12)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      crurwlk(dessta,jz)=crurwlk(dessta,jz) + sngl(tcrk4)
      end if
C.....TTC SUBWAY RAIL STATION TO ZONE FOR GO RAIL TRIPS
      dessta=asta3(1,1)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrw1)
      end if
      dessta=asta3(1,2)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrw2)
      end if
      dessta=asta3(1,3)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrb1)
      end if
      dessta=asta3(1,4)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrb2)
      end if
      dessta=asta3(1,5)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrp1)
      end if
      dessta=asta3(1,6)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrp2)
      end if
      dessta=asta3(1,7)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrp3)
      end if
      dessta=asta3(1,8)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrp4)
      end if
      dessta=asta3(1,9)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrk1)
      end if
      dessta=asta3(1,10)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrk2)
      end if
      dessta=asta3(1,11)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrk3)
      end if
      dessta=asta3(1,12)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      crurstaz(dessta,jz)=crurstaz(dessta,jz) + sngl(tcrk4)
      end if
C.....TTC SUBWAY WALK EGRESS STATION-T0-DESTINATION ZONE
      dessta=asta(2,1)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turw1)
      end if
      dessta=asta(2,2)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turw2)
      end if
      dessta=asta(2,3)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turb1)
      end if
      dessta=asta(2,4)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turb2)
      end if
      dessta=asta(2,5)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turp1)
      end if
      dessta=asta(2,6)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turp2)
      end if
      dessta=asta(2,7)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turp3)
      end if
      dessta=asta(2,8)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turp4)
      end if
      dessta=asta(2,9)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turk1)
      end if
      dessta=asta(2,10)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turk2)
      end if
      dessta=asta(2,11)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turk3)
      end if
      dessta=asta(2,12)-MAX_IZONES
      if(staind(dessta,jz).eq.1) then
      urwlk(dessta,jz)=urwlk(dessta,jz) + sngl(turk4)
      end if
C.....TTC SUBWAY STATION-T0-DESTINATION ZONE
      dessta=asta(2,1)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turw1)
      end if
      dessta=asta(2,2)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turw2)
      end if
      dessta=asta(2,3)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turb1)
      end if
      dessta=asta(2,4)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turb2)
      end if
      dessta=asta(2,5)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp1)
      end if
      dessta=asta(2,6)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp2)
      end if
      dessta=asta(2,7)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp3)
      end if
      dessta=asta(2,8)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turp4)
      end if
      dessta=asta(2,9)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk1)
      end if
      dessta=asta(2,10)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk2)
      end if
      dessta=asta(2,11)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk3)
      end if
      dessta=asta(2,12)-MAX_IZONES
      if(staind(dessta,jz).eq.2) then
      urstaz(dessta,jz)=urstaz(dessta,jz) + sngl(turk4)
      end if
c...WALK TO GO RAIL
      orista=osta(1,1)-MAX_IZONES
      wlkcr(iz,orista)=wlkcr(iz,orista) + sngl(tcrw1)
      orista=osta(1,2)-MAX_IZONES
      wlkcr(iz,orista)=wlkcr(iz,orista) + sngl(tcrw2)
c...BUS TO GO RAIL
      orista=osta(1,3)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb1)
      orista=osta(1,4)-MAX_IZONES
      bcr(iz,orista)=bcr(iz,orista) + sngl(tcrb2)
c...WALK TO TTC SUBWAY
      orista=osta(2,1)-MAX_IZONES
      wlkur(iz,orista)=wlkur(iz,orista) + sngl(turw1)
      orista=osta(2,2)-MAX_IZONES
      wlkur(iz,orista)=wlkur(iz,orista) + sngl(turw2)
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
C
C SUMMARIZE TRIPS BY VENUE FOR SPECIAL EVENT MODEL
C
      IF(EVENTSP) THEN
      KJZ=0
      DO K=1,MAXSP
      IF(JZ.EQ.EQUIV(SPZONE(K))) KJZ=K
      END DO
      SPSUM(KJZ,1)=SPSUM(KJZ,1)+TDRV0
      SPSUM(KJZ,2)=SPSUM(KJZ,2)+TDRV2
      SPSUM(KJZ,3)=SPSUM(KJZ,3)+TDRV3
      SPSUM(KJZ,4)=SPSUM(KJZ,4)+TCR
      SPSUM(KJZ,5)=SPSUM(KJZ,5)+TUR
      SPSUM(KJZ,6)=SPSUM(KJZ,6)+TGB
      SPSUM(KJZ,7)=SPSUM(KJZ,7)+TSTR
      SPSUM(KJZ,8)=SPSUM(KJZ,8)+TRPD
      SPSUM(KJZ,9)=SPSUM(KJZ,9)+TNMOT
      END IF
C
C OUTPUT WATERLOO UTILITY VALUES
C
      IF(WATERLOO) THEN
      if(msta(1).gt.0) then
      orista=iequiv(znesta(msta(1),1))
      dessta=iequiv(znesta(msta(1),2))
      else
      orista=0
      dessta=0
      end if
      WRITE(182,9744) IEQUIV(IZ),IEQUIV(JZ),C,UTIL(1),UTIL(3),
     *                UTIL(5),UTIL(9),
     *                UTIL(39),UTIL(41),UTIL(45),IEQUIV(OSTA(1,1)),
     *                IEQUIV(ASTA(1,1)),IEQUIV(OSTA(1,3)),
     *                IEQUIV(ASTA(1,3)),IEQUIV(OSTA(1,5)),
     *                IEQUIV(ASTA(1,5)),
     *                IEQUIV(OSTA(1,9)),IEQUIV(ASTA(1,9)),
     *                ORISTA,DESSTA,PROSTA3(1,5),PRASTA3(1,5)
 9744 FORMAT(I4,',',I4,',',I2,7(',',F12.5),12(',',I4))
      END IF
C   
C ----- Access Market Segmentation Loop End
 2000 CONTINUE
C
C     SPEVENT SAVE LOGSUM VALUES
C
      IF(SPEVENT.OR.VISITOR) THEN
      IF(ULOGSUM(C).NE.0.0) ULOGSUM(C)=LOG(ULOGSUM(C))
      DO K=1,MAXSP
      IF(IEQUIV(JZ).EQ.SPZONE(K)) SPLOGSUM(IZ,K)=ULOGSUM(C)     
      END DO
      END IF
C
      IF(ULOGSUM(C).NE.0.0.AND.LSBASE) THEN
      ULOGSUM(C)=LOG(ULOGSUM(C))
      IF(IZ.EQ.JZ) ULOGSUM(C)=0.0
C.................................................................
      IF(DEBUG) THEN
      WRITE(26,7040) C,ULOGSUM(C)
 7040 FORMAT(//' UPPER LEVEL LOGSUM COMPUTATIONS - MARKET ',I1/
     *       ' -------------------------------'/
     *       ' UPPER LEVEL LOGSUM       =',F10.5)
      END IF
C................................................................. 
      END IF
C
C OUTPUT FTA USER BENEFIT RECORD
C
      IF(USERBEN.AND.(.NOT.AIRTRN)) THEN
      IF(CAPRES.AND.(ITER.LT.(CITER-1))) GO TO 1001
       IF(IZ.EQ.JZ) THEN
       AUTEXP=0.99
       FWALK(1)=1.0
       FWALK(2)=0.0
       FSHAR(1)=0.0
       FSHAR(2)=0.0
       END IF
       MSEG=C
       WRITE(178) IZ,JZ,MSEG,TPERIN(C),TPERIN(C),AUTEXP,
     *          FWALK(1),FSHAR(1),FWALK(2),FSHAR(2)
      END IF
 1001 CONTINUE
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9426) C,TPERIN(C),TPERIN(C),AUTEXP,
     *               FWALK(1),FSHAR(1),FWALK(2),FSHAR(2),
     *               TESUM(2,C)
 9426 FORMAT(/1X,'SUMMARY OF FTA USER BENEFIT DATA   '/
     *        1X,'-----------------------------------'/
     *        1X,'MARKET SEGMENT               =',I10/
     *        1X,'PERSON TRIPS IN MARKET       =',F10.4/
     *        1X,'MOTORIZED TRIPS IN MARKET    =',F10.4/
     *        1X,'NON TRANSIT EXP UTILITY      =',E12.5/
     *        1X,'% PERSON TRIPS -  WALK MARKET=',F10.5/
     *        1X,' TRANSIT SHARE -  WALK MARKET=',F10.5/
     *        1X,'% PERSON TRIPS - DRIVE MARKET=',F10.5/
     *        1X,' TRANSIT SHARE - DRIVE MARKET=',F10.5/
     *        1X,'TOTAL TRANST TRIPS           =',F10.5/)
      END IF
C....................................................................
c ----- Market Segmentation Loop End
  100 CONTINUE
C
C  OUTPUT DESTINATION LOGSUM VALUES
C
      IF(LSBASE.AND.(.NOT.WATERLOO)) THEN
      RECNO = ((IZ-1)*MXZONES) + JZ
      DO T=1,NCATS
      FILENO=130+T
      WRITE(FILENO,REC=RECNO) ULOGSUM(T)
      END DO      
      END IF
C
c ----- Destination Zone Loop End
      end do
C
C  OUTPUT AUTO ZONE LEVEL MATRICES
C      
      IF(VEHOUT.AND.(.NOT.AIRTRN).AND.(.NOT.SPEVENT).AND.
     *  (.NOT.VISITOR)) THEN
      IIZ=IZ
      DO T=1,10
      FILENO=160+T
      WRITE(FILENO) IIZ,(AUTTRP(K,T),K=1,4000)
      END DO
      WRITE(191) IIZ,(AUTTRP(K,11),K=1,4000)
      WRITE(192) IIZ,(AUTTRP(K,12),K=1,4000)
      END IF
C
C  OUTPUT TRANSIT ZONE LEVEL MATRICES
C
      IF(TRIPSOUT.AND.(.NOT.AIRTRN).AND.(.NOT.SPEVENT).AND.
     *  (.NOT.VISITOR)) THEN
      IIZ=IZ
      DO T=1,10
      FILENO=50+T
      WRITE(FILENO) IIZ,(OUTTRP(K,T),K=1,4000)
      END DO
      DO T=1,4
      FILENO=123+T
      WRITE(FILENO) IIZ,(OUTTRP(K,(T+10)),K=1,4000)
      END DO
      WRITE(61) IIZ,(OUTTRP(K,15),K=1,4000)
      IF(MFTRIPS(16).GT.0) WRITE(180) IIZ,(OUTTRP(K,16),K=1,4000)
      IF(MFTRIPS(17).GT.0) WRITE(181) IIZ,(OUTTRP(K,17),K=1,4000)
      END IF
C
C  OUTPUT TRANSIT ZONE LEVEL MATRICES -- VSKIM
C
      IF(VSKIM.AND.(.NOT.AIRTRN).AND.(.NOT.SPEVENT).AND.
     *  (.NOT.VISITOR)) THEN
      IIZ=IZ
      DO T=1,25
      FILENO=300+T
      WRITE(FILENO) IIZ,(VSKMTRP(K,T),K=1,4000)
      END DO
      END IF
C
C  OUTPUT AIR PASSENGER TRIPS
C      
      IF(AIRPASS.AND.TRIPSOUT.AND.VEHOUT.AND.(.NOT.AIRTRN)) THEN
      IIZ=IZ
      DO T=1,5
      FILENO=200+T
      WRITE(FILENO) IIZ,(AIRTRP(K,T),K=1,4000)
      END DO
      WRITE(278) IIZ,(AIRTRP(K,6),K=1,4000)
      END IF
c ----- Origin Zone Loop End
      end do
C
C     SPECIAL EVENT TRIP DISTRIBUTION MODEL
C
      IF(SPEVENT) THEN
      DO JZ=1,MAXSP
C     WRITE(26,11113) JZ,SPZONE(JZ)
11113 FORMAT(' JZ=',I2,' SPZONE=',I5)
      DO IZ=1,MAX_IZONES
      SPPERSON(IZ,JZ)=ZHHD(16,IZ)*EXP(0.0126*SPLOGSUM(IZ,JZ))*100.0
C     WRITE(26,11112) IEQUIV(IZ),ZHHD(16,IZ),SPPERSON(IZ,JZ),
C    *                SPLOGSUM(IZ,JZ)
11112 FORMAT(' IZ=',I5,' ZHHD(16)=',F10.5,' SPPERSON=',F10.5,
     *       ' SPLOGSUM=',F10.5)
      SPTOTAL(JZ)=SPTOTAL(JZ)+SPPERSON(IZ,JZ)
      END DO
      END DO
      DO JZ=1,MAXSP
C     WRITE(26,11114) SPZONE(JZ),SPTOTAL(JZ),SPDATA(JZ,1)
11114 FORMAT(' SPZONE=',I5,' SPTOTAL=',F9.1,' SPDATA=',F8.0)
      SPTOTAL(JZ)=SPDATA(JZ,1)/SPTOTAL(JZ)
      END DO 
C...NORMALIZE PERSON TRIPS TO ATTENDANCE TOTAL
      DO JZ=1,MAXSP
C     WRITE(26,11113) JZ,SPZONE(JZ)
      DO IZ=1,MAX_IZONES
      SPPERSON(IZ,JZ)=SPPERSON(IZ,JZ)*SPTOTAL(JZ)*ATTFCT
      DIZ=DEQUIV(IEQUIV(IZ))
      DJZ=DEQUIV(SPZONE(JZ))
      IF(DIZ.GT.0.AND.DIZ.LT.41.AND.DJZ.GT.0.AND.DJZ.LT.41) THEN
      SPDIST(DIZ,DJZ,JZ)=SPDIST(DIZ,DJZ,JZ)+SPPERSON(IZ,JZ)
      SPDIST((MAXPD+1),DJZ,JZ)=SPDIST((MAXPD+1),DJZ,JZ)+SPPERSON(IZ,JZ)
      SPDIST(DIZ,(MAXPD+1),JZ)=SPDIST(DIZ,(MAXPD+1),JZ)+SPPERSON(IZ,JZ)
      SPDIST((MAXPD+1),(MAXPD+1),JZ)=SPDIST((MAXPD+1),(MAXPD+1),JZ)+
     *        SPPERSON(IZ,JZ)
      END IF
      END DO
      END DO
C...SUMMARIZE SP PERSON TRIPS BY DISTRICT
      DO K=1,MAXSP
      WRITE(176,9048) SPNAME(K),DNAME
 9048 FORMAT(A35,31(',',A10))
      DO IZ=1,31
      WRITE(176,9046) DNAME(IZ),(SPDIST(IZ,JZ,K),JZ=1,(MAXPD+1))
 9046 FORMAT(A35,41(',',F10.1))
      END DO
      END DO
      WRITE(*,8006) 
 8006 FORMAT(/' Special Event Mode Choice Model Computations'/
     *        ' --------------------------------------------'/)
      tesum=0.0
      aesum=0.0
      stasum=0.0
      stasum2=0.0
      stasum3=0.0
      stasum4=0.
      stasum5=0.0
      ttrip=0.0
      gbusivt=0.0
      gbustivt=0.0
      gbusrat=0.0
      rapdrat=0.0
      txfers=0.0
      tottrn=0.0
      totaut=0.0
      nonmot=0.0
      avlttc=0.0
      estcbd=0.0  
      ptrip=0.0
      call fileopen
      eventsp=.true.
      spevent=.false.
      go to 1000
      END IF
C
C     VISITOR TRIP DISTRIBUTION MODEL
C
      IF(VISITOR) THEN
      DO JZ=1,MAXSP
C     WRITE(26,11113) JZ,SPZONE(JZ)
      DO IZ=1,MAX_IZONES
      IF(ZHHD(1,IZ).LE.0.0) CYCLE
      UTILVS=0.0
      PROBVS=0.0
      UTILVS=0.21710*SPLOGSUM(IZ,JZ)+KVIS(JZ)
      PROBVS=EXP(UTILVS)/(EXP(UTILVS)+1.0)
      SPPERSON(IZ,JZ)=ZHHD(1,IZ)*PROBVS*2.77*2.0
C     WRITE(26,11115) IEQUIV(IZ),UTILVS,PROBVS,ZHHD(1,IZ),
C    *                SPPERSON(IZ,JZ),SPLOGSUM(IZ,JZ)
11115 FORMAT(' IZ=',I5,' UTILVS=',F10.5,' PROBVS=',F10.5,
     *       ' ZHHD(1)=',F10.1,' SPPERSON=',F10.5,
     *       ' SPLOGSUM=',F10.5)
      SPTOTAL(JZ)=SPTOTAL(JZ)+SPPERSON(IZ,JZ)
      END DO
      END DO
      DO JZ=1,MAXSP
C     WRITE(26,11114) SPZONE(JZ),SPTOTAL(JZ),SPDATA(JZ,1)
      SPTOTAL(JZ)=SPDATA(JZ,1)/SPTOTAL(JZ)
      END DO 
C...NORMALIZE PERSON TRIPS TO ATTENDANCE TOTAL
      DO JZ=1,MAXSP
      DO IZ=1,MAX_IZONES
      SPPERSON(IZ,JZ)=SPPERSON(IZ,JZ)*SPTOTAL(JZ)*ATTFCT      
      DIZ=DEQUIV(IEQUIV(IZ))
      DJZ=DEQUIV(SPZONE(JZ))
      IF(DIZ.GT.0.AND.DIZ.LT.31.AND.DJZ.GT.0.AND.DJZ.LT.31) THEN
      SPDIST(DIZ,DJZ,JZ)=SPDIST(DIZ,DJZ,JZ)+SPPERSON(IZ,JZ)
      SPDIST(31,DJZ,JZ)=SPDIST(31,DJZ,JZ)+SPPERSON(IZ,JZ)
      SPDIST(DIZ,31,JZ)=SPDIST(DIZ,31,JZ)+SPPERSON(IZ,JZ)
      SPDIST(31,31,JZ)=SPDIST(31,31,JZ)+SPPERSON(IZ,JZ)
      END IF
      END DO
      END DO
C...SUMMARIZE VISITOR PERSON TRIPS BY DISTRICT
      DO K=1,MAXSP
      WRITE(176,9048) SPNAME(K),DNAME
      DO IZ=1,31
      WRITE(176,9046) DNAME(IZ),(SPDIST(IZ,JZ,K),JZ=1,31)
      END DO
      END DO
      WRITE(*,8007) 
 8007 FORMAT(/' Visitor Mode Choice Model Computations'/
     *        ' --------------------------------------'/)
      tesum=0.0
      aesum=0.0
      stasum=0.0
      stasum2=0.0
      stasum3=0.0
      stasum4=0.0
      stasum5=0.0
      ttrip=0.0
      gbusivt=0.0
      gbustivt=0.0
      gbusrat=0.0
      rapdrat=0.0
      txfers=0.0
      tottrn=0.0
      totaut=0.0
      nonmot=0.0
      avlttc=0.0
      estcbd=0.0  
      ptrip=0.0
      call fileopen
      eventsp=.true.
      visitor=.false.
      go to 1000
      END IF
C
C  EMPLOYEE AND AIR PASSENGER MODEL
C
	    if(air.and.airtrn) then
	    airtrn=.false.
      tesum=0.0
      aesum=0.0
      stasum=0.0
      stasum2=0.0
      stasum3=0.0
      stasum4=0.0
      stasum5=0.0
      ttrip=0.0
      gbusivt=0.0
      gbustivt=0.0
      gbusrat=0.0
      rapdrat=0.0
      txfers=0.0
      tottrn=0.0
      totaut=0.0
      nonmot=0.0
      avlttc=0.0
      estcbd=0.0  
      ptrip=0.0
      call fileopen
	    go to 1000
	    end if
C
C     SUMMARIZE SPECIAL EVENT MODE CHOICE RESULTS BY VENUE
C
      IF(EVENTSP) THEN
      WRITE(26,9149)
 9149 FORMAT(//
     *       '   SPECIAL EVENT MODE USE SUMMARY BY VENUE'/
     *       '   ----------------------------------------'/
     *       '                                          DRIVE',
     *       '       2         3+        GO       TTC       GO',
     *       '        BUS      RAPID      WALK'/
     *       '   ZONE            VENUE                  ALONE',
     *       '     PERSON    PERSON     RAIL     SUBWAY     BUS',
     *       '    STREETCAR    BUS       BIKE     TOTAL'/
     *       '  -----  ------------------------------  --------',
     *       '  --------  --------  --------  --------  --------',
     *       '  --------  --------  --------  --------')
      DO K=1,MAXSP
      DO K1=1,9
      SPSUM(K,10)=SPSUM(K,10)+SPSUM(K,K1)
      SPSUM(51,K1)=SPSUM(51,K1)+SPSUM(K,K1)
      SPSUM(51,10)=SPSUM(51,10)+SPSUM(K,K1)
      END DO
      WRITE(26,9150) SPZONE(K),SPNAME(K),(SPSUM(K,K1),K1=1,10)
 9150 FORMAT(2X,I5,2X,A30,10(2X,F8.0))
      END DO
      WRITE(26,9151) (SPSUM(51,K1),K1=1,10)
 9151 FORMAT(/2X,'TOTAL',32X,10(1X,F9.0))
      END IF
C ===================================================================
C     TRIP REPORTING
C ===================================================================
      CI=NCATS+1
      DO C=1,NCATS
      DO K=1,39
      TESUM(K,CI)=TESUM(K,CI)+TESUM(K,C)
      END DO 
      END DO
      CI=NCATS+1
C     IF(VSKIM) GO TO 8886
      WRITE(26,7001)
 7001 FORMAT(//,30X,'R E P O R T   1',/,
     *          20X,
     *          'SUMMARIZE TRIPS BY MODE AND MARKET SEGMENT',//,
     *       1X,'  MARKET   ','         ','    2    ','      3    '/
     *       1X,' SEGMENT  ','  DRIVE  ','  PERSON ','    PERSON ',
     *      10X,'   NON    ',' SCHOOL  '/
     *       1X,'  LEVEL  ','   ALONE ','    AUTO ','      AUTO ',
     *          ' TRANSIT','   MOTORIZED','    BUS  ','   TOTAL  '/
     *       1X,'---------','---------','---------','----------',
     *          '----------','----------','----------','----------')
      DO C=1,NCATS
      TDRV0=TESUM(21,C)+TESUM(22,C)
      TDRV2=TESUM(23,C)+TESUM(24,C)+TESUM(25,C)+TESUM(26,C)
      TDRV3=TESUM(27,C)+TESUM(28,C)+TESUM(29,C)+TESUM(30,C)
      WRITE(26,7002) C,TDRV0,TDRV2,TDRV3,TESUM(2,C),
     *               TESUM(36,C),TESUM(6,C),TESUM(1,C)
 7002 FORMAT(5X,I1,2X,7F10.0)
      IF(CSVRPT) THEN
      WRITE(173,7050) C,TDRV0,TDRV2,TDRV3,TESUM(2,C),
     *               TESUM(36,C),TESUM(6,C),TESUM(1,C)
 7050 FORMAT(I1,15(',',F10.0))      
      
      END IF
      END DO
      TDRV0=TESUM(21,CI)+TESUM(22,CI)
      TDRV2=TESUM(23,CI)+TESUM(24,CI)+TESUM(25,CI)+TESUM(26,CI)
      TDRV3=TESUM(27,CI)+TESUM(28,CI)+TESUM(29,CI)+TESUM(30,CI)
      WRITE(26,7003) TDRV0,TDRV2,TDRV3,TESUM(2,CI),
     *               TESUM(36,CI),TESUM(6,CI),TESUM(1,CI)
 7003 FORMAT(/' TOTAL  ',7F10.0)
      IF(CSVRPT) WRITE(173,7051) TDRV0,TDRV2,TDRV3,TESUM(2,CI),
     *               TESUM(36,CI),TESUM(6,CI),TESUM(1,CI)
 7051 FORMAT('TOTAL',15(',',F10.0))
C
C
C SUMMARIZE AUTO TRIP VALUES FOR REGION
C
      WRITE(26,9225)
 9225 FORMAT(//,30X,'R E P O R T   1B',/,
     *          20X,
     *          'SUMMARIZE AUTO PERSON TRIPS BY MARKET SEGMENT',//,
     *       1X,'  MARKET ',' DRIVE   ','   DRIVE ',' 2 PERSON',
     *          ' 2 PERSON',' 2 PERSON',' 2 PERSON',' 3 PERSON',
     *          ' 3 PERSON',' 3 PERSON',' 3 PERSON'/
     *       1X,' SEGMENT ',' ALONE   ','   ALONE ',' NON TOLL',
     *          ' NON TOLL','   TOLL  ','   TOLL  ',' NON TOLL',
     *          ' NON TOLL','   TOLL  ','   TOLL  '/
     *       1X,'  LEVEL  ','NON TOLL ','    TOLL ',' NON HOV ',
     *          '   HOV   ',' NON HOV ','   HOV   ',' NON HOV ',
     *          '   HOV   ',' NON HOV ','   HOV   ',/
     *       1X,'---------','---------','---------','---------',
     *          '---------','---------','---------','---------',
     *          '---------','---------','---------')
      DO C=1,NCATS
      WRITE(26,9302) C,(TESUM(T,C),T=21,30)
 9302 FORMAT(5X,I1,3X,10F9.0)
      IF(CSVRPT) WRITE(173,7050) C,(TESUM(T,C),T=21,30)
      END DO
      WRITE(26,9303) (TESUM(T,CI),T=21,30)
 9303 FORMAT(/,2X,'TOTAL',2X,14F9.0)
       IF(CSVRPT) WRITE(173,7051) (TESUM(T,CI),T=21,30)
      WRITE(26,9901)
 9901 FORMAT(//,30X,'R E P O R T   1C',/,
     *          20X,
     *          'SUMMARIZE NON-MOTORIZED TRIPS BY MARKET SEGMENT',//,
     *       1X,'  MARKET   '/
     *       1X,'  SEGMENT ',/
     *       1X,'  LEVEL  ','   WALK  ','   BIKE  ',/
     *       1X,'---------','---------','---------'/)
      DO C=1,NCATS
      WRITE(26,9902) C,TESUM(34,C),TESUM(35,C)
 9902 FORMAT(5X,I1,2X,2F10.0)
      IF(CSVRPT) WRITE(173,7050) C,TESUM(34,C),TESUM(35,C)
      END DO
      WRITE(26,9903) TESUM(34,CI),TESUM(35,CI)
      IF(CSVRPT) WRITE(173,7051) TESUM(34,CI),TESUM(35,CI)
 9903 FORMAT(/,2X,'TOTAL',1X,2F10.0)
      WRITE(26,7004)
 7004 FORMAT(//,30X,'R E P O R T   2',/,
     *          20X,
     *          'SUMMARIZE TRANSIT TRIPS BY MARKET SEGMENT',//,
     *       1X,' MARKET  '/
     *       1X,' SEGMENT ','    GO    ','   TTC    ','     GO   ',
     *                      '   BUS    ','   RAPID  '/
     *       1X,'  LEVEL  ','    RAIL  ','  SUBWAY  ','     BUS  ',
     *                      ' STREECAR ','    BUS   ',
     *                      '  TOTAL   '/
     *       1X,'---------','----------','----------','----------',
     *                      '----------','----------',
     *                      '----------')
      DO C=1,NCATS
      WRITE(26,7005) C,TESUM(3,C),TESUM(4,C),TESUM(5,C),
     *                 TESUM(37,C),TESUM(38,C),TESUM(2,C)
 7005 FORMAT(3X,I1,4X,6F10.1)
      IF(CSVRPT) WRITE(173,7050) C,TESUM(3,C),TESUM(4,C),TESUM(5,C),
     *                 TESUM(37,C),TESUM(38,C),TESUM(2,C)
      END DO
      WRITE(26,7006) TESUM(3,CI),TESUM(4,CI),TESUM(5,CI),
     *               TESUM(37,CI),TESUM(38,CI),TESUM(2,CI)      
 7006 FORMAT(/,2X,'TOTAL',1X,6F10.1)
      IF(CSVRPT) WRITE(173,7051) TESUM(3,CI),TESUM(4,CI),TESUM(5,CI),
     *                 TESUM(37,C),TESUM(38,C),TESUM(2,C)
C
C GO RAIL AND THEN TTC SUBWAY
C
      IF(AIRPASS) THEN
      WRITE(26,7207)
 7207 FORMAT(//,30X,'R E P O R T   3',/,
     *          20X,
     *          'SUMMARIZE GO RAIL BY MARKET SEGMENT',//,
     *       1X,' MARKET  ','          ','          ',
     *          '   PARK   ','   KISS   ','          '/
     *       1X,' SEGMENT ','          ','          ',
     *          '   AND    ','    AND   ','          '/
     *       1X,'  LEVEL  ','    WALK  ','   BUS    ',
     *          '   RIDE   ','    RIDE  ','  UBER    ',
     *          '  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------','----------','----------',
     *          '----------')
      DO C=1,NCATS
      WRITE(26,7209) C,(TESUM(K,C),K=9,12),TESUM(39,C),TESUM(3,C)
 7209 FORMAT(5X,I1,2X,6F10.1)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=9,12),TESUM(39,CI),TESUM(3,CI)
      ELSE
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
      IF(CSVRPT) WRITE(173,7050) C,(TESUM(K,C),K=9,12),TESUM(3,C)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=9,12),TESUM(3,CI)
 7010 FORMAT(/,2X,'TOTAL',1X,6F10.1)
      IF(CSVRPT) WRITE(173,7051) (TESUM(K,CI),K=9,12),TESUM(3,CI)
      END IF
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
      IF(CSVRPT) WRITE(173,7050) C,(TESUM(K,C),K=13,16),TESUM(4,C)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=13,16),TESUM(4,CI)
      IF(CSVRPT) WRITE(173,7051) (TESUM(K,CI),K=13,16),TESUM(4,CI)
C
      WRITE(26,7031)
 7031 FORMAT(//,30X,'R E P O R T   5',/,
     *          20X,
     *          'SUMMARIZE GO BUS BY MARKET SEGMENT',/)
      WRITE(26,7035)
 7035 FORMAT(1X,' MARKET  '/
     *       1X,' SEGMENT '/
     *       1X,'  LEVEL  ','    WALK  ','   DRIVE  ',
     *          '  TOTAL   '/
     *       1X,'---------','----------','----------',
     *          '----------')
      DO C=1,NCATS
      WRITE(26,7009) C,(TESUM(K,C),K=7,8),TESUM(5,C)
      IF(CSVRPT) WRITE(173,7050) C,(TESUM(K,C),K=7,8),TESUM(5,C)
      END DO
      WRITE(26,7010) (TESUM(K,CI),K=7,8),TESUM(5,CI)
      IF(CSVRPT) WRITE(173,7051) (TESUM(K,CI),K=7,8),TESUM(5,CI)
C
      WRITE(26,7034)
 7034 FORMAT(//,30X,'R E P O R T   6',/,
     *          20X,
     *          'SUMMARIZE BUS/STREETCAR BY MARKET SEGMENT',/)
      WRITE(26,7035)
      DO C=1,NCATS
      WRITE(26,7009) C,TESUM(17,C),TESUM(19,C),TESUM(37,C)
      IF(CSVRPT) WRITE(173,7050) C,TESUM(17,C),TESUM(19,C),TESUM(37,C)
      END DO
      WRITE(26,7010) TESUM(17,CI),TESUM(19,CI),TESUM(37,CI)
      IF(CSVRPT) WRITE(173,7051) TESUM(17,CI),TESUM(19,CI),TESUM(37,CI)
      WRITE(26,7036)
 7036 FORMAT(//,30X,'R E P O R T   7',/,
     *          20X,
     *          'SUMMARIZE RAPID BUS BY MARKET SEGMENT',/)
      WRITE(26,7035)
      DO C=1,NCATS
      WRITE(26,7009) C,TESUM(18,C),TESUM(20,C),TESUM(38,C)
      IF(CSVRPT) WRITE(173,7050) C,TESUM(18,C),TESUM(20,C),TESUM(38,C)
      END DO
      WRITE(26,7010) TESUM(18,CI),TESUM(20,CI),TESUM(38,CI)
      IF(CSVRPT) WRITE(173,7051) TESUM(18,CI),TESUM(20,CI),TESUM(38,CI)
C
C  SUMMARIZE STATION MODE OF ACCESS DATA
C
      IF(AIRPASS) THEN
      WRITE(26,7220)
 7220 FORMAT(//,30X,'R E P O R T   5A',/,
     *          20X,
     *          'SUMMARIZE GO RAIL STATION ACCESS VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '  WALK  ','   BUS  ','  PARK  ','  KISS  ',
     *          '        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' ACCESS ','  RIDE  ','  RIDE  ',
     *          '  UBER  ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','--------','--------',
     *          '--------','-------  ')
        TTWLK=0.0
	      TTBUS=0.0
	      TTPNR=0.0
	      TTKNR=0.0  
	      TTUBER=0.0
	      TTOTAL=0.0
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM(K,5)=STASUM(K,1)+STASUM(K,2)+STASUM(K,3)+STASUM(K,4)+
     *            STASUM5(K,1)
      TTWLK=TTWLK+STASUM(K,1)
	    TTBUS=TTBUS+STASUM(K,2)
	    TTPNR=TTPNR+STASUM(K,3)
	    TTKNR=TTKNR+STASUM(K,4)
	    TTUBER=TTUBER+STASUM5(K,1)
	    TTOTAL=TTOTAL+STASUM(K,5)
      IF(STASUM(K,5).GT.0.1) THEN
	WRITE(26,7021) IEQUIV(KS),STANAME(K),(STASUM(K,L),L=1,4),
     *               STASUM5(K,1),STASUM(K,5)
	IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),(STASUM(K,L),L=1,4),
     *               STASUM5(K,1),STASUM(K,5)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTPNR,TTKNR,TTUBER,TTOTAL
      IF(CSVRPT) WRITE(173,7053) TTWLK,TTBUS,TTPNR,TTKNR,TTUBER,TTOTAL
      ELSE
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
      IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),
     *           (STASUM(K,L),L=1,5)
 7052 FORMAT(I4,',',A29,10(',',F8.0))
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
 7022 FORMAT(/3X,'TOTAL',31X,6F8.0)
      IF(CSVRPT) WRITE(173,7053) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
 7053 FORMAT('TOTAL,',15(',',F10.0))
      END IF
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
      IF(.NOT.AIRPASS) STASUM5(K,2)=0.0
      STASUM2(K,4)=STASUM2(K,1)+STASUM2(K,2)+STASUM2(K,3)+STASUM5(K,2)
      TTWLK=TTWLK+STASUM2(K,1)+STASUM5(K,2)
      TTBUS=TTBUS+STASUM2(K,2)
      TTPNR=TTPNR+STASUM2(K,3)
      TTOTAL=TTOTAL+STASUM2(K,4)
      IF(STASUM2(K,4).GT.0.1) THEN
	    WRITE(26,7024) IEQUIV(KS),STANAME(K),
     *                  (STASUM2(K,L),L=1,4)
 7024 FORMAT(2X,I4,3X,A29,1X,4F8.0)
      IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),
     *                  (STASUM2(K,L),L=1,4)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTPNR,TTOTAL
      IF(CSVRPT) WRITE(173,7053) TTWLK,TTBUS,TTPNR,TTOTAL
      TTWLK=0.0
      TTBUS=0.0
      TTOTAL=0.0
      WRITE(26,7065)
 7065 FORMAT(//,30X,'R E P O R T   5C',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION VOLUMES FOR',
     *          ' EGRESS FROM GO RAIL TRIPS',//,
     *       1X,' STATION ','                              ',
     *          '        ','        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','------  ')
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM3(K,3)=STASUM3(K,1)+STASUM3(K,2)
      TTWLK=TTWLK+STASUM3(K,1)
      TTBUS=TTBUS+STASUM3(K,2)
      TTOTAL=TTOTAL+STASUM3(K,3)
      IF(STASUM3(K,3).GT.0.1) THEN
	    WRITE(26,7026) IEQUIV(KS),STANAME(K),
     *                  (STASUM3(K,L),L=1,3)
	    IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),
     *                  (STASUM3(K,L),L=1,3)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTOTAL
      IF(CSVRPT) WRITE(173,7053) TTWLK,TTBUS,TTOTAL
      IF(TTCACC) THEN
      TTWLK=0.0
      TTBUS=0.0
      TTOTAL=0.0
      WRITE(26,7066)
 7066 FORMAT(//,30X,'R E P O R T   5D',/,
     *          20X,
     *          'SUMMARIZE URBAN RAIL STATION VOLUMES FOR',
     *          ' ACCESS TO GO RAIL TRIPS',//,
     *       1X,' STATION ','                              ',
     *          '        ','        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','------  ')
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      STASUM4(K,3)=STASUM4(K,1)+STASUM4(K,2)
      TTWLK=TTWLK+STASUM4(K,1)
      TTBUS=TTBUS+STASUM4(K,2)
      TTOTAL=TTOTAL+STASUM4(K,3)
      IF(STASUM4(K,3).GT.0.1) THEN
	    WRITE(26,7026) IEQUIV(KS),STANAME(K),
     *                  (STASUM4(K,L),L=1,3)
	    IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),
     *                  (STASUM4(K,L),L=1,3)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTOTAL
      END IF
      TTWLK=0.0
      TTBUS=0.0
      TTOTAL=0.0
C     WRITE(26,7067)
 7067 FORMAT(//,30X,'R E P O R T   5E',/,
     *          20X,
     *          'SUMMARIZE UPX STATION LEVEL VOLUMES',//,
     *       1X,' STATION ','                              ',
     *          '        ','        ',/,
     *       1X,' NUMBER ','        STATION NAME           ',
     *          ' ACCESS ',' EGRESS ','  TOTAL ',/,
     *       1X,'--------','-------------------------------',
     *          '--------','--------','------  ')
      DO K=1,MAX_STATIONS
      KS=K+MAX_IZONES
      DENOM=STASUM6(K,1)+STASUM6(K,2)
      TTWLK=TTWLK+STASUM6(K,1)
      TTBUS=TTBUS+STASUM6(K,2)
      TTOTAL=TTOTAL+DENOM
      IF(DENOM.GT.0.1) THEN
C     WRITE(26,7026) IEQUIV(KS),STANAME(K),
C    *                  (STASUM6(K,L),L=1,2),DENOM
      END IF
      END DO
C     WRITE(26,7022) TTWLK,TTBUS,TTOTAL
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
	IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),
     *          (STASUM(K,L),L=6,10)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
      IF(CSVRPT) WRITE(173,7053) TTWLK,TTBUS,TTPNR,TTKNR,TTOTAL
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
      IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),
     *                  (STASUM2(K,L),L=5,7)
      END IF
      END DO
      WRITE(26,7022) TTWLK,TTBUS,TTOTAL
      IF(CSVRPT) WRITE(173,7053) TTWLK,TTBUS,TTOTAL
C
C     GO BUS DRIVE ACCESS SUMMARY
C
      WRITE(26,7027)
 7027 FORMAT(//,30X,'R E P O R T  7A',/,
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
      IF(CSVRPT) WRITE(173,7052) IEQUIV(KS),STANAME(K),STASUM(K,11)
      ENDIF
      END DO
      WRITE(26,7022) TTPNR
      IF(CSVRPT) WRITE(173,7053) TTPNR
C
C     OUTPUT GO BUS IN-VEHICLE RATIO SUMMARIES
C
      IF(CALIB) THEN
      DO K=1,101
      WRITE(102,9020) (K-1),GBUSIVT(K,1),GBUSTIVT(K,1),GBUSRAT(K,1),
     *                      GBUSIVT(K,2),GBUSTIVT(K,2),GBUSRAT(K,2)
      WRITE(150,9020) (K-1),RAPDRAT(K,1),RAPDRAT(K,2)
 9020 FORMAT(I3,6(',',F10.2))
      END DO
      END IF
C.... TRANSIT DISTRICT LEVEL SUMMARIES
      IF(CALIB.OR.CSVRPT) THEN
      DO K2=1,6
      if(calib) WRITE(129,9047) tname(k2),(dname(k),k=1,(maxpd+1))
      if(csvrpt) WRITE(173,9047) tname(k2),(dname(k),k=1,(maxpd+1))
 9047 FORMAT(A13,41(',',A35))
      DO K=1,(MAXPD+1)
      if(calib) 
     *  WRITE(129,9016) DNAME(K),(TOTTRN(K,K1,K2),K1=1,(MAXPD+1))
      if(csvrpt) 
     *  WRITE(173,9016) DNAME(K),(TOTTRN(K,K1,K2),K1=1,(MAXPD+1))
 9016 FORMAT(A35,41(',',F10.2))
      END DO    
      END DO 
      END IF
      IF(CALIB) THEN
C.... NON MOTORIZED DISTRICT LEVEL SUMMARIES
      DO K2=1,3
      WRITE(158,9047) nname(k2),(dname(k),k=1,(maxpd+1))
      DO K=1,(MAXPD+1)
      WRITE(158,9016) DNAME(K),(NONMOT(K,K1,K2),K1=1,(MAXPD+1))
      END DO    
      END DO 
C.... TTC AVAILABLE PERSON TRIPS
      DO K2=1,2
      IF(K2.EQ.1)  K3=3
      IF(K2.EQ.2)  K3=5
      WRITE(154,9047) tname(k3),(dname(k),k=1,(maxpd+1))
      DO K=1,(MAXPD+1)
      WRITE(154,9016) DNAME(K),(AVLTTC(K,K1,K2),K1=1,(MAXPD+1))
      END DO    
      END DO       
C
C     SUMMARIZE TRANSFER INCIDENCE
C
       DO K=1,6
       DO K1=1,5
       TXFERS(K,6)=TXFERS(K,6)+TXFERS(K,K1)
       END DO 
       END DO
       WRITE(26,9516) (TXFERS(1,K1),K1=1,6),
     *                (TXFERS(3,K2),K2=1,6),
     *                (TXFERS(5,K2),K2=1,6),
     *                (TXFERS(2,K2),K2=1,6),
     *                (TXFERS(4,K2),K2=1,6),
     *                (TXFERS(6,K2),K2=1,6)
 9516  FORMAT(//'       BUS TRANSFER INCIDENCE SUMMARY'/
     *          ' -----------------------------------------'/
     *          ' WALK ACCESS     0          1         2   ',
     *          '     3         4+      TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------  --------'/
     *          ' BUS/STRCAR ',6(2X,F8.0)/
     *          ' RAPID BUS  ',6(2X,F8.0)/
     *          ' GO BUS     ',6(2X,F8.0)//
     *          ' DRIVE ACCESS    0          1         2   ',
     *          '     3          4+     TOTAL'/
     *          ' -----------  --------  --------  --------',
     *          '  --------  --------  --------'/
     *          ' BUS/STRCAR ',6(2X,F8.0)/
     *          ' RAPID BUS  ',6(2X,F8.0)/
     *          ' GO BUS     ',6(2X,F8.0)//)
       WRITE(139,9517) (TXFERS(1,K1),K1=1,6),
     *                (TXFERS(3,K2),K2=1,6),
     *                (TXFERS(5,K2),K2=1,6),
     *                (TXFERS(2,K2),K2=1,6),
     *                (TXFERS(4,K2),K2=1,6),
     *                (TXFERS(6,K2),K2=1,6)
 9517  FORMAT('BUS TRANSFER INCIDENCE SUMMARY'/
     *          'WALK_ACCESS,0,1,2,3,4+,TOTAL'/
     *          'BUS/STRCAR',6(',',F8.0)/
     *          'RAPID BUS',6(',',F8.0)/
     *          'GO BUS',6(',',F8.0)/
     *          'DRIVE_ACCESS,0,1,2,3,4+,TOTAL'/
     *          'BUS/STRCAR',6(',',F8.0)/
     *          'RAPID BUS',6(',',F8.0)/
     *          'GO BUS',6(',',F8.0))
      END IF
c
c     write out Rail Segment to Segment File
c
      if(calib) then
      write(137,312) iter
      write(137,306) gorname
  306 format('Segment',10(',',a25),',','Total')
      do k=1,maxgln
      write(137,305) gorname(k),(gorail(k,k1),k1=1,(maxgln+1))
  305 format(a25,21(',',f12.2))
      end do
      write(137,307) (gorail((maxgln+1),k1),k1=1,(maxgln+1))
  307 format('Total',21(',',f12.2))  
      write(137,308) ttcname
  308 format('Segment',4(',',a25),',','Total')
      do k=1,maxtln
      write(137,305) ttcname(k),(ttcsubway(k,k1),k1=1,(maxtln+1))
      end do
      write(137,307) (ttcsubway((maxtln+1),k1),k1=1,(maxtln+1))
      end if
c
      if(csvrpt) call prtseg(maxgln,gorail)
      if(csvrpt) call prtseg(maxtln,ttcsubway)
c
C.... AUTO DISTRICT LEVEL SUMMARIES
      IF(CALIB.OR.CSVRPT) THEN
      DO K2=1,4
      if(calib)
     * WRITE(129,9047) autoname(k2),(dname(k),k=1,(maxpd+1))
      if(csvrpt)
     * WRITE(173,9047) autoname(k2),(dname(k),k=1,(maxpd+1))
      DO K=1,(MAXPD+1)
      if(calib)
     * WRITE(129,9016) DNAME(K),(TOTAUT(K,K1,K2),K1=1,(MAXPD+1))
      if(csvrpt)
     * WRITE(173,9016) DNAME(K),(TOTAUT(K,K1,K2),K1=1,(MAXPD+1))
      END DO    
      END DO 
      END IF
c
      if(calib) then
      write(137,313)
  313 format('GO,Rail,Avaialble,Person,Trips')
      write(137,306) gorname
      do k=1,maxgln
      write(137,305) gorname(k),(gorailp1(k,k1),k1=1,(maxgln+1))
      end do
      write(137,307) (gorailp1((maxgln+1),k1),k1=1,(maxgln+1))
      write(137,313)
      write(137,306) gorname
      do k=1,maxgln
      write(137,305) gorname(k),(gorailp2(k,k1),k1=1,(maxgln+1))
      end do
      write(137,307) (gorailp2((maxgln+1),k1),k1=1,(maxgln+1))
      write(137,313)
      write(137,306) gorname
      do k=1,maxgln
      write(137,305) gorname(k),(gorailp3(k,k1),k1=1,(maxgln+1))
      end do
      write(137,307) (gorailp3((maxgln+1),k1),k1=1,(maxgln+1))
      write(137,313)
      write(137,306) gorname
      do k=1,maxgln
      write(137,305) gorname(k),(gorailp4(k,k1),k1=1,(maxgln+1))
      end do
      write(137,307) (gorailp4((maxgln+1),k1),k1=1,(maxgln+1))
c...station access summary
      if(calib) write(138,312) iter
  312 format('Calibration Iteration=',i2)
      write(138,309)
  309 format('GO Rail Station Access Summary'/
     *       'Station,Name,Walk,Bus,Park_Ride,Kiss_Ride,Total')
      do k=1,250
      if(stanum(k).eq.1) then
      orista=iequiv(k+max_izones)
      write(138,310) 
     *       orista,staname(k),(stasum(k,k1),k1=1,5)
  310 format(i5,',',a35,5(',',f10.1))
      end if
      end do
      write(138,314)
  314 format('GO Rail Station Egress Summary'/
     *       'Station,Name,Walk,Transit,Drive,Total')
      do k=1,250
      if(stanum(k).eq.1) then
      orista=iequiv(k+max_izones)
      write(138,310) 
     *       orista,staname(k),(stasum2(k,k1),k1=1,4)
      end if
      end do
      write(138,311)
  311 format(//'TTC Subway Station Access Summary'/
     *       'Station,Name,Walk,Bus,Park_Ride,Kiss_Ride,Total')
      do k=1,250
      if(stanum(k).eq.2) then
      orista=iequiv(k+max_izones)
      write(138,310) 
     *       orista,staname(k),(stasum(k,k1),k1=6,10)
      end if
      end do
      write(138,315)
  315 format(//'TTC Subway Station Egress Summary'/
     *       'Station,Name,Walk,Bus,Total')
      do k=1,250
      if(stanum(k).eq.2) then
      orista=iequiv(k+max_izones)
      write(138,310) 
     *       orista,staname(k),(stasum2(k,k1),k1=5,7)
      end if
      end do
      end if
C
C     WRITE OUT TTC SUBWAY STATION-TO-STATON TRIPS
C
      if(calib) then
      open(157,file='ttcsubway_urss.csv',status='unknown',
     *         form='formatted')
      do k=1,75
      k1=iequiv(k+max_izones)
      write(157,316) k1,staname(k),(urss(k,k2),k2=1,75)
  316 format(i5,',',a35,75(',',f10.1))
      end do
      end if
C
C     SELF CALIBRATION ANALYSIS
C
      if(calib) then
      iter=iter+1
      write(26,7032) iter
 7032 format(/' Calibration Iteration ',i2/
     *        ' -------------------------'/)
      write(*,7032) iter
      call scalib(iter,tesum,ttrip,ptrip,tottrn,gorail,ttcsubway,
     *            stasum,nonmot,stasum2)
      tesum=0.0
      if(.not.capres) stasum=0.0
      stasum2=0.0
      stasum3=0.0
      ttrip=0.0
      gbusivt=0.0
      gbustivt=0.0
      gbusrat=0.0
      rapdrat=0.0
      txfers=0.0
      tottrn=0.0
      nonmot=0.0
      avlttc=0.0
      estcbd=0.0  
      ptrip=0.0
      if(cvalue.gt.0.and.(.not.debug)) then
      close(140,status='keep')
      open(140,file=fbucket,status='old',form='binary') 
      end if       
      if(iter.lt.niter.and.(.not.capres)) then
       call fileopen
       if(ccode(27).or.ccode(28)) then
       stasta=0.0
       imode=1
       call station(stasta,imode,upxivt,gorchk)
       imode=2
       call station(stasta,imode,upxivt,ttcchk)
       end if
      go to 1000
      end if
      end if
C------------------------------------------------------------------
      IF(AIRPASS) THEN
      WRITE(26,9261)
 9261 FORMAT(//,30X,'R E P O R T   11',/,
     *          20X,'SUMMARIZE AIR PASSENGER TRIPS BY MODE',//,
     *       1X,'              2         3'/     
     *       1X,'  DRIVE     PERSON    PERSON            ',
     *          '   LIMO      RENTAL             ON-CALL ',
     *          '  PUBLIC'/     
     *       1X,'  ALONE      AUTO      AUTO     DROPOFF ',
     *          '  TOWN CAR    CAR       TAXI    SHUTTLE ',
     *          '  TRANSIT     TNC       TOTAL'/  
     *       1X,' --------  --------  --------  --------  --------',
     *          '  --------  --------  --------  --------',
     *          '  --------  --------')
      WRITE(26,9262) (AESUM(K),K=1,3),(AESUM(K1),K1=5,12)
 9262 FORMAT(12(2X,F8.1))
      DO NI=1,11
      PNRTRP(NI)=AESUM(NI)/AESUM(12)
      END DO
      WRITE(26,9733) (PNRTRP(K),K=1,3),(PNRTRP(K1),K1=5,11)
 9733 FORMAT(/11(2X,F8.4))
      NI=EQUIV(9863)-MAX_IZONES
      DENOM=AESUM(10)-STASUM2(NI,4)
      PNRTRP(1)=AESUM(1)+AESUM(2)+AESUM(3)
      OPEN(156,FILE=FAIRCALIB,STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(156,9734) AESUM(1),AESUM(2),AESUM(3),AESUM(5),
     *                AESUM(7),AESUM(6),AESUM(8),AESUM(9),
     *                DENOM,AESUM(11),STASUM2(NI,4),
     *                AESUM(12),PNRTRP(1),AESUM(10)
 9734 FORMAT('DRIVE_ALONE,',F8.1/
     *       '2_PERSON,',F8.1/
     *       '3+_PERSON,',F8.1/
     *       'DROPOFF,',F8.1/
     *       'RENTAL,',F8.1/
     *       'LIMO,',F8.1/
     *       'TAXI,',F8.1/
     *       'ON_CALL,'F8.1/
     *       'TRANSIT,',F8.1/
     *       'UBER,',F8.1/
     *       'UPX,',F8.1/
     *       'TOTAL,',F8.1/
     *       'TOT_DRIVE,',F8.1/
     *       'TOT_TRN,',F8.1)
      END IF
C--------------------------------------------------------------------------
C
C SUMMARIZE AIR PASSENGER PARKING LOT CHOICE RESULTS
C
      IF(AIR.AND.(.NOT.AIRTRN)) THEN
      PNRTRP(1)=0.0
      PNRTRP(2)=0.0
      PNRTRP(3)=0.0
      PNRTRP(4)=0.0
      PNRTRP(5)=0.0
      WRITE(159,47) ITER
   47 FORMAT('ITERATION,',I4)
      WRITE(26,337)
  337 FORMAT(//20X,'          R E P O R T  4'/
     *   20X,'     PEARSON PARKING LOT CHOICE RESULTS'//
     *       ' PARKING   TOTAL        MODE DISTRIBUTION       ',
     *       '  AVAILABLE   V/C   SHADOW'/
     *       '   LOT    VEHICLES    WALK     SHUTTLE   TRANSIT',
     *       '   SPACES    RATIO   PRICE'/
     *       ' -------  --------  --------  --------  --------',
     *       '  --------  ------  --------')
      DO 44 NI=1,50
      KJZ=PEQUIV(NI)
      IF(PEQUIV(NI).LE.0) GO TO 44
      IF(PRKDATA(NI,1).EQ.0) GO TO 44
      VCRATIO=0.0
      PSPACES=0.0
      IF(AIRPASS) THEN
      PSPACES=(PRKDATA(NI,3)*PRKDATA(NI,18))*2.0
      ELSE
      PSPACES=(PRKDATA(NI,4)*PRKDATA(NI,17))*2.0
      IF(.NOT.EMPPRK) PSPACES=(PSPACES +
     *        PRKDATA(NI,3)*PRKDATA(NI,18))*2.0
      END IF
      IF(PSPACES.GT.0) VCRATIO=LOTRIPS(NI,1)/(PSPACES*AIROCC)
      SHDPRICE=0.0
      IF(VCRATIO.GT.1.05) THEN
      SHDPRICE=LOG(1.0/VCRATIO)
      CNVRGE=.FALSE.
      END IF
      PRKDATA(NI,19)=PRKDATA(NI,19)+0.5*SHDPRICE
      IF(LOTRIPS(NI,1).GT.0) THEN
      WRITE(26,45) IEQUIV(KJZ),(LOTRIPS(NI,K),K=1,4),PSPACES,VCRATIO,
     *             PRKDATA(NI,19)
   45 FORMAT(I8,5(2X,F8.1),2X,F6.2,2X,F8.5,2X,F8.5)
      WRITE(159,46) IEQUIV(KJZ),(LOTRIPS(NI,K),K=1,4),PSPACES,VCRATIO,
     *              PRKDATA(NI,19)
   46 FORMAT(I4,5(',',F8.1),',',F6.2,',',F10.5) 
      END IF  
      PNRTRP(1)=PNRTRP(1)+LOTRIPS(NI,1)
      PNRTRP(2)=PNRTRP(2)+LOTRIPS(NI,2)
      PNRTRP(3)=PNRTRP(3)+LOTRIPS(NI,3)
      PNRTRP(4)=PNRTRP(4)+LOTRIPS(NI,4)
      PNRTRP(5)=PNRTRP(5)+PSPACES
   44 CONTINUE
      VCRATIO=PNRTRP(1)/(2*PNRTRP(5))
      WRITE(26,345) (PNRTRP(K),K=1,5),VCRATIO
  345 FORMAT(/2X,'TOTAL',1X,5(2X,F8.1),2X,F6.2)
      PNRTRP(1)=0.0
      PNRTRP(2)=0.0
      PNRTRP(3)=0.0
      PNRTRP(4)=0.0
      PNRTRP(5)=0.0
      PNRTRP(6)=0.0
      PNRTRP(7)=0.0    
      PNRTRP(10)=0.0
      WRITE(26,335)
  335 FORMAT(//20X,'          R E P O R T  5'/
     *    20X,'    PEARSON PARKING TRANSIT CHOICE RESULTS'//
     *       ' PARKING   ATTR    TRANSIT'/
     *       '   LOT     ZONE     TRIPS    MODE'/
     *       ' -------  -------  --------  ----')
      DO 3836 NI=1,50
      KJZ=PEQUIV(NI)
      IF(KJZ.LE.0) GO TO 3836
      DO 3837 JZ=1,50
      K1=AEQUIV(JZ)
      DO 3839 K=1,5
      IF(LOTTRN(NI,JZ,K).GT.0.001) WRITE(26,336) IEQUIV(KJZ),IEQUIV(K1),
     *    LOTTRN(NI,JZ,K),K,TNAME(K+1)
      PNRTRP(10)=PNRTRP(10)+LOTTRN(NI,JZ,K)
      PNRTRP(K)=PNRTRP(K)+LOTTRN(NI,JZ,K)
  336 FORMAT(I8,2X,I7,2X,F8.1,2X,I2,3X,'(',A13,')')
 3839 CONTINUE
 3837 CONTINUE
 3836 CONTINUE
      WRITE(26,3845) PNRTRP(10),(PNRTRP(K),K=1,5)
 3845 FORMAT(/19X,F8.2//,
     *  10X,'  PEARSON PARKING LOT CHOICE'/
     *  10X,'  PRIMARY MODE TRANSIT CHOICE RESULTS'//
     *       1X,'GO RAIL        =',F8.2/
     *       1X,'TTC SUBWAY     =',F8.2/
     *       1X,'GO BUS         =',F8.2/
     *       1X,'BUS/STREETCAR  =',F8.2/
     *       1X,'RAPID BUS      =',F8.2/)
      END IF
c     open(280,file='upx_non_airport.csv',status='unknown',
c    *         form='formatted')
      do k1=1,max_stations
      do k2=1,max_stations
      if(upxsta(k1,k2).gt.0.01) then
c     write(280,8033) iequiv(k1+max_izones),staname(k1),
c    *                iequiv(k2+max_izones),staname(k2),
c    *                upxsta(k1,k2)
 8033 format(i5,',',a37,',to,',i5,',',a37,',',f8.2)
      end if
      end do
      end do
C     
C     STATION CAPACITY RESTRAINT
C
      if(capres) then
      if(.not.calib) iter=iter+1
      write(26,7033) iter
 7033 format(/' Station Capacity Restraint Iteration ',i2/
     *        ' -------------------------------------'/)
      write(*,7033) iter
      call stacap(iter,stasum)
C     tesum=0.0
C     stasum=0.0
C     stasum2=0.0
C     stasum3=0.0
C     ttrip=0.0
C     estcbd=0.0
C     ptrip=0.0
C     close(140,status='keep')
C     open(140,file=fbucket,status='old',form='binary')  
C     if(iter.lt.citer) then
C     call fileopen
C     go to 1000
C     end if
      end if
C     
C     PEARSON PARKING LOT CAPACITY RESTRAINT
C
      if(air.and.(.not.airtrn)) then
      iter=iter+1
      tesum=0.0
      stasum=0.0
      stasum2=0.0
      stasum3=0.0
      ttrip=0.0
      estcbd=0.0
      ptrip=0.0
      lottrn=0.0
      lotrips=0.0
      aesum=0.0
      if(iter.le.citer.and.(.not.cnvrge)) then
      write(26,7063) iter
 7063 format(/' Pearson Parking Lot Capacity Restraint Iteration ',i2/
     *        ' -------------------------------------------------'/)
      write(*,7063) iter
      call fileopen
      cnvrge=.true.
      go to 1000
      end if
      end if
C
C     OUTPUT STATION LEVEL TRIP MATRICES
C 
      IF(TRIPSOUT) THEN
C.....GO RAIL & TTC SUBWAY STATION-TO-STATION
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,DJZ)=CRSS(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(184) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,DJZ)=URSS(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(44) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,(MAX_STATIONS-1)
      DO JZ=1,(MAX_STATIONS-1)
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,DJZ)=CRURSS(IZ,JZ)+URCRSS(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(21) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
C.....GO RAIL & TTC SUBWAY WALK TO STATION
      TRNTRP=0.0
      DO IZ=1,MAX_IZONES
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(IZ,DJZ)=WLKCR(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(185) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO     
      TRNTRP=0.0
      DO IZ=1,MAX_IZONES
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(IZ,DJZ)=WLKUR(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(187) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO  
C.....GO RAIL & TTC SUBWAY BUS TO STATION
      TRNTRP=0.0
      DO IZ=1,MAX_IZONES
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(IZ,DJZ)=BCR(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(45) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,MAX_IZONES
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(IZ,DJZ)=BUR(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(47) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,MAX_IZONES
      DO JZ=1,MAX_STATIONS
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(IZ,DJZ)=DTRAN(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(50) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
C.....GO RAIL, TTC SUBWAY WALK EGRESS STATION-TO-ZONE
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_IZONES
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,JZ)=CRWLK(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(186) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_IZONES
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,JZ)=URWLK(IZ,JZ)+CRURWLK(IZ,JZ)
	    END DO
	    END DO   
	    DO IIZ=1,4000
	    WRITE(188) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO  
C.....GO RAIL, TTC SUBWAY & GO BUS STATION-TO-ZONE
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_IZONES
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,JZ)=CRSTAZ(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(46) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_IZONES
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,JZ)=URSTAZ(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(48) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_IZONES
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,JZ)=GBSTAZ(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(49) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      TRNTRP=0.0
      DO IZ=1,MAX_STATIONS
      DO JZ=1,MAX_IZONES
	    DIZ=IZ+MAX_IZONES
	    DJZ=JZ+MAX_IZONES
	    TRNTRP(DIZ,JZ)=CRURSTAZ(IZ,JZ)
	    END DO
	    END DO    
	    DO IIZ=1,4000
	    WRITE(171) IIZ,(TRNTRP(IIZ,JZ),JZ=1,4000)
	    END DO
      END IF
C
C     FILE CLEANUP
C
 8886 CONTINUE
      IF(DEBUG) THEN
      CLOSE(101,STATUS='DELETE')
      CLOSE(103,STATUS='DELETE')
	    END IF
	    IF(CALIB) THEN
	    WRITE(26,8888)
 8888 FORMAT(//' NUMBER OF IJ PAIRS BY MARKET SEGMENT'/
     *       ' ------------------------------------')
      DO K=1,NCATS
      WRITE(26,8889) K,IJPAIRS(K)
 8889 FORMAT(' MARKET SEGMENT (',I1,')=',I8)
      END DO
      WRITE(26,8887) IJPAIRS(7)
 8887 FORMAT(' TOTAL             =',I8)
      END IF
C      if(maxiz.gt.0.and.maxjz.gt.0) then
C      write(26,77777) iequiv(maxiz),iequiv(maxjz),maxvalue    
C      else
C      write(26,77777) maxiz,maxjz,maxvalue
C77777 format(//' maxiz=',i5,' maxjz=',i5,' maxvalue=',f14.4//)
C      end if
      CLOSE(140,STATUS='DELETE')
      CLOSE(190,STATUS='DELETE')
      CALL GETTIM(IHR,IMIN,ISEC,I100)
      WRITE(26,8004) IHR,IMIN,ISEC
      WRITE(*,8004) IHR,IMIN,ISEC
 8004 FORMAT(/' Program Completed: ',I2,':',I2,':',I2)
      end
