C-------------------------------------------------------------------
C        STATION --> STATION UTILITY COMPUTATION SUBROUTINE
C        FOR 4-PART PATH SELECTION
C-------------------------------------------------------------------
       SUBROUTINE PART4(jz,sta,stasta,stazne,imode,
     *                  matr,morg,mdst,mind,mutil)
       INCLUDE 'stadat.inc'
       INCLUDE 'param.inc'
	     include 'mlogitpar.inc'
	     include 'dvalues.inc'
C
C DATA DECLARATIONS
C
      integer*2     IMODE,sc,sc2,sc3,sc4,sta,morg,mdst,matr,egrind,mind
      integer*2     jz
      INTEGER*4     IC,DC,ur1,ur2,dc2,dc3
      integer*4     crind(max_stations)
      REAL*4        STASTA(5,MAX_STATIONS,MAX_STATIONS),
     *              STAZNE(7,MAX_STATIONS,MAX_IZONES)
      real*4        util,mutil,ktxfer
      CHARACTER*15  NAME(2)
      DATA          NAME/'GO Rail        ',
     *                   '               '/
      mutil=-99999.8
      matr=0
      morg=0
      mdst=0
      mind=0
      if(imode.gt.1) then
      write(26,7006) imode
 7006 format(' PART4 7006 (F) REQUESTED MODE (',i1,') OUT OF RANGE')
      stop
      end if
c
c     origin station
c
      sc=sta-max_izones
      if(sc.lt.0) return
      ic=iequiv(sta)
      if(sdetail) write(26,7003) ic,staname(sc),imode
 7003 format(/' Origin Station=',i5,1x,a37,' imode=',i1)
c
c     destination station
c
      do sc2=1,max_stations
      dc=iequiv(sc2+max_izones)
      if(sc.eq.sc2) cycle
      if(stanum(sc2).ne.imode) cycle
      if(stadata(sc2,6).ne.1.0) cycle      
      if(sdetail) write(26,7007) iequiv(equiv3(sc2,1)),
     *               staname(equiv3(sc2,1)-max_izones),
     *               iequiv(equiv3(sc2,2)),
     *               staname(equiv3(sc2,2)-max_izones)
 7007 format(' equiv3(1)=',i5,1x,a37,1x,
     *       ' equiv3(2)=',i5,1x,a37)
c
c     co-located urban rail station ?
c
      do sc3=1,max_stations  
      ur1=iequiv(sc3+max_izones)
      if(stanum(sc3).ne.2) cycle
      if(stadata(sc3,6).ne.1.0) cycle
      dc2=equiv3(sc2,1)
      dc2=iequiv(dc2)
      dc3=equiv3(sc2,2)
      dc3=iequiv(dc3)
      if(dc2.eq.ur1.or.dc3.eq.ur1) then
      if(sdetail) write(26,7002) dc,staname(sc2),ur1,staname(sc3)
 7002 format(' match for CR/HSR station=',i5,1x,a37,
     *       ' and UR station=',i5,1x,a37)
c
c     loop through all urban rail stations
c
      do sc4=1,max_stations
      ur2=iequiv(sc4+max_izones)
      if(stanum(sc4).ne.2) cycle
      if(stadata(sc4,6).ne.1.0) cycle
      if(sc3.eq.sc4) cycle
      if(stasta(2,sc,sc2).eq.0) cycle
      if(stasta(2,sc3,sc4).eq.0) cycle
      util=-999.9
      ktxfer=kcrurtx/(lsum1trn*lsum2cr*lsum3cw)
      if(dc.eq.gounion) ktxfer=0.0
      if(stazne(2,sc4,jz).ne.0.0) then
      util=stasta(2,sc,sc2)+stasta(2,sc3,sc4)+stazne(2,sc4,jz)+
     *          coeff(7)*stazne(3,sc4,jz) +
     *          coeff(7)*(stadata(sc,9)+stadata(sc2,9)) +
     *          coeff(7)*(stadata(sc3,9)+stadata(sc4,9)) +
     *          ktxfer
      egrind=staind(sc4,jz)
      end if
c
      if(sdetail) write(26,7001) name(imode),ic,staname(sc),
     *               dc,staname(sc2),
     *               ur1,staname(sc3),ur2,staname(sc4),
     *               stadata(sc,9),stadata(sc2,9),
     *               stadata(sc3,9),stadata(sc4,9),
     *               stasta(2,sc,sc2),
     *               stasta(2,sc3,sc4),
     *               stazne(2,sc4,jz),stazne(3,sc4,jz),
     *               ktxfer,util,
     *               staind(sc4,jz),mutil
 7001 format(/' 4 PART PATH CONSIDERATION FOR ',A15,/
     *       ' ------------------------------'/
     *       ' ORIGIN STATION                       =',I5,1X,A37/
     *       ' DESTINATION STATION                  =',I5,1X,A37/
     *       ' URBAN RAIL ORIGIN STATION            =',I5,1X,A37/
     *       ' URBAN RAIL DESTINATION STATION       =',I5,1X,A37/
     *       ' STATION PLATFORM TIME (ORG STA)      =',F10.5/
     *       ' STATION PLATFORM TIME (DEST STA)     =',F10.5/
     *       ' STATION PLATFORM TIME (UR ORG STA)   =',F10.5/
     *       ' STATION PLATFORM TIME (UR DEST STA)  =',F10.5/
     *       ' PRIMARY STATION-TO-STATION UTILITY   =',F10.5/
     *       ' URBAN RAIL STATION-TO-STATION UTILITY=',F10.5/
     *       ' STATION-TO-ZONE UTILITY              =',F10.5/
     *       ' STATION-TO-ZONE WALK TIME            =',F10.5/
     *       ' GO RAIL --> TTC SUBWAY TRANSFER      =',F10.5/
     *       ' TOTAL EGRESS     UTILITY             =',F10.5/
     *       ' EGRESS INDICATOR                     =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2'//
     *       ' SELECTED UTILITY                     =',F10.5/)
      if(util.gt.mutil) then
      mutil=util
      matr=sc2
      morg=sc3
      mdst=sc4
      mind=egrind
      end if
      end do
      end if
      end do
      end do
      if(debug) then
      if(matr.gt.0) then
      write(26,7001) name(imode),ic,staname(sc),
     *               iequiv(matr+max_izones),staname(matr),
     *               iequiv(morg+max_izones),staname(morg),
     *               iequiv(mdst+max_izones),staname(mdst),
     *               stadata(sc,9),stadata(matr,9),
     *               stadata(morg,9),stadata(mdst,9),
     *               stasta(2,sc,matr),
     *               stasta(2,morg,mdst),
     *               stazne(2,mdst,jz),stazne(3,mdst,jz),
     *               ktxfer,mutil,
     *               staind(mdst,jz),mutil
      write(26,7004) iequiv(sc+max_izones),staname(sc),
     *               iequiv(matr+max_izones),staname(matr),
     *               iequiv(morg+max_izones),staname(morg),
     *               iequiv(mdst+max_izones),staname(mdst),
     *               mind,mutil
 7004 format(/' SELECTED URBAN RAIL STATIONS & UTILITY'/
     *       ' --------------------------------------'/
     *       ' COMMUTER RAIL ORIGIN STATION  =',i5,1x,a37/
     *       ' COMMUTER RAIL DESTINATION STA =',i5,1x,a37/
     *       ' URBAN RAIL ORIGIN STATION     =',i5,1x,a37/
     *       ' URBAN RAIL DESTINATION STATION=',i5,1x,a37/
     *       ' EGRESS INDICATOR              =',i5/
     *       ' MINIMUM UTILITY               =',f10.5/)
      else
      write(26,7005) iequiv(sc+max_izones),staname(sc)
 7005 format(/' SELECTED URBAN RAIL STATIONS & UTILITY'/
     *       ' --------------------------------------'/
     *       ' COMMUTER RAIL ORIGIN STATION  =',i5,1x,a37/
     *       ' *** NO PATH AVAILABLE ***'/)
      end if
      end if
      return
      end
             