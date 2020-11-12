C-------------------------------------------------------------------
C     URBAN RAIL ACCESS TO COMMUTER RAIL SUBROUTINE
C-------------------------------------------------------------------
      subroutine teval(jz,ista,znesta,znestau,bsta,bdsta,butil,stasta,
     *                 stazne,msta,dsta,mutil)
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
c
c DATA DECLARATIONS
c
      integer*2      znesta(50,3),bsta(2,2),ista,bdsta(2,2),sc1,sc2,jz
      integer*2      sta,ur1,cr1,uracc,uregr,msta,urunion
      integer*2      zonesta(max_izones),imode,dsta,osta
      real*4         znestau(50),butil(2,2)
      real*4         stasta(5,max_stations,max_stations)
      real*4         stazne(7,max_stations,max_izones)
      real*4         mutil,unionivt,inveh,ivtdiff
      character*3    ans(2)
      data           ans/'---','YES'/
      mutil=-999.9
      msta=0
      sc1=bsta(1,ista)-max_izones
      sc2=bdsta(1,ista)-max_izones
      urunion=equiv(ttcunion)-max_izones
c -------------------------------------------------------------------
      if(debug) then
      write(26,9001) ista,
     *    iequiv(bsta(1,ista)),staname(sc1),
     *    iequiv(bdsta(1,ista)),staname(sc2),
     *    butil(1,ista),staind(sc2,jz),stazne(3,sc2,jz)
 9001 format(' TTC SUBWAY ACCESS TO GO RAIL BUS EVALUATION'/
     *       ' -------------------------------------------'/
     *       ' GO RAIL BUS STATON NO    =',I4/
     *       ' GO RAIL ACCESS STATION   =',I4,1X,A20/
     *       '         EGRESS STATION   =',I4,1X,A20/
     *       ' GO RAIL BUS    UTILITY   =',F10.5/
     *       ' GO RAIL EGRESS INDICATOR =',I4/
     *       ' GO RAIL WALK EGRESS TIME =',F8.2/)
      end if
c ------------------------------------------------------------------
      do sta=1,50
      if(zneref(sta,1).le.0) cycle
      if(znesta(sta,1).le.0) cycle
      cr1=zneref(sta,1)-max_izones
      ur1=zneref(sta,2)-max_izones
      uracc=znesta(sta,1)-max_izones
      if(airpass.and.ttcind(uracc).ne.0) then
      if(debug) write(26,9006) iequiv(znesta(sta,1)),staname(uracc),
     *          ans(ttcind(uracc)+1)
 9006 FORMAT(//' TTC SUBWAY STATION ELIGIBILITY'/
     *       ' --------------------------------'/
     *       ' OSTA       NAME            ELM'/
     *       ' ---- --------------------  ---'/
     * 1X,I4,1x,A20,2X,A3)
      cycle
      end if
      uregr=znesta(sta,2)-max_izones
      inveh=stasta(3,uracc,uregr)
      unionivt=stasta(3,uracc,urunion)
      ivtdiff=inveh-unionivt
c ------------------------------------------------------------------
      if(debug) then
      write(26,9002) sta,iequiv(zneref(sta,1)),staname(cr1),
     *               iequiv(zneref(sta,2)),staname(ur1),
     *               iequiv(znesta(sta,1)),staname(uracc),
     *               iequiv(znesta(sta,2)),staname(uregr),
     *               znesta(sta,3),znestau(sta),
     *               inveh,unionivt,ivtdiff
 9002 format(' STATION EQUIVALENCE - OPTION ',I2/
     *       ' -------------------------------'/
     *       '  GO    RAIL STATION EQUIV  =',I4,1X,A20/
     *       '  URBAN RAIL STATION EQUIV  =',I4,1X,A20/
     *       '  URBAN RAIL ACCESS STATION =',I4,1X,A20/
     *       '  URBAN RAIL EGRESS STATION =',I4,1X,A20/
     *       '  URBAN RAIL MODE INDICATOR =',I4/
     *       '  URBAN RAIL ACCESS UTILITY =',F10.5//
     *       '  URBAN RAIL IN-VEHICLE TIME=',F10.2/
     *       '  URBAN RAIL IVT TO UNTION  =',F10.2/
     *       '  IN-VEHICLE DIFFERENCE     =',F10.2)
      end if
c ------------------------------------------------------------------
      if(ivtdiff.gt.acoef(10)) then
      if(debug) write(26,9008) ivtdiff,acoef(10)
 9008 format(/'  IN-VEHICLE TIME DIFFERENCE=',F6.2,
     *       ' IS GREATER THAN ACOEF(10)=',F6.2/)
      cycle
      end if
      imode=1
      osta=zneref(sta,1)
	    call egrsta(jz,osta,stasta,stazne,dsta,imode,zonesta)
      cr2=dsta-max_izones
      if(cr1.lt.0.OR.cr2.lt.0.or.cr2.eq.max_stations) cycle
      util=znestau(sta) + stasta(2,cr1,cr2) +
     *      stazne(2,cr2,jz) + 
     *      coeff(7)* (stadata(cr1,9) + stadata(cr2,9))
C....................................................................
      IF(DEBUG) THEN
      WRITE(26,9003) STA,IEQUIV(JZ),
     *               IEQUIV(ZNESTA(STA,1)),STANAME(URACC),
     *               IEQUIV(ZNESTA(STA,2)),
     *               STANAME(UREGR),ZNESTAU(STA),STASTA(2,CR1,CR2),
     *               STAZNE(2,CR2,JZ),STADATA(CR1,9),STAIND(CR2,JZ),
     *               STADATA(CR2,9),UTIL
 9003 FORMAT(/1X,'TTC SUBWAY ACCESS #',I2,
     *           ' --> GO RAIL RAIL UTILITY COMPUTATION -- '/
     *       1X,'----------------------------------------'/
     *       1X,'DESTINATION ZONE  =',I10/
     *       1X,'ACCESS STATION    =',I10,5X,A37/
     *       1X,'EGRESS STATION    =',I10,5X,A37/
     *       1X,'ACCESS    UTILITY =',F10.5/
     *       1X,'STA-->STA UTILITY =',F10.5/
     *       1X,'STA-->ZNE UTILITY =',F10.5/
     *       1X,'ACCESS PLATFORM TIME=',F10.5/
     *       1X,'EGRESS INDICATOR    =',I10,
     *          ' DIRECT WALK=1,BUS TRANSFER=2'/
     *       1X,'EGRESS PLATFORM TIME=',F10.5/
     *       1X,'TOTAL     UTILITY =',F10.5/)
      END IF
C.....................................................................
      if(util.gt.butil(1,ista)) then
      IF(DEBUG) WRITE(26,9004) UTIL,BUTIL(1,ISTA)
 9004 FORMAT(/' TTC SUBWAY ACCESS SELECTED'/
     *       ' --------------------------'/
     *       ' TTC SUBWAY   ACCESS UTILITY=',F10.5/
     *       ' GO RAIL BUS  ACCESS UTILITY=',F10.5/)
      IF(AIRCALIB.AND.DEBUG) THEN
      WRITE(343,9007) sta,iequiv(znesta(sta,1)),staname(uracc),
     *                iequiv(znesta(sta,2)),staname(uregr),util
 9007 FORMAT(2X,I2,3X,I4,2X,A30,2X,I4,2X,A30,2X,F10.5)
      END IF
      if(util.gt.mutil) then
      mutil=util
      msta=sta
      end if
      ELSE
      IF(DEBUG) WRITE(26,9005) UTIL,BUTIL(1,ISTA)
 9005 FORMAT(/' TTC SUBWAY ACCESS NOT SELECTED'/
     *       ' ------------------------------'/
     *       ' TTC SUBWAY   ACCESS UTILITY=',F10.5/
     *       ' GO RAIL BUS  ACCESS UTILITY=',F10.5/)
      IF(AIRCALIB.AND.DEBUG) THEN
      WRITE(343,9007) sta,iequiv(znesta(sta,1)),staname(uracc),
     *                iequiv(znesta(sta,2)),staname(uregr),util
      END IF
      END IF
      end do
      return
      end
