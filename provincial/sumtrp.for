C-------------------------------------------------------------------
C      STATION EGRESS TRIP OUTPUT SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE SUMTRP(imode,jz,dsta,egrind,trips,
     *            crwlk,crstaz,cruber,staegr)
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
C
      integer*2 imode,jz,dsta,dc,egrind
      real*4    trips,staegr(3,max_stations,max_izones)
      real*4    egress(3)
      real*4    crwlk(max_stations,max_izones)
      real*4    crstaz(max_stations,max_izones)
      real*4    cruber(max_stations,max_izones)
C
      dc=dsta-max_izones
      egress(1)=staegr(1,dc,jz)
      egress(3)=staegr(2,dc,jz)
      egress(2)=1.0-egress(1)-egress(3)
      if(egrind.eq.0) then
      crwlk(dc,jz)=crwlk(dc,jz)+trips*egress(1)
      crstaz(dc,jz)=crstaz(dc,jz)+trips*egress(2)
      cruber(dc,jz)=cruber(dc,jz)+trips*egress(3)
C --------------------------------------------------------
      if(debug.and.sdetail) then
      write(26,9001) imode,iequiv(jz),iequiv(dsta),egrind,trips,
     *               crwlk(dc,jz),crstaz(dc,jz),cruber(dc,jz)
 9001 format(' imode=',i1,' jz=',i4,' dsta=',i4,
     *       ' egrind=',i2,' trips=',f8.5,
     *       ' walk=',f8.5,' bus=',f8.5,' uber=',f8.5)
      end if
C --------------------------------------------------------------
      end if
      return
      end

	     