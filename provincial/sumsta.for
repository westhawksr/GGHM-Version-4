C-------------------------------------------------------------------
C      STATION EGRESS SUMMARY SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE SUMSTA(IMODE,JZ,DSTA,TRIPS,STASUM2,STAEGR,STASUM3,
     *                  DIRWALK,EGRWALK)
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
C
      integer*2 imode,jz,dsta,dc,ic
      integer*4 windex
      real*4    trips,staegr(3,max_stations,max_izones)
      real*4    egress(3),dirwalk(1000,4000)
      real*8    stasum2(max_stations,8)
      real*8    stasum3(max_stations,5)
      real*8    egrwalk(51,4)
C
      dc=dsta-max_izones
      ic=iequiv(dsta)
      windex=ifix(dirwalk(dc,jz))+1
      windex=min0(windex,51)
C
      egress(1)=staegr(1,dc,jz)
      egress(3)=staegr(2,dc,jz)
      egress(2)=1.0-egress(1)-egress(3)
C.....GO RAIL EGRESS
      if(imode.eq.1) then
      stasum2(dc,1)=stasum2(dc,1)+trips*egress(1)
      stasum2(dc,2)=stasum2(dc,2)+trips*egress(2)
      stasum2(dc,3)=stasum2(dc,3)+trips*egress(3)
      if(ic.eq.gounion) then
      egrwalk(windex,1)=egrwalk(windex,1)+trips*egress(1)
      end if
      egrwalk(windex,3)=egrwalk(windex,3)+trips*egress(1)
C --------------------------------------------------------
      if(debug.and.sdetail) then
      write(26,9001) imode,iequiv(jz),iequiv(dsta),trips,
     *              (stasum2(dc,k),k=1,3)
 9001 format(' imode=',i1,' jz=',i4,' dsta=',i4,
     *       ' trips=',f8.5,' stasum2=',3(1x,f8.5))
      end if
C --------------------------------------------------------------
      end if
C.....TTC SUBWAY EGRESS
      if(imode.eq.2) then
      stasum2(dc,5)=stasum2(dc,5)+trips*egress(1)
      stasum2(dc,6)=stasum2(dc,6)+trips*egress(2)
      stasum2(dc,8)=stasum2(dc,8)+trips*egress(3)
      if(ic.eq.ttcunion) then
      egrwalk(windex,2)=egrwalk(windex,2)+trips*egress(1)
      end if
      egrwalk(windex,4)=egrwalk(windex,4)+trips*egress(1)
C --------------------------------------------------------
      if(debug.and.sdetail) then
      write(26,9002) imode,iequiv(jz),iequiv(dsta),trips,
     *              (stasum2(dc,k),k=5,6),stasum2(dc,8)
 9002 format(' imode=',i1,' jz=',i4,' dsta=',i4,
     *       ' trips=',f8.5,' stasum2=',3(1x,f8.5))
      end if
C --------------------------------------------------------------
      end if
C.....GO RAIL EGRESS BY TTC SUBWAY
      if(imode.eq.3) then
      stasum3(dc,2)=stasum3(dc,2)+trips*egress(1)
      stasum3(dc,3)=stasum3(dc,3)+trips*egress(2)
      stasum3(dc,4)=stasum3(dc,4)+trips*egress(3)
      if(ic.eq.ttcunion) then
      egrwalk(windex,2)=egrwalk(windex,2)+trips*egress(1)
      end if
      egrwalk(windex,4)=egrwalk(windex,4)+trips*egress(1)
C --------------------------------------------------------
      if(debug.and.sdetail) then
      write(26,9003) imode,iequiv(jz),iequiv(dsta),trips,
     *              (stasum3(dc,k),k=2,4)
 9003 format(' imode=',i1,' jz=',i4,' dsta=',i4,
     *       ' trips=',f8.5,' stasum3=',3(1x,f8.5))
      end if
C --------------------------------------------------------------
      end if
      return
      end

	     