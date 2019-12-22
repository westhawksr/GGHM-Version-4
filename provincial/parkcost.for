C***********************************************************
C     parking cost model computation                       *
C***********************************************************
      subroutine parkcost
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
	    integer*2   unit,type,orgzn,destzn,printzn,zone
	    integer*4   iz,jz,qzone(5),indval
	    real*4      hdist(4000)
	    real*4      srow(1000,1000)
	    real*4      prow(1000)
	    real*4      arow(1000,4000)
	    real*4      temp,tarea,sdist,zdensity,rtemp
      real*4      denom,supply,fecost(4000),pcost
      real*4      a,b,c,d,e,y
      real*4      qprkcst(5),ranval,adjval
C
      a=35.0
      b=-83.49
      sdist=0.5
      aprkcst=0.0
      aprkzone=0
C -----------------------------------------------------------------
C     compute parking cost based upon employment density
C -----------------------------------------------------------------
      if(prkmdl) then
C     open(189,file='parking_cost_model.csv',status='unknown',
C    *         form='formatted')
C     write(189,1)
C   1 format('zone,prkcst,est_prkcst')
      write(*,55559)
55559 format(/' Zonal Parking Cost Model'/)
      do iz=1,max_izones
      nk=mod(iz,100)
      if(nk.EQ.0.and.debug.and.sdetail) WRITE(*,8001) iz,iequiv(iz)
      type=1
      orgzn=iz
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
c
c     compute floating density value
c
      temp=0.0
      tarea=0.0
      do jz=1,max_izones
      if(hdist(jz).lt.sdist) then
      temp=temp+zden(jz,3)
      tarea=tarea+zden(jz,6)
      end if
      end do
      if(tarea.gt.0.0) then
      zden(iz,7)=(temp/tarea)
      end if
      end do
c
c     compute parking cost estimates for all zones - floating density
c
      do iz=1,max_izones
      zone=iequiv(iz)
      if(zone.le.0) cycle
      if(prkcst(iz).le.0) cycle
c...parking supply
      if(zden(jz,7).ne.0.0) then
      rtemp=a*alog(zden(iz,7))+b
      else
      rtemp=0.0
      end if
      temp=amax1(rtemp,0.0)
      supply=temp*zden(iz,6)
c...parking cost
      if(zden(iz,6).gt.0.and.supply.gt.0) then
      fecost(iz)=2.76+0.0922*(supply/zden(iz,6))
      else
      rtemp=a*alog(zden(iz,7))*0.75
      temp=amax1(rtemp,0.0)
      supply=temp*zden(iz,6)
      if(zden(iz,6).gt.0) fecost(iz)=2.50+0.07*(supply/zden(iz,6))
      end if
      pcost=prkcst(iz)/100.0
C     write(189,503) iequiv(iz),pcost,fecost(iz)
  503 format(i4,4(',',f8.2))
C...compute revised parking cost using pivot point
      xratio=(fecost(iz)*100.0)/mdlprk(iz)
      prkcst(iz)=prkcst(iz)*xratio
      end do
      close(85,status='keep')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      end if
c ---------------------------------------------------------------
c     determine alternate parking location
c --------------------------------------------------------------- 
      if(avmdl.and.altpark) then
      write(*,55558)
55558 format(' Alternative Parking Location Model'/)
      if(debug) then
      open(259,file='altpark_model.csv',status='unknown',
     *                   form='formatted')
      write(259,55553)
55553 format('dest_zone,altzone,altcost')
      end if
      do iz=1,max_izones
      nk=mod(iz,100)
      if(nk.EQ.0.and.debug) WRITE(*,8001) iz,iequiv(iz)
 8001 FORMAT(' Processing Zone=',I5,' (',i5,')')
      type=1
      orgzn=iz
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
      index=0
      qprkcst=0.0
      qzone=0
      do jz=1,max_zones
      if((prkcst(jz).gt.0).and.(hdist(jz).lt.5.0)) then
      if(zhhd(6,iz).le.0.0.and.prkmdl) cycle
      if(prkcst(jz).lt.prkcst(iz)) then
      index=index+1
      qprkcst(index)=prkcst(jz)
      qzone(index)=jz
      if(index.eq.5) go to 600
      end if
      end if
      end do  
  600 continue
      if(debug.and.sdetail) write(26,55557) iequiv(iz),index,prkcst(iz)
55557 format(' iz=',i4,' index=',i1,' prkcst=',f8.2)
      if(index.eq.0) cycle
      do k=1,5
      if(debug.and.sdetail) write(26,55556) qzone(k),qprkcst(k)
55556 format(' qzone=',I4,' qprkcst=',F8.2)
      end do
      if(index.eq.1) then
      indval=1
      else
  500 call random(ranval)
      adjval=ranval*float(index)
      indval=ifix(adjval)  
      if(indval.le.0) go to 500
      end if
      aprkcst(iz)=qprkcst(indval)
      aprkzone(iz)=qzone(indval)
      if(debug.and.sdetail) then
      write(26,55555) ranval,adjval,indval,aprkcst(iz),
     *                iequiv(aprkzone(iz))
55555 format(' randval=',f6.2,' adjval=',f6.2,' indval=',i2,
     *       ' aprkcst=',f8.2,' aprkzone=',i4)
      end if
      printzn=0
      if(aprkzone(iz).gt.0) printzn=aprkzone(iz)
      if(debug) write(259,55554) iequiv(iz),printzn,aprkcst(iz)
55554 format(i4,',',i4,',',f8.2)
      end do
c
      close(85,status='keep')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      end if
      return
      end
