c --------------------------------------------------------------------
c    mfread -- read emme/4 binary matrix data and store in array form
c
c --------------------------------------------------------------------
      subroutine mfread(unit,type,orgzn,destzn,frow,srow,prow,arow)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
	    integer*2   unit,type,orgzn,destzn
	    integer*4   recno,rec1,iz,jz
	    real*4      frow(4000)
	    real*4      srow(1000,1000)
	    real*4      prow(1000)
	    real*4      arow(1000,4000)
c
c     initialize all arrays
c
      frow=0.0
      srow=0.0
      prow=0.0
      arow=0.0
c
      if(type.eq.1) then 
    1 read(unit,end=99) iz,frow
      if(iz.lt.orgzn) go to 1
      if(orgzn.ne.iz) then
      write(26,99992) orgzn,iz,unit
99992 format(' Type 1 -- Expecting Zone=',i4,' Encountered=',i4,
     *       ' for unit=',i3)
      stop 
      end if
      end if
c 
      if(type.eq.2) then 
      do i=1,max_zones
      read(unit,end=99) iz,frow
	    if(iz.le.max_izones) cycle
	    iz=i-max_izones
	    do j=(max_izones+1),max_zones
	    jz=j-max_izones
      srow(iz,jz)=frow(j)
      end do
      end do
      end if
c
      if(type.eq.3) then
    2 read(unit,end=99) iz,frow
      if(iz.lt.orgzn) go to 2
      if(orgzn.ne.iz) then
      write(26,99991) orgzn,iz,unit
99991 format(' Type 3 -- Expecting Zone=',i4,' Encountered=',i4,
     *       ' for unit=',i3)
      stop 
      end if
	    do j=(max_izones+1),max_zones
	    jz=j-max_izones
	    prow(jz)=frow(j)
	    end do
      end if
c
      if(type.eq.4) then 
      do i=1,max_zones
      read(unit,end=99) iz,frow
      if(iz.le.max_izones) cycle
      iz=i-max_izones
	    do j=1,max_izones
      arow(iz,j)=frow(j)
      end do
      end do
      end if
   99 return
      end
