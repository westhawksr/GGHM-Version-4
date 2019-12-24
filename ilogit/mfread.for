c --------------------------------------------------------------------
c    mfread -- read emme/4 matrix data and store in array form
c
c --------------------------------------------------------------------
      subroutine mfread(unit,type,orgzn,destzn,frow,srow,prow,arow)
      include 'ilogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
	    integer*2   unit,type,orgzn,destzn,iz,jz
	    integer*4   recno,rec1
	    real*4      mfval,maxval,minval
	    real*4      frow(max_izones)
	    real*4      srow(max_stations,max_stations)
	    real*4      prow(max_stations)
	    real*4      arow(max_stations,max_izones)
c
c     initialize all arrays
c
      frow=0.0
      srow=0.0
      prow=0.0
      arow=0.0
      maxval=0.1E+21
      minval=-0.1E+21
c
c     check number of zones and file size
c    
	    recno=((mxzones-1)*mxzones)+mxzones
	    read(unit,rec=recno,err=98) mfval
      recno=recno+1
      read(unit,rec=recno,err=99) mfval
      write(26,199) unit
      write(*,199)  unit
  199 format(//' mfread 99 (f) number of zones specified for the',
     *       ' emme/4 matrix on unit ',i3,' is less than file size'//)
      stop
   98 write(26,198) unit
      write(*,198)  unit
  198 format(//' mfread 98 (f) number of zones specified for the',
     *     ' emme/4 matrix on unit ',i3' is greater than file size'//)
      stop
   99 continue
      if(type.eq.1) then 
      i=orgzn
	    do j=1,max_izones
	          iz=i
	          jz=j
            rec1 = ((i-1)*mxzones) + j
            read(unit, rec=rec1) mfval
            if(mfval.gt.minval.and.mfval.lt.maxval) then
            frow(jz)=mfval
            end if
      end do
      end if 
      if(type.eq.2) then 
      do i=orgzn,destzn
	    do j=orgzn,destzn
	          iz=i-max_izones
	          jz=j-max_izones
            rec1 = ((i-1)*mxzones) + j
            read(unit, rec=rec1) mfval
            if(mfval.gt.minval.and.mfval.lt.maxval) then
c           write(26,94) iequiv(i),iequiv(j),mfval
c  94 format(' i=',i5,' j=',i5,' mfval=',f8.4)
            srow(iz,jz)=mfval
            end if
      end do
      end do
      end if
      if(type.eq.3) then 
      i=orgzn
	    do j=(max_izones+1),destzn
	          iz=i
	          jz=j-max_izones
            rec1 = ((i-1)*mxzones) + j
            read(unit, rec=rec1) mfval
            if(mfval.gt.minval.and.mfval.lt.maxval) then
            prow(jz)=mfval
            end if
      end do
      end if
      if(type.eq.4) then 
      do i=orgzn,destzn
	    do j=1,max_izones
	          iz=i-max_izones
	          jz=j
            rec1 = ((i-1)*mxzones) + j
            read(unit, rec=rec1) mfval
            if(mfval.gt.minval.and.mfval.lt.maxval) then
            arow(iz,jz)=mfval
            end if
      end do
      end do
      end if
      return
      end
