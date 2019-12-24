c --------------------------------------------------------------------
c    mtxconv -- read and write emme/4 matrix data
c
c --------------------------------------------------------------------
      subroutine mtxconv(units,newunit)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      integer*2   unit,units(15),newunit
	    integer*2   iz,jz,t,time(4)
	    integer*4   recno,rec1
	    real*4      mfval,maxval,minval
	    real*4      row(max_izones,max_izones)
c
c     initialize all arrays
c
      row=0.0
      maxval=0.1E+21
      minval=-0.1E+21
      call gettim(time(1),time(2),time(3),time(4))
      write(*,100) (time(k),k=1,3)
  100 format(/'Starting Time: ',I2,':',I2,':',I2/)
      open(newunit,file='temp.bin',status='unknown',form='binary')
c
c     check number of zones and file size
c    
      recno=((mxzones-1)*mxzones)+mxzones
      read(units(1),rec=recno,err=98) mfval
      recno=recno+1
      read(units(1),rec=recno,err=99) mfval
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
      do t=1,15
      if(units(t).eq.0) cycle
      unit=units(t)
      do iz=1,max_izones
      row=0.0
	    do jz=1,max_izones
            rec1 = ((iz-1)*mxzones) + jz
            read(unit, rec=rec1) mfval
            if(mfval.gt.minval.and.mfval.lt.maxval) then
            row(iz,jz)=mfval
            end if
      end do
      write(newunit) iz,t,(row(iz,jz),jz=1,max_izones)
      end do
      end do
      call gettim(time(1),time(2),time(3),time(4))
      write(*,101) (time(k),k=1,3)
  101 format('Ending Time: ',I2,':',I2,':',I2/)
      return
      end
