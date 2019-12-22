c
c     UPX Station-to-Station Summary
c
      subroutine upxsum(orista,dessta,trips,upxsta,stasum6)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
      integer*4 orista,dessta
      real*4    trips,upxsta(max_stations,max_stations)
      real*8    stasum6(max_stations,2)
c
c     store upx station to station trips
c  
      if(upxind(orista,dessta).gt.0) then
      upxsta(orista,dessta)=upxsta(orista,dessta)+trips
      stasum6(orista,1)=stasum6(orista,1)+trips
      stasum6(dessta,2)=stasum6(dessta,2)+trips
      end if
c
      return
      end