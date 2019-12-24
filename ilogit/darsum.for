c
c     Drive Access Ratio Summary Subroutine
c
      subroutine darsum(orista,jz,trips,hdist,hwydst,tlfsum)
      include 'ilogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
      integer*2 jz
      integer*4 orista,idar
      real*4    trips,hdist(max_stations),dar
      real*4    hwydst(max_izones)
      real*8    tlfsum(51)
c
c     compute drive access ratio
c      
      dar=hdist(orista)/hwydst(jz)
      idar=ifix(dar*10.0)+1
      idar=max0(idar,1)
      idar=min0(idar,51)
      tlfsum(idar)=tlfsum(idar)+trips
c
      return
      end
