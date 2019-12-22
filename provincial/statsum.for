c
c     Station Line to Station Line Summary Subroutine
c
      subroutine statsum(maxlne,orista,dessta,trips,tsum)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
      integer*4 totlne,maxlne
      integer*4 orista,dessta,origline,destline
      real*4    trips
      real*8    tsum(21,21)
c
c     store Rail Line Segment Trips
c  
      totlne=maxlne+1  
      origline=stadata(orista,8)
      destline=stadata(dessta,8)
      if(origline.le.maxgln.and.destline.le.maxgln.and.
     *   origline.gt.0.and.destline.gt.0) then
      tsum(origline,destline)=tsum(origline,destline) + trips
      tsum(totlne,destline)=tsum(totlne,destline) + trips
      tsum(origline,totlne)=tsum(origline,totlne) + trips
      tsum(totlne,totlne)=tsum(totlne,totlne) + trips
      end if
c
      return
      end
