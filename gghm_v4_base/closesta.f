C***********************************************************
C     determine closest zone for each station              *
C***********************************************************
      subroutine closesta
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
      integer*2   ista,unit,type,orgzn,destzn
      integer*2   closezone(1000)
	    integer*4   iz,jz,jz2
	    real*4      hdist(4000)
	    real*4      srow(1000,1000)
	    real*4      prow(1000)
	    real*4      arow(1000,4000)
	    real*4      znsta(4000,1000)
	    real*4      mindist,minzone
c --------------------------------------------------------
c     store distance from zone to every station
c -------------------------------------------------------
      znsta=0.0
      closezone=0
      close(85,status='keep')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      do iz=1,max_izones
      type=1
      orgzn=iz
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
      do jz=(max_izones+1),max_zones
      jz2=jz-max_izones
      znsta(iz,jz2)=hdist(jz)
      end do
      end do
c ------------------------------------------------------------
c     determine closest zone for every TTC and GO Rail station
c ------------------------------------------------------------
      write(26,9001) 
 9001 format(/'            CLOSEST ZONE TO EACH STATION'/
     *       ' ----------------------------------------------------'//
     *       ' STATION             NAME                 DIST   ZONE'/
     *       ' ------- ------------------------------  ------  ----')
      do ista=1,max_stations
      mindist=99999.9
      minzone=0
      if(stanum(ista).eq.3) cycle
      if(stadata(ista,6).ne.1.0) cycle
      do jz=1,max_izones
      if(znsta(jz,ista).le.0.0) cycle
      if(znsta(jz,ista).lt.mindist) then
      mindist=znsta(jz,ista)
      minzone=jz
      end if
      end do
c....................................................................
      write(26,9002) iequiv(ista+max_izones),staname(ista),mindist,
     *               iequiv(minzone)
 9002 format(1x,i7,1x,a30,2x,f6.2,2x,i4)
c....................................................................
      closezone(ista)=minzone
      end do
      return
      end
