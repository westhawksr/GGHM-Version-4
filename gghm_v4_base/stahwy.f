C***********************************************************
C     highway distances from station to pearson airport    *
C***********************************************************
      subroutine stahwy
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
      integer*2   ista,unit,type,orgzn,destzn
      integer*2   closezone(1000),zonesort(4000)
	    integer*4   iz,jz,jz2
	    real*4      hdist(4000)
	    real*4      srow(1000,1000)
	    real*4      prow(1000)
	    real*4      arow(1000,4000)
	    real*4      znsta(4000,1000)
	    real*4      mindist,minzone
      real*4      dairport,dunion,dttcunion,dtemp
      real*4      airdist(1000,2)
c --------------------------------------------------------
c     store distance from zone to every station
c -------------------------------------------------------
      znsta=0.0
      closezone=0
      zonesort=0
      airdist=0.0
      close(85,status='keep')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      do iz=1,max_izones
c.....drive alone highway distance
      type=1
      orgzn=iz
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
      do jz=(max_izones+1),max_zones
      jz2=jz-max_izones
      znsta(iz,jz2)=hdist(jz)
      end do
      end do
c ----------------------------------------------------------
c     determine closest zone for every TTC subway station
c ----------------------------------------------------------
      do ista=1,max_stations
      mindist=99999.9
      minzone=0
      if(stanum(ista).ne.2) cycle
      if(stadata(ista,6).ne.1.0) cycle
      do jz=1,max_izones
      if(znsta(jz,ista).le.0.0) cycle
      if(znsta(jz,ista).lt.mindist) then
      mindist=znsta(jz,ista)
      minzone=jz
      end if
      end do
c....................................................................
      if(ldebug) then
      write(26,8001) iequiv(ista+max_izones),mindist,iequiv(minzone)
 8001 format(i4,' mindist=',f8.2,' minzone=',i4)
      end if
c....................................................................
      closezone(ista)=minzone
      zonesort(minzone)=ista
      end do
      close(85,status='keep')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
c ---------------------------------------------------------------
c     compute distance direct to pearson
c     compute distance via Union to pearson
c --------------------------------------------------------------
      jz=equiv(4378)
      jz2=closezone(equiv(ttcunion)-max_izones)
      do iz=1,max_izones
      if(zonesort(iz).le.0) cycle
      type=1
      orgzn=iz
c.....drive alone highway distance
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
c.....obtain distances from station to Union and Pearson
      dairport=hdist(jz)
      dtemp=hdist(jz2)
c.....store data by station
      airdist(zonesort(iz),1)=dairport
      airdist(zonesort(iz),2)=dtemp
c.....check for additional stations with the same closest zone
      do ista=1,max_stations
      if(closezone(ista).eq.iz) then
      airdist(ista,1)=dairport
      airdist(ista,2)=dtemp
      end if
      end do
c...........................................................
      if(ldebug) then
      ista=zonesort(iz)+max_izones
      write(26,9001) iequiv(ista),staname(zonesort(iz)),
     *               iequiv(iz),dairport,dtemp
 9001 format(/' STATION COMPARATIVE DISTANCES'/
     *        ' -----------------------------'/
     *        ' TTC SUBWAY STATION                =',I8,1X,A35/
     *        ' CLOSEST ZONE                      =',I8/
     *        ' DIRECT DISTANCE TO PEARSON        =',F8.2/
     *        ' DISTANCE TO TTC UNION             =',F8.2)
      end if
c..............................................................
      end do
c.....determine Union to Pearson distance
      iz=equiv(ttcunion)-max_izones
      dttcunion=airdist(iz,1)
      if(ldebug) then
      write(26,9002) dttcunion
 9002 format(/' DISTANCE TTC UNION TO PEARSON     =',F8.2/)
      end if
c.....finalize station to pearson distances
      do ista=1,max_stations
      if(stanum(ista).ne.2) cycle
      if(stadata(ista,6).ne.1.0) cycle
      airdist(ista,2)=airdist(ista,2)+dttcunion
c...........................................................
      if(ldebug) then
      iz=ista+max_izones
      write(26,9003) iequiv(iz),staname(ista),
     *               airdist(ista,1),airdist(ista,2)
 9003 format(/' FINAL STATION DISTANCE VALUES'/
     *        ' -----------------------------'/
     *        ' TTC SUBWAY STATION                =',I8,1X,A35/
     *        ' DIRECT DISTANCE TO PEARSON        =',F8.2/
     *        ' DISTANCE TO PEARSON VIA UNION     =',F8.2)
      end if
c..............................................................
      end do
      return
      end
