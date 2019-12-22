c*********************************************************************
c  subroutine bucket: bucket rounds the input person trip matrices   *
c*********************************************************************
c
      subroutine bucket
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
	    integer*2   unit,type,orgzn,destzn
      integer*4   iz,jz,jjz,diz,djz
      integer*4   nonzero(6,2),pindex,nochange(6)
      real*4      mtx(4000),mout(max_izones),prct
	    real*4      srow(1000,1000)
	    real*4      prow(1000),cvalue,qvalue
	    real*4      arow(1000,4000)
      real*4      hdist(4000)
      real*8      tesminp(6),tesmout(6),rem,srem,arem
      real*8      ptrip(25,6,2),intra(6)
      real*8      pertrp(50,50,3)
      character*13 pname(3)
      data        pname/'Observed','Estimated','Est-Obs'/
      nochange=0
c
c     open output file
c
      open(140,file=fbucket,status='unknown',form='binary')
      if(calib) open(160,file='..\outputs\person_trip.csv',
     *         status='unknown',
     *         form='formatted')
 7005 format(/' Person Trip Matrix Bucket Rounding'/
     *        ' ----------------------------------'/)
      write(*,7005)
      intra=0.0
c ---------------------------------------------------------------
c     Origin Zone Loop
c ---------------------------------------------------------------
      do iz=1,max_izones
      if(.not.ioi(iz)) cycle
      nk=mod(iz,1000)
      if(nk.EQ.0) WRITE(*,8043) iz,iequiv(iz)
 8043 FORMAT(' Processing Origin Zone=',I5,' (',i5,')')
      type=1
      orgzn=iz
      destzn=1
      unit=85
      call mfread(unit,type,orgzn,destzn,hdist,srow,prow,arow)
c -------------------------------------------------
c     Market Segment Loop
c -------------------------------------------------
      do k=1,ncats
c
      if(ncats.lt.k) cycle
      unit=33+k
      call mfread(unit,type,orgzn,destzn,mtx,srow,prow,arow)
c
c     write(26,7001) iz,k
 7001 format(' iz=',i5,' k=',i1)
c
      rem=0.0
      srem=0.0
      mout=0.0
      arem=0.0
c ----------------------------------------------------------------
c     Destination Zone Loop
c ----------------------------------------------------------------
      do jz=1,max_izones
      diz=iequiv(iz)
      djz=iequiv(jz)
      diz=dequiv(diz)
      djz=dequiv(djz)
      if(diz.gt.0.and.djz.gt.0) then
      pertrp(diz,djz,1)=pertrp(diz,djz,1)+mtx(jz)
      pertrp(31,djz,1)=pertrp(31,djz,1)+mtx(jz)
      pertrp(diz,31,1)=pertrp(diz,31,1)+mtx(jz)
      pertrp(31,31,1)=pertrp(31,31,1)+mtx(jz)
      end if
      if(iz.eq.jz) then
      mout(jz)=mtx(jz)
      tesminp(k)=tesminp(k)+mtx(jz)
      cycle
      end if
      pindex=ifix(hdist(jz)/5.0)+1
      if(pindex.gt.25) pindex=25
      ptrip(pindex,k,1)=ptrip(pindex,k,1)+mtx(jz)
      tesminp(k)=tesminp(k)+mtx(jz) 
      if(mtx(jz).lt.tvalue(k)) then
       qvalue=tvalue(k)*0.20
       if((pindex.gt.pvalue(k)).and.(mtx(jz).lt.qvalue)) then
       mout(jz)=mtx(jz)
       srem=srem+mtx(jz)
       go to 1000  
       end if
      rem=rem+mtx(jz)
      else
      mout(jz)=mtx(jz)
      srem=srem+mtx(jz)
      end if
 1000 continue
      end do
      if(sdetail) write(26,7002) iz,k,rem,srem,arem,tesminp(k)
c
      if(srem.gt.0) then
      arem=(rem+srem)/srem
      else
       arem=1.0
       rem=0.0
       srem=0.0
       cvalue=tvalue(k)*lvalue(k)
       do jz=1,max_izones
        if(iz.eq.jz) cycle
        if(mtx(jz).lt.cvalue) then
        rem=rem+mtx(jz)
        else
        mout(jz)=mtx(jz)
        srem=srem+mtx(jz)
        end if
       end do
      if(sdetail) write(26,7002) iz,k,rem,srem,arem,tesminp(k)
c
       if(srem.gt.0) then
       arem=(rem+srem)/srem
       else
       arem=1.0
       do jz=1,max_izones
       mout(jz)=mtx(jz)
       if(mtx(jz).gt.0) nochange(k)=nochange(k)+1
       end do
       end if
      end if
      if(sdetail) write(26,7002) iz,k,rem,srem,arem,tesminp(k)
 7002 format(' iz=',i4,' k=',i1,' rem=',f8.2,' srem=',f8.2,
     *       ' arem=',f10.4,' tesminp=',f10.2)
C
C     OUTPUT BUCKET ROUNDED TRIP MATRIX
C
      do jz=1,max_izones
      diz=iequiv(iz)
      djz=iequiv(jz)
      diz=dequiv(diz)
      djz=dequiv(djz)
      if(iz.eq.jz) then
      intra(k)=intra(k)+mout(jz)
      tesmout(k)=tesmout(k)+mout(jz)
      if(diz.gt.0.and.djz.gt.0) then
      pertrp(diz,djz,2)=pertrp(diz,djz,2)+mout(jz)
      pertrp(31,djz,2)=pertrp(31,djz,2)+mout(jz)
      pertrp(diz,31,2)=pertrp(diz,31,2)+mout(jz)
      pertrp(31,31,2)=pertrp(31,31,2)+mout(jz)
      end if
      cycle
      end if 
      mout(jz)=mout(jz)*arem
      tesmout(k)=tesmout(k)+mout(jz)
      if(diz.gt.0.and.djz.gt.0) then
      pertrp(diz,djz,2)=pertrp(diz,djz,2)+mout(jz)
      pertrp(31,djz,2)=pertrp(31,djz,2)+mout(jz)
      pertrp(diz,31,2)=pertrp(diz,31,2)+mout(jz)
      pertrp(31,31,2)=pertrp(31,31,2)+mout(jz)
      end if
      if(mtx(jz).gt.0) nonzero(k,1)=nonzero(k,1)+1
      if(mout(jz).gt.0) nonzero(k,2)=nonzero(k,2)+1
      pindex=ifix(hdist(jz)/5.0)+1
      if(pindex.gt.25) pindex=25
      ptrip(pindex,k,2)=ptrip(pindex,k,2)+mout(jz)
      end do
      if(.not.ptest) write(140) iz,k,(mout(jjz),jjz=1,max_izones)
      if(sdetail) write(26,7003) k,tesmout(k),intra(k)
 7003 format(' k=',i1,' tesmout=',f8.2,' intra=',f8.2)
      end do                                 !Market Segmentation Loop End
      end do                                 !Origin Zone Loop End
      do k=1,ncats
      prct=(float(nonzero(k,2))/float(nonzero(k,1)))*100.0
      if(ptest) write(26,7004) k,tesminp(k),tesmout(k),nonzero(k,1),
     *          nonzero(k,2),prct,tvalue(k),intra(k),nochange(k)
 7004 format(' k=',i1,' tesminp=',f10.1,' tesmout=',f10.1,
     *         ' input pairs=',i10,' output pairs=',i10,
     *         '  (',f5.1,' %)',' tvalue=',f5.2,' intra=',f10.1,
     *         ' nochange=',i10)
      end do
      close(140,status='keep') 
      open(140,file=fbucket,status='old',form='binary')  
      if(calib) then
      open(146,file='..\outputs\ptrip_tlf.csv',status='unknown',
     *         form='formatted')
      write(146,7007)
 7007 format('market,index,original,bucket')
      do k=1,ncats
      do k1=1,25
      k2=k1*5
      write(146,7006) k,k2,ptrip(k1,k,1),ptrip(k1,k,2)
 7006 format(i2,',',i4,2(',',f12.2))
      end do
      end do
      end if
C.... PERSON TRIP MATRIX COMPARISON
      if(calib) then
      do k1=1,(maxpd+1)
      do k2=1,(maxpd+1)
      pertrp(k1,k2,3)=pertrp(k1,k2,2)-pertrp(k1,k2,1)
      end do
      end do
      DO K2=1,3
      WRITE(160,9047) pname(k2),(dname(k),k=1,(maxpd+1))
 9047 FORMAT(A13,31(',',A35))
      DO K=1,(MAXPD+1)
      WRITE(160,9016) DNAME(K),(pertrp(K,K1,K2),K1=1,(MAXPD+1))
 9016 FORMAT(A35,31(',',F10.2))
      END DO    
      END DO 
      end if
      if(ptest) then
      close(140,status='delete')
      stop
      end if
      close(85,status='keep')
      open(85,file=fhwy0(2),
     *       status='old',form='binary')
      close(34,status='keep')
      open(34,file=ftotper(1),
     *       status='old',form='binary')
      if(ncats.gt.1) then
      close(35,status='keep')
      close(36,status='keep')
      close(37,status='keep')
      close(38,status='keep')
      close(39,status='keep')      
      open(35,file=ftotper(2),
     *       status='old',form='binary')
      open(36,file=ftotper(3),
     *       status='old',form='binary')
      open(37,file=ftotper(4),  
     *       status='old',form='binary')
      open(38,file=ftotper(5),
     *       status='old',form='binary')
      open(39,file=ftotper(6),
     *       status='old',form='binary')
      end if
      return
      end
