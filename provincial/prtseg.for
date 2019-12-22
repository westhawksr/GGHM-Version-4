c
c     Station Line to Station Line Summary Subroutine
c
      subroutine prtseg(maxlne,tsum)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
c
      integer*2 lineno(20)
      integer*4 totlne,maxlne
      real*8    tsum(21,21)
      data      lineno/1,2,3,4,5,6,7,8,9,10,
     *                11,12,13,14,15,16,17,18,19,20/
      if(maxlne.le.3) then
      write(26,9001) maxlne
 9001 format(' PRTSEG 9001 (F) Maximum Line Number for GO Rail',
     *       ' or TTC Subway is Less than 4')
      STOP 9001
      end if
      goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) maxlne
    4 write(173,404) (lineno(k),k=1,maxlne)
      go to 21
    5 write(173,405) (lineno(k),k=1,maxlne)
      go to 21
    6 write(173,406) (lineno(k),k=1,maxlne)
      go to 21
    7 write(173,407) (lineno(k),k=1,maxlne)
      go to 21
    8 write(173,408) (lineno(k),k=1,maxlne)
      go to 21
    9 write(173,409) (lineno(k),k=1,maxlne)
      go to 21
   10 write(173,410) (lineno(k),k=1,maxlne)
      go to 21      
   11 write(173,411) (lineno(k),k=1,maxlne)
      go to 21 
   12 write(173,412) (lineno(k),k=1,maxlne)
      go to 21 
   13 write(173,413) (lineno(k),k=1,maxlne)
      go to 21     
   14 write(173,414) (lineno(k),k=1,maxlne)
      go to 21      
   15 write(173,415) (lineno(k),k=1,maxlne)
      go to 21 
   16 write(173,416) (lineno(k),k=1,maxlne)
      go to 21 
   17 write(173,417) (lineno(k),k=1,maxlne)
      go to 21         
   18 write(173,418) (lineno(k),k=1,maxlne)
      go to 21 
   19 write(173,419) (lineno(k),k=1,maxlne)
      go to 21 
   20 write(173,420) (lineno(k),k=1,maxlne)
   21 continue
      do k=1,maxlne
      goto (61,22,23,24,25,26,27,28,29,30,31,
     *      32,33,34,35,36,37,38,39,40) maxlne
   24 write(173,504) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   25 write(173,505) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   26 write(173,506) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   27 write(173,507) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   28 write(173,508) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   29 write(173,509) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   30 write(173,510) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   31 write(173,511) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   32 write(173,512) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   33 write(173,513) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   34 write(173,514) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   35 write(173,515) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41      
   36 write(173,516) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   37 write(173,517) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   38 write(173,518) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   39 write(173,519) k,(tsum(k,k1),k1=1,(maxlne+1))
      go to 41
   40 write(173,520) k,(tsum(k,k1),k1=1,(maxlne+1))
   41 continue
      end do 
      go to (61,42,43,44,45,46,47,48,49,50,51,52,
     *       53,54,55,56,57,58,59,60) maxlne   
   44 write(173,604) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   45 write(173,605) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   46 write(173,606) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   47 write(173,607) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   48 write(173,608) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61      
   49 write(173,609) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   50 write(173,610) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   51 write(173,611) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   52 write(173,612) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   53 write(173,613) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61         
   54 write(173,614) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   55 write(173,615) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   56 write(173,616) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   57 write(173,617) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   58 write(173,618) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61         
   59 write(173,619) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
      go to 61
   60 write(173,620) (tsum((maxlne+1),k1),k1=1,(maxlne+1))
   61 continue 
  404 format('Segment',4(',',i2),',','Total') 
  405 format('Segment',5(',',i2),',','Total')
  406 format('Segment',6(',',i2),',','Total')
  407 format('Segment',7(',',i2),',','Total')
  408 format('Segment',8(',',i2),',','Total')
  409 format('Segment',9(',',i2),',','Total')     
  410 format('Segment',10(',',i2),',','Total') 
  411 format('Segment',11(',',i2),',','Total')
  412 format('Segment',12(',',i2),',','Total')
  413 format('Segment',13(',',i2),',','Total')
  414 format('Segment',14(',',i2),',','Total')
  415 format('Segment',15(',',i2),',','Total')
  416 format('Segment',16(',',i2),',','Total')
  417 format('Segment',17(',',i2),',','Total')
  418 format('Segment',18(',',i2),',','Total')
  419 format('Segment',19(',',i2),',','Total')
  420 format('Segment',20(',',i2),',','Total')  
  504 format(i2,5(',',f12.2))
  505 format(i2,6(',',f12.2))
  506 format(i2,7(',',f12.2))
  507 format(i2,8(',',f12.2))
  508 format(i2,9(',',f12.2))
  509 format(i2,10(',',f12.2))
  510 format(i2,11(',',f12.2))
  511 format(i2,12(',',f12.2))
  512 format(i2,13(',',f12.2))
  513 format(i2,14(',',f12.2))
  514 format(i2,15(',',f12.2))
  515 format(i2,16(',',f12.2))
  516 format(i2,17(',',f12.2))
  517 format(i2,18(',',f12.2))
  518 format(i2,19(',',f12.2))
  519 format(i2,20(',',f12.2))
  520 format(i2,21(',',f12.2))
  604 format('Total',5(',',f12.2))
  605 format('Total',6(',',f12.2))
  606 format('Total',7(',',f12.2))
  607 format('Total',8(',',f12.2))
  608 format('Total',9(',',f12.2))
  609 format('Total',10(',',f12.2))
  610 format('Total',11(',',f12.2))
  611 format('Total',12(',',f12.2))
  612 format('Total',13(',',f12.2))
  613 format('Total',14(',',f12.2))
  614 format('Total',15(',',f12.2))
  615 format('Total',16(',',f12.2))
  616 format('Total',17(',',f12.2))
  617 format('Total',18(',',f12.2))
  618 format('Total',19(',',f12.2))
  619 format('Total',20(',',f12.2))
  620 format('Total',21(',',f12.2)) 
    1 continue
    2 continue
    3 continue
   22 continue
   23 continue
   42 continue
   43 continue
      return
      end
