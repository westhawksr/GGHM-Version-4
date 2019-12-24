C-------------------------------------------------------------------
C     COMPUTE UBER UTILITY COMPONENTS SUBROUTINE
C-------------------------------------------------------------------
      SUBROUTINE UBERCOMP(iz,htime,hdist,coeffwait,coefftime,
     *                    ubercost,uberacc,kwait)
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
	    integer*2    iz
	    integer*4    yindex,zindex
	    real*4       coeffwait,coefftime
	    real*4       ranval,kwait,ubercost,uberacc
	    real*4       htime,hdist
C
	    ubercost=0.0
	    uberacc=0.0
      zindex=ifix(zhhd(7,iz))
      if(zindex.eq.0) zindex=1
C
C..OBTAIN WAIT TIME FROM RANDOM DRAW OF FREQUENCY DISTRIBUTION
C
  100 call random(ranval)
      yindex=ifix(ranval*1000.0)
      if(yindex.le.0) go to 100
      kwait=fdist(yindex,zindex)
C
      ubercost=2.50+0.18*htime+0.81*hdist+2.75
      uberacc=coeffwait*kwait + coefftime*htime
      return
      end
