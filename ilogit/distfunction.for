c
c     Compute Distance Function
c
      subroutine distfunction(hwydist,value)
      include 'ilogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
      real*4  hwydist,value,a,b,c,d,e
      a=-0.12697
      b=-0.00232
      c=0.041322
      d=1.04068
      e=0.75591
c
      if(hwydist.le.0.0) hwydist=0.001
      value=a+b*hwydist+c*log(hwydist)+(d/hwydist)+(e/(hwydist*hwydist))
      return
      end
