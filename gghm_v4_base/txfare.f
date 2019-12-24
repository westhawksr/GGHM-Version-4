c
c     GO Rail <--> TTC Subway Fare Computaton
c
      subroutine txfare(cfare)
      include 'mlogitpar.inc'
      include 'param.inc'
      include 'dvalues.inc'
      include 'stadat.inc'
      real*4  cfare
c
c     adjust computed fare for transfers
c
      cfare=cfare+bfarettc*100.0
      cfare=cfare+ttcdscv*100.0
      cfare=(1.0-ttcdscf)*cfare
      return
      end
