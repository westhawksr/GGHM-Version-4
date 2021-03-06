C
C DECLARATIONS REQUIRED FOR STATION ATTRIBUTE DATA FILE
C
      REAL*8        STADATA(1000,17)
C
C  1= DAILY PARKING COST
C  2= TYPE (1=GO RAIL, 2=TTC SUBWAY, 3=GO BUS)
C  3= AVAILABLE PARKING CAPACITY (TOTAL CAPACITY - USED)
C  4= TOTAL PARKING CAPACITY
C  5= SHADOW PRICE
C  6= ACTIVE? (1=YES, 0=NO)
C  7= DRIVE ACCESS INDICATOR (1=PNR, 2=KNR, 0=NONE)
C  8= LINE NUMBER
C  9= ENTRANCE TO STATION PLATFORM TIME
C 10= OFF PEAK PARKING COST
C 11= STATION PNR WALK ACCESS TIME
C 12= STATION PNR DRIVE ACCESS TIME
C 13= STATION KNR WALK ACCESS TIME
C 14= STATION KNR DRIVE ACCESS TIME
C 15= FREE PARKING SPACES
C 16= PAID PARKING SPACES
C 17= USED PARKING SPACES
C 
      REAL*8        ZNEDIST(8,50)
      REAL*8        STADIST(8,50)
      REAL*4        STAVCR(1000,50)
      CHARACTER*37  STANAME(1000)
      INTEGER*2     STANUM(1000),STAIND(1000,4000)
      INTEGER*2     EQUIV3(1000,2),SEQUIV(1000)
C
C   STANUM= STATION TYPE (1=GO RAIL, 2=TTC SUBWAY, 3=GO BUS) 
C
C DEFINE COMMON
C
      COMMON/STASTR/STANAME
      COMMON/STALST/STANUM,STAIND,EQUIV3,SEQUIV
      COMMON/STADAT/STADATA,STADIST,ZNEDIST,STAVCR
