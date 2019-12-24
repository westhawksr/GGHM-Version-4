C**********************************************************
C   SUBROUTINE RDFILES: OBTAIN VARIABLE NAMES             *
C**********************************************************
      SUBROUTINE RDFILES
      INCLUDE 'param.inc'
      INTEGER*4    IT,ST,TT,UT,VT,XT,LEN(2),RLEN
      CHARACTER*1  COMMA,BLANK
      CHARACTER*10 FILES(2)
      REAL*4       NAMES(2)
      CHARACTER*90 STRING,CNAMES(2)
      DATA         COMMA/','/,BLANK/' '/
      DATA         FILES/'DFAREGR','TEST'/
      DATA         NAMES/2*' '/
      DATA         LEN/6,6/
C
C SEARCH FOR ONE FILE NAME AT A TIME
C
      DO 100 IT=1,2
      REWIND 1
    1 READ(1,101,END=100) STRING
  101 FORMAT(A90)
      RLEN=90-LEN(IT)
      DO 200 ST=1,RLEN
      TT=ST+LEN(IT)
      IF(STRING(ST:TT).EQ.FILES(IT)) THEN
C
C NOW SEARCH FOR COMMA
C
      DO 300 UT=TT,90
      IF((STRING(UT:UT).EQ.COMMA).OR.(STRING(UT:UT).EQ.BLANK)) THEN
      VT=TT+2
      XT=UT-1
      READ(STRING(VT:XT),9012) NAMES(IT)
 9012 FORMAT(F8.0)
      CNAMES(IT)=STRING(VT:XT)
      GO TO 100
      END IF
  300 CONTINUE
      END IF
  200 CONTINUE
      GO TO 1
  100 CONTINUE
C
C SET FILE NAMES TO INPUT STRINGS RECEIVED
C
      DFAREGR=NAMES(1)
C     WRITE(*,11111) DFAREGR,CNAMES(1),VT,XT
11111 FORMAT(//' DFAREGR=',F8.4,' CNAMES(1)=',A40,' VT=',I3,' XT=',I3)
      RETURN
      END
