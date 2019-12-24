C***********************************************************
C     SPECIAL EVENT MODEL DESTINATION CHOICE SIZE TERM     *
C***********************************************************
      SUBROUTINE SPTGEN
      include 'stadat.inc'
      include 'param.inc'
	    include 'mlogitpar.inc'
	    include 'dvalues.inc'
	    INTEGER*4 IZ
	    REAL*4    TOTPOP,TOTEMP,REGINC,TOTHLD,TOTINC4
	    TOTPOP=0.0
	    TOTEMP=0.0
	    TOTHLD=0.0
	    REGINC=0.0
	    TOTINC4=0.0
C
C     COMPUTE TOTAL REGIONAL POPULATION, EMPLOYMENT AND 
C     REGIONAL MEAN INCOME
C
      DO IZ=1,MAX_IZONES
      TOTPOP=TOTPOP+ZHHD(10,IZ)
      TOTEMP=TOTEMP+ZHHD(11,IZ)
      TOTHLD=TOTHLD+ZHHD(9,IZ)
      REGINC=REGINC+ZHHD(9,IZ)*ZHHD(8,IZ)
      END DO
      REGINC=REGINC/TOTHLD
      WRITE(26,9001) TOTPOP,TOTHLD,TOTEMP,REGINC
 9001 FORMAT(//' SPECIAL EVENT MODEL REGIONAL SUMMARY:'/
     *         ' -------------------------------------'/
     *         ' TOTAL POPULATION     =',F10.0/
     *         ' TOTAL HOUSEHOLDS     =',F10.0/
     *         ' TOTAL EMPLOYMENT     =',F10.0/
     *         ' REGIONAL MEAN INCOME =',F10.0/)
C
C     COMPUTE HOUSEHOLDS BY INCOME GROUP
C
      CALL INCOME(REGINC)
      DO IZ=1,MAX_IZONES
      TOTINC4=TOTINC4+ZHHD(14,IZ)
      END DO
C
C     COMPUTE ZONAL LEVEL PROPORTION OF REGIONAL POPULATION AND EMPLOYMENT
C
      DO IZ=1,MAX_IZONES
      ZHHD(12,IZ)=ZHHD(10,IZ)/TOTPOP
      ZHHD(13,IZ)=ZHHD(11,IZ)/TOTEMP
      ZHHD(15,IZ)=ZHHD(14,IZ)/TOTINC4
      ZHHD(16,IZ)=0.4456*ZHHD(12,IZ)+0.1265*ZHHD(13,IZ)+
     *            0.4635*ZHHD(15,IZ)
      ZHHD(16,IZ)=ZHHD(16,IZ)**0.9683
      END DO
      RETURN
      END
